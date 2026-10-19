'use strict';
const statusEl = document.getElementById('status');
const HEADERS = { 'Content-Type': 'application/json', 'X-PAI-Client': 'pwa' };
// The app keeps these itself: connected accounts, peers' shared keys and the push signing key.
const MANAGED = ['google-', 'fleet-peer-', 'web-push-'];

function say(text) { statusEl.textContent = text; }

const b64u = buf => btoa(String.fromCharCode(...new Uint8Array(buf))).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
const unb64u = s => Uint8Array.from(atob(s.replace(/-/g, '+').replace(/_/g, '/') + '==='.slice((s.length + 3) % 4)), c => c.charCodeAt(0));

async function call(method, path, body) {
  const response = await fetch(path, { method, headers: HEADERS, credentials: 'same-origin', cache: 'no-store', body: body ? JSON.stringify(body) : undefined });
  const json = await response.json().catch(() => ({}));
  return { ok: response.ok, status: response.status, json };
}

// A fresh passkey check: the same one a sign-in does, to confirm this change is yours.
async function stepUp() {
  const challenge = (await call('POST', '/v1/auth/challenge', { purpose: 'step-up' })).json;
  const cred = await navigator.credentials.get({ publicKey: {
    challenge: unb64u(challenge.challenge), rpId: challenge.rp_id, userVerification: 'required', timeout: 60000 } });
  const done = await call('POST', '/v1/auth/step-up', {
    credential_id: b64u(cred.rawId), challenge: challenge.challenge,
    client_data_json: b64u(cred.response.clientDataJSON), authenticator_data: b64u(cred.response.authenticatorData),
    signature: b64u(cred.response.signature) });
  if (!done.ok) throw new Error(done.json.error || 'The passkey check failed.');
}

async function change(path, body) {
  let result = await call('POST', path, body);
  if (result.status === 403 && result.json.step_up) {
    say('Confirm with your passkey…');
    await stepUp();
    result = await call('POST', path, body);
  }
  if (!result.ok) throw new Error(result.json.error || ('Not done (' + result.status + ').'));
  return result.json;
}

function row(secret) {
  const node = document.createElement('div');
  node.className = 'item';
  const text = document.createElement('div');
  const name = document.createElement('div');
  name.className = 'name';
  name.textContent = secret.name;
  const meta = document.createElement('div');
  meta.className = 'meta';
  meta.textContent = 'for ' + secret.hosts.join(', ') + ' · set ' + new Date(secret.updated_at).toLocaleDateString();
  text.append(name, meta);
  node.appendChild(text);
  if (MANAGED.some(prefix => secret.name.startsWith(prefix))) {
    const managed = document.createElement('div');
    managed.className = 'managed';
    managed.textContent = 'managed by the app';
    node.appendChild(managed);
  } else {
    const remove = document.createElement('button');
    remove.textContent = 'Remove';
    remove.addEventListener('click', async () => {
      if (!confirm('Remove ' + secret.name + '? Anything that uses it stops working until it is added again.')) return;
      try { await change('/v1/secrets/remove', { name: secret.name }); say('Removed ' + secret.name + '.'); refresh(); }
      catch (error) { say(error.name === 'NotAllowedError' ? 'Cancelled.' : error.message); }
    });
    node.appendChild(remove);
  }
  return node;
}

async function refresh() {
  const result = await call('GET', '/v1/secrets');
  const list = document.getElementById('list');
  list.textContent = '';
  if (!result.ok) { say(result.json.error || 'Could not load.'); return; }
  const secrets = result.json.secrets;
  if (!secrets.length) {
    const none = document.createElement('p');
    none.className = 'muted';
    none.textContent = 'No secrets yet.';
    list.appendChild(none);
  }
  for (const secret of secrets) list.appendChild(row(secret));
}

document.getElementById('add').addEventListener('submit', async event => {
  event.preventDefault();
  const name = document.getElementById('f-name').value.trim();
  const hosts = document.getElementById('f-hosts').value.split(',').map(h => h.trim().toLowerCase()).filter(Boolean);
  const value = document.getElementById('f-value').value;
  const button = document.getElementById('f-save');
  button.disabled = true;
  try {
    await change('/v1/secrets', { name, hosts, value });
    document.getElementById('f-value').value = '';
    document.getElementById('f-name').value = '';
    document.getElementById('f-hosts').value = '';
    say('Saved ' + name + '. The value is not shown again.');
    refresh();
  } catch (error) { say(error.name === 'NotAllowedError' ? 'Cancelled.' : error.message); }
  button.disabled = false;
});

refresh().catch(error => say(error.message));
