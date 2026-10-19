(function () {
  'use strict';
  const root = document.getElementById('settings');
  const status = document.getElementById('status');
  const filter = document.getElementById('filter');
  const changedOnly = document.getElementById('changed-only');
  const HEADERS = { 'Content-Type': 'application/json', 'X-PAI-Client': 'pwa' };
  let rows = [];

  // Text size, kept on this device.
  function fontControls() {
    const size = window.paiShell.getTextSize();
    document.getElementById('font-size').textContent = size + ' px';
    document.getElementById('font-smaller').disabled = size <= 12;
    document.getElementById('font-larger').disabled = size >= 22;
  }
  for (const [id, delta] of [['font-smaller', -1], ['font-larger', 1], ['font-reset', 0]]) {
    document.getElementById(id).onclick = () => {
      window.paiShell.setTextSize(delta ? window.paiShell.getTextSize() + delta : 14);
      fontControls();
    };
  }
  window.addEventListener('pai-text-size', fontControls);
  fontControls();

  function show(text, bad) { status.textContent = text; status.className = 'status' + (bad ? ' error' : ''); }
  function same(a, b) { return JSON.stringify(a) === JSON.stringify(b); }

  const b64u = buf => btoa(String.fromCharCode(...new Uint8Array(buf))).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
  const unb64u = s => Uint8Array.from(atob(s.replace(/-/g, '+').replace(/_/g, '/') + '==='.slice((s.length + 3) % 4)), c => c.charCodeAt(0));

  async function call(method, path, body) {
    const response = await fetch(path, { method, headers: HEADERS, credentials: 'same-origin', cache: 'no-store',
                                         body: body === undefined ? undefined : JSON.stringify(body) });
    const json = await response.json().catch(() => ({}));
    return { ok: response.ok, status: response.status, json };
  }

  // A fresh passkey check, the same one a sign-in does, to confirm this change is yours.
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

  async function change(name, value) {
    let result = await call('POST', '/v1/settings', { name, value, reason: 'changed on the settings page' });
    if (result.status === 403 && result.json.step_up) {
      show('Confirm with your passkey…');
      await stepUp();
      result = await call('POST', '/v1/settings', { name, value, reason: 'changed on the settings page' });
    }
    if (!result.ok) throw new Error(result.json.error || ('Not saved (' + result.status + ').'));
  }

  // One editor per kind of value. read() returns the value to save, or throws a message.
  function editorFor(row) {
    if (row.type === 'boolean') {
      const el = document.createElement('select');
      for (const v of [true, false]) { const o = document.createElement('option'); o.value = String(v); o.textContent = String(v); el.appendChild(o); }
      el.value = String(row.value === true);
      return { el, read: () => el.value === 'true' };
    }
    if (Array.isArray(row.one_of)) {
      const el = document.createElement('select');
      for (const v of row.one_of) { const o = document.createElement('option'); o.value = String(v); o.textContent = String(v); el.appendChild(o); }
      el.value = String(row.value);
      return { el, read: () => (typeof row.one_of[0] === 'number' ? Number(el.value) : el.value) };
    }
    if (row.type === 'integer' || row.type === 'number') {
      const el = document.createElement('input');
      el.type = 'number';
      el.step = row.type === 'integer' ? '1' : 'any';
      if (row.min !== null) el.min = row.min;
      if (row.max !== null) el.max = row.max;
      el.value = row.value;
      return { el, read: () => {
        if (el.value.trim() === '') throw new Error('Enter a number.');
        return row.type === 'integer' ? Number.parseInt(el.value, 10) : Number(el.value);
      } };
    }
    if (row.type === 'string') {
      const long = typeof row.value === 'string' && (row.value.length > 60 || row.value.includes('\n'));
      const el = document.createElement(long ? 'textarea' : 'input');
      if (long) el.className = 'jsonbox'; else el.type = 'text';
      el.value = row.value;
      return { el, read: () => el.value };
    }
    const el = document.createElement('textarea');
    el.className = 'jsonbox';
    el.spellcheck = false;
    el.value = JSON.stringify(row.value, null, 1);
    return { el, read: () => {
      try { return JSON.parse(el.value); } catch (error) { throw new Error('That is not valid JSON: ' + error.message); }
    } };
  }

  function card(row) {
    const node = document.createElement('div');
    node.className = 'setting';
    const info = document.createElement('div');
    const name = document.createElement('div'); name.className = 'name'; name.textContent = row.name;
    const description = document.createElement('div'); description.className = 'description'; description.textContent = row.description;
    const meta = document.createElement('div'); meta.className = 'meta';
    const limits = [];
    if (row.min !== null) limits.push('at least ' + row.min);
    if (row.max !== null) limits.push('at most ' + row.max);
    meta.textContent = row.type + (limits.length ? ' · ' + limits.join(', ') : '')
      + (row.changed_by === 'operator' ? '' : ' · the agent may change this within bounds');
    info.append(name, description, meta);

    const editor = editorFor(row);
    const save = document.createElement('button'); save.textContent = 'Save'; save.disabled = true;
    const reset = document.createElement('button'); reset.textContent = 'Use default';
    reset.disabled = same(row.value, row.default);
    const dirty = () => { try { return !same(editor.read(), row.value); } catch (e) { return true; } };
    editor.el.addEventListener('input', () => { save.disabled = !dirty(); });
    editor.el.addEventListener('change', () => { save.disabled = !dirty(); });
    async function apply(value) {
      save.disabled = true; reset.disabled = true;
      show('Saving ' + row.name + '…');
      try {
        await change(row.name, value);
        await load();
        show(row.name + ' saved.');
      } catch (error) {
        show(error.name === 'NotAllowedError' ? 'Cancelled.' : error.message, error.name !== 'NotAllowedError');
        save.disabled = !dirty(); reset.disabled = same(row.value, row.default);
      }
    }
    save.onclick = () => { try { apply(editor.read()); } catch (error) { show(error.message, true); } };
    reset.onclick = () => apply(row.default);
    const buttons = document.createElement('div'); buttons.className = 'row'; buttons.append(save, reset);
    const edit = document.createElement('div'); edit.append(editor.el, buttons);

    const effective = document.createElement('div'); effective.className = 'effective';
    effective.textContent = row.changed ? 'Changed from the default: ' + JSON.stringify(row.default) : 'At its default';
    node.append(info, edit, effective);
    return node;
  }

  function render() {
    const words = filter.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
    const shown = rows.filter(row => {
      if (changedOnly.checked && !row.changed) return false;
      const text = (row.name + ' ' + row.group + ' ' + row.description).toLowerCase();
      return words.every(w => text.includes(w));
    });
    root.replaceChildren();
    let group = null;
    for (const row of shown) {
      if (row.group !== group) {
        group = row.group;
        const heading = document.createElement('div'); heading.className = 'group'; heading.textContent = group;
        root.appendChild(heading);
      }
      root.appendChild(card(row));
    }
    if (!shown.length) show('No setting matches.');
    else show(shown.length + ' of ' + rows.length + ' settings' + (changedOnly.checked ? ' (changed only)' : ''));
  }

  async function load() {
    const result = await call('GET', '/v1/settings');
    if (!result.ok) throw new Error(result.json.error || ('Could not load settings (' + result.status + ').'));
    rows = result.json.settings;
    render();
  }

  filter.addEventListener('input', render);
  changedOnly.addEventListener('change', render);
  load().catch(error => show(error.message, true));
})();
