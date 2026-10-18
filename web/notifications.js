'use strict';
const statusEl = document.getElementById('status');
const stateEl = document.getElementById('push-state');
const listEl = document.getElementById('subscriptions');
const policyEl = document.getElementById('policy');
const HEADERS = { 'Content-Type': 'application/json', 'X-PAI-Request': 'same-origin' };

function say(text) { statusEl.textContent = text; }

async function api(path, options) {
  const response = await fetch(path, Object.assign({ credentials: 'same-origin', cache: 'no-store' }, options));
  const body = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(body.error || ('Request failed (' + response.status + ')'));
  return body;
}

function keyBytes(base64url) {
  const padded = base64url.replace(/-/g, '+').replace(/_/g, '/') + '='.repeat((4 - base64url.length % 4) % 4);
  return Uint8Array.from(atob(padded), c => c.charCodeAt(0));
}

async function refresh() {
  const info = await api('/api/notifications/push/key');
  const supported = 'serviceWorker' in navigator && 'PushManager' in window;
  stateEl.textContent = supported
    ? (Notification.permission === 'granted' ? 'Permission granted.' : 'Permission: ' + Notification.permission + '.')
    : 'This browser does not support Web Push. On iOS, install the app to the home screen first.';
  if (!info.subject_set) stateEl.textContent += ' The VAPID contact is not set in the policy yet.';
  const subs = await api('/api/notifications/push/subscriptions');
  listEl.textContent = '';
  for (const item of subs.subscriptions) {
    const li = document.createElement('li');
    li.textContent = item.host + ' (added ' + new Date(item.created_at * 1000 - 2208988800000).toLocaleDateString() + ')';
    const revoke = document.createElement('button');
    revoke.textContent = 'Revoke';
    revoke.addEventListener('click', async () => {
      try {
        await api('/api/notifications/push/subscriptions', { method: 'POST', headers: HEADERS, body: JSON.stringify({ unsubscribe: item.id }) });
        const registration = await navigator.serviceWorker.getRegistration();
        const current = registration && await registration.pushManager.getSubscription();
        if (current) await current.unsubscribe();
        say('Revoked.'); refresh();
      } catch (error) { say(error.message); }
    });
    li.appendChild(revoke);
    listEl.appendChild(li);
  }
  policyEl.value = JSON.stringify(await api('/api/notifications/policy'), null, 2);
}

document.getElementById('enable').addEventListener('click', async () => {
  try {
    if (!('serviceWorker' in navigator) || !('PushManager' in window)) throw new Error('Web Push is not available in this browser.');
    const permission = await Notification.requestPermission();
    if (permission !== 'granted') throw new Error('Notification permission was not granted.');
    const info = await api('/api/notifications/push/key');
    const registration = await navigator.serviceWorker.register('/service-worker.js');
    await navigator.serviceWorker.ready;
    const subscription = await registration.pushManager.subscribe({
      userVisibleOnly: true, applicationServerKey: keyBytes(info.public_key)
    });
    await api('/api/notifications/push/subscriptions', { method: 'POST', headers: HEADERS, body: JSON.stringify({ endpoint: subscription.endpoint }) });
    say('Notifications enabled on this device.'); refresh();
  } catch (error) { say(error.message); }
});

function addLine(list, text) {
  const li = document.createElement('li');
  li.textContent = text;
  list.appendChild(li);
}

function ago(seconds) {
  const delta = Math.max(0, Math.round(Date.now() / 1000 + 2208988800 - seconds));
  return delta < 90 ? delta + ' s ago' : delta < 5400 ? Math.round(delta / 60) + ' min ago' : Math.round(delta / 3600) + ' h ago';
}

async function refreshState() {
  const state = await api('/api/notifications/state');
  document.getElementById('state-summary').textContent =
    'Notifications are ' + (state.policy_enabled ? 'ON' : 'OFF') + ' (policy revision ' + state.policy_revision + '). ' +
    (state.quiet_now ? 'Quiet hours are in effect. ' : '') +
    'Sent in the last hour: ' + state.sent_last_hour + ' of ' + state.limits.per_hour + '; today: ' + state.sent_last_day +
    ' of ' + state.limits.per_day + '. Held: ' + state.held + '.';
  const list = document.getElementById('channel-state');
  list.textContent = '';
  const terminal = state.channels['web-terminal'], push = state.channels['web-push'], telegram = state.channels.telegram;
  addLine(list, 'Terminal: ' + (terminal.enabled ? 'enabled' : 'DISABLED'));
  let pushText = 'Push: ' + (push.enabled ? 'enabled' : 'DISABLED') + '; ' + push.subscriptions + ' device(s) subscribed; ' +
    'contact ' + (push.contact_set ? 'set (' + push.contact_kind + ')' : 'NOT SET') + '; key ' + (push.vapid_key_present ? 'present' : 'missing');
  if (push.last_result) {
    pushText += '; last push ' + ago(push.last_result.at) + ': ' + push.last_result.reason +
      (push.last_result.status_classes.length ? ' (' + push.last_result.status_classes.join(', ') + ')' : '');
  } else { pushText += '; no push sent since the app started'; }
  addLine(list, pushText);
  addLine(list, 'Telegram: ' + (telegram.enabled ? 'enabled' : 'disabled') +
    (telegram.token_stored ? '; token stored' : '; no token') + (telegram.paired ? '; paired' : '; not paired'));
  const problems = document.getElementById('problems');
  problems.textContent = '';
  for (const problem of state.problems) addLine(problems, problem.text);
  const needsEnable = state.problems.some(p => p.code === 'push-disabled-with-subscriptions');
  document.getElementById('enable-push-delivery').hidden = !needsEnable;
  // Always present: the push contact is required before any push can be sent.
  document.getElementById('contact-state').textContent = push.contact_set
    ? 'A push contact is set. Enter a new one below to replace it.'
    : 'No push contact is set, so push cannot be sent. Enter a mailto: address or an https: address (a throwaway address is fine). It is only given to the push service so it can reach you if something is wrong.';
}

document.getElementById('contact-save').addEventListener('click', async () => {
  const field = document.getElementById('contact');
  const contact = field.value.trim();
  if (!/^(mailto:[^\s@]+@[^\s@]+|https:\/\/[^\s]+)$/i.test(contact)) {
    say('The contact must start with mailto: followed by an address, or with https://. Nothing was saved.');
    return;
  }
  try {
    await api('/api/notifications/policy', { method: 'POST', headers: HEADERS, body: JSON.stringify({ web_push_subject: contact }) });
    field.value = '';
    say('Push contact saved. Send a test to check it.');
    refresh(); refreshState();
  } catch (error) { say('The contact was not saved: ' + error.message); }
});

document.getElementById('enable-push-delivery').addEventListener('click', async () => {
  try {
    // A partial body: the server merges it onto the current policy.
    const change = { enabled: true, channels: { 'web-push': { enabled: true } } };
    const contact = document.getElementById('contact').value.trim();
    if (contact) change.web_push_subject = contact;
    await api('/api/notifications/policy', { method: 'POST', headers: HEADERS, body: JSON.stringify(change) });
    say('Push delivery enabled. Send a test to check it.');
    refresh(); refreshState();
  } catch (error) { say(error.message); }
});

document.getElementById('test').addEventListener('click', async () => {
  try {
    const result = await api('/api/notifications/test', { method: 'POST', headers: HEADERS, body: '{}' });
    say('Test: ' + result.reason + '.');
    const list = document.getElementById('test-result');
    list.textContent = '';
    for (const [name, channel] of Object.entries(result.channels)) {
      addLine(list, name + ': ' + channel.result + ' (' + channel.detail + ')');
    }
    refreshState();
  } catch (error) { say(error.message); }
});

document.getElementById('save').addEventListener('click', async () => {
  try {
    const policy = JSON.parse(policyEl.value);
    await api('/api/notifications/policy', { method: 'POST', headers: HEADERS, body: JSON.stringify(policy) });
    say('Policy saved.'); refresh();
  } catch (error) { say(error.message); }
});

refresh().catch(error => say(error.message));
refreshState().catch(error => say(error.message));

async function refreshTelegram() {
  const state = await api('/api/notifications/telegram');
  document.getElementById('telegram-state').textContent =
    'Token ' + (state.token_stored ? 'stored' : 'not stored') + '; pairing ' + (state.pairing_armed ? 'armed' : 'not armed') +
    (state.pairing_locked ? ' (locked: run /telegram-setup again)' : '') + '; ' + state.allowlist_size + ' allowlisted id(s).';
}

document.getElementById('telegram-save').addEventListener('click', async () => {
  const field = document.getElementById('telegram-token');
  try {
    await api('/api/notifications/telegram', { method: 'POST', headers: HEADERS, body: JSON.stringify({ token: field.value.trim() }) });
    field.value = '';
    say('Token stored. Message your bot now, then enter the code it sends you with /telegram-pair CODE in the chat.');
    refreshTelegram();
  } catch (error) { say(error.message); }
});

document.getElementById('telegram-remove').addEventListener('click', async () => {
  try {
    await api('/api/notifications/telegram', { method: 'POST', headers: HEADERS, body: JSON.stringify({ remove: true }) });
    say('Token removed.'); refreshTelegram();
  } catch (error) { say(error.message); }
});

refreshTelegram().catch(error => say(error.message));

// Messages kept in shadow mode, for the operator to rate.
async function refreshShadow() {
  const section = document.getElementById('shadow-review');
  const list = document.getElementById('shadow-list');
  let body;
  try { body = await api('/api/notifications/shadow'); } catch (error) { return; }
  section.hidden = !(body.shadow || body.messages.length);
  list.textContent = '';
  if (!body.messages.length) {
    const li = document.createElement('li');
    li.className = 'muted';
    li.textContent = 'Nothing kept yet.';
    list.appendChild(li);
  }
  for (const m of body.messages) {
    const li = document.createElement('li');
    const when = document.createElement('div');
    when.className = 'when';
    when.textContent = new Date(m.at).toLocaleString() + ' · ' + m.topic + ' · importance ' + m.importance;
    const summary = document.createElement('div');
    summary.textContent = m.summary;
    li.append(when, summary);
    if (m.message && m.message !== m.summary) {
      const more = document.createElement('div');
      more.className = 'muted';
      more.style.whiteSpace = 'pre-wrap';
      more.textContent = m.message;
      li.appendChild(more);
    }
    const rate = document.createElement('div');
    rate.className = 'rate';
    for (const [value, label] of [['useful', 'Useful'], ['noise', 'Noise'], ['wrong', 'Wrong']]) {
      const b = document.createElement('button');
      b.textContent = label;
      if (m.rating === value) b.className = 'chosen';
      b.addEventListener('click', async () => {
        try {
          await api('/api/notifications/shadow', { method: 'POST', headers: HEADERS, body: JSON.stringify({ id: m.id, rating: value }) });
          refreshShadow();
        } catch (error) { say(error.message); }
      });
      rate.appendChild(b);
    }
    li.appendChild(rate);
    list.appendChild(li);
  }
}
refreshShadow();
