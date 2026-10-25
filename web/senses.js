'use strict';
const statusEl = document.getElementById('status');
const HEADERS = { 'Content-Type': 'application/json', 'X-PAI-Request': 'same-origin' };

function say(text) { statusEl.textContent = text; }

async function api(path, options) {
  const response = await fetch(path, Object.assign({ credentials: 'same-origin', cache: 'no-store' }, options));
  if (response.status === 401) { location.href = '/login?next=' + encodeURIComponent(location.pathname); throw new Error('sign in'); }
  const body = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(body.error || ('Request failed (' + response.status + ')'));
  return body;
}

function post(path, body) {
  return api(path, { method: 'POST', headers: HEADERS, body: JSON.stringify(body || {}) });
}

function el(tag, text, className) {
  const node = document.createElement(tag);
  if (text) node.textContent = text;
  if (className) node.className = className;
  return node;
}

function button(label, className, action) {
  const b = el('button', label, className);
  b.type = 'button';
  b.onclick = async () => {
    b.disabled = true;
    try { await action(); } catch (e) { say(e.message); } finally { b.disabled = false; }
  };
  return b;
}

const STATE_WORDS = { live: 'on', disabled: 'off', lost: 'lost' };

function drawSources(data) {
  const box = document.getElementById('sources');
  box.replaceChildren();
  const sources = data.sources || [];
  if (!sources.length) {
    box.append(el('p', 'No sources yet. Add one below once a perception service is ready to watch it.', 'muted'));
    return;
  }
  for (const s of sources) {
    const item = el('div', null, 'item');
    const what = el('div', s.name || s.source_id, 'what');
    what.append(el('span', STATE_WORDS[s.state] || s.state, 'state ' + s.state));
    item.append(what);
    item.append(el('div', s.source_id + ' · epoch ' + s.epoch + (s.notify_owner === true ? ' · this agent tells you about it' : ' · another agent tells you about it'), 'meta'));
    const row = el('div', null, 'row');
    if (s.state === 'live') {
      row.append(button('Stop', 'stop', async () => { await post('/api/perception/stop', { source_id: s.source_id }); say('Stopped ' + (s.name || s.source_id) + '.'); load(); }));
    } else {
      row.append(button('Start', 'primary', async () => { await post('/api/perception/start', { source_id: s.source_id }); say('Started ' + (s.name || s.source_id) + '.'); load(); }));
    }
    if (s.kind === 'phone' && s.state !== 'disabled') {
      row.append(button(cameraRunning() && window.paiCamera.source() === s.source_id ? 'This device is sending' : 'Use this device as a camera', 'primary', async () => {
        if (!window.paiCamera) { say('The camera is still loading; try again in a moment.'); return; }
        window.paiCamera.onmessage = say;
        await window.paiCamera.start(s.source_id, s.name);
        say('This device is now a camera for ' + (s.name || s.source_id) + '. It keeps sending as you move between pages until you stop it.');
      }));
    }
    row.append(button('Remove', '', async () => {
      if (!confirm('Remove ' + (s.name || s.source_id) + '? Its key stops working.')) return;
      await post('/api/perception/sources', { source_id: s.source_id, remove: true }); say('Removed.'); load();
    }));
    item.append(row);
    box.append(item);
  }
}

function drawPresent(data) {
  const box = document.getElementById('present');
  box.replaceChildren();
  for (const line of data.present || []) box.append(el('div', line, 'item'));
  for (const t of data.tracks || []) {
    const item = el('div', null, 'item');
    const who = t.correction ? 'corrected to ' + t.correction : (t.recognition === 'candidate' ? 'possibly ' + t.candidate : 'not known');
    item.append(el('div', t.source_id + ', track ' + t.track_id + ': ' + who, 'what'));
    const input = el('input');
    input.placeholder = 'Who is it? (a correction replaces any recognition)';
    input.maxLength = 80;
    item.append(input);
    item.append(el('div', null, 'row'));
    item.lastChild.append(button('Correct', '', async () => {
      if (!input.value.trim()) return;
      await post('/api/perception/correct', { source_id: t.source_id, track_id: t.track_id, correction: input.value.trim() });
      say('Corrected.'); load();
    }));
    box.append(item);
  }
  if (!box.childNodes.length) box.append(el('div', 'Nothing to perceive: no source is configured.', 'muted'));
}

// This device as a camera lives in the app shell (camera.js), so it keeps
// running from page to page with a visible badge; this page only starts it.
function cameraRunning() { return window.paiCamera && window.paiCamera.running(); }
window.addEventListener('pai-camera', () => load());

async function load() {
  try {
    const data = await api('/api/perception/sources');
    drawSources(data);
    drawPresent(data);
  } catch (e) { say(e.message); }
}

document.getElementById('stop-all').onclick = async () => {
  try {
    if (cameraRunning()) await window.paiCamera.stop();
    const r = await post('/api/perception/stop', {});
    say('Stopped ' + (r.stopped || []).length + ' source' + ((r.stopped || []).length === 1 ? '' : 's') + '.');
    load();
  } catch (e) { say(e.message); }
};

document.getElementById('add').onclick = async () => {
  const id = document.getElementById('source-id').value.trim();
  if (!id) { say('Give the source an id.'); return; }
  try {
    const r = await post('/api/perception/sources', {
      source_id: id, name: document.getElementById('source-name').value.trim() || id,
      kind: document.getElementById('source-kind').value,
      notify_owner: document.getElementById('source-owner').checked });
    const box = document.getElementById('new-key');
    box.replaceChildren(el('p', 'Key for ' + r.source_id + ' (shown once; give it to the perception service):', 'sub'), el('div', r.key, 'key'));
    say('Added ' + r.source_id + '. It is off until you start it.');
    load();
  } catch (e) { say(e.message); }
};

load();
setInterval(load, 10000);
