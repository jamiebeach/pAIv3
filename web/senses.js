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
      row.append(button(phone.running ? 'Sending from this phone' : 'Use this phone as a camera', 'primary', () => startPhone(s)));
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

// A phone as a camera: frames go from this page straight to Eyes under a
// short-lived lease, about one a second. Nothing is kept on the phone, and
// the agent never receives the pictures, only what Eyes perceives in them.
const phone = { running: false, stream: null, timer: null, lease: null, sent: 0 };

async function startPhone(source) {
  if (phone.running) return;
  if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) { say('This browser cannot open the camera here.'); return; }
  const lease = await post('/api/perception/phone', { source_id: source.source_id });
  phone.stream = await navigator.mediaDevices.getUserMedia({ video: { facingMode: 'environment', width: { ideal: 640 } }, audio: false });
  const video = document.getElementById('phone-video');
  video.srcObject = phone.stream;
  await video.play();
  Object.assign(phone, { running: true, lease, sent: 0 });
  document.getElementById('phone').hidden = false;
  const canvas = document.createElement('canvas');
  const every = 1000 / Math.max(1, lease.frames_per_second || 1);
  phone.timer = setInterval(() => {
    if (!video.videoWidth) return;
    const scale = Math.min(1, 640 / video.videoWidth);
    canvas.width = Math.round(video.videoWidth * scale);
    canvas.height = Math.round(video.videoHeight * scale);
    canvas.getContext('2d').drawImage(video, 0, 0, canvas.width, canvas.height);
    canvas.toBlob(async (jpeg) => {
      if (!jpeg || !phone.running) return;
      try {
        const r = await fetch(lease.frames_url, { method: 'POST', body: jpeg,
          headers: { 'Content-Type': 'image/jpeg', 'Authorization': 'Bearer ' + lease.token } });
        if (r.status === 401 || r.status === 409) { stopPhone('Eyes ended this camera (stopped, restarted or lease expired).'); return; }
        phone.sent += 1;
        document.getElementById('phone-meta').textContent = phone.sent + ' frames sent to Eyes · ' + (source.name || source.source_id);
      } catch (e) { document.getElementById('phone-meta').textContent = 'Could not reach Eyes: ' + e.message; }
    }, 'image/jpeg', 0.7);
  }, every);
  say('This phone is now a camera for ' + (source.name || source.source_id) + '.');
}

function stopPhone(why) {
  if (!phone.running) return;
  clearInterval(phone.timer);
  if (phone.stream) phone.stream.getTracks().forEach((t) => t.stop());
  Object.assign(phone, { running: false, stream: null, timer: null, lease: null });
  document.getElementById('phone').hidden = true;
  say(why || 'This phone stopped being a camera.');
  load();
}

document.getElementById('phone-stop').onclick = () => stopPhone();
// Leaving the page or locking the phone ends the camera; it never runs unseen.
document.addEventListener('visibilitychange', () => { if (document.hidden) stopPhone('The camera stopped when this page was hidden.'); });

async function load() {
  try {
    const data = await api('/api/perception/sources');
    drawSources(data);
    drawPresent(data);
  } catch (e) { say(e.message); }
}

document.getElementById('stop-all').onclick = async () => {
  try {
    stopPhone('Stopped with every sense.');
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
