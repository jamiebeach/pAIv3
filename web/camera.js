/* This device as a camera for the agents' senses (beads v3-byw.11 and after).
   Lives in the app shell, so it follows the operator from page to page: a
   visible "camera on" badge on every page, picking up again on the next page
   within a second. Frames go straight to pAI-Eyes under a short-lived lease;
   the agent never receives them, only what Eyes perceives, unless the
   operator lets her see them with each chat message. */
(function () {
  'use strict';
  if (typeof window === 'undefined' || window.paiCamera) return;

  const KEY = 'pai.camera';
  const HEADERS = { 'Content-Type': 'application/json', 'X-PAI-Request': 'same-origin' };
  const cam = { running: false, source: null, name: null, facing: 'environment', chat: false,
                stream: null, timer: null, lease: null, sent: 0, video: null, badge: null, renewing: false };

  function saved() { try { return JSON.parse(sessionStorage.getItem(KEY) || 'null'); } catch (_) { return null; } }
  function save() {
    try {
      if (cam.running) sessionStorage.setItem(KEY, JSON.stringify({ source: cam.source, name: cam.name, facing: cam.facing, chat: cam.chat }));
      else sessionStorage.removeItem(KEY);
    } catch (_) {}
  }
  function announce() { window.dispatchEvent(new CustomEvent('pai-camera', { detail: { running: cam.running, chat: cam.chat } })); }

  async function getLease() {
    const r = await fetch('/api/perception/phone', { method: 'POST', credentials: 'same-origin', headers: HEADERS,
                                                     body: JSON.stringify({ source_id: cam.source }) });
    const body = await r.json().catch(() => ({}));
    if (!r.ok) throw new Error(body.error || ('Could not start the camera (' + r.status + ')'));
    return body;
  }

  function el(tag, attrs, text) {
    const n = document.createElement(tag);
    Object.entries(attrs || {}).forEach(([k, v]) => n.setAttribute(k, v));
    if (text) n.textContent = text;
    return n;
  }

  function drawBadge() {
    if (cam.badge) cam.badge.remove();
    const style = 'position:fixed;z-index:95;right:calc(56px + env(safe-area-inset-right));top:calc(6px + env(safe-area-inset-top));' +
      'background:var(--panel,#161a21);color:var(--text,var(--fg,#e6e8ec));border:1px solid #55d6a0;border-radius:8px;padding:4px;' +
      'font:11px system-ui,sans-serif;box-shadow:0 6px 18px rgba(0,0,0,.35);width:92px';
    const badge = el('div', { id: 'pai-camera-badge', style, role: 'status' });
    cam.video.setAttribute('style', 'width:100%;border-radius:6px;display:block;' + (cam.facing === 'user' ? 'transform:scaleX(-1);' : ''));
    const head = el('div', { style: 'display:flex;align-items:center;gap:6px;margin:4px 0' });
    head.append(el('span', { style: 'color:#55d6a0' }, '●'), el('span', {}, 'camera'));
    const meta = el('div', { id: 'pai-camera-meta', style: 'color:var(--dim,var(--muted,#9aa3b2));font-size:11px;margin-bottom:4px' },
                    (cam.name || cam.source) + ' · ' + cam.sent + ' frames');
    const chat = el('label', { style: 'display:flex;gap:4px;align-items:center;font-size:11px;margin-bottom:4px', title: 'Send the camera with each chat message' });
    const box = el('input', { type: 'checkbox' });
    box.checked = cam.chat;
    box.onchange = () => { cam.chat = box.checked; save(); announce(); };
    chat.append(box, el('span', {}, 'with chat'));
    const row = el('div', { style: 'display:flex;gap:4px' });
    const btn = 'flex:1;font:inherit;font-size:11px;padding:3px 4px;border-radius:6px;cursor:pointer;background:transparent;color:inherit;border:1px solid var(--line,var(--border,#2a2f3a))';
    const flip = el('button', { type: 'button', style: btn, title: 'Switch camera' }, 'flip');
    flip.onclick = async () => { cam.facing = cam.facing === 'user' ? 'environment' : 'user'; await openStream(); save(); drawBadge(); };
    const stop = el('button', { type: 'button', style: btn + ';border-color:#ff7b88;color:#ff7b88;font-weight:600' }, 'stop');
    stop.onclick = () => window.paiCamera.stop('Camera stopped.');
    row.append(flip, stop);
    badge.append(cam.video, head, meta, chat, row);
    document.body.appendChild(badge);
    cam.badge = badge;
  }

  async function openStream() {
    if (cam.stream) cam.stream.getTracks().forEach((t) => t.stop());
    cam.stream = await navigator.mediaDevices.getUserMedia({ video: { facingMode: cam.facing, width: { ideal: 640 } }, audio: false });
    cam.video.srcObject = cam.stream;
    cam.stream.getVideoTracks().forEach((t) => { t.onended = () => { if (cam.running) window.paiCamera.stop('The camera was taken away by the device.'); }; });
    await cam.video.play();
  }

  function frame(quality) {
    const v = cam.video;
    if (!v || !v.videoWidth) return null;
    const canvas = document.createElement('canvas');
    const scale = Math.min(1, 640 / v.videoWidth);
    canvas.width = Math.round(v.videoWidth * scale);
    canvas.height = Math.round(v.videoHeight * scale);
    canvas.getContext('2d').drawImage(v, 0, 0, canvas.width, canvas.height);
    return canvas;
  }

  function tick() {
    const canvas = frame();
    if (!canvas) return;
    canvas.toBlob(async (jpeg) => {
      if (!jpeg || !cam.running) return;
      try {
        const r = await fetch(cam.lease.frames_url, { method: 'POST', body: jpeg,
          headers: { 'Content-Type': 'image/jpeg', 'Authorization': 'Bearer ' + cam.lease.token } });
        if (r.status === 401 && !cam.renewing) {
          // The lease ran out: ask for a new one while the source is still on.
          cam.renewing = true;
          try { cam.lease = await getLease(); } catch (e) { window.paiCamera.stop(e.message); } finally { cam.renewing = false; }
          return;
        }
        if (r.status === 409) { window.paiCamera.stop('The camera was stopped (from this or another agent\'s page).'); return; }
        cam.sent += 1;
        const meta = document.getElementById('pai-camera-meta');
        if (meta) meta.textContent = (cam.name || cam.source) + ' · ' + cam.sent + ' frames';
      } catch (_) { /* Eyes unreachable for a moment: the next frame tries again */ }
    }, 'image/jpeg', 0.7);
  }

  window.paiCamera = {
    async start(source, name, options) {
      if (cam.running) await window.paiCamera.stop();
      if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) throw new Error('This browser cannot open the camera here.');
      Object.assign(cam, { source, name: name || source, sent: 0,
                           facing: (options && options.facing) || cam.facing, chat: !!(options && options.chat) });
      cam.lease = await getLease();
      cam.video = cam.video || el('video', { playsinline: '', muted: '' });
      cam.video.muted = true;
      await openStream();
      cam.running = true;
      clearInterval(cam.timer);
      cam.timer = setInterval(tick, 1000 / Math.max(1, cam.lease.frames_per_second || 1));
      drawBadge(); save(); announce();
    },
    async stop(why) {
      clearInterval(cam.timer);
      if (cam.stream) cam.stream.getTracks().forEach((t) => t.stop());
      Object.assign(cam, { running: false, stream: null, timer: null, lease: null });
      if (cam.badge) { cam.badge.remove(); cam.badge = null; }
      save(); announce();
      if (why && window.paiCamera.onmessage) window.paiCamera.onmessage(why);
    },
    running() { return cam.running; },
    source() { return cam.running ? cam.source : null; },
    chatting() { return cam.running && cam.chat; },
    snapshot() {
      // A frame for a chat message: what the camera shows as it is sent.
      const canvas = frame();
      return canvas ? canvas.toDataURL('image/jpeg', 0.8) : null;
    },
    onmessage: null
  };

  // Picking up again on the next page: the browser remembers the permission,
  // so this usually needs no new prompt; if it does not, the camera stays off.
  function resume() {
    const s = saved();
    if (s && s.source) window.paiCamera.start(s.source, s.name, { facing: s.facing, chat: s.chat }).catch(() => { try { sessionStorage.removeItem(KEY); } catch (_) {} });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', resume);
  else resume();
})();
