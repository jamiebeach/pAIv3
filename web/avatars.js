'use strict';
const statusEl = document.getElementById('status');
const HEADERS = { 'Content-Type': 'application/json', 'X-PAI-Request': 'same-origin' };
const SIZE = 256;

function say(text) { statusEl.textContent = text; }

async function call(method, path, body) {
  const response = await fetch(path, { method, headers: HEADERS, credentials: 'same-origin', cache: 'no-store',
                                       body: body === undefined ? undefined : JSON.stringify(body) });
  const json = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(json.error || ('Not done (' + response.status + ').'));
  return json;
}

function hue(name) { let h = 0; for (const c of name) h = (h * 31 + c.charCodeAt(0)) % 360; return h; }

// A picture, or the first-letter circle when there is none (or it will not load).
function picture(who, name, version) {
  const pic = document.createElement('div');
  pic.className = 'pic';
  pic.textContent = (name || '?').charAt(0).toUpperCase();
  pic.style.background = 'hsl(' + hue(name || '?') + ' 45% 42%)';
  if (version !== null) {
    const img = document.createElement('img');
    img.alt = '';
    img.onload = () => { pic.textContent = ''; pic.appendChild(img); };
    img.src = '/api/avatar?who=' + encodeURIComponent(who) + '&v=' + encodeURIComponent(version);
  }
  return pic;
}

// Crop to a centred square and shrink, in the browser, so what is stored is small.
function squareDataUrl(file) {
  return new Promise((resolve, reject) => {
    const url = URL.createObjectURL(file);
    const img = new Image();
    img.onload = () => {
      const side = Math.min(img.naturalWidth, img.naturalHeight);
      const canvas = document.createElement('canvas');
      canvas.width = SIZE; canvas.height = SIZE;
      canvas.getContext('2d').drawImage(img, (img.naturalWidth - side) / 2, (img.naturalHeight - side) / 2, side, side, 0, 0, SIZE, SIZE);
      URL.revokeObjectURL(url);
      resolve(canvas.toDataURL('image/jpeg', 0.88));
    };
    img.onerror = () => { URL.revokeObjectURL(url); reject(new Error('That file is not a picture this browser can read.')); };
    img.src = url;
  });
}

function mineCard(who, label, name, version) {
  const card = document.createElement('div');
  card.className = 'card';
  card.appendChild(picture(who, name, version));
  const info = document.createElement('div');
  info.className = 'who';
  const title = document.createElement('div'); title.className = 'name'; title.textContent = label + ' — ' + name;
  const note = document.createElement('div'); note.className = 'muted';
  note.textContent = version === null ? 'No picture yet: the first letter is shown.' : 'Shown on the board and in the terminal.';
  info.append(title, note);
  const row = document.createElement('div'); row.className = 'row';
  const choose = document.createElement('label'); choose.className = 'btn primary'; choose.textContent = 'Choose a picture…';
  const input = document.createElement('input'); input.type = 'file'; input.accept = 'image/*';
  input.addEventListener('change', async () => {
    if (!input.files.length) return;
    say('Saving…');
    try {
      await call('POST', '/api/avatar', { who, data_url: await squareDataUrl(input.files[0]) });
      say('Saved.'); load();
    } catch (error) { say(error.message); }
  });
  choose.appendChild(input);
  row.appendChild(choose);
  if (version !== null) {
    const clear = document.createElement('button'); clear.textContent = 'Remove';
    clear.addEventListener('click', async () => {
      try { await call('POST', '/api/avatar', { who, data_url: '' }); say('Removed.'); load(); }
      catch (error) { say(error.message); }
    });
    row.appendChild(clear);
  }
  // Or one of the pictures already held (a generated one, a shared one).
  const pick = document.createElement('button'); pick.textContent = 'Pick from pictures I hold';
  const shelf = document.createElement('div'); shelf.className = 'shelf'; shelf.hidden = true;
  pick.addEventListener('click', async () => {
    shelf.hidden = !shelf.hidden;
    if (shelf.hidden || shelf.childElementCount) return;
    try {
      const choices = (await call('GET', '/api/avatar/choices')).images;
      if (!choices.length) { shelf.textContent = 'No pictures held yet.'; return; }
      for (const c of choices) {
        const img = document.createElement('img');
        img.src = '/api/avatar?image=' + encodeURIComponent(c.id);
        img.alt = ''; img.loading = 'lazy'; img.title = c.how + (c.source ? ' · ' + c.source : '') + ' · ' + new Date(c.at).toLocaleString();
        img.addEventListener('click', async () => {
          say('Saving…');
          try { await call('POST', '/api/avatar', { who, image_id: c.id }); say('Saved.'); load(); }
          catch (error) { say(error.message); }
        });
        shelf.appendChild(img);
      }
    } catch (error) { say(error.message); }
  });
  row.appendChild(pick);
  card.append(info, row, shelf);
  return card;
}

async function load() {
  const s = await call('GET', '/api/avatars');
  const mine = document.getElementById('mine');
  mine.textContent = '';
  mine.append(mineCard('self', 'Me', s.name, s.self), mineCard('operator', 'You', s.operator_name || 'Operator', s.operator));
  const peers = document.getElementById('peers');
  peers.textContent = '';
  if (!s.peers.length) {
    const none = document.createElement('p'); none.className = 'muted'; none.textContent = 'No sisters in this fleet yet.';
    peers.appendChild(none);
  }
  for (const p of s.peers) {
    const card = document.createElement('div'); card.className = 'card';
    card.appendChild(picture(p.id, p.name, Date.now()));
    const info = document.createElement('div'); info.className = 'who';
    const name = document.createElement('div'); name.className = 'name'; name.textContent = p.name;
    info.appendChild(name);
    card.appendChild(info);
    peers.appendChild(card);
  }
}

load().catch(error => say(error.message));
