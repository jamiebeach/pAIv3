'use strict';
const statusEl = document.getElementById('status');
const HEADERS = { 'Content-Type': 'application/json', 'X-PAI-Request': 'same-origin' };

function say(text) { statusEl.textContent = text; }

async function api(path, options) {
  const response = await fetch(path, Object.assign({ credentials: 'same-origin', cache: 'no-store' }, options));
  const body = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(body.error || ('Request failed (' + response.status + ')'));
  return body;
}

function send(body) {
  return api('/api/carried', { method: 'POST', headers: HEADERS, body: JSON.stringify(body) });
}

function el(tag, text, className) {
  const node = document.createElement(tag);
  if (text) node.textContent = text;
  if (className) node.className = className;
  return node;
}

// A datetime-local value, 2026-10-19T09:00, is already the local wall time the server expects.
function localValue(id) { const v = document.getElementById(id).value; return v ? v : null; }

function itemNode(item, closed) {
  const node = el('div', '', 'item');
  node.appendChild(el('div', item.what, 'what'));
  node.appendChild(el('div', item.why, 'why'));
  const meta = [];
  if (item.due_local) meta.push('due ' + item.due_local);
  if (item.condition) meta.push('waiting for: ' + item.condition);
  if (item.consider_local) meta.push('look again ' + item.consider_local);
  if (item.work_id) meta.push('work ' + item.work_id);
  if (item.times_considered) meta.push('looked at ' + item.times_considered + '×');
  meta.push(item.by === 'operator' ? 'added by you' : item.by === 'conversation' ? 'from our conversation' : 'added by me');
  if (item.note) meta.push('note: ' + item.note);
  node.appendChild(el('div', meta.join(' · '), 'meta'));
  if (!closed) {
    const row = el('div', '', 'row');
    for (const [label, state] of [['Done', 'done'], ['Drop', 'dropped']]) {
      const button = el('button', label);
      button.addEventListener('click', async () => {
        try { await send({ action: 'close', id: item.id, state }); say(label + '.'); refresh(); }
        catch (error) { say(error.message); }
      });
      row.appendChild(button);
    }
    node.appendChild(row);
  }
  return node;
}

async function refresh() {
  const body = await api('/api/carried');
  const open = document.getElementById('open');
  open.textContent = '';
  if (!body.items.length) open.appendChild(el('p', 'Nothing carried right now.', 'muted'));
  for (const item of body.items) open.appendChild(itemNode(item, false));
  const closed = document.getElementById('closed');
  closed.textContent = '';
  const all = body.closed.concat(body.dropped);
  if (!all.length) closed.appendChild(el('p', 'Nothing yet.', 'muted'));
  for (const item of all) closed.appendChild(itemNode(item, true));
}

document.getElementById('f-add').addEventListener('click', async () => {
  try {
    await send({
      action: 'add', what: document.getElementById('f-what').value, why: document.getElementById('f-why').value,
      due_at: localValue('f-due'), consider_at: localValue('f-consider'),
      condition: document.getElementById('f-condition').value || null
    });
    for (const id of ['f-what', 'f-why', 'f-due', 'f-consider', 'f-condition']) document.getElementById(id).value = '';
    document.getElementById('adder').open = false;
    say('Carrying it.'); refresh();
  } catch (error) { say(error.message); }
});

refresh().catch(error => say(error.message));
