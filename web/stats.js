'use strict';
const statusEl = document.getElementById('status');

function el(tag, text, className) {
  const node = document.createElement(tag);
  if (text !== undefined && text !== null) node.textContent = String(text);
  if (className) node.className = className;
  return node;
}

function bar(xp, from, to) {
  const outer = el('div', null, 'bar');
  const inner = el('div');
  const span = Math.max(1, to - from);
  inner.style.width = Math.max(0, Math.min(100, ((xp - from) / span) * 100)) + '%';
  outer.appendChild(inner);
  return outer;
}

function when(at) {
  const d = new Date(at);
  return isNaN(d) ? '' : d.toLocaleDateString(undefined, { month: 'short', day: 'numeric' });
}

function sheet(rootId, data, cls) {
  const root = document.getElementById(rootId);
  root.replaceChildren();
  const win = el('section', null, 'window');
  const title = el('div', null, 'title');
  title.append(el('span', data.name || '', 'name'), el('span', cls, 'muted'), el('span', 'LV ' + data.level, 'lvl'));
  win.appendChild(title);
  win.appendChild(bar(data.xp, data.level_xp, data.next_xp));
  win.appendChild(el('div', data.xp + ' XP · ' + (data.next_xp - data.xp) + ' to level ' + (data.level + 1), 'delta'));
  win.appendChild(el('h3', 'Skills'));
  for (const s of data.skills || []) {
    const row = el('div', null, 'skill');
    const head = el('div', null, 'row');
    head.append(el('span', s.name), el('span', 'LV ' + s.level, 'lvl'));
    row.appendChild(head);
    row.appendChild(bar(s.xp, s.level_xp, s.next_xp));
    const change = s.this_week - s.last_week;
    const delta = el('div', s.xp + ' XP · this week ' + (s.this_week >= 0 ? '+' : '') + s.this_week + ' ', 'delta');
    if (change) delta.appendChild(el('span', (change > 0 ? '▲ ' : '▼ ') + Math.abs(change) + ' vs last week', change > 0 ? 'up' : 'down'));
    row.appendChild(delta);
    win.appendChild(row);
  }
  if ((data.metrics || []).length) {
    win.appendChild(el('h3', 'Measures'));
    const table = el('table');
    for (const m of data.metrics) {
      const best = (m.history || []).find(h => h.best);
      const tr = el('tr');
      tr.append(el('td', m.domain), el('td', m.metric.replace(/_/g, ' ')),
                el('td', m.latest + (m.unit ? ' ' + m.unit : '')),
                el('td', best ? '★ best ' + when(best.at) : when(m.at)));
      table.appendChild(tr);
    }
    win.appendChild(table);
  }
  if ((data.recent || []).length) {
    win.appendChild(el('h3', 'Log'));
    for (const r of data.recent) {
      const line = el('div', null, 'log');
      line.append(el('span', (r.amount > 0 ? '+' : '') + r.amount, 'amt ' + (r.amount < 0 ? 'down' : 'up')),
                  document.createTextNode(' ' + r.reason + ' · ' + r.skill + ' · ' + when(r.at)));
      win.appendChild(line);
    }
  }
  root.appendChild(win);
}

async function load() {
  try {
    const response = await fetch('/api/progress', { credentials: 'same-origin', cache: 'no-store' });
    const body = await response.json().catch(() => ({}));
    if (!response.ok) throw new Error(body.error || ('Request failed (' + response.status + ')'));
    sheet('agent', body.agent, 'pAI');
    sheet('operator', body.operator, 'Operator');
    const rules = document.getElementById('rules');
    rules.replaceChildren();
    for (const r of body.rules || []) {
      const tr = el('tr');
      tr.append(el('td', r.skill), el('td', r.what), el('td', (r.xp > 0 ? '+' : '') + r.xp));
      rules.appendChild(tr);
    }
    statusEl.textContent = '';
  } catch (error) {
    statusEl.textContent = 'Could not load stats: ' + error.message;
  }
}

load();
