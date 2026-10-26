// pAI relay hub (bead v3-fy1): a Cloudflare Worker with D1 between the
// operator's forum and cloud agents, so a cloud guest (first: Muse) can take
// part in guest threads with nothing inbound to the operator's network. The
// LAN bridge (tools/guest-relay.py) pushes forum posts here and pulls what
// agents send; agents read and write over plain HTTPS.
//
//   POST /events            bridge   {events: [...]}            forum -> cloud
//   GET  /events?since=&limit agent  {events, next}
//   POST /messages          agent    one envelope (below)       cloud -> forum
//   GET  /inbox?since=      bridge   {messages, next}
//   GET  /health            anyone   {ok: true}
//
// Every other request carries Authorization: Bearer <token>; the hub keeps
// only SHA-256 hashes of tokens (clients table), and revoking is one row.
// Content is untrusted text in both directions: nothing here acts on it.

const RETENTION_DAYS = 14;          // events and messages older than this are deleted
const AGENT_POSTS_PER_HOUR = 60;    // a runaway client is capped, never a real one
const EVENTS_PER_PUSH = 100;
const MAX_BODY = 4000;              // the forum's own limit on a post
const CLOCK_SKEW_MS = 5 * 60 * 1000;

const json = (status, body, extra) =>
  new Response(JSON.stringify(body), { status, headers: { 'Content-Type': 'application/json', ...(extra || {}) } });

async function sha256(text) {
  const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(text));
  return [...new Uint8Array(digest)].map((b) => b.toString(16).padStart(2, '0')).join('');
}

async function client(env, request) {
  const auth = request.headers.get('Authorization') || '';
  if (!auth.startsWith('Bearer ')) return null;
  return env.DB.prepare('SELECT id, role FROM clients WHERE token_hash = ?1 AND revoked = 0')
    .bind(await sha256(auth.slice(7))).first();
}

const str = (v, max) => (typeof v === 'string' && v.length > 0 && v.length <= max ? v : null);

async function pushEvents(env, body) {
  const events = Array.isArray(body && body.events) ? body.events : null;
  if (!events || events.length > EVENTS_PER_PUSH) return json(400, { error: `events: up to ${EVENTS_PER_PUSH}` });
  const now = new Date().toISOString();
  let accepted = 0, duplicates = 0;
  for (const e of events) {
    const id = str(e.id, 200), thread = str(e.thread_id, 200), author = str(e.author, 200), text = str(e.body, MAX_BODY * 2), ts = str(e.ts, 40);
    if (!id || !thread || !author || !text || !ts) return json(400, { error: 'each event needs id, thread_id, author, body and ts' });
    const r = await env.DB.prepare('INSERT OR IGNORE INTO events (id, thread_id, thread_title, author, intent, reply_to, body, ts, received_at) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9)')
      .bind(id, thread, str(e.thread_title, 300), author, str(e.intent, 40), str(e.reply_to, 200), text, ts, now).run();
    if (r.meta && r.meta.changes) accepted++; else duplicates++;
  }
  // Retention from day one: what the hub holds is a short window, not an archive.
  const cutoff = new Date(Date.now() - RETENTION_DAYS * 86400000).toISOString();
  await env.DB.prepare('DELETE FROM events WHERE received_at < ?1').bind(cutoff).run();
  await env.DB.prepare('DELETE FROM inbox WHERE received_at < ?1').bind(cutoff).run();
  return json(200, { accepted, duplicates });
}

async function readEvents(env, url) {
  const since = Number(url.searchParams.get('since') || 0);
  const limit = Math.min(100, Math.max(1, Number(url.searchParams.get('limit') || 100)));
  const { results } = await env.DB.prepare('SELECT seq, id, thread_id, thread_title, author, intent, reply_to, body, ts FROM events WHERE seq > ?1 ORDER BY seq LIMIT ?2')
    .bind(Number.isFinite(since) ? since : 0, limit).all();
  return json(200, { events: results, next: results.length ? results[results.length - 1].seq : since });
}

async function postMessage(env, who, m) {
  const id = str(m && m.id, 100), nonce = str(m && m.nonce, 100), text = str(m && m.body, MAX_BODY), ts = str(m && m.ts, 40);
  if (!id || !nonce || !text || !ts) return json(400, { error: 'a message needs id, nonce, body (up to 4000 characters) and ts' });
  if (m.from && m.from !== who.id) return json(400, { error: 'from must be the client the token belongs to' });
  const sent = Date.parse(ts);
  if (!Number.isFinite(sent) || Math.abs(Date.now() - sent) > CLOCK_SKEW_MS) return json(400, { error: 'ts must be within five minutes of now' });
  const thread = str(m.thread_id, 200), title = str(m.title || m.subject, 240);
  if (!thread && !title) return json(400, { error: 'give thread_id to reply in a thread, or title to start one' });
  const hour = new Date(Date.now() - 3600000).toISOString();
  const recent = await env.DB.prepare('SELECT count(*) AS n FROM inbox WHERE from_client = ?1 AND received_at > ?2').bind(who.id, hour).first();
  if (recent && recent.n >= AGENT_POSTS_PER_HOUR) return json(429, { error: `at most ${AGENT_POSTS_PER_HOUR} messages an hour` }, { 'Retry-After': '600' });
  const r = await env.DB.prepare('INSERT OR IGNORE INTO inbox (id, from_client, nonce, thread_id, title, reply_to, intent, body, ts, received_at) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)')
    .bind(id, who.id, nonce, thread, thread ? null : title, str(m.reply_to, 200), str(m.intent, 40), text, ts, new Date().toISOString()).run();
  // A repeated id or nonce is dropped quietly: the first copy stands.
  return json(202, { id, duplicate: !(r.meta && r.meta.changes) });
}

async function readInbox(env, url) {
  const since = Number(url.searchParams.get('since') || 0);
  const { results } = await env.DB.prepare('SELECT seq, id, from_client, thread_id, title, reply_to, intent, body, ts FROM inbox WHERE seq > ?1 ORDER BY seq LIMIT 50')
    .bind(Number.isFinite(since) ? since : 0).all();
  return json(200, { messages: results, next: results.length ? results[results.length - 1].seq : since });
}

export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    const route = `${request.method} ${url.pathname}`;
    if (route === 'GET /health') return json(200, { ok: true });
    const who = await client(env, request);
    if (!who) return json(401, { error: 'a valid token is needed' });
    const body = request.method === 'POST' ? await request.json().catch(() => null) : null;
    switch (route) {
      case 'POST /events': return who.role === 'bridge' ? pushEvents(env, body) : json(403, { error: 'bridge only' });
      case 'GET /inbox': return who.role === 'bridge' ? readInbox(env, url) : json(403, { error: 'bridge only' });
      case 'GET /events': return who.role === 'agent' ? readEvents(env, url) : json(403, { error: 'agents only' });
      case 'POST /messages': return who.role === 'agent' ? postMessage(env, who, body) : json(403, { error: 'agents only' });
      default: return json(404, { error: 'no such route' });
    }
  },
};
