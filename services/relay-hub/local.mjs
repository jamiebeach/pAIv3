// Run the relay hub locally, for tests and trying it out (bead v3-fy1): the
// Worker as written, over Node's built-in SQLite standing in for D1.
//
//   node services/relay-hub/local.mjs serve PORT [client:role:token ...]
//
// Each client is registered from id:role:token (the token is hashed, as in D1).
import { DatabaseSync } from 'node:sqlite';
import { createServer } from 'node:http';
import { readFileSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import worker from './worker.js';

// The small part of the D1 API the Worker uses.
export function localD1(path = ':memory:') {
  const db = new DatabaseSync(path);
  db.exec(readFileSync(fileURLToPath(new URL('./schema.sql', import.meta.url)), 'utf8'));
  const statement = (sql, args = []) => ({
    bind: (...values) => statement(sql, values),
    first: async () => db.prepare(sql).get(...args) ?? null,
    all: async () => ({ results: db.prepare(sql).all(...args) }),
    run: async () => { const r = db.prepare(sql).run(...args); return { meta: { changes: Number(r.changes) } }; },
  });
  return { prepare: (sql) => statement(sql), raw: db };
}

export function addClient(env, id, role, token) {
  env.DB.raw.prepare('INSERT INTO clients (id, token_hash, role) VALUES (?, ?, ?)')
    .run(id, createHash('sha256').update(token).digest('hex'), role);
}

export async function call(env, method, path, token, body) {
  const request = new Request('http://hub.local' + path, {
    method, headers: { ...(token ? { Authorization: 'Bearer ' + token } : {}), 'Content-Type': 'application/json' },
    body: body === undefined ? undefined : JSON.stringify(body) });
  const response = await worker.fetch(request, env);
  return { status: response.status, body: await response.json() };
}

if (process.argv[2] === 'serve') {
  const env = { DB: localD1() };
  for (const spec of process.argv.slice(4)) { const [id, role, token] = spec.split(':'); addClient(env, id, role, token); }
  createServer(async (req, res) => {
    const chunks = [];
    for await (const c of req) chunks.push(c);
    const response = await worker.fetch(new Request('http://hub.local' + req.url, {
      method: req.method, headers: req.headers, body: ['GET', 'HEAD'].includes(req.method) ? undefined : Buffer.concat(chunks) }), env);
    res.writeHead(response.status, { 'Content-Type': 'application/json' });
    res.end(await response.text());
  }).listen(Number(process.argv[3] || 8787), '127.0.0.1', () => console.log('relay hub on', process.argv[3] || 8787));
}
