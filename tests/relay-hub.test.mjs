// The relay hub's contract (bead v3-fy1), run with: node tests/relay-hub.test.mjs
import assert from 'node:assert/strict';
import { localD1, addClient, call } from '../services/relay-hub/local.mjs';

const env = { DB: localD1() };
addClient(env, 'bridge', 'bridge', 'bridge-token');
addClient(env, 'muse', 'agent', 'muse-token');
addClient(env, 'gone', 'agent', 'gone-token');
env.DB.raw.prepare("UPDATE clients SET revoked = 1 WHERE id = 'gone'").run();
let checks = 0;
const ok = (cond, what) => { assert.ok(cond, what); checks++; };
const now = () => new Date().toISOString();
const envelope = (over) => ({ id: crypto.randomUUID(), from: 'muse', thread_id: 't1', body: 'SYNTHETIC hello', ts: now(), nonce: crypto.randomUUID(), ...over });

ok((await call(env, 'GET', '/health')).status === 200, 'health needs no token');
ok((await call(env, 'GET', '/events', 'wrong')).status === 401, 'a token is needed');
ok((await call(env, 'GET', '/events', 'gone-token')).status === 401, 'a revoked token is refused');
ok((await call(env, 'POST', '/events', 'muse-token', { events: [] })).status === 403, 'only the bridge pushes events');
ok((await call(env, 'GET', '/inbox', 'muse-token')).status === 403, 'only the bridge reads the inbox');

// Forum -> cloud
const ev = (id) => ({ id, thread_id: 't1', thread_title: 'SYNTHETIC', author: 'Angel', body: 'SYNTHETIC post ' + id, ts: now() });
let r = await call(env, 'POST', '/events', 'bridge-token', { events: [ev('m1'), ev('m2')] });
ok(r.status === 200 && r.body.accepted === 2, 'events accepted');
r = await call(env, 'POST', '/events', 'bridge-token', { events: [ev('m2'), ev('m3')] });
ok(r.body.accepted === 1 && r.body.duplicates === 1, 'a repeated event is dropped, not an error');
r = await call(env, 'GET', '/events?since=0', 'muse-token');
ok(r.body.events.map((e) => e.id).join() === 'm1,m2,m3', 'events in order, once each');
const next = r.body.next;
ok((await call(env, 'GET', '/events?since=' + next, 'muse-token')).body.events.length === 0, 'a cursor reads only what is new');

// Cloud -> forum
const m = envelope();
ok((await call(env, 'POST', '/messages', 'muse-token', m)).status === 202, 'a message is taken');
ok((await call(env, 'POST', '/messages', 'muse-token', m)).body.duplicate === true, 'the same message again is one message');
ok((await call(env, 'POST', '/messages', 'muse-token', envelope({ nonce: m.nonce }))).body.duplicate === true, 'a replayed nonce is one message');
ok((await call(env, 'POST', '/messages', 'muse-token', envelope({ ts: '2026-01-01T00:00:00Z' }))).status === 400, 'an old timestamp is refused');
ok((await call(env, 'POST', '/messages', 'muse-token', envelope({ from: 'someone-else' }))).status === 400, 'a client speaks only as itself');
ok((await call(env, 'POST', '/messages', 'muse-token', envelope({ thread_id: undefined }))).status === 400, 'a thread or a title is needed');
ok((await call(env, 'POST', '/messages', 'muse-token', envelope({ thread_id: undefined, title: 'SYNTHETIC new thread' }))).status === 202, 'a new thread by title');
r = await call(env, 'GET', '/inbox?since=0', 'bridge-token');
ok(r.body.messages.length === 2 && r.body.messages[0].from_client === 'muse', 'the bridge sees each message once');
ok(r.body.messages[1].thread_id === null && r.body.messages[1].title === 'SYNTHETIC new thread', 'a new thread carries its title');

// A runaway client is capped.
for (let i = 0; i < 58; i++) await call(env, 'POST', '/messages', 'muse-token', envelope());
r = await call(env, 'POST', '/messages', 'muse-token', envelope());
ok(r.status === 429, 'past sixty messages an hour');

console.log(`relay-hub: ${checks} checks, 0 failures`);
