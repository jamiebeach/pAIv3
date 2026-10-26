-- pAI relay hub (bead v3-fy1): Cloudflare D1. Apply with
--   wrangler d1 execute pai-relay --remote --file services/relay-hub/schema.sql
CREATE TABLE IF NOT EXISTS clients (
  id TEXT PRIMARY KEY,                 -- 'bridge', 'muse', ...
  token_hash TEXT NOT NULL,            -- SHA-256 (hex) of the client's bearer token
  role TEXT NOT NULL CHECK (role IN ('bridge', 'agent')),
  revoked INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE IF NOT EXISTS events (    -- forum -> cloud
  seq INTEGER PRIMARY KEY AUTOINCREMENT,
  id TEXT NOT NULL UNIQUE,             -- the forum's message id
  thread_id TEXT NOT NULL, thread_title TEXT, author TEXT NOT NULL,
  intent TEXT, reply_to TEXT, body TEXT NOT NULL, ts TEXT NOT NULL,
  received_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS inbox (     -- cloud -> forum
  seq INTEGER PRIMARY KEY AUTOINCREMENT,
  id TEXT NOT NULL UNIQUE,             -- the client's message id
  from_client TEXT NOT NULL, nonce TEXT NOT NULL,
  thread_id TEXT, title TEXT, reply_to TEXT, intent TEXT, body TEXT NOT NULL,
  ts TEXT NOT NULL, received_at TEXT NOT NULL,
  UNIQUE (from_client, nonce)
);
CREATE INDEX IF NOT EXISTS events_received_idx ON events(received_at);
CREATE INDEX IF NOT EXISTS inbox_client_idx ON inbox(from_client, received_at);
