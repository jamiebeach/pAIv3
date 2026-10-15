/* pAI's service worker intentionally caches no authenticated or private data. It establishes
   the installable app boundary; every request stays network-only under the
   existing authenticated web adapter.

   Web Push (pai-t8b.5): a push carries NO payload. On a push the worker fetches
   the pending summary and deep link from the authenticated notifications
   endpoint (the browser sends the session cookie), shows a notification for each
   item, and remembers only the id of the newest item it showed so it does not
   repeat itself. If the fetch fails it shows a generic notice, because a push
   that shows nothing can cost the subscription. Only internal paths are ever
   opened. */
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', event => event.waitUntil(self.clients.claim()));

const STATE_DB = 'pai-notification-state';
const STATE_STORE = 'state';
const MAX_SHOWN_PER_PUSH = 3;

function openState() {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(STATE_DB, 1);
    request.onupgradeneeded = () => request.result.createObjectStore(STATE_STORE);
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}

async function readLastId() {
  try {
    const db = await openState();
    return await new Promise(resolve => {
      const request = db.transaction(STATE_STORE).objectStore(STATE_STORE).get('lastId');
      request.onsuccess = () => resolve(request.result || null);
      request.onerror = () => resolve(null);
    });
  } catch (error) { return null; }
}

async function writeLastId(id) {
  try {
    const db = await openState();
    await new Promise(resolve => {
      const transaction = db.transaction(STATE_STORE, 'readwrite');
      transaction.objectStore(STATE_STORE).put(id, 'lastId');
      transaction.oncomplete = resolve;
      transaction.onerror = resolve;
    });
  } catch (error) { /* best effort */ }
}

function internalPath(value) {
  return typeof value === 'string' && value.startsWith('/') && !value.startsWith('//') &&
    value.length <= 200 ? value : '/terminal';
}

async function showPending() {
  const last = await readLastId();
  const url = '/api/notifications/pending' + (last ? '?after=' + encodeURIComponent(last) : '');
  let shown = 0;
  try {
    const response = await fetch(url, { credentials: 'include', cache: 'no-store' });
    if (response.ok) {
      const body = await response.json();
      const items = Array.isArray(body.items) ? body.items.slice(-MAX_SHOWN_PER_PUSH) : [];
      for (const item of items) {
        if (typeof item.summary !== 'string' || !item.summary) continue;
        await self.registration.showNotification(item.summary.slice(0, 140), {
          tag: String(item.id || 'pai'),
          data: { url: internalPath(item.deep_link) },
          renotify: false
        });
        shown += 1;
        await writeLastId(String(item.id));
      }
    }
  } catch (error) { /* fall through to the generic notice */ }
  if (!shown) {
    await self.registration.showNotification('pAI has a message', {
      tag: 'pai-generic', data: { url: '/terminal' }
    });
  }
}

self.addEventListener('push', event => event.waitUntil(showPending()));

self.addEventListener('notificationclick', event => {
  event.notification.close();
  const target = internalPath(event.notification.data && event.notification.data.url);
  event.waitUntil((async () => {
    const windows = await self.clients.matchAll({ type: 'window', includeUncontrolled: true });
    for (const client of windows) {
      if ('focus' in client) {
        await client.focus();
        if ('navigate' in client) { try { await client.navigate(target); } catch (error) { /* ignore */ } }
        return;
      }
    }
    await self.clients.openWindow(target);
  })());
});
