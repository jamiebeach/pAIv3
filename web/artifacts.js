(async function () {
  'use strict';
  const params = new URLSearchParams(location.search);
  const library = document.getElementById('library');
  async function get(url) {
    const response = await fetch(url, {cache: 'no-store'});
    const data = await response.json();
    if (!response.ok) throw new Error(data.error || 'Unable to load document');
    return data;
  }
  function text(tag, value, className) {
    const element = document.createElement(tag);
    element.textContent = value;
    if (className) element.className = className;
    return element;
  }
  function meta(item) {
    return item.state + ' · revision ' + item.revision + ' · ' + new Date(item.at).toLocaleString() +
      (item.work_id ? ' · work ' + item.work_id : '');
  }
  try {
    if (params.has('id')) {
      const query = new URLSearchParams({id: params.get('id'), revision: params.get('revision') || ''});
      const data = await get('/api/artifacts/source?' + query);
      document.getElementById('title').textContent = data.artifact.title;
      document.title = data.artifact.title + ' · Made things';
      document.getElementById('detail').textContent = meta(data.artifact);
      document.getElementById('download').href = '/api/artifacts/download?' + query;
      document.getElementById('preview').src = '/api/artifacts/preview?' + query;
      document.getElementById('viewer').hidden = false;
    } else {
      const data = await get('/api/artifacts');
      if (!data.items.length) library.appendChild(text('p', 'Nothing saved yet. Ask for a document, report or illustrated card.', 'sub'));
      data.items.forEach(item => {
        const row = text('article', '', 'item');
        row.appendChild(text('h2', item.title));
        row.appendChild(text('div', meta(item), 'meta'));
        if (item.note) row.appendChild(text('p', item.note));
        const actions = text('div', '', 'actions');
        const open = text('a', 'Open document'); open.href = item.url;
        const download = text('a', 'Download HTML');
        download.href = '/api/artifacts/download?' + new URLSearchParams({id: item.artifact_id, revision: item.revision});
        actions.append(open, download); row.appendChild(actions); library.appendChild(row);
      });
    }
  } catch (error) { document.getElementById('error').textContent = error.message; }
})();
