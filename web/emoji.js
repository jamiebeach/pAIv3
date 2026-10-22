'use strict';
// A small emoji picker: search by name, categories, recently used first.
// window.paiEmoji.open(anchor, onPick) shows it beside ANCHOR;
// window.paiEmoji.insertInto(textarea, emoji) puts an emoji at the cursor.
(function () {
  var DATA = [
    ['Smileys', [
      ['😀', 'grin smile happy'], ['😃', 'smile happy joy'], ['😄', 'smile laugh happy'], ['😁', 'grin beam'],
      ['😆', 'laugh squint'], ['😅', 'sweat smile relief'], ['🤣', 'rofl rolling laugh'], ['😂', 'joy tears laugh'],
      ['🙂', 'slight smile'], ['🙃', 'upside down'], ['😉', 'wink'], ['😊', 'blush smile'], ['😇', 'halo innocent angel'],
      ['🥰', 'love hearts adore'], ['😍', 'heart eyes love'], ['🤩', 'star struck wow'], ['😘', 'kiss'], ['😋', 'yum tasty'],
      ['😛', 'tongue'], ['😜', 'wink tongue silly'], ['🤪', 'zany crazy'], ['🤓', 'nerd geek'], ['😎', 'cool sunglasses'],
      ['🥳', 'party celebrate'], ['😏', 'smirk'], ['😌', 'relieved calm'], ['😴', 'sleep tired'], ['🤔', 'thinking hmm'],
      ['🤨', 'raised eyebrow skeptical'], ['😐', 'neutral'], ['😑', 'expressionless'], ['😶', 'no mouth speechless'],
      ['🙄', 'eye roll'], ['😬', 'grimace awkward'], ['😮', 'open mouth surprised'], ['😯', 'hushed'], ['😲', 'astonished shock'],
      ['😳', 'flushed embarrassed'], ['🥺', 'pleading puppy eyes'], ['😢', 'cry sad tear'], ['😭', 'sob crying'],
      ['😤', 'triumph huff'], ['😠', 'angry'], ['😡', 'rage angry'], ['🤯', 'mind blown exploding'], ['😱', 'scream fear'],
      ['🤗', 'hug'], ['🤭', 'giggle oops'], ['🤫', 'shush quiet'], ['🫡', 'salute'], ['🫠', 'melting'], ['😈', 'devil mischief'],
      ['👀', 'eyes look see'], ['💀', 'skull dead lol'], ['🤖', 'robot bot ai'], ['👻', 'ghost boo'], ['😺', 'cat smile']
    ]],
    ['Gestures', [
      ['👍', 'thumbs up yes like ok'], ['👎', 'thumbs down no dislike'], ['👏', 'clap applause bravo'], ['🙌', 'raised hands hooray'],
      ['🙏', 'pray thanks please'], ['🤝', 'handshake deal agree'], ['👋', 'wave hello bye'], ['✌️', 'peace victory'],
      ['🤞', 'fingers crossed luck'], ['👌', 'ok perfect'], ['🤌', 'chef kiss'], ['💪', 'muscle strong flex'], ['🫶', 'heart hands'],
      ['👊', 'fist bump'], ['✋', 'raised hand stop'], ['☝️', 'point up'], ['👉', 'point right'], ['🤷', 'shrug dunno'],
      ['🤦', 'facepalm'], ['🙋', 'raise hand me'], ['💁', 'tipping hand info'], ['🧠', 'brain smart think']
    ]],
    ['Hearts', [
      ['❤️', 'red heart love'], ['🧡', 'orange heart'], ['💛', 'yellow heart'], ['💚', 'green heart'], ['💙', 'blue heart'],
      ['💜', 'purple heart'], ['🖤', 'black heart'], ['🤍', 'white heart'], ['💖', 'sparkling heart'], ['💕', 'two hearts'],
      ['💔', 'broken heart'], ['❤️‍🔥', 'heart on fire'], ['💯', 'hundred perfect'], ['✨', 'sparkles magic'], ['🔥', 'fire lit hot'],
      ['⭐', 'star'], ['🌟', 'glowing star'], ['💫', 'dizzy'], ['⚡', 'lightning zap'], ['💥', 'boom'], ['🎉', 'tada party celebrate'],
      ['🎊', 'confetti'], ['🏆', 'trophy win'], ['🥇', 'gold medal first'], ['🎯', 'target bullseye'], ['🚀', 'rocket launch ship']
    ]],
    ['Things', [
      ['✅', 'check done yes'], ['☑️', 'checkbox'], ['❌', 'cross no wrong'], ['⚠️', 'warning'], ['❓', 'question'], ['❗', 'exclamation'],
      ['💡', 'idea bulb'], ['📌', 'pin'], ['📎', 'paperclip attach'], ['📝', 'memo note write'], ['📚', 'books read'], ['📖', 'book open'],
      ['🔍', 'search magnify'], ['🔧', 'wrench fix'], ['🛠️', 'tools build'], ['⚙️', 'gear settings'], ['🐛', 'bug'], ['🧪', 'test lab'],
      ['💻', 'laptop computer'], ['🖥️', 'desktop'], ['📱', 'phone'], ['⌛', 'hourglass wait'], ['⏰', 'alarm clock'], ['📅', 'calendar date'],
      ['💰', 'money bag'], ['📈', 'chart up growth'], ['📉', 'chart down'], ['🎮', 'game controller'], ['♟️', 'chess pawn'],
      ['🎲', 'dice'], ['🎵', 'music note'], ['🎨', 'art palette'], ['📷', 'camera photo'], ['🔒', 'lock secure'], ['🔑', 'key'],
      ['🗡️', 'dagger sword'], ['🛡️', 'shield'], ['🧙', 'mage wizard'], ['🐉', 'dragon'], ['⚔️', 'swords battle']
    ]],
    ['Nature & food', [
      ['🐶', 'dog puppy'], ['🐱', 'cat'], ['🐾', 'paw prints'], ['🦊', 'fox'], ['🐻', 'bear'], ['🦉', 'owl'], ['🦋', 'butterfly'],
      ['🌱', 'seedling grow'], ['🌸', 'blossom flower'], ['🌈', 'rainbow'], ['☀️', 'sun'], ['🌙', 'moon night'], ['❄️', 'snow cold'],
      ['🌊', 'wave ocean'], ['☕', 'coffee'], ['🍵', 'tea'], ['🍕', 'pizza'], ['🍔', 'burger'], ['🍰', 'cake'], ['🍷', 'wine'],
      ['🍺', 'beer'], ['🥂', 'cheers toast'], ['🍎', 'apple'], ['🥑', 'avocado'], ['🏃', 'run running'], ['🎾', 'tennis padel'],
      ['🏋️', 'weights gym'], ['🚴', 'bike cycling'], ['🧘', 'yoga meditate']
    ]]
  ];
  var RECENT_KEY = 'pai-emoji-recent';

  function recent() { try { return JSON.parse(localStorage.getItem(RECENT_KEY) || '[]'); } catch (e) { return []; } }
  function remember(emoji) {
    try {
      var list = recent().filter(function (e) { return e !== emoji; });
      list.unshift(emoji);
      localStorage.setItem(RECENT_KEY, JSON.stringify(list.slice(0, 16)));
    } catch (e) {}
  }

  var style = document.createElement('style');
  style.textContent =
    '.emoji-panel{position:absolute;z-index:1000;width:min(320px,calc(100vw - 32px));max-height:320px;display:flex;flex-direction:column;' +
    'background:var(--panel,#161a21);color:var(--fg,#e6e8ec);border:1px solid var(--line,#2a2f3a);border-radius:10px;box-shadow:0 8px 24px rgba(0,0,0,.35);padding:8px}' +
    '.emoji-panel input{width:100%;font:inherit;font-size:14px;padding:6px 8px;margin:0 0 6px;border:1px solid var(--line,#2a2f3a);border-radius:6px;background:var(--bg,#101216);color:inherit}' +
    '.emoji-panel .grid{overflow-y:auto;flex:1}' +
    '.emoji-panel h4{font-size:11px;font-weight:600;color:var(--muted,#9aa3b2);margin:6px 2px 2px;text-transform:uppercase;letter-spacing:.05em}' +
    '.emoji-panel button.e{font-size:22px;line-height:1;width:36px;height:36px;border:0;border-radius:6px;background:transparent;cursor:pointer;padding:0}' +
    '.emoji-panel button.e:hover,.emoji-panel button.e:focus{background:var(--line,#2a2f3a);outline:none}' +
    '.emoji-btn{font-size:18px;line-height:1;padding:4px 8px;border:1px solid var(--line,#2a2f3a);border-radius:6px;background:transparent;cursor:pointer}';
  document.head.appendChild(style);

  var panel = null;
  function close() {
    if (panel) { panel.remove(); panel = null; }
    document.removeEventListener('mousedown', outside, true);
    document.removeEventListener('keydown', escape, true);
  }
  function outside(ev) { if (panel && !panel.contains(ev.target)) close(); }
  function escape(ev) { if (ev.key === 'Escape') { ev.stopPropagation(); close(); } }

  function open(anchor, onPick) {
    close();
    panel = document.createElement('div');
    panel.className = 'emoji-panel';
    panel.setAttribute('role', 'dialog');
    panel.setAttribute('aria-label', 'Choose an emoji');
    var search = document.createElement('input');
    search.type = 'search'; search.placeholder = 'Search emoji'; search.setAttribute('aria-label', 'Search emoji');
    var grid = document.createElement('div'); grid.className = 'grid';
    panel.append(search, grid);

    function button(emoji, name) {
      var b = document.createElement('button');
      b.type = 'button'; b.className = 'e'; b.textContent = emoji; b.title = name || '';
      b.setAttribute('aria-label', name || emoji);
      b.onclick = function () { remember(emoji); close(); onPick(emoji); };
      return b;
    }
    function section(title, items) {
      if (!items.length) return;
      var h = document.createElement('h4'); h.textContent = title; grid.appendChild(h);
      items.forEach(function (it) { grid.appendChild(button(it[0], it[1])); });
    }
    function render() {
      grid.replaceChildren();
      var q = search.value.trim().toLowerCase();
      if (q) {
        var hits = [];
        DATA.forEach(function (c) { c[1].forEach(function (it) { if (it[1].indexOf(q) !== -1) hits.push(it); }); });
        if (hits.length) section('Results', hits);
        else { var p = document.createElement('div'); p.textContent = 'No match.'; p.style.color = 'var(--muted,#9aa3b2)'; grid.appendChild(p); }
        return;
      }
      section('Recent', recent().map(function (e) { return [e, '']; }));
      DATA.forEach(function (c) { section(c[0], c[1]); });
    }
    search.oninput = render;
    search.onkeydown = function (ev) {
      if (ev.key === 'Enter') { ev.preventDefault(); var first = grid.querySelector('button.e'); if (first) first.click(); }
      if (ev.key === 'ArrowDown') { ev.preventDefault(); var b = grid.querySelector('button.e'); if (b) b.focus(); }
    };
    grid.onkeydown = function (ev) {
      var buttons = Array.prototype.slice.call(grid.querySelectorAll('button.e'));
      var i = buttons.indexOf(document.activeElement);
      if (i < 0) return;
      var step = { ArrowRight: 1, ArrowLeft: -1, ArrowDown: 8, ArrowUp: -8 }[ev.key];
      if (step) { ev.preventDefault(); var next = buttons[Math.max(0, Math.min(buttons.length - 1, i + step))]; if (next) next.focus(); }
    };
    render();
    document.body.appendChild(panel);
    var r = anchor.getBoundingClientRect();
    var top = r.bottom + window.scrollY + 4;
    if (r.bottom + 330 > window.innerHeight && r.top > 340) top = r.top + window.scrollY - panel.offsetHeight - 4;
    var left = Math.min(r.left + window.scrollX, window.scrollX + document.documentElement.clientWidth - panel.offsetWidth - 16);
    panel.style.top = top + 'px';
    panel.style.left = Math.max(window.scrollX + 16, left) + 'px';
    document.addEventListener('mousedown', outside, true);
    document.addEventListener('keydown', escape, true);
    search.focus();
  }

  function insertInto(field, emoji) {
    var start = field.selectionStart == null ? field.value.length : field.selectionStart;
    var end = field.selectionEnd == null ? start : field.selectionEnd;
    field.value = field.value.slice(0, start) + emoji + field.value.slice(end);
    var at = start + emoji.length;
    field.focus();
    try { field.setSelectionRange(at, at); } catch (e) {}
    field.dispatchEvent(new Event('input', { bubbles: true }));
  }

  window.paiEmoji = { open: open, close: close, insertInto: insertInto };
})();
