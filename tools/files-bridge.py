"""Read-only project files for one agent. Run with a private JSON config path.

No shell, Git command, file mutation, or arbitrary URL operation is exposed.
The separate admin token may change the folder allow-list, never file content.
See docs/behaviour/host-folders.md for setup and the precise trust boundary.
"""
import hashlib
import hmac
import json
import os
from pathlib import Path
import re
import secrets
import stat
import sys
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

MAX_FILE = 1024 * 1024
PAGE = 12000
MAX_ENTRIES = 100
MAX_SCAN = 1000
ID = re.compile(r"^[a-zA-Z0-9_-]{1,48}$")
DENIED_DIRS = {"node_modules", "private", ".ssh", ".aws", ".azure", ".gnupg",
               ".kube", ".docker", ".codex", ".claude", "__pycache__"}
DENIED_NAMES = {"credentials", "credentials.json", "secrets.json", "secrets.toml",
                ".npmrc", ".pypirc", ".netrc", "id_rsa", "id_ed25519",
                "id_ecdsa", ".git-credentials", "keyring.json"}
DENIED_SUFFIXES = {".pem", ".key", ".pfx", ".p12", ".crt", ".cer", ".token",
                   ".db", ".sqlite", ".sqlite3"}
SECRET = re.compile(
    r"-----BEGIN (?:[A-Z ]*PRIVATE KEY|OPENSSH PRIVATE KEY)-----"
    r"|(?:gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,})"
    r"|(?:AKIA|ASIA)[A-Z0-9]{16}"
    r"|(?:sk-[A-Za-z0-9_-]{20,}|xox[baprs]-[A-Za-z0-9-]{15,})"
    r"|https?://[^\s/:]+:[^\s/@]+@"
    r"|(?im:^\s*(?:export\s+)?[\w.-]*(?:api[_-]?key|password|secret|token)"
    r"[\w.-]*\s*[:=]\s*[\"']?[^\s\"'${}<]{12,})")


class Refused(Exception):
    pass


def blocked(name):
    lower = name.lower()
    return (lower in DENIED_DIRS or lower in DENIED_NAMES or lower.startswith('.env')
            or re.fullmatch(r'(con|prn|aux|nul|com[1-9]|lpt[1-9])(?:\..*)?', lower) is not None
            or Path(lower).suffix in DENIED_SUFFIXES)


def linked(info):
    return stat.S_ISLNK(info.st_mode) or bool(getattr(info, 'st_file_attributes', 0) & 0x400)


def within(path, root):
    try:
        return os.path.commonpath([os.path.normcase(str(path)), os.path.normcase(str(root))]) == os.path.normcase(str(root))
    except ValueError:
        return False


def final_path(fd):
    """Resolve the opened handle, not just its pre-open name (rename/link races)."""
    if os.name == 'nt':
        import ctypes
        from ctypes import wintypes
        import msvcrt
        fn = ctypes.WinDLL('kernel32', use_last_error=True).GetFinalPathNameByHandleW
        fn.argtypes = [wintypes.HANDLE, wintypes.LPWSTR, wintypes.DWORD, wintypes.DWORD]
        fn.restype = wintypes.DWORD
        buf = ctypes.create_unicode_buffer(32768)
        n = fn(msvcrt.get_osfhandle(fd), buf, len(buf), 0)
        if not 0 < n < len(buf):
            raise Refused('Cannot verify the opened file.')
        name = buf.value
        if name.startswith('\\\\?\\UNC\\'):
            name = '\\\\' + name[8:]
        elif name.startswith('\\\\?\\'):
            name = name[4:]
        return Path(name)
    for base in ('/proc/self/fd', '/dev/fd'):
        p = Path(base) / str(fd)
        if p.exists():
            return p.resolve(strict=True)
    raise Refused('This host cannot verify opened file handles.')


class Bridge:
    def __init__(self, config_path):
        self.path = Path(config_path).resolve(strict=True)
        self.lock = threading.RLock()
        self.config = json.loads(self.path.read_text(encoding='utf-8'))
        self.tokens = {}
        self.protected = {self.path}
        for role in ('read', 'admin'):
            path = Path(self.config[role + '_token_file'])
            if not path.is_absolute():
                path = self.path.parent / path
            if not path.exists():
                path.parent.mkdir(parents=True, exist_ok=True)
                fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
                with os.fdopen(fd, 'w', encoding='ascii') as f:
                    f.write(secrets.token_urlsafe(32))
            self.protected.add(path.resolve(strict=True))
            self.tokens[role] = path.read_text(encoding='ascii').strip()
            if len(self.tokens[role]) < 32:
                raise ValueError('Bridge tokens must contain at least 32 characters.')
        if self.tokens['read'] == self.tokens['admin']:
            raise ValueError('Read and administration tokens must differ.')
        self.roots = self.validate_roots(self.config.get('folders', []))

    def validate_roots(self, folders):
        if not isinstance(folders, list) or len(folders) > 16:
            raise Refused('Supply at most 16 folders.')
        roots = {}
        for item in folders:
            if not isinstance(item, dict) or not ID.fullmatch(str(item.get('id', ''))):
                raise Refused('Each folder needs a short id of letters, digits, dash or underscore.')
            name = item['id']
            raw = item.get('path')
            if not isinstance(raw, str) or len(raw) > 512 or not Path(raw).is_absolute():
                raise Refused('Each folder needs an absolute host path.')
            p = Path(raw)
            if name in roots or not p.is_dir() or linked(p.lstat()):
                raise Refused('Folder ids must be unique and paths must be real directories.')
            if any(blocked(part) for part in p.parts):
                raise Refused('Credential and private directories cannot be shared.')
            roots[name] = p.resolve(strict=True)
        return roots

    def folders(self):
        return [{'id': k, 'path': str(v)} for k, v in self.roots.items()]

    def replace_folders(self, folders):
        roots = self.validate_roots(folders)
        config = dict(self.config, folders=[{'id': k, 'path': str(v)} for k, v in roots.items()])
        temporary = self.path.with_suffix('.tmp')
        try:
            fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
            with os.fdopen(fd, 'w', encoding='utf-8', newline='\n') as f:
                json.dump(config, f, indent=2)
                f.flush()
                os.fsync(f.fileno())
            os.replace(temporary, self.path)
        except Exception:
            # Do not remove a pre-existing file owned by someone else.
            raise Refused('Could not save folder permissions; no change was applied.') from None
        self.config, self.roots = config, roots
        return {'folders': self.folders()}

    def target(self, folder, relative):
        root = self.roots.get(folder)
        if root is None:
            raise Refused('That folder is not shared.')
        if not isinstance(relative, str) or len(relative) > 2048 or '\\' in relative or ':' in relative:
            raise Refused('Use a relative path with forward slashes.')
        parts = relative.split('/') if relative else []
        if any(p in ('', '.', '..') or p.rstrip(' .') != p or blocked(p) for p in parts):
            raise Refused('That path is excluded.')
        current = root
        if linked(root.lstat()) or root.resolve(strict=True) != root:
            raise Refused('The shared folder changed; reconnect it.')
        for part in parts:
            current /= part
            if linked(current.lstat()):
                raise Refused('Links and junctions are not followed.')
        resolved = current.resolve(strict=True)
        if not within(resolved, root) or resolved in self.protected:
            raise Refused('That path is outside the permitted files.')
        return root, resolved

    def run(self, args):
        # Revocation and each read share the lock, so a successful revocation
        # cannot race a subsequent read using the old allow-list.
        with self.lock:
            action = args.get('action')
            if action == 'folders':
                return {'folders': self.folders()}
            if action not in ('list', 'read'):
                raise Refused('action must be folders, list or read.')
            root, path = self.target(args.get('folder'), args.get('path', ''))
            offset = args.get('offset', 0)
            if type(offset) is not int or not 0 <= offset <= MAX_FILE:
                raise Refused('offset must be a nonnegative bounded integer.')
            if action == 'list':
                if not path.is_dir():
                    raise Refused('That path is not a directory.')
                entries, scanned, more = [], 0, False
                with os.scandir(path) as iterator:
                    for i, entry in enumerate(iterator):
                        if i < offset:
                            continue
                        if scanned >= MAX_SCAN or len(entries) >= MAX_ENTRIES:
                            more = True
                            break
                        scanned += 1
                        try:
                            _, checked = self.target(args['folder'], str(Path(entry.path).relative_to(root)).replace('\\', '/'))
                            info = checked.stat()
                            if stat.S_ISREG(info.st_mode) and info.st_nlink != 1:
                                continue
                            entries.append({'name': entry.name, 'kind': 'directory' if stat.S_ISDIR(info.st_mode) else 'file'})
                        except (Refused, OSError):
                            continue
                return {'entries': entries, 'next_offset': offset + scanned if more else None}
            if not stat.S_ISREG(path.stat().st_mode):
                raise Refused('Only regular text files are readable.')
            fd = os.open(path, os.O_RDONLY | getattr(os, 'O_NOFOLLOW', 0)
                         | getattr(os, 'O_NONBLOCK', 0) | getattr(os, 'O_BINARY', 0))
            with os.fdopen(fd, 'rb') as f:
                info = os.fstat(f.fileno())
                if not stat.S_ISREG(info.st_mode) or info.st_nlink != 1 or info.st_size > MAX_FILE:
                    raise Refused('Only regular, single-link text files up to 1 MiB are readable.')
                actual = final_path(f.fileno())
                if not within(actual, root) or actual in self.protected or actual != path:
                    raise Refused('The file changed during access.')
                data = f.read(MAX_FILE + 1)
            if len(data) > MAX_FILE or b'\0' in data:
                raise Refused('This is not a bounded text file.')
            try:
                content = data.decode('utf-8')
            except UnicodeDecodeError:
                raise Refused('Only UTF-8 text is readable.') from None
            if SECRET.search(content) or any(t in content for t in self.tokens.values()):
                raise Refused('This file appears to contain credentials and is withheld.')
            end = min(len(content), offset + PAGE)
            return {'text': content[offset:end], 'next_offset': end if end < len(content) else None,
                    'sha256': hashlib.sha256(data).hexdigest(), 'characters': len(content)}


def handler(bridge):
    class Handler(BaseHTTPRequestHandler):
        def reply(self, status, body):
            data = json.dumps(body).encode('utf-8')
            self.send_response(status)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(len(data)))
            self.send_header('Cache-Control', 'no-store')
            self.end_headers()
            self.wfile.write(data)

        def do_POST(self):
            role = 'admin' if self.path == '/folders' else 'read'
            given = self.headers.get('Authorization', '').encode()
            if not hmac.compare_digest(given, ('Bearer ' + bridge.tokens[role]).encode()):
                return self.reply(401, {'error': 'not authorised'})
            if self.path not in ('/run', '/folders'):
                return self.reply(404, {'error': 'no such route'})
            try:
                length = int(self.headers.get('Content-Length', '0'))
                if not 0 < length <= 16384:
                    raise Refused('Request must be between 1 and 16384 bytes.')
                self.connection.settimeout(10)
                args = json.loads(self.rfile.read(length))
                if not isinstance(args, dict):
                    raise Refused('Supply an object.')
                with bridge.lock:
                    result = bridge.replace_folders(args.get('folders')) if role == 'admin' else bridge.run(args)
                self.reply(200, result)
            except (Refused, ValueError, OSError) as e:
                self.reply(400, {'error': str(e) if isinstance(e, Refused) else 'Invalid request or unavailable path.'})

        def log_message(self, fmt, *args):
            # No request bodies, tokens, file contents or filesystem exception details.
            sys.stderr.write('files-bridge: ' + fmt % args + '\n')
    return Handler


if __name__ == '__main__':
    bridge = Bridge(sys.argv[1])
    address = bridge.config.get('address', '127.0.0.1')
    port = bridge.config.get('port', 8770)
    print(f'files-bridge listening on {address}:{port}', flush=True)
    ThreadingHTTPServer((address, port), handler(bridge)).serve_forever()
