"""Synthetic security and paging checks; never reads a real project or secret."""
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import threading
import unittest
import sys
from urllib.error import HTTPError
from urllib.request import Request, urlopen

sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location('files_bridge', Path(__file__).resolve().parents[1] / 'tools/files-bridge.py')
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)


class FilesBridgeTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name)
        self.root = self.base / 'project'
        self.root.mkdir()
        self.config = self.base / 'bridge.json'
        self.config.write_text(json.dumps({'read_token_file': 'reader.token', 'admin_token_file': 'admin.token',
                                          'folders': [{'id': 'demo', 'path': str(self.root)}]}))
        self.bridge = m.Bridge(self.config)

    def tearDown(self):
        self.temp.cleanup()

    def read(self, path, **kw):
        return self.bridge.run(dict(action='read', folder='demo', path=path, **kw))

    def test_git_metadata_and_paged_read(self):
        (self.root / '.git').mkdir()
        (self.root / '.git' / 'HEAD').write_text('ref: refs/heads/main\n', newline='\n')
        self.assertEqual('ref: refs/heads/main\n', self.read('.git/HEAD')['text'])
        text = 'A synthetic report.\n' * 1200
        (self.root / 'report.txt').write_text(text, newline='\n')
        first = self.read('report.txt')
        second = self.read('report.txt', offset=first['next_offset'])
        self.assertEqual(text, first['text'] + second['text'])
        self.assertEqual(first['sha256'], second['sha256'])

    def test_credential_paths_and_traversal(self):
        for name in ('.env', '.env.sample', 'credentials.json', 'secret.pem', '.npmrc'):
            (self.root / name).write_text('synthetic')
            with self.assertRaises(m.Refused):
                self.read(name)
        for name in ('../bridge.json', '/etc/passwd', 'a/../file', 'C:/file', 'a\\b', 'file:stream', 'file.', 'CON', 'NUL.txt'):
            with self.assertRaises(m.Refused):
                self.read(name)
        names = [r['name'] for r in self.bridge.run(dict(action='list', folder='demo'))['entries']]
        self.assertEqual([], names)

    def test_credentials_are_checked_before_any_page_is_returned(self):
        text = ('safe text\n' * 2000) + '\nAPI_TOKEN=' + 'synthetic_not_a_real_token_123456\n'
        (self.root / 'settings.txt').write_text(text)
        with self.assertRaises(m.Refused):
            self.read('settings.txt')
        (self.root / '.git').mkdir()
        (self.root / '.git' / 'config').write_text('[remote]\nurl=https://user:synthetic@example.test/repo\n')
        with self.assertRaises(m.Refused):
            self.read('.git/config')

    def test_links_binary_size_and_other_roots(self):
        outside = self.base / 'outside.txt'
        outside.write_text('not shared')
        try:
            (self.root / 'alias').symlink_to(outside)
        except OSError:
            pass  # Windows without symlink privilege: hardlink check still runs.
        else:
            with self.assertRaises(m.Refused):
                self.read('alias')
        os.link(outside, self.root / 'hardlink')
        with self.assertRaises(m.Refused):
            self.read('hardlink')
        (self.root / 'binary').write_bytes(b'x\x00y')
        with self.assertRaises(m.Refused):
            self.read('binary')
        (self.root / 'huge').write_bytes(b'a' * (m.MAX_FILE + 1))
        with self.assertRaises(m.Refused):
            self.read('huge')
        with self.assertRaises(m.Refused):
            self.bridge.run(dict(action='list', folder='other'))

    def test_directory_paging_and_revocation(self):
        for i in range(105):
            (self.root / f'item-{i}').write_text('x')
        first = self.bridge.run(dict(action='list', folder='demo'))
        second = self.bridge.run(dict(action='list', folder='demo', offset=first['next_offset']))
        self.assertEqual(105, len(first['entries']) + len(second['entries']))
        self.bridge.replace_folders([])
        with self.assertRaises(m.Refused):
            self.read('item-0')
        self.assertEqual([], m.Bridge(self.config).folders())

    def test_read_token_cannot_administer_or_write(self):
        server = m.ThreadingHTTPServer(('127.0.0.1', 0), m.handler(self.bridge))
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        def post(path, role, data):
            req = Request(f'http://127.0.0.1:{server.server_port}' + path, json.dumps(data).encode(),
                          {'Authorization': 'Bearer ' + self.bridge.tokens[role], 'Content-Type': 'application/json'})
            with urlopen(req, timeout=5) as response:
                return json.load(response)
        try:
            self.assertEqual('demo', post('/run', 'read', {'action': 'folders'})['folders'][0]['id'])
            with self.assertRaises(HTTPError) as e:
                post('/folders', 'read', {'folders': []})
            self.assertEqual(401, e.exception.code)
            for action in ('write', 'exec', 'admin', 'delete'):
                with self.assertRaises(HTTPError) as e:
                    post('/run', 'read', {'action': action})
                self.assertEqual(400, e.exception.code)
            self.assertEqual([], post('/folders', 'admin', {'folders': []})['folders'])
        finally:
            server.shutdown()
            server.server_close()
            thread.join()


if __name__ == '__main__':
    unittest.main()
