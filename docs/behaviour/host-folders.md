# Read-only host folders

The operator grants project folders to one agent through Settings > Folders.
The `host_files` tool lists folders, lists a directory, and reads UTF-8 files.
It cannot write files, execute programs, invoke Git, or change permissions.
Only operator-audience thoughts receive it. Inputs remain external content.

`tools/files-bridge.py` runs on the host, outside the agent container. One
configuration and two independent tokens belong to each agent. The read
token cannot call `/folders`, the administration route. The operator API
requires a valid operator session and recent passkey verification before
using the administration token. Permission changes and tool accesses create
small audit events without file contents. Ordinary tool-result recording
preserves the data the agent actually reads.

`.git` is allowed. The same exclusions apply inside it: `.env*`, private and
credential directories, key/certificate files, credential configuration,
database files, and detected credential content are withheld. Binary objects
are not readable text. The bridge checks the whole bounded file before
returning any page. Pattern checks are supplementary, not a guarantee that
arbitrary text contains no secret; share project folders intended for the
agent, not a home directory or credential store.

No symlinks, junctions, reparse points, alternate streams, or hard-linked
files are read. The opened file handle is checked against the canonical
root, including on Windows. Files are at most 1 MiB; pages at most 12,000
characters, with a SHA-256 digest and continuation offset. Directory pages
return at most 100 entries, scanning at most 1,000 after the offset. A local
process controlling a shared folder is trusted not to mutate it adversarially;
directory pagination is not a snapshot. Revocation serializes with reads.

## Setup

Keep the configuration and token files outside version control. A synthetic
example (use a distinct port and token pair for each agent):

```json
{
  "address": "127.0.0.1",
  "port": 8770,
  "read_token_file": "files-read.token",
  "admin_token_file": "files-admin.token",
  "folders": [{"id": "project", "path": "Z:/projects/example"}]
}
```

Run `python tools/files-bridge.py private/files-bridge.json`. Missing tokens
are created without printing them. On Windows restrict the private directory
ACL to the operator. Choose an address reachable by the container; use a
private host network and firewall, not public exposure. This local bridge
uses bearer authentication over HTTP, matching the existing host bridge;
use a TLS proxy if it crosses an untrusted network.

Set `host_files.url` to the bridge base URL in Settings. Install its read
token as the host-bound secret `host-files` and its administration token as
`host-files-admin`, both bound only to the bridge host. Use `pai secret-set`
with each token file redirected to stdin while the agent is stopped; never
paste tokens into chat or put values in settings. Restart the agent, then
open Settings > Folders to view, add, or revoke folders. An unavailable bridge
is reported there; file access degrades without blocking conversation.

Host startup/service installation remains an operator deployment choice.
GitHub authentication, repository writes, test execution, and PR publication
are separate capabilities; sharing a folder authorizes none of those.

Qualification: `tests/files-bridge-test.py` exercises filesystem exclusions,
credential detection, paging, revocation, and token separation;
`mind-host-files-test.lisp` tests audience and transport limits; the folder
case in `api-pwa-test.lisp` tests session and step-up enforcement.
