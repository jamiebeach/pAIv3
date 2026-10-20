# Durable documents and cards

The `document` tool makes an actual downloadable HTML document, rather than a
message describing one. It is available only to operator-audience thoughts.
A document contains a title, subtitle, theme, layout and sections of plain text
with optional stored image IDs and captions. Images are embedded in the saved
HTML so downloads remain self-contained. This slice supports reports and cards;
it does not export PDF, run arbitrary HTML/CSS/JavaScript, or publish to peers.

## Life of a document

1. `save` creates an immutable draft revision. Saving with `artifact_id` adds a
   revision; it never overwrites the previous document.
2. `list` finds the newest 50 revisions, optionally for a docket work item.
   `read` returns the structured source for resuming and revising.
3. `keep` retains privately; `abandon` sets it aside; `deliver` requires an
   introduction and posts a normal operator web notice linking the exact version.
   Delivery and disposition are a single kernel append batch. Repeating delivery
   of an already delivered revision has no effect. No phone push is implied.

Drafts are private from other agents, not hidden from the operator: the signed-in
operator may inspect them in Made things. Saving alone does not add scratch work
to conversation context. The library retains disposition and revision history.
Docket runs and child thoughts inherit the work-item link automatically. Resumed
work can list that item's artifacts and read the previous structured source.
The existing scheduler and docket spending bounds remain authoritative.

## Storage and preview

The event log holds small metadata events with thought/work provenance and a
content digest. Authored source and rendered HTML live in the encrypted blob
store alongside images. The digest covers both source and rendering. Preserve
both ledger and blob store in backup/restore; replay reconstructs metadata and
never invokes the model or regenerates document content. Delivery verifies content availability and digest before creating a notice. Missing bytes are an
explicit content-unavailable error, not silently recreated content.

Preview is an iframe with no sandbox allowances. The response adds a sandbox CSP,
blocks all network sources except embedded image data, and disables forms, base
URLs and active content. The renderer escapes every text field, uses fixed theme
and layout values, and accepts only existing image IDs. It never accepts caller
markup, CSS, URLs or remote fonts. Downloads contain an equivalent restrictive
meta CSP. Four images, 24 sections, 120000 source characters and 32 MiB rendered
UTF-8 bound each operation. There is no document cache or corpus in memory.

## Remaining work

This is the first v3-6nl slice. Richer layouts, other artifact types, qualified PDF
export, deliberate autonomous work triggers and budget policy review remain on
the parent. No personality, affection or scheduled outreach is prescribed. No
GitHub write authority or host-folder write access is granted.
