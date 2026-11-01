# Plugins

Code: `src/api/plugins.lisp`. Operator request 2026-10-31: let the agent gain
capabilities (an embodied avatar first; system-1 senses and more later) without
their code living in this repository.

## What a plugin is

An ASDF system in its own folder under the plugins directory:

    $PAI_PLUGINS/<name>/<name>.asd        (default: ~/plugins/, which in the
                                           containers is /home/pai/plugins on the
                                           agent's volume)

At start, before the agent opens, `load-plugins` loads every such folder. A
plugin that fails to load is recorded and skipped; it never stops the agent.
Removing the folder (and restarting) removes the plugin. `peek plugins` lists
what was found and whether each loaded.

## What a plugin can do

It runs in the agent's process, so every existing extension point is open to it:

- `define-tool`: a new ability the agent can call.
- `define-slot-source`, plus a profile slot: something new in the agent's context.
- `define-projection`: state of its own, rebuilt from the ledger like any other.
- `*periodic-hooks*`: background work, such as a system-1 sensor.
- `define-plugin-route "<name>" (agent method subpath origin) ...`: web pages and
  endpoints at `/plugins/<name>/...`, behind the operator's own sign-in (a POST
  must also be same-origin). `plugin-reply` and `plugin-static` help; static
  files are served only from inside the folder given.
- `(plugin-menu-entry "<name>" "<label>")`: offer the plugin's page,
  `/plugins/<name>/`, in the operator's menu. The menu reads `GET /api/v2/plugins`
  (signed in), which lists only entries whose plugin answers a route.

## What it must not do

The same rules as the agent's own code: no secrets in its folder or source, egress
through the kernel's transport, operator authority only through the signed-in
origin it is handed. It is trusted code: install only plugins you have read.
Plugins are in-process, so slow work belongs in a separate program the plugin
runs (the avatar's text-to-speech is a separate process), not in a request thread.
