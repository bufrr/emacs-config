# AGENTS.md

Guidance for coding agents working in this repository.

## Repository Overview

This is a personal Doom Emacs configuration with a GTD-focused workflow. The JavaScript GTD app has moved to the standalone AgentDeck repository.

Main files:

- `init.el`: Doom module selection.
- `config.el`: Personal Emacs, org, GTD, dashboard, and blog configuration.
- `packages.el`: Extra Doom package declarations.
- `CLAUDE.md`: Older assistant guidance; keep it broadly aligned with this file when changing agent-facing instructions.

## Working Rules

- Do not edit Doom internals under `~/.emacs.d/`; only edit this config repo unless explicitly asked.
- Preserve user data and local changes. Do not reset, checkout, or delete unrelated work.
- Prefer small, scoped changes that follow the existing style.
- Use `rg`/`rg --files` for search.
- AgentDeck app code lives in `~/agentdeck`, not in this repo.

## Doom Emacs Changes

When changing `config.el`, `init.el`, or `packages.el`:

- Use Doom idioms such as `after!`, `use-package!`, `add-hook!`, and `map!`.
- Keep `lexical-binding` headers intact.
- Run or recommend `doom sync` after config/package/module changes.
- Use `doom doctor` when investigating Doom or package-load issues.

Useful commands:

```sh
doom sync
doom doctor
doom env
```

## GTD Data Model

Configured Org paths:

- `~/org/gtd/current.org`
- `~/org/gtd/archive.org`

AgentDeck imports Org files as an initial seed, uses SQLite as the primary UI store, and exports Org text via its export endpoint. If the Org files are absent, AgentDeck can still start with an empty SQLite store.

AgentDeck has no built-in authentication. Bind it to `127.0.0.1` unless the user explicitly asks for broader network exposure and accepts that risk.

## AgentDeck

Path:

```sh
cd ~/agentdeck
```

Package scripts:

```sh
npm ci
npm test
npm run test:e2e:install
npm run test:e2e
npm start
```

Notes:

- `package.json` declares the supported Node runtime.
- The server defaults to `127.0.0.1:8787`.
- For UI changes, run both `npm test` and `npm run test:e2e`.
- The E2E suite starts its own temporary server and temporary GTD data; it should not mutate real `~/org/gtd` files.

Manual smoke checks:

```sh
curl http://127.0.0.1:8787/api/state
```

For browser-visible UI changes, use headless Playwright screenshots for desktop and mobile viewports when possible.

## Documentation Consistency

When behavior changes, update the relevant docs in the same pass:

- `README.md` for user-facing workflow and keybindings.
- `CLAUDE.md` and `AGENTS.md` for assistant-facing workflow.

If documentation and code disagree, trust the code and tests first, then update the docs.
