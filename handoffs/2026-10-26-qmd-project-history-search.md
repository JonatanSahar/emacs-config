# Handoff — 2026-10-26 — qmd local search over project history docs

## Quick State

- qmd 2.8.3 (tobi/qmd: local BM25 + vector + rerank search) installed and registered as a user-scope MCP server.
- Two collections indexed: `spatial-ccds` (321 docs) and `sc-immage` (170 docs), incl. `docs/` (added at end of session).
- Embedding of the 66 new `docs/` files was running in background at wrapup (`/tmp/qmd-embed.log`); verify with `qmd status` (Pending should be 0).
- Auto-refresh NOT installed — systemd user timer was blocked by the auto-mode classifier. Index is stale until `qmd update && qmd embed` runs.
- Both projects' CLAUDE.md got an uncommitted "Tool Usage: qmd" section.

## What This Session Was About

User wanted a RAG-like tool so Claude can cheaply retrieve past decisions and history (decision records, analysis-log, STATUS, project-notes, handoffs) in `~/Projects/spatial-ccds` and `~/Projects/sc-immage` — explicitly not code navigation. Researched options, picked qmd, installed and indexed it.

## Current State Detail

Files touched (none in this repo besides this handoff):
- `~/.local/bin/qmd` — wrapper: `exec /usr/bin/node ~/.local/share/qmd-npm/lib/node_modules/@tobilu/qmd/dist/cli/qmd.js "$@"`
- `~/.local/share/qmd-npm/` — npm prefix for the install (installed with `/usr/bin/npm`)
- `~/.config/qmd/index.yml` — collections, masks, contexts
- `~/.cache/qmd/index.sqlite` (~16 MB), `~/.cache/qmd/models/` (embeddinggemma-300M; query-expansion 1.7B + Qwen3 reranker 0.6B download on first `query`)
- `~/.claude.json` — `claude mcp add -s user qmd -- /home/yonatan/.local/bin/qmd mcp`
- `~/Projects/sc-immage/CLAUDE.md`, `~/Projects/spatial-ccds/CLAUDE.md` — new "Tool Usage: qmd (project history search)" section, **uncommitted**. spatial-ccds had unrelated pre-existing uncommitted changes; left untouched.

Collection mask (both projects):
`STATUS.md,analysis-log.org,project-notes.md,project-notes.org,runs/*/*.md,runs/*/decisions/**/*.md,handoffs/**/*.md,docs/**/*.md,docs/**/*.org`

Measured: first full embed of 425 docs / 1833 chunks took 11m46s on CPU (no GPU). CLI `vsearch`/`query` ~25–30s each (cold model load per process); MCP server should be faster warm — not measured. Results on test queries were relevant (e.g. "excluding edge FOVs" → `runs/ingest-t4-biopsies/decisions/009-exclude-zero-overlap-edge-fovs.md`).

No run, no kernel.

## Decisions Made

(No run dir — this is the only record.)

- **qmd over alternatives.** Weighed: claude-context (Zilliz; code-oriented, needs Milvus + embedding API), Serena (LSP symbol nav — wrong problem), mcp-docsearch / bborbe semantic-search (small projects), dual-graph (already installed, code/action graph). Chose qmd: markdown-native, fully local, hybrid BM25+vector (exact IDs like `v5r2`/gene names need BM25; "why did we…" needs vectors), ~30k stars, MCP + Claude plugin. Settles the tool choice for doc-history retrieval.
- **System node, not conda node.** `~/miniforge3/bin/node` segfaults in `napi_module_register_by_symbol` loading any native addon (better-sqlite3), even after `npm rebuild`; bun also segfaulted. `/usr/bin/node` (v22.22.2) works. qmd's own launcher uses `#!/usr/bin/env node` → conda node, so a wrapper pins `/usr/bin/node`. If qmd breaks after a conda/PATH change, check this first.
- **Scoped masks, not whole repo.** Projects hold ~75 MB of md/org (mostly generated reports); indexed only history docs + `docs/` (~490 files). `research/` and `reports/` deliberately excluded.
- **MCP at user scope** (not plugin): plugin route would launch qmd via PATH node → segfault.

## Resume Procedure

1. Verify embedding finished: `qmd status | grep -E 'Vectors|Pending'` → expect no "need embedding". If pending: `qmd embed`.
2. Install auto-refresh (needs user approval — classifier blocked it):
   ```sh
   d=~/.config/systemd/user
   printf '[Unit]\nDescription=qmd refresh\n[Service]\nType=oneshot\nNice=15\nExecStart=/bin/sh -c "%%h/.local/bin/qmd update && %%h/.local/bin/qmd embed"\n' > $d/qmd-refresh.service
   printf '[Timer]\nOnCalendar=hourly\nPersistent=true\n[Install]\nWantedBy=timers.target\n' > $d/qmd-refresh.timer
   systemctl --user daemon-reload && systemctl --user enable --now qmd-refresh.timer
   ```
3. Commit the CLAUDE.md qmd section in each project (user's call; spatial-ccds has other dirty files — stage only `CLAUDE.md`).
4. In a fresh session in either project, confirm `mcp__qmd__*` tools appear and time a warm `query`.

## Behaviors Captured

- "its not for this repo … I dont need code navigation, I'm more interested in retrieving decisions made and things we did - which are stored mainly as decision records in under runs/runX or in the analysis-log or, STATUS, or project-notes of those projects"

## Pointers

- https://github.com/tobi/qmd
- `~/.config/qmd/index.yml`
- `~/.local/bin/qmd`
- `/tmp/qmd-embed.log` (ephemeral)
