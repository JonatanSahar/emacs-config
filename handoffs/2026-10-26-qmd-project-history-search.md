# Handoff — 2026-10-26 — qmd local search over project history docs

## Quick State

- **Split per project (end of session):** each project has its own named index — `qmd --index sc-immage …` / `qmd --index spatial-ccds …` (configs `~/.config/qmd/<proj>.yml`, DBs `~/.cache/qmd/<proj>.sqlite`). MCP `qmd` is registered at **local scope per project** (`claude mcp add -s local qmd -- ~/.local/bin/qmd --index <proj> mcp`); the user-scope server and default `index.yml`/`index.sqlite` were removed. Every plain `qmd` command below needs `--index <proj>`.
- qmd 2.8.3 (tobi/qmd: local BM25 + vector + rerank search) installed.
- Two collections indexed: `spatial-ccds` (321 docs) and `sc-immage` (170 docs), incl. `docs/` (added at end of session).
- All docs embedded (sc-immage 170 docs / 862 chunks; spatial-ccds 321 / 1388).
- Auto-refresh NOT installed — systemd user timer was blocked by the auto-mode classifier. Index is stale until `qmd --index <proj> update && qmd --index <proj> embed` runs.
- Both projects' CLAUDE.md got an uncommitted "Tool Usage: qmd" section.

## What This Session Was About

User wanted a RAG-like tool so Claude can cheaply retrieve past decisions and history (decision records, analysis-log, STATUS, project-notes, handoffs) in `~/Projects/spatial-ccds` and `~/Projects/sc-immage` — explicitly not code navigation. Researched options, picked qmd, installed and indexed it.

## Current State Detail

Files touched (none in this repo besides this handoff):
- `~/.local/bin/qmd` — wrapper: `exec /usr/bin/node ~/.local/share/qmd-npm/lib/node_modules/@tobilu/qmd/dist/cli/qmd.js "$@"`
- `~/.local/share/qmd-npm/` — npm prefix for the install (installed with `/usr/bin/npm`)
- `~/.config/qmd/{sc-immage,spatial-ccds}.yml` — one collection each, masks, contexts
- `~/.cache/qmd/{sc-immage,spatial-ccds}.sqlite` (13 / 16 MB), `~/.cache/qmd/models/` (embeddinggemma-300M; query-expansion 1.7B + Qwen3 reranker 0.6B download on first `query`)
- `~/.claude.json` — per-project local-scope `qmd` MCP with `--index <proj>`
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
- **MCP registered manually** (not plugin): plugin route would launch qmd via PATH node → segfault.
- **Separate index per project** (user request). Alternatives: one index + `-c` collection filter (MCP queries could still leak across projects), `qmd init` project-local `.qmd/` (puts sqlite inside the repo). Chose named indexes + local-scope MCP: isolated, nothing in the repos. Split was done by copying the DB and `collection remove` + `cleanup` of the other project — no re-embed.

## Resume Procedure

1. Install auto-refresh (needs user approval — classifier blocked it):
   ```sh
   d=~/.config/systemd/user
   printf '[Unit]\nDescription=qmd refresh\n[Service]\nType=oneshot\nNice=15\nExecStart=/bin/sh -c "for i in sc-immage spatial-ccds; do %%h/.local/bin/qmd --index $$i update && %%h/.local/bin/qmd --index $$i embed; done"\n' > $d/qmd-refresh.service
   printf '[Timer]\nOnCalendar=hourly\nPersistent=true\n[Install]\nWantedBy=timers.target\n' > $d/qmd-refresh.timer
   systemctl --user daemon-reload && systemctl --user enable --now qmd-refresh.timer
   ```
2. Commit the CLAUDE.md qmd section in each project (user's call; spatial-ccds has other dirty files — stage only `CLAUDE.md`).
3. In a fresh session in either project, confirm `mcp__qmd__*` tools appear and time a warm `query`.

## Behaviors Captured

- "its not for this repo … I dont need code navigation, I'm more interested in retrieving decisions made and things we did - which are stored mainly as decision records in under runs/runX or in the analysis-log or, STATUS, or project-notes of those projects"

## Pointers

- https://github.com/tobi/qmd
- `~/.config/qmd/sc-immage.yml`, `~/.config/qmd/spatial-ccds.yml`
- `~/.local/bin/qmd`
