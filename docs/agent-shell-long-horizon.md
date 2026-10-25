# agent-shell: queue-by-typing and long-horizon restart

Code: `ai.el`, inside `(after! agent-shell ...)`. Keybindings: `keybindings.el`.

## Session bar

`SPC a b` toggles a bottom session bar; `SPC a o` still opens the
completion picker. The bar groups sessions by working directory, sorting
directories and session names alphabetically so switching sessions does not
reorder the list. It shows the picker's names, status, and session titles.

Use `j`/`k` to move, `RET` or click to open a session, `g` to refresh, and `q`
to hide the bar. `C-k` (or `C-l`) moves to the window above it. Choosing a session replaces
the buffer in the window from which you opened the bar, respecting the viewport
preference. The bar refreshes while visible and can coexist with Treemacs.

## Queue by typing (TUI-style)

While the agent is busy, any printable key in the agent-shell buffer opens the
queue minibuffer prefilled with that character. `RET` enqueues (or submits
immediately if the shell went idle), `C-g` abandons. When idle, keys insert as
usual.

Mechanism: `[remap self-insert-command]` in `agent-shell-mode-map` points at
`my/agent-shell-self-insert-or-queue`, which checks `shell-maker-busy` and
calls `agent-shell-prompt-queue` with `agent-shell--prompt-queue-read :initial`.
Same idiom upstream uses in `agent-shell-send-region`.

Related keys:

| Key | Where | Action |
|---|---|---|
| `C-c o` | insert state, global | `SPC a` agents submenu (org buffers keep their own `C-c o`) |
| `C-c C-q` | agent-shell buffer | `agent-shell-prompt-queue` |
| `C-c C-r` | agent-shell buffer | `agent-shell-prompt-steer` |

## Long-horizon restart (Emacs equivalent of `~/.local/bin/claude-lh`)

Goal: an agent-shell session checkpoints itself at the context threshold, runs
`/wrapup`, restarts, and resumes from the handoff, exactly like a terminal
session launched via `claude-lh`. Nothing in `/wrapup`, the Stop hook
(`~/.claude/hooks/context-checkpoint.sh`) or the project SessionStart hook
changes. Emacs only replaces the two jobs the shell wrapper did.

### Terminal pipeline (unchanged)

1. `claude-lh` exports `CLAUDE_LH=<its pid>` and runs `claude` in a loop.
2. Stop hook fires at every turn end. `CLAUDE_LH` unset: no-op. Set: reads the
   transcript token count; over threshold it blocks the stop and tells the
   model to run `wrapup`.
3. `/wrapup` step 5 writes the handoff and `.claude/pending_resume.<pid>`, then,
   if `kill -0 $CLAUDE_LH` succeeds, touches `.claude/relaunch.<pid>` and
   `.claude/auto_resume.<pid>`.
4. Next Stop: hook sees `relaunch.<pid>` and SIGTERMs claude.
5. `claude-lh` sees `relaunch.<pid>`: removes it, touches `auto_resume.<pid>`,
   relaunches `claude ... resume`.
6. Project SessionStart hook sees `pending_resume.<pid>` + `auto_resume.<pid>`:
   prints the handoff plus the "resume unprompted" directive.

### What Emacs does instead

| Step | agent-shell |
|---|---|
| 1 | One `sleep infinity` process per project dir (`my/agent-shell-lh-sentinels`). Its pid is injected as `CLAUDE_LH` into the ACP process env via `:around` advice on `agent-shell-anthropic-make-claude-client`. |
| 2-4 | Unchanged. Hooks run inside `claude-agent-acp` just like the TUI. The hook's kill: `pgrep -P <sleep pid>` finds nothing, so it walks its own parent chain and kills this shell's SDK `claude` process. agent-shell sees a failed prompt. |
| 5 | Every shell (`:filter-return` advice on `agent-shell--start`) subscribes to `turn-complete` and `error`. Handler `my/agent-shell-lh-maybe-relaunch`: if `.claude/relaunch.<pid>` exists, remove it, touch `auto_resume.<pid>`, then `agent-shell-restart` (same buffer name, fresh session) and submit `resume` on `prompt-ready`. |
| 6 | Unchanged. The new ACP process gets the same sentinel pid, so the suffixed flags match. |

### Why a `sleep` sentinel and not the Emacs pid

- `/wrapup` requires `CLAUDE_LH` to be a live pid (`kill -0`), or it refuses
  to arm the relaunch flags.
- The Stop hook kills the first `*claude*` child of `$CLAUDE_LH`. With the
  Emacs pid that could be another project's agent. The sentinel has no
  children, so the hook falls back to its parent-chain walk and always hits
  the right process.
- The sentinel outlives shell restarts, so flag suffixes stay stable across
  the checkpoint.

### Manual use

- `/wrapup +flag` from an agent-shell: same path, restart happens at the next
  turn end.
- Plain `/wrapup`: writes only `pending_resume`, no restart. Next shell opened
  in that project shows the handoff via the SessionStart hook.
- Pause checkpointing for one shell: `touch .claude/no_checkpoint.<pid>`
  (pid is visible in the sentinel process list, `M-x list-processes`).

### Trap: inherited `CLAUDE_LH`

agent-shell uses `:inherit-env t`. If the Emacs daemon was started from a
`claude-lh` terminal, every shell inherits that wrapper's pid as `CLAUDE_LH`
(observed 2026-09-10: agent-shell in `.doom.d` had `CLAUDE_LH=917707`, a live
wrapper whose child was a *terminal* claude session). The Stop hook then nags
correctly, but arming the relaunch flag would kill the other terminal session
and Emacs would never restart the shell. The sentinel env entry is consed in
front of the inherited environment so new shells override it; a shell started
before `ai.el` loaded does not. Check with `echo $CLAUDE_LH` from the agent and
pause with `touch .claude/no_checkpoint.$CLAUDE_LH` if it is not a sentinel.

### Requirements and limits

- Project needs a `.claude/` dir and the SessionStart resume hook (present in
  `agentic-science`, absent in `.doom.d`).
- Only applies to shells started after `ai.el` loads; existing shells have no
  `CLAUDE_LH`.
- One `sleep` per project per Emacs session is never reaped (ponytail).
- Verified end to end on 2026-09-10 in `.doom.d`: sentinel `CLAUDE_LH` in the
  ACP env, Stop hook nag at threshold, hook SIGTERM of the SDK process (exit
  143) once the flag was armed, Emacs restart of the shell with the same
  sentinel pid, and `resume` submitted automatically.

## Codex fresh-thread long horizon

Codex long-horizon work deliberately starts a new ACP `session/new` with an
empty transcript, injects the durable handoff, then sends plain `resume` as the
first user prompt. It does **not** use `codex resume`, `agent-shell-reload`, or
any other operation that retains or restores the old session context.

The installed pieces are:

- `model_context_window = 1050000` for the current GPT-5.6/GPT-6 models, with
  `model_auto_compact_token_limit = 1050000` so the automatic trigger does not
  coincide with the 220k notice. A trusted `PreCompact` hook vetoes automatic
  compaction even at the context limit; manual `/compact` remains available.
- An independent `Stop` hook reads the latest context usage from the Codex
  transcript and requests `$wrapup` once per session at 220k tokens. Transcript
  parsing is best-effort because Codex does not promise a stable format.
- An independent Codex `$wrapup` skill using native Codex subagents.
- `/home/yonatan/.codex/hooks/codex-lh.sh`, the sole owner of marker creation,
  validation, transition, and SessionStart handoff injection.
- An agent-shell watcher that exports the canonical `CODEX_LH_ROOT`, keeps a
  stable per-buffer `CODEX_LH_ID` across `agent-shell-restart`, starts the
  fresh session, and submits `resume` only after injection is acknowledged.

### State location and isolation

All markers live in `/tmp/codex-lh/<full-project-sha256>/`, where the hash is
the full SHA-256 of the canonical project root. This keeps protocol state out
of the worktree, makes it writable even when the project is read-only to a
sandboxed Codex process, and prevents projects with the same directory name
from sharing state. Every project gets its own directory automatically; future
sessions write flags through `codex-lh.sh`, not directly into their worktrees.

`CODEX_LH_ROOT` and `CODEX_LH_ID` jointly identify one project and one
agent-shell buffer within it. Agent-shell injects its canonical project root;
the helper accepts the hook or command `cwd` only when it equals or lies below
that root. Without `CODEX_LH_ROOT`, the helper falls back to the canonical Git
root, then to the canonical current directory when outside Git. Non-Git
projects therefore use the same protocol and per-project isolation.

Each marker payload binds the canonical project root to a canonical, readable
handoff file inside that project. The helper rejects invalid IDs, mismatched
payloads, cross-project handoffs, and handoffs larger than 64 KiB.

### Marker lifecycle

| Marker | Purpose | Removal or transition |
|---|---|---|
| `pending_resume` | Manual handoff for the next fresh Codex session. | A successful startup `SessionStart` injects it, then removes it. |
| `pending_resume.<id>` | Handoff owned by one automatic restart. | Removed only after agent-shell successfully submits `resume`. |
| `relaunch.<id>` | Complete automatic-restart request written last by `$wrapup +flag`. | `codex-lh.sh prepare-restart` clears any stale `injected.<id>`, then atomically moves it to `auto_resume.<id>`. |
| `auto_resume.<id>` | Authorizes scoped injection and the automatic first prompt. | Removed after successful `resume` submission. |
| `injected.<id>` | SessionStart acknowledgement that the handoff reached the new context. | Removed after successful `resume` submission. |

For `$wrapup +flag`, the helper writes `pending_resume.<id>` and then
`relaunch.<id>`. The Emacs watcher sees the relaunch marker and invokes
`codex-lh.sh prepare-restart`; the helper validates both payloads, removes any
stale injection acknowledgement from an earlier attempt, and moves
`relaunch.<id>` to `auto_resume.<id>`. Agent-shell then restarts into a fresh
ACP session while preserving the same root and ID.

The Codex `SessionStart` hook runs `codex-lh.sh session-start` for `startup`
only. For an automatic restart it requires matching scoped pending and
auto-resume markers, emits the handoff as `additionalContext`, and writes
`injected.<id>` only after producing that output. At `prompt-ready`, Emacs
requires all three of `pending_resume.<id>`, `auto_resume.<id>`, and
`injected.<id>` before submitting the ordinary user prompt `resume`; it then
removes all three. Missing acknowledgement leaves the markers in place and no
prompt is sent.

A plain `$wrapup` writes unsuffixed `pending_resume` for a manually opened
fresh session. SessionStart injects and consumes that marker, but Emacs does
not submit an automatic prompt.

Codex reviews hook command hashes before trusting changed hooks. Open `/hooks`
in the CLI and trust the `PreCompact` veto, `Stop` wrapup reminder, and
`SessionStart` handoff hooks. Until trusted, the first two are skipped;
without a trusted SessionStart hook, no `injected.<id>` acknowledgement appears
and agent-shell correctly refuses to send `resume`.

The installed helper test suite passes manual arm/startup consumption,
automatic arm/prepare/SessionStart acknowledgement, root/ID/handoff
validation, cross-project isolation, and atomic temporary-file cleanup. The
Codex skill validates, `hooks.json` parses, and targeted Emacs assertions pass
for Git and non-Git project roots, environment override and ID preservation,
and acknowledgement-gated `resume` plus cleanup. `check-parens`, the final
Doom build, and `git diff --check` also pass. The only remaining verification
is one live agent-shell/Codex ACP end-to-end cycle from `$wrapup +flag`
through fresh-session `resume`.
