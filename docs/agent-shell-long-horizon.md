# agent-shell: queue-by-typing and long-horizon restart

Code: `ai.el`, inside `(after! agent-shell ...)`. Keybindings: `keybindings.el`.

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
