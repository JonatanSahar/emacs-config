# Agent Shell Codex model picker handoff

## Quick State

- No analysis run or Jupyter kernel is active; the focused model-picker repair is complete.
- Global `@agentclientprotocol/codex-acp` was updated from `1.11.0` to `2.2.2`; no Doom configuration change was needed for the fix.
- A real ACP `initialize` and `session/new` verified that the updated bridge advertises all seven expected Codex model families.
- The remaining required action is to run `M-x agent-shell-restart` in the existing Emacs Agent Shell buffer, then confirm the refreshed picker.
- Existing changes in `ai.el`, `config.el`, `docs/agent-shell-long-horizon.md`, and `keybindings.el` are unrelated dirty state and remain unstaged and uncommitted.

## What This Session Was About

The Emacs Agent Shell Codex picker exposed only the GPT-5.6 family plus GPT-6 Astra even though the standalone Codex TUI `0.162.1` exposed newer GPT-6 models. Investigation established that Agent Shell gets its dynamic picker data from `codex-acp`, not directly from the interactive `codex` TUI, and that the installed ACP bridge was stale.

The global bridge was updated and its response was verified end to end. No model was pinned and no repository configuration was changed for the repair.

## Current State Detail

- Active analysis run: none.
- Jupyter kernel: none.
- ACP verification process: terminated with Ctrl-C after a successful `session/new` response.
- Live blocker: an Agent Shell process started before the npm update still has the old executable image and must be restarted before its picker can change.
- Updated global installation: `/home/yonatan/miniforge3/lib/node_modules/@agentclientprotocol/codex-acp/`.
- Updated executable link: `/home/yonatan/miniforge3/bin/codex-acp`.
- Verified bridge version: `2.2.2`.
- The ACP package carries its own `@openai/codex 0.160.1`; the independently installed standalone Codex TUI remains `0.162.1`.
- Verified ACP model choices: `gpt-6.1-sol`, `gpt-6-astra`, `gpt-6-sol`, `gpt-6-luna`, `gpt-5.6-sol`, `gpt-5.6-terra`, and `gpt-5.6-luna`.
- Reasoning efforts are separate from the model picker; detailed `availableModels` entries include model/effort pairs.
- Repository files already modified before or independently of this repair, and left untouched: `ai.el`, `config.el`, `docs/agent-shell-long-horizon.md`, and `keybindings.el`.
- No scratch code, notebook code, source patch, or kernel state needs persistence.

## Decisions Made

- **Update the ACP bridge instead of pinning a model.** Update the globally installed `@agentclientprotocol/codex-acp` package from `1.11.0` to `2.2.2`, leaving `agent-shell-openai-default-model-id` unset. Alternatives weighed were adding `(setq agent-shell-openai-default-model-id "gpt-6.1-sol")`, altering Agent Shell Lisp to inject model entries, treating the limited list as an account rollout issue, and updating the already-current standalone Codex TUI. The rationale was the user's explicit direction, "i'm not for setting the model manually, the picker should display the latest availabe. probably a version issue", together with the finding that Agent Shell obtains `availableModels` dynamically from `codex-acp`. Updating the stale bridge fixes the source of the picker data while preserving normal dynamic model discovery. This settles that future Agent Shell sessions should obtain the current picker from ACP without a local hard-coded default. If the picker becomes stale again while the TUI is current, compare `codex-acp --version` with `npm view @agentclientprotocol/codex-acp version` before changing Doom configuration. DecisionRecord: none, because this repository has no active run directory; this handoff is the full record.
- **Preserve unrelated dirty repository work.** Do not edit, stage, or commit the existing Doom configuration modifications during this focused dependency repair. Alternatives weighed were folding the global dependency update into the existing long-horizon Agent Shell work, creating a repository change that pins or auto-updates the package, and committing all current modifications during wrap-up. The rationale was that the model-picker fix required only updating the global executable, while ownership and status of the existing 228-line worktree diff were not established in this session. Folding those changes into this fix would risk misattribution or bundling unrelated work. This settles that `ai.el`, `config.el`, `docs/agent-shell-long-horizon.md`, and `keybindings.el` must remain explicitly identified as pre-existing dirty state, and no wrap-up commit should include them until their ownership and readiness are established separately. DecisionRecord: none; this handoff is the full record.

## Resume Procedure

1. First action: in the existing Emacs Agent Shell buffer, run `M-x agent-shell-restart`.
2. Open the Agent Shell model picker and verify it lists `gpt-6.1-sol`, `gpt-6-astra`, `gpt-6-sol`, `gpt-6-luna`, `gpt-5.6-sol`, `gpt-5.6-terra`, and `gpt-5.6-luna`.
3. If the picker is still stale, compare the installed and current bridge versions before editing Doom configuration:

   ```sh
   /home/yonatan/miniforge3/bin/codex-acp --version
   npm view @agentclientprotocol/codex-acp version
   ```

4. Also open, not next: determine ownership and readiness of the existing changes in `ai.el`, `config.el`, `docs/agent-shell-long-horizon.md`, and `keybindings.el` before staging or committing any of them.

## Behaviors Captured

- "the TUI does show recent models"
- "the picker only offers these  fix please"
- "i'm not for setting the model manually, the picker should display the latest availabe. probably a version issue"

## Pointers

- `/home/yonatan/.doom.d/ai.el` — Agent Shell configuration inspected during diagnosis; it uses login authentication and sets only the default session mode (`"agent"`), not a model.
- `/home/yonatan/.doom.d/docs/agent-shell-long-horizon.md` — existing unrelated dirty documentation; do not fold it into this repair without establishing ownership and readiness.
- `/home/yonatan/miniforge3/bin/codex-acp` — executable Agent Shell launches through `agent-shell-openai-codex-acp-command`.
- `/home/yonatan/miniforge3/lib/node_modules/@agentclientprotocol/codex-acp/` — updated global ACP bridge package.
- Active run or DecisionRecord path: none; this repository has no active run directory.
