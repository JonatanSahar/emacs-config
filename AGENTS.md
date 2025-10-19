# Repository Guidelines

## Project Structure & Module Organization
Core Doom modules live in `init.el`; `config.el` layers UI and behavior settings. Shared helpers sit in `functions.el` (prefixed `my/` or `+`), key maps in `keybindings.el`, and theming in `themes-and-fonts.el`. `packages.el` lists dependencies while `package-config.el` fine-tunes them. Automation belongs in `scripts/` (notably `jupytext_monitor.py`), editor assets in `snippets/`, and reference files in `misc/`. `custom.el` is generated—do not edit it. `pyrightconfig.json` powers static checks for Python helpers.

## Build, Test, and Development Commands
- `~/.emacs.d/bin/doom sync` — refresh packages and autoloads after editing `init.el` or `packages.el`.
- `~/.emacs.d/bin/doom build` — byte-compile modules to catch syntax issues before launching Emacs.
- `~/.emacs.d/bin/doom doctor` — run Doom’s sanity checks after major refactors or upstream pulls.
- `python scripts/jupytext_monitor.py ~/Notebooks` — mirror notebooks via Jupytext; optionally activate `systemctl --user enable --now scripts/jupytext-monitor.service`.

## Coding Style & Naming Conventions
Keep Emacs Lisp files lexically bound (`-*- lexical-binding: t -*-`) and indented with two spaces; align plist keywords for readability. Name private helpers with the `my/` prefix and Doom augmentations with `+` or `--` suffixes to mirror existing patterns. Favor `use-package!`, `after!`, and `map!` macros instead of direct `require`. For Python utilities follow Black/PEP 8 formatting, log via `logging`, and expose flags through `argparse`. Document non-obvious flows using complete docstrings.

## Testing Guidelines
Run `~/.emacs.d/bin/doom doctor` and launch Emacs with `~/.emacs.d/bin/doom run` after changing core modules to confirm a clean startup. Validate new keymaps or commands with `SPC h d k` (describe key) and `SPC h d f` (describe function). Execute `pyright --project pyrightconfig.json` or targeted scripts before shipping Python updates. Expand `snippets/` entries inside a `doom sandbox` buffer to ensure they render correctly.

## Commit & Pull Request Guidelines
Follow the Conventional Commit style already in use (`feat:`, `refactor:`, `fix:`) with concise, imperative summaries and optional scopes (e.g., `feat(org): …`). Group related configuration changes per commit to keep bisecting straightforward. Pull requests should include a short rationale, affected modules list, reproduction steps, and screenshots or gifs for UI-facing tweaks. Reference issue numbers when applicable and call out any manual post-merge steps (such as rerunning `doom sync`).
