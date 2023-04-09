(require-theme 'modus-themes)

;; Maybe define some palette overrides, such as by using our presets
(setq modus-themes-common-palette-overrides
      `(
        ;; From the section "Make the mode line borderless"
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)

        ;; From the section "Make matching parenthesis more or less intense"
        (bg-paren-match bg-magenta-intense)
        (underline-paren-match fg-main)

        ;; And expand the preset here.  Note that the ,@ works because
        ;; we use the backtick for this list, instead of a straight
        ;; quote.
        ,@modus-themes-preset-overrides-intense))

(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-intense)

;; Add all your customizations prior to loading the themes.
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `italic', `WEIGHT'
      modus-themes-prompts '(italic bold)

      ;; The `modus-themes-completions' is an alist that reads two
      ;; keys: `matches', `selection'.  Each accepts a nil value (or
      ;; empty list) or a list of properties that can include any of
      ;; the following (for WEIGHT read further below):
      ;;
      ;; `matches'   :: `underline', `italic', `WEIGHT'
      ;; `selection' :: `underline', `italic', `WEIGHT'
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold italic text-also)))

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      ;; The `modus-themes-headings' is an alist: read the manual's
      ;; node about it or its doc string.  Basically, it supports
      ;; per-level configurations for the optional use of
      ;; `variable-pitch' typography, a height value as a multiple of
      ;; the base font size (e.g. 1.5), and a `WEIGHT'.
      modus-themes-headings
      '((0 . (variable-pitch light 1.05))
        (1 . (variable-pitch light 1.05))
        (2 . (variable-pitch regular 1.05))
        (3 . (variable-pitch regular 1.05))
        (4 . (variable-pitch regular 1.05))
        (5 . (variable-pitch 1.05)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.05))
        (7 . (variable-pitch 1.05))
        (t . (variable-pitch 1.05))
        (agenda-date . (1.1))
        (agenda-structure . (variable-pitch light 1.1)))
      )

;; Optionally define a key to switch between Modus themes.  Also check
;; the user option `modus-themes-to-toggle'.
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

;; Load the theme of your choice.
(load-theme 'modus-operandi-tinted :no-confirm)





;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
(setq ef-themes-to-toggle '(ef-summer ef-winter))

(setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 . (variable-pitch light 1.05))
        (1 . (variable-pitch light 1.05))
        (2 . (variable-pitch regular 1.05))
        (3 . (variable-pitch regular 1.05))
        (4 . (variable-pitch regular 1.05))
        (5 . (variable-pitch 1.05)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.05))
        (7 . (variable-pitch 1.05))
        (t . (variable-pitch 1.05))))

;; They are nil by default...
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui t)

;; Read the doc string or manual for this one.  The symbols can be
;; combined in any order.
(setq ef-themes-region '(intense no-extend neutral))

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
;; (load-theme 'ef-summer :no-confirm)

;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
;; (ef-themes-select 'ef-summer)

;; The themes we provide are recorded in the `ef-themes-dark-themes',
;; `ef-themes-light-themes'.
