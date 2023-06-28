;;; unbinding.el -*- lexical-binding: t; -*-

(map!
    :i "C-z" nil
    :i  "C-k" nil
    :nv  "C-u" nil
    :nv "E" nil
    :nv "W" nil
    :n "gk" nil
    :n "zj" nil
    :n "zz" nil
    :n "zh" nil
    :n "zj" nil
    :n "zk" nil
    :n "zl" nil
    :n "z=" nil
    :n "gs" nil
    :n "gf" nil
    :vn "S" nil
    :nvi "C-h" nil
    :nvi "C-j" nil
    :nvi "C-k" nil
    :nvi "C-l" nil
    :nvi "C-s-b" nil
    :nvi "C-s-a" nil
    :nvi "C-s-e" nil
    :nvi "M-q" nil
    :nvi "C-c p" nil
    :nvi "M-l" nil
    :nvi "C-v" nil
    :nvi "C-<prior>" nil
    :nvi "C-<next>" nil
    :nv "gN" nil
    :nv "gn" nil
    :nvi "C-n" nil

    (:map org-mode-map
        :nv "j" nil
        :nv "E" nil
        :nv "W" nil
        :n "gz" nil
        :n "gh" nil
        :n "gj" nil
        :n "gk" nil
        :n "gl" nil
        :n "gJ" nil
        :n "gK" nil
        :n "z=" nil

        :n "gf" nil
        :n "gs" nil
        :n "zh" nil
        :n "zj" nil
        :n "zk" nil
        :n "zl" nil
        :n "zl" nil

        :nvi "C-v" nil
        :nvi "M-l" nil
        :nvi "C-c p" nil
        :nvi "C-i" nil
        :nvi "C-h" nil
        :nvi "C-j" nil
        :nvi "C-k" nil
        :nvi "C-l" nil
        :nvi "C-M-i" nil
        :nvi "C-<prior>" nil
        :nvi "C-<next>" nil
        )
    (:map org-roam-preview-map :desc "universal argument" "C-u" nil)
    (:map org-roam-mode-map :desc "universal argument" "C-u" nil)

    (:map evil-org-mode-map
        :nvi [C-o] nil
        :nvi [C-i] nil
        :nvi [C-j] nil
        :nvi [C-k] nil
        :nvi "C-S-L" nil)

    (:map helm-find-files-map
     [control-backspace] nil)

    (:map dired-mode-map
     :n "s" nil
     :n "/" nil)

   :i "C-v" nil) ;;unmap a bunch of keys

(map!
 :map evil-org-mode-map
 :i "C-h" nil
 :i "C-l" nil
 :i "C-k" nil
 :i "C-j" nil
)
