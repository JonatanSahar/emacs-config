;;; agent-shell-sidebar-test.el --- Sidebar regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defun sidebar-test-load-forms (file predicate)
  "Evaluate top-level forms in FILE accepted by PREDICATE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (funcall predicate form)
              (eval form t))))
      (end-of-file nil))))

;; Load the actual definitions without starting Doom or an agent process.
(sidebar-test-load-forms
 (expand-file-name "../ai.el" (file-name-directory load-file-name))
 (lambda (form)
   (and (symbolp (car-safe (cdr-safe form)))
        (string-prefix-p "my/agent-shell-sidebar-"
                         (symbol-name (cadr form))))))

(sidebar-test-load-forms
 (expand-file-name ".local/straight/repos/agent-shell/agent-shell.el"
                   user-emacs-directory)
 (lambda (form)
   (memq (car-safe (cdr-safe form))
         '(agent-shell--read-shell-buffer agent-shell--icon-affixation
           agent-shell--buffer-name-prefix))))

(defvar agent-shell-show-config-icons t)
(defvar agent-shell-buffer-name-format 'default)
(defvar agent-shell-prefer-viewport-interaction nil)
(defvar agent-shell-display-action nil)
(defvar-local agent-shell--state nil)
(defvar-local sidebar-test-status 'ready)

(defun sidebar-test-goto-session (buffer)
  "Select the row belonging to BUFFER, skipping directory headings."
  (goto-char (or (text-property-any (point-min) (point-max)
                                    'my/agent-shell-sidebar-buffer buffer)
                 (ert-fail "Missing session row"))))

(defmacro sidebar-test-with-sessions (&rest body)
  "Run BODY with two isolated shells and a sidebar buffer."
  (declare (indent 0))
  `(let ((first (generate-new-buffer "Claude Agent @ sidebar-one"))
         (second (generate-new-buffer "Claude Agent @ sidebar-two"))
         (sidebar (generate-new-buffer " *sidebar-test*")))
     (unwind-protect
         (progn
           (dolist (buffer (list first second))
             (with-current-buffer buffer
               (setq agent-shell--state
                     (copy-tree
                      '((:agent-config . ((:identifier . claude)
                                         (:buffer-name . "Claude")))
                        (:session . ((:title . "  shared title\nsecond line"))))))))
           (with-current-buffer second (setq sidebar-test-status 'blocked))
           (cl-letf (((symbol-function 'agent-shell-buffers)
                      (lambda () (list first second)))
                     ((symbol-function 'agent-shell-status)
                      (lambda () sidebar-test-status))
                     ((symbol-function 'agent-shell--config-icon)
                      (lambda (&rest _) "C")))
             ,@body))
       (dolist (buffer (list sidebar first second))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest sidebar-labels-match-installed-picker ()
  (sidebar-test-with-sessions
    (dolist (variant '(homogeneous mixed long-title missing-title))
      (with-current-buffer second
        (setf (map-elt (map-elt agent-shell--state :agent-config) :identifier)
              (if (eq variant 'mixed) 'codex 'claude))
        (setf (map-elt (map-elt agent-shell--state :session) :title)
              (pcase variant
                ('long-title (make-string 60 ?x))
                ('missing-title nil)
                (_ "  shared title\nsecond line"))))
      (dolist (icons '(t nil))
      (let ((agent-shell-show-config-icons icons)
            candidates)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt table &rest _)
                     (setq candidates (all-completions "" table))
                     (car candidates))))
          (should (eq first (agent-shell--read-shell-buffer))))
        (let ((expected
               (mapcar (lambda (parts)
                         (concat (nth 1 parts) (car parts)))
                       (agent-shell--icon-affixation candidates)))
              (actual (my/agent-shell-sidebar-entries)))
          (should (equal (mapcar #'cdr actual) (list first second)))
          (should (equal (mapcar #'substring-no-properties expected)
                         (mapcar (lambda (entry)
                                   (substring-no-properties (car entry)))
                                 actual)))))))))

(ert-deftest sidebar-refresh-preserves-buffer-identity ()
  (sidebar-test-with-sessions
    (let ((entries (list (cons "same label" first) (cons "same label" second))))
      (cl-letf (((symbol-function 'my/agent-shell-sidebar-entries)
                 (lambda () entries)))
        (with-current-buffer sidebar
          (my/agent-shell-sidebar-mode)
          (my/agent-shell-sidebar-refresh t)
          (goto-char (point-min))
          (while (and (< (point) (point-max))
                      (not (eq (get-text-property
                                (point) 'my/agent-shell-sidebar-buffer)
                               second)))
            (forward-char))
          (should (< (point) (point-max)))
          (setq entries (reverse entries))
          (my/agent-shell-sidebar-refresh)
          (should (eq (get-text-property (point) 'my/agent-shell-sidebar-buffer)
                      second))
          (let ((tick (buffer-chars-modified-tick)))
            (my/agent-shell-sidebar-refresh)
            (should (= tick (buffer-chars-modified-tick)))))))))

(ert-deftest sidebar-directory-groups-ignore-mru-and-preserve-selection ()
  (sidebar-test-with-sessions
    (let ((third (generate-new-buffer "Claude Agent @ sidebar-zero")))
      (unwind-protect
          (progn
            (with-current-buffer first
              (setq default-directory "/tmp/sidebar-b/"))
            (with-current-buffer second
              (setq default-directory "/tmp/sidebar-b/"))
            (with-current-buffer third
              (setq default-directory "/tmp/sidebar-a/"
                    agent-shell--state
                    (copy-tree (buffer-local-value 'agent-shell--state first))))
            (let ((order (list second first third)))
              (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () order)))
                (with-current-buffer sidebar
                  (my/agent-shell-sidebar-mode)
                  (my/agent-shell-sidebar-refresh t)
                  (let ((third-position (text-property-any (point-min) (point-max)
                                                           'my/agent-shell-sidebar-buffer third))
                        (first-position (text-property-any (point-min) (point-max)
                                                           'my/agent-shell-sidebar-buffer first))
                        (second-position (text-property-any (point-min) (point-max)
                                                            'my/agent-shell-sidebar-buffer second)))
                    (should (< third-position first-position second-position)))
                  (let ((rendered (buffer-string)))
                    (should (string-match-p "/tmp/sidebar-a" rendered))
                    (should (string-match-p "/tmp/sidebar-b" rendered))
                    (setq order (reverse order))
                    (my/agent-shell-sidebar-refresh)
                    (should (equal-including-properties rendered (buffer-string))))
                  (goto-char (point-min))
                  (should-not (get-text-property (point) 'my/agent-shell-sidebar-buffer))
                  (should-error (my/agent-shell-sidebar-open) :type 'user-error)
                  (sidebar-test-goto-session second)
                  (with-current-buffer second
                    (rename-buffer "Claude Agent @ sidebar-aaa" t)
                    (setq default-directory "/tmp/sidebar-0/"))
                  (my/agent-shell-sidebar-refresh)
                  (should (eq (get-text-property (point) 'my/agent-shell-sidebar-buffer)
                              second))
                  (should (< (point)
                             (text-property-any (point-min) (point-max)
                                                'my/agent-shell-sidebar-buffer third)))))))
        (kill-buffer third)))))

(ert-deftest sidebar-control-k-leaves-bar-upward ()
  (should (eq (lookup-key my/agent-shell-sidebar-mode-map (kbd "C-k"))
              #'windmove-up)))

(ert-deftest sidebar-empty-and-stale-rows-are-safe ()
  (sidebar-test-with-sessions
    (cl-letf (((symbol-function 'my/agent-shell-sidebar-entries)
               (lambda () nil)))
      (with-current-buffer sidebar
        (my/agent-shell-sidebar-mode)
        (my/agent-shell-sidebar-refresh t)
        (should (> (buffer-size) 0))
        (should-error (my/agent-shell-sidebar-open) :type 'user-error)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "dead" 'my/agent-shell-sidebar-buffer first)))
        (goto-char (point-min))
        (kill-buffer first)
        (should-error (my/agent-shell-sidebar-open) :type 'user-error)))))

(ert-deftest sidebar-kill-cleans-up-timer ()
  (let ((sidebar (generate-new-buffer " *sidebar-timer-test*")) timer)
    (unwind-protect
        (progn
          (with-current-buffer sidebar
            (my/agent-shell-sidebar-mode)
            (setq timer (run-at-time 60 60 #'ignore)
                  my/agent-shell-sidebar-timer timer))
          (kill-buffer sidebar)
          (should-not (memq timer timer-list)))
      (when timer (cancel-timer timer))
      (when (buffer-live-p sidebar) (kill-buffer sidebar)))))

(ert-deftest sidebar-activation-replaces-origin-with-shell-or-viewport ()
  (sidebar-test-with-sessions
    (let ((viewport (generate-new-buffer " *sidebar-viewport*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let* ((editor-window (selected-window))
                   (other-window (split-window-right))
                   (sidebar-window
                    (display-buffer-in-side-window
                     sidebar '((side . bottom) (slot . -2) (window-height . 8)))))
              (set-window-buffer other-window first)
              (set-window-parameter sidebar-window 'my/agent-shell-sidebar-origin-window
                                    editor-window)
              (select-window sidebar-window)
              (my/agent-shell-sidebar-mode)
              (my/agent-shell-sidebar-refresh t)
              (sidebar-test-goto-session first)
              (dolist (prefer '(nil t))
                (select-window sidebar-window)
                (let ((agent-shell-prefer-viewport-interaction prefer))
                  (cl-letf (((symbol-function 'agent-shell-viewport--buffer)
                             (lambda (&rest args)
                               (should (eq (plist-get args :shell-buffer) first))
                               (should (plist-get args :existing-only))
                               viewport)))
                    (my/agent-shell-sidebar-open)))
                (should (eq (window-buffer) (if prefer viewport first)))
                (should (eq (selected-window) editor-window))
                (should (eq (window-buffer sidebar-window) sidebar))
                (should (eq (window-buffer other-window) first)))))
        (kill-buffer viewport)))))

(ert-deftest sidebar-dead-origin-falls-back-to-ordinary-window ()
  (sidebar-test-with-sessions
    (save-window-excursion
      (delete-other-windows)
      (let* ((origin (selected-window))
             (fallback (split-window-right))
             (sidebar-window
              (display-buffer-in-side-window
               sidebar '((side . bottom) (slot . -2) (window-height . 8)))))
        (set-window-parameter sidebar-window 'my/agent-shell-sidebar-origin-window origin)
        (delete-window origin)
        (select-window sidebar-window)
        (my/agent-shell-sidebar-mode)
        (my/agent-shell-sidebar-refresh t)
        (sidebar-test-goto-session first)
        (my/agent-shell-sidebar-open)
        (should (eq (selected-window) fallback))
        (should (eq (window-buffer fallback) first))
        (should (eq (window-buffer sidebar-window) sidebar))))))

(ert-deftest sidebar-hidden-tick-resumes-after-window-restoration ()
  (with-temp-buffer
    (my/agent-shell-sidebar-mode)
    (let ((timer (run-at-time 60 60 #'ignore)))
      (unwind-protect
          (progn
            (setq my/agent-shell-sidebar-timer timer)
            (cl-letf (((symbol-function 'my/agent-shell-sidebar-entries)
                       (lambda () (ert-fail "Hidden sidebar polled sessions"))))
              (my/agent-shell-sidebar-tick (current-buffer)))
            (should (eq my/agent-shell-sidebar-timer timer))
            (should (memq timer timer-list))
            (let (refreshed)
              (cl-letf (((symbol-function 'get-buffer-window)
                         (lambda (&rest _) (selected-window)))
                        ((symbol-function 'my/agent-shell-sidebar-refresh)
                         (lambda (&rest _) (setq refreshed t))))
                (my/agent-shell-sidebar-tick (current-buffer)))
              (should refreshed)))
        (cancel-timer timer)))))

(ert-deftest sidebar-toggle-coexists-with-file-tree ()
  (sidebar-test-with-sessions
    (let ((tree (generate-new-buffer " *sidebar-file-tree*"))
          (original-require (symbol-function 'require))
          (original-get-buffer-create (symbol-function 'get-buffer-create)))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let* ((editor-window (selected-window))
                   (tree-window
                    (display-buffer-in-side-window
                     tree '((side . left) (slot . -1) (window-width . 20)))))
              (select-window editor-window)
              (cl-letf (((symbol-function 'require)
                         (lambda (feature &rest args)
                           (if (eq feature 'agent-shell) feature
                             (apply original-require feature args))))
                        ((symbol-function 'get-buffer-create)
                         (lambda (name &rest args)
                           (if (equal name "*Agent sessions*") sidebar
                             (apply original-get-buffer-create name args)))))
                (my/agent-shell-sidebar-toggle))
              (let ((sidebar-window (selected-window)))
                (should (eq (window-buffer sidebar-window) sidebar))
                (should (eq (window-buffer tree-window) tree))
                (should-not (eq sidebar-window tree-window))
                (should (eq (window-parameter sidebar-window 'window-side) 'bottom))
                (should (eq (window-parameter sidebar-window 'my/agent-shell-sidebar-origin-window)
                            editor-window))
                (set-buffer sidebar)
                (sidebar-test-goto-session first)
                (my/agent-shell-sidebar-open)
                (should (eq (window-buffer) first))
                (should (eq (selected-window) editor-window))
                (should (eq (window-buffer sidebar-window) sidebar))
                (should (eq (window-buffer tree-window) tree))
                (my/agent-shell-sidebar-fit sidebar-window)
                (should (= (window-total-height sidebar-window) 4))
                (let ((inhibit-read-only t))
                  (with-current-buffer sidebar
                    (goto-char (point-max))
                    (insert "extra\nextra\nextra\n")))
                (select-window sidebar-window)
                (my/agent-shell-sidebar-fit sidebar-window)
                (should (> (window-total-height sidebar-window) 4)))))
        (kill-buffer tree)))))

;;; agent-shell-sidebar-test.el ends here
