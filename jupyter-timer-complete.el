;;; jupyter-timer-complete.el --- Complete Jupyter timer setup -*- lexical-binding: t; -*-

;; This file sets up the Jupyter execution timer for Doom Emacs
;; Load this after starting Emacs: M-x load-file RET ~/.doom.d/jupyter-timer-complete.el RET

;;; Step 1: Force load the modified source files

(message "")
(message "=================================================")
(message "  JUPYTER EXECUTION TIMER - COMPLETE SETUP")
(message "=================================================")
(message "")

;; Delete compiled files to ensure source is used
(message "[1/5] Removing compiled bytecode...")
(let ((build-dir "~/.emacs.d/.local/straight/build-30.2.50/jupyter/"))
  (when (file-directory-p (expand-file-name build-dir))
    (dolist (file (directory-files (expand-file-name build-dir) t "\\.elc$"))
      (delete-file file)))
  (message "  ✓ Deleted .elc files"))

;; Unload and reload jupyter-repl
(message "")
(message "[2/5] Reloading jupyter-repl...")
(unload-feature 'jupyter-repl t)
(load-file "~/.emacs.d/.local/straight/repos/jupyter/jupyter-repl.el")
(message "  ✓ Loaded from source")

;; Verify modifications are present
(message "")
(message "[3/5] Verifying modifications...")
(let ((all-ok t))
  (unless (boundp 'jupyter-repl-show-execution-time)
    (message "  ✗ jupyter-repl-show-execution-time missing!")
    (setq all-ok nil))
  (unless (fboundp 'jupyter-repl--on-execution-start)
    (message "  ✗ jupyter-repl--on-execution-start missing!")
    (setq all-ok nil))
  (if all-ok
      (message "  ✓ All modifications present")
    (error "Modifications not found in source file!")))

;;; Step 1.5: Apply State Tracking Fix (CRITICAL)

(message "")
(message "[1.5/6] Applying state tracking fix...")

(defvar-local jupyter-repl--previous-execution-state nil
  "Previous execution state for detecting transitions.")

(defun jupyter-timer--with-associated-buffers (client fn)
  "Call FN in each buffer associated with CLIENT."
  (if (fboundp 'jupyter-repl--with-associated-buffers)
      (jupyter-repl--with-associated-buffers client fn)
    (when (and client (object-of-class-p client 'jupyter-repl-client))
      (let ((repl-buffer (oref client buffer)))
        (when (buffer-live-p repl-buffer)
          (with-current-buffer repl-buffer
            (funcall fn)))))))

;; Override the jupyter-handle-status method with proper state tracking and error handling
(cl-defmethod jupyter-handle-status ((client jupyter-repl-client) req msg)
  (condition-case err
      (let* ((stored-state (jupyter-with-repl-buffer client
                             jupyter-repl--previous-execution-state))
             (old-state (or stored-state "idle"))
             ;; Check if msg is valid before trying to use it
             (new-state (if (and msg (not (vectorp msg)))
                            (jupyter-with-message-content msg (execution_state)
                              execution_state)
                          ;; If msg is invalid/vector, use prior state
                          old-state)))

        ;; Only proceed if we have valid states
        (when (and old-state new-state)
          ;; Handle state transitions for execution timer
          (cond
           ;; Transition to busy - start timer
           ((and (equal new-state "busy")
                 (not (equal old-state "busy")))
            (jupyter-timer--with-associated-buffers
             client #'jupyter-repl--on-execution-start))

           ;; Transition to idle - stop timer
           ((and (equal new-state "idle")
                 (equal old-state "busy"))
            (jupyter-timer--with-associated-buffers
             client (lambda ()
                      (jupyter-repl--on-execution-complete t)))))

          ;; Store current state for next time
          (jupyter-with-repl-buffer client
            (setq jupyter-repl--previous-execution-state new-state))

          ;; Original idle handling
          (when (equal new-state "idle")
            (jupyter-with-repl-buffer client
              (save-excursion
                (when (ignore-errors
                        (progn (jupyter-repl-goto-cell req) t))
                  (jupyter-repl-cell-unmark-busy))
                ;; Update the cell count and reset the prompt
                (goto-char (point-max))
                (jupyter-repl-update-cell-count (oref client execution-count)))))))
    (error
     (message "Error in jupyter-handle-status: %s (msg type: %s)"
              err (type-of msg))))

  (jupyter-timer--with-associated-buffers client #'force-mode-line-update))

;; Protect execute-reply as well, just in case
(cl-defmethod jupyter-handle-execute-reply ((client jupyter-repl-client) req msg)
  (condition-case err
      (jupyter-with-message-content msg (status payload)
        (let ((completion-status (if (equal status "ok") 'success 'error)))
          (jupyter-timer--with-associated-buffers
           client
           (lambda ()
             (when (and jupyter-repl--completion-timestamp
                        (< (- (float-time) jupyter-repl--completion-timestamp) 0.5))
               (setq jupyter-repl--last-completion-status completion-status)
               (force-mode-line-update)))))
        (when payload
          (jupyter-with-repl-buffer client
            (jupyter-handle-payload payload))))
    (error
     (message "Error in jupyter-handle-execute-reply: %s" err))))

(message "  ✓ Patched handlers with error protection")

;; Redefine the timer function with safety wrapper
(defun jupyter-repl--update-execution-display (buffer)
  "Called by timer to update modeline during execution."
  (condition-case err
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and jupyter-repl--execution-start-time
                     (or (derived-mode-p 'jupyter-repl-mode)
                         jupyter-repl-interaction-mode))
            (force-mode-line-update))))
    (error
     (message "Error in timer update: %s" err))))

(message "  ✓ Patched timer function with error protection")

;;; Step 2: Define safe display function

(defun jupyter-safe-interaction-mode-line ()
  "Safely call jupyter-repl-interaction-mode-line with error handling."
  (condition-case err
      (when (fboundp 'jupyter-repl-interaction-mode-line)
        (jupyter-repl-interaction-mode-line))
    (error
     (message "Error in jupyter mode line: %s" err)
     " Jupyter[Error]")))

(defun jupyter-timer-display ()
  "Display jupyter status in the current buffer when possible."
  (condition-case err
      (cond
       ((or (derived-mode-p 'jupyter-repl-mode)
            (bound-and-true-p jupyter-repl-interaction-mode))
        (jupyter-safe-interaction-mode-line))
       ((and (boundp 'jupyter-current-client)
             jupyter-current-client)
        (let ((repl-buf (oref jupyter-current-client buffer)))
          (when (buffer-live-p repl-buf)
            (with-current-buffer repl-buf
              (jupyter-safe-interaction-mode-line))))))
    (error
     (message "Error in display function: %s" err)
     " Jupyter[?]")))

;;; Step 3: Set up header-line display hooks

(message "")
(message "[4/5] Installing display hooks...")

;; For REPL buffers - show their own state
(defun jupyter-timer-setup-repl-buffer ()
  "Set up header-line in REPL buffer."
  (when (derived-mode-p 'jupyter-repl-mode)
    (setq header-line-format
          '(:eval (jupyter-safe-interaction-mode-line)))))

;; For source buffers - show associated REPL state
(defun jupyter-timer-setup-source-buffer ()
  "Set up header-line in source buffer."
  (when (and (boundp 'jupyter-repl-interaction-mode)
             jupyter-repl-interaction-mode)
    (setq header-line-format
          '(:eval (jupyter-timer-display)))))


;; Add hooks
(add-hook 'jupyter-repl-mode-hook #'jupyter-timer-setup-repl-buffer)
(add-hook 'jupyter-repl-interaction-mode-hook #'jupyter-timer-setup-source-buffer)

(message "  ✓ Added hooks for jupyter-repl-mode")
(message "  ✓ Added hooks for jupyter-repl-interaction-mode")

;;; Step 4: Clear any stale timer state and apply to existing buffers

(message "")
(message "[5/6] Clearing stale timer state...")

(let ((cleared-count 0))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (or (derived-mode-p 'jupyter-repl-mode)
                (bound-and-true-p jupyter-repl-interaction-mode))
        ;; Stop timer
        (when (and (boundp 'jupyter-repl--execution-timer)
                   jupyter-repl--execution-timer)
          (cancel-timer jupyter-repl--execution-timer)
          (setq jupyter-repl--execution-timer nil))
        ;; Clear state
        (when (boundp 'jupyter-repl--execution-start-time)
          (setq jupyter-repl--execution-start-time nil))
        (when (boundp 'jupyter-repl--last-completion-status)
          (setq jupyter-repl--last-completion-status nil))
        (when (boundp 'jupyter-repl--completion-timestamp)
          (setq jupyter-repl--completion-timestamp nil))
        (setq jupyter-repl--previous-execution-state nil)
        (setq cleared-count (1+ cleared-count)))))
  (message "  ✓ Cleared %d buffer(s)" cleared-count))

(message "")
(message "[6/6] Applying to existing buffers...")

(let ((repl-count 0)
      (source-count 0))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (cond
       ;; REPL buffers
       ((derived-mode-p 'jupyter-repl-mode)
        (setq header-line-format
              '(:eval (jupyter-safe-interaction-mode-line)))
        (setq repl-count (1+ repl-count)))

       ;; Source buffers with interaction mode
       ((and (boundp 'jupyter-repl-interaction-mode)
             jupyter-repl-interaction-mode)
        (setq header-line-format
              '(:eval (jupyter-timer-display)))
        (setq source-count (1+ source-count))))))

  (message "  ✓ Applied to %d REPL buffer(s)" repl-count)
  (message "  ✓ Applied to %d source buffer(s)" source-count))

;;; Step 6: Success message

(message "")
(message "=================================================")
(message "  ✓ INSTALLATION COMPLETE!")
(message "=================================================")
(message "")
(message "The Jupyter execution timer is now active.")
(message "")
(message "WHERE TO LOOK:")
(message "  - Check the HEADER-LINE (top of window)")
(message "  - In both REPL buffers and Python source files")
(message "")
(message "WHAT YOU'LL SEE:")
(message "  - Idle:      Jupyter[-]")
(message "  - Running:   Jupyter ⏳ X.Xs (updates every 0.5s)")
(message "  - Success:   Jupyter ✓ (for 2 seconds)")
(message "  - Error:     Jupyter ✗ (for 2 seconds)")
(message "")
(message "TEST IT:")
(message "  1. Go to your Python file (03-normalize.py)")
(message "  2. Execute a cell with C-c C-c or your usual binding")
(message "  3. Watch the HEADER-LINE at the top of the window")
(message "")
(message "TROUBLESHOOTING:")
(message "  If still not working, run: M-x jupyter-timer-test")
(message "  If timer shows wrong time: M-x jupyter-timer-clear")
(message "")

;;; Utility functions

(defun jupyter-timer-clear ()
  "Clear any stale jupyter timer state."
  (interactive)
  (let ((cleared-count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (or (derived-mode-p 'jupyter-repl-mode)
                  (bound-and-true-p jupyter-repl-interaction-mode))
          (when (and (boundp 'jupyter-repl--execution-timer)
                     jupyter-repl--execution-timer)
            (cancel-timer jupyter-repl--execution-timer)
            (setq jupyter-repl--execution-timer nil))
          (when (boundp 'jupyter-repl--execution-start-time)
            (setq jupyter-repl--execution-start-time nil))
          (when (boundp 'jupyter-repl--last-completion-status)
            (setq jupyter-repl--last-completion-status nil))
          (when (boundp 'jupyter-repl--completion-timestamp)
            (setq jupyter-repl--completion-timestamp nil))
          (setq jupyter-repl--previous-execution-state nil)
          (setq cleared-count (1+ cleared-count)))))
    (force-mode-line-update t)
    (message "✓ Cleared timer state in %d buffer(s)" cleared-count)))

;;; Troubleshooting function

(defun jupyter-timer-test ()
  "Test the jupyter timer in current buffer."
  (interactive)

  (message "")
  (message "=== Jupyter Timer Test ===")
  (message "")
  (message "Buffer: %s" (buffer-name))
  (message "Major mode: %s" major-mode)
  (message "")

  ;; Check client
  (if (not (and (boundp 'jupyter-current-client) jupyter-current-client))
      (progn
        (message "✗ No jupyter-current-client in this buffer!")
        (message "  This buffer is not associated with a Jupyter REPL.")
        (message "  Run jupyter-repl-associate-buffer first."))

    (let* ((client jupyter-current-client)
           (repl-buf (oref client buffer)))

      (message "✓ Client found: %s" (type-of client))
      (message "✓ REPL buffer: %s" repl-buf)
      (message "")

      ;; Check functions exist
      (if (not (fboundp 'jupyter-repl-interaction-mode-line))
          (message "✗ jupyter-repl-interaction-mode-line not defined!")

        (message "Testing display function:")
        (let ((display-result (jupyter-timer-display)))
          (message "  Returns: %S" display-result))

        (message "")
        (message "Manually triggering busy state in REPL...")

        (with-current-buffer repl-buf
          (setq jupyter-repl--execution-start-time (float-time))
          (unless jupyter-repl--execution-timer
            (setq jupyter-repl--execution-timer
                  (run-at-time 0.5 0.5 #'jupyter-repl--update-execution-display
                               (current-buffer))))))

      (force-mode-line-update t)
      (sit-for 0.1)

      (message "")
      (message "Display should now show: %S" (jupyter-timer-display))
      (message "")
      (message "✓ Check the HEADER-LINE at the top of this window!")
      (message "  It should show: Jupyter ⏳ X.Xs")
      (message "")
      (message "If you don't see anything:")
      (message "  1. Look at the very TOP of the window (header-line)")
      (message "  2. Make sure header-line-format is set:")
      (message "     M-: header-line-format")
      (message "  3. Check if it returns the :eval form"))))

;;; Make it permanent (optional)

(defun jupyter-timer-add-to-config ()
  "Add this setup to your config.el permanently."
  (interactive)

  (let ((config-file "~/.doom.d/config.el")
        (setup-code "\n;; Jupyter execution timer\n(after! jupyter\n  (load-file \"~/.doom.d/jupyter-timer-complete.el\"))\n"))

    (if (file-exists-p config-file)
        (progn
          (with-temp-buffer
            (insert-file-contents config-file)
            (goto-char (point-max))
            (insert setup-code)
            (write-file config-file))
          (message "")
          (message "✓ Added to %s" config-file)
          (message "  The timer will now load automatically on startup."))
      (message "✗ Could not find %s" config-file))))

(provide 'jupyter-timer-complete)

;;; jupyter-timer-complete.el ends here
