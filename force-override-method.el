;;; force-override-method.el --- Force override jupyter-handle-status -*- lexical-binding: t; -*-

(defun force-override-handle-status ()
  "Forcefully override the jupyter-handle-status method."
  (interactive)

  (message "")
  (message "=== Force Overriding jupyter-handle-status ===")
  (message "")

  ;; Remove ALL methods for jupyter-handle-status
  (message "[1/3] Removing old methods...")
  (when (fboundp 'jupyter-handle-status)
    (fmakunbound 'jupyter-handle-status)
    (message "  ✓ Cleared all methods"))

  ;; Reload jupyter-client.el first (has the base generic)
  (message "")
  (message "[2/3] Reloading jupyter-client.el...")
  (load-file "~/.emacs.d/.local/straight/repos/jupyter/jupyter-client.el")
  (message "  ✓ Loaded")

  ;; Reload jupyter-repl.el (has our modified method)
  (message "")
  (message "[3/3] Reloading jupyter-repl.el with modified method...")
  (load-file "~/.emacs.d/.local/straight/repos/jupyter/jupyter-repl.el")
  (message "  ✓ Loaded")

  (message "")
  (message "=== Method Override Complete ===")
  (message "")
  (message "Now test with: M-x test-method-works")
  (message ""))

(defun test-method-works ()
  "Test if the overridden method works."
  (interactive)

  (unless (and (boundp 'jupyter-current-client) jupyter-current-client)
    (error "No jupyter client"))

  (let* ((client jupyter-current-client)
         (repl-buf (oref client buffer)))

    (message "")
    (message "=== Testing Overridden Method ===")
    (message "")

    ;; Clear state first
    (with-current-buffer repl-buf
      (when jupyter-repl--execution-timer
        (cancel-timer jupyter-repl--execution-timer)
        (setq jupyter-repl--execution-timer nil))
      (setq jupyter-repl--execution-start-time nil)
      (setq jupyter-repl--last-completion-status nil)
      (setq jupyter-repl--completion-timestamp nil))

    (message "State cleared.")
    (message "")

    ;; Send busy message
    (message "Sending 'busy' status...")
    (jupyter-handle-status client nil '(:content (:execution_state "busy")))

    (sit-for 0.1)

    (with-current-buffer repl-buf
      (message "After busy:")
      (message "  start-time: %s" jupyter-repl--execution-start-time)
      (message "  timer: %s" (if jupyter-repl--execution-timer "RUNNING" "nil"))

      (if jupyter-repl--execution-start-time
          (message "  ✓ start-time SET - method is working!")
        (message "  ✗ start-time still nil - method NOT working!")))

    (message "")
    (sit-for 2)

    ;; Send idle message
    (message "Sending 'idle' status...")
    (jupyter-handle-status client nil '(:content (:execution_state "idle")))

    (sit-for 0.1)

    (with-current-buffer repl-buf
      (message "After idle:")
      (message "  start-time: %s" jupyter-repl--execution-start-time)
      (message "  timer: %s" (if jupyter-repl--execution-timer "RUNNING" "nil"))
      (message "  completion-status: %s" jupyter-repl--last-completion-status)
      (message "  completion-timestamp: %s" jupyter-repl--completion-timestamp)

      (if (and (not jupyter-repl--execution-start-time)
               jupyter-repl--last-completion-status)
          (message "  ✓ Completion triggered correctly!")
        (message "  ✗ Completion NOT triggered!")))

    (message "")
    (message "Check header-line - should show checkmark!")
    (force-mode-line-update t)))

(provide 'force-override-method)

;;; Usage:
;;; M-x force-override-handle-status - Override the method
;;; M-x test-method-works            - Test it works
