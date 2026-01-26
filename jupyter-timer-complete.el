;;; jupyter-timer-complete.el --- Jupyter timer patch -*- lexical-binding: t; -*-

;; This file patches emacs-jupyter to enable the execution timer.
;; Load this AFTER Jupyter is loaded (e.g., in your config.el).

(require 'jupyter-client)
(require 'jupyter-repl)

(message "")
(message "=== Applying Jupyter Timer Patch ===")

;;; 1. Define Helper Functions

(defvar-local jupyter-repl--previous-execution-state nil
  "Previous execution state for detecting transitions.")

(defun jupyter-timer--with-associated-buffers (client fn)
  "Call FN in each buffer associated with CLIENT."
  (when (and client (object-of-class-p client 'jupyter-repl-client))
    ;; Update REPL buffer
    (when (slot-boundp client 'buffer)
      (let ((repl-buffer (oref client buffer)))
        (when (buffer-live-p repl-buffer)
          (with-current-buffer repl-buffer
            (funcall fn)))))
    ;; Update other associated buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (boundp 'jupyter-current-client)
                   (eq jupyter-current-client client)
                   (boundp 'jupyter-repl-interaction-mode)
                   jupyter-repl-interaction-mode
                   (not (eq buf (when (slot-boundp client 'buffer)
                                  (oref client buffer)))))
          (message "[Jupyter] Updating buffer: %s" (buffer-name))
          (funcall fn))))))

;;; 2. Override Methods

;; Helper to get state from any associated buffer
(defun jupyter-timer--get-stored-state (client)
  "Get previous execution state from any buffer associated with CLIENT."
  (catch 'found
    (jupyter-timer--with-associated-buffers
     client
     (lambda ()
       (when (boundp 'jupyter-repl--previous-execution-state)
         (throw 'found jupyter-repl--previous-execution-state))))
    nil))

;; Override jupyter-handle-status for REPL clients (CRITICAL: Must match original signature)
(cl-defmethod jupyter-handle-status ((client jupyter-repl-client) req msg)
  (condition-case err
      (let* ((stored-state (jupyter-timer--get-stored-state client))
             (old-state (or stored-state "idle"))
             ;; Check if msg is valid
             (new-state (if (and msg (not (vectorp msg)))
                            (jupyter-with-message-content msg (execution_state)
                              execution_state)
                          old-state)))

        ;; Only proceed if we have valid states
        (when (and old-state new-state)
          ;; Handle state transitions
          (cond
           ;; Busy transition
           ((and (equal new-state "busy")
                 (not (equal old-state "busy")))
            (jupyter-timer--with-associated-buffers
             client
             (lambda ()
               (when (fboundp 'jupyter-repl--on-execution-start)
                 (jupyter-repl--on-execution-start)))))

           ;; Idle transition
           ((and (equal new-state "idle")
                 (equal old-state "busy"))
            (jupyter-timer--with-associated-buffers
             client
             (lambda ()
               (when (fboundp 'jupyter-repl--on-execution-complete)
                 (jupyter-repl--on-execution-complete t))))))

          ;; Store state in ALL associated buffers
          (jupyter-timer--with-associated-buffers
           client
           (lambda ()
             (setq jupyter-repl--previous-execution-state new-state)))

          ;; Original idle handling (safe version)
          (when (equal new-state "idle")
            (when (and (object-of-class-p client 'jupyter-repl-client)
                       (slot-boundp client 'buffer)
                       (buffer-live-p (oref client buffer)))
              (with-current-buffer (oref client buffer)
                (save-excursion
                  (when (ignore-errors
                          (progn (jupyter-repl-goto-cell req) t))
                    (jupyter-repl-cell-unmark-busy))
                  (when (slot-boundp client 'execution-count)
                    (goto-char (point-max))
                    (jupyter-repl-update-cell-count (oref client execution-count)))))))))
    (error
     (message "Error in jupyter-handle-status: %s" err)))

  ;; Force update all buffers
  (jupyter-timer--with-associated-buffers client #'force-mode-line-update))

;; Protect execute-reply
(cl-defmethod jupyter-handle-execute-reply ((client jupyter-kernel-client) req msg)
  (condition-case err
      (jupyter-with-message-content msg (status payload)
        (let ((completion-status (if (equal status "ok") 'success 'error)))
          (jupyter-timer--with-associated-buffers
           client
           (lambda ()
             (when (boundp 'jupyter-repl--completion-timestamp)
               (setq jupyter-repl--last-completion-status completion-status)
               (force-mode-line-update)))))
        (when payload
          (when (and (object-of-class-p client 'jupyter-repl-client)
                     (slot-boundp client 'buffer)
                     (buffer-live-p (oref client buffer)))
            (with-current-buffer (oref client buffer)
              (jupyter-handle-payload payload)))))
    (error
     (message "Error in jupyter-handle-execute-reply: %s" err))))

;;; 3. Timer and Display Functions

;; Format time helper
(defun jupyter-timer--format-time (seconds)
  "Format SECONDS into a readable string."
  (cond
   ((< seconds 60)
    (format "%.1fs" seconds))
   ((< seconds 3600)
    (format "%dm %02ds" (/ seconds 60) (mod seconds 60)))
   (t
    (format "%dh %02dm" (/ seconds 3600) (mod (/ seconds 60) 60)))))

;; Override the mode line display function
(defun jupyter-repl-interaction-mode-line ()
  "Return a mode line string with execution timing."
  (pcase jupyter-current-client
    ((and client (cl-type jupyter-repl-client))
     (let* ((disconnected (not (jupyter-hb-beating-p client)))
            (exec-state (or jupyter-repl--previous-execution-state
                            (jupyter-execution-state client)))
            (busy (equal exec-state "busy")))
       (cond
        ;; Disconnected
        (disconnected
         (format jupyter-repl-interaction-mode-line-format "x"))

        ;; Busy with timer
        ((and busy jupyter-repl--execution-start-time)
         (let* ((elapsed (- (float-time) jupyter-repl--execution-start-time))
                (formatted-time (jupyter-timer--format-time elapsed)))
           (format " JuPy[%s]" formatted-time)))

        ;; Success flash
        ((and jupyter-repl--last-completion-status
              jupyter-repl--completion-timestamp
              (< (- (float-time) jupyter-repl--completion-timestamp) 2.0))
         (if (eq jupyter-repl--last-completion-status 'success)
             " JuPy[✓]"
           " JuPy[✗]"))

        ;; Standard busy
        (busy
         (format jupyter-repl-interaction-mode-line-format "*"))

        ;; Idle
        (t
         (format jupyter-repl-interaction-mode-line-format "-")))))
    (_ "")))

(defun jupyter-repl--update-execution-display (buffer)
  "Called by timer to update modeline during execution."
  (condition-case err
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (boundp 'jupyter-repl--execution-start-time)
                     jupyter-repl--execution-start-time)
            ;; Update all associated buffers
            (when (and (boundp 'jupyter-current-client)
                       jupyter-current-client)
              (jupyter-timer--with-associated-buffers
               jupyter-current-client
               #'force-mode-line-update)))))
    (error nil))) ;; Silently ignore timer errors

(defun jupyter-repl--start-execution-timer ()
  "Start a timer to update execution duration display."
  (when (bound-and-true-p jupyter-repl-show-execution-time)
    (unless (and (boundp 'jupyter-repl--execution-timer)
                 jupyter-repl--execution-timer)
      (setq jupyter-repl--execution-timer
            (run-at-time 0.5 0.5 #'jupyter-repl--update-execution-display (current-buffer))))))

;; Safe display function
(defun jupyter-safe-interaction-mode-line ()
  (condition-case nil
      (when (fboundp 'jupyter-repl-interaction-mode-line)
        (jupyter-repl-interaction-mode-line))
    (error " Jupyter[?]")))

(defun jupyter-timer-display ()
  "Display jupyter status from associated REPL buffer."
  (condition-case nil
      (cond
       ;; Case 1: We are in the REPL buffer
       ((derived-mode-p 'jupyter-repl-mode)
        (jupyter-safe-interaction-mode-line))

       ;; Case 2: We are in a source buffer with a client
       ((and (boundp 'jupyter-current-client)
             jupyter-current-client
             (slot-boundp jupyter-current-client 'buffer))
        (let ((repl-buf (oref jupyter-current-client buffer)))
          (if (buffer-live-p repl-buf)
              (with-current-buffer repl-buf
                (jupyter-safe-interaction-mode-line))
            " JuPy[DeadRepl]")))

       ;; Case 3: No client
       (t ""))
    (error " Jupyter[?]")))

;;; 4. Setup Hooks

(defun jupyter-timer-setup-hooks ()
  (when (derived-mode-p 'jupyter-repl-mode)
    (setq header-line-format '(:eval (jupyter-safe-interaction-mode-line))))
  (when (and (boundp 'jupyter-repl-interaction-mode)
             jupyter-repl-interaction-mode)
    (setq header-line-format '(:eval (jupyter-timer-display)))))

(add-hook 'jupyter-repl-mode-hook #'jupyter-timer-setup-hooks)
(add-hook 'jupyter-repl-interaction-mode-hook #'jupyter-timer-setup-hooks)

;; Apply to existing
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (jupyter-timer-setup-hooks)))

(message "✓ Jupyter Timer Patch Applied")

(provide 'jupyter-timer-complete)
