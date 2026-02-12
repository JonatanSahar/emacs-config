;;; jupyter-timer-fix.el --- Fix for Jupyter execution timer in source buffers -*- lexical-binding: t; -*-

(require 'jupyter-repl)

;;; 0. Safety Definitions & Variables
(defvar jupyter-repl-show-execution-time t
  "Whether to show execution time in modeline during cell execution.")

(defvar jupyter-repl-completion-flash-duration 2.0
  "Duration in seconds to show completion status before returning to idle.")

;; Ensure these are defined as buffer-local
(defvar-local jupyter-repl--execution-start-time nil
  "Float-time when current cell execution started.")

(defvar-local jupyter-repl--execution-timer nil
  "Timer object for updating execution duration display.")

(defvar-local jupyter-repl--last-completion-status nil
  "Status of last completed execution: 'success, 'error, or nil.")

(defvar-local jupyter-repl--completion-timestamp nil
  "Float-time when last execution completed (for flash effect).")

(defvar-local jupyter-repl--last-duration nil
  "Duration of the last completed execution in seconds.")

;;; 0.1 Helper to choose where execution state lives
(defun jupyter-repl--execution-state-buffer (&optional client)
  "Return the REPL buffer for CLIENT if available, else the current buffer."
  (let ((client (or client (and (boundp 'jupyter-current-client)
                                jupyter-current-client))))
    (if (and client
             (object-of-class-p client 'jupyter-repl-client)
             (slot-boundp client 'buffer)
             (buffer-live-p (oref client buffer)))
        (oref client buffer)
      (current-buffer))))

;;; 1. Helper to update all client buffers
(defun jupyter-repl--update-client-modelines (client)
  "Update mode lines and header lines for all buffers associated with CLIENT."
  (let ((repl-buffer (oref client buffer)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (or (eq buf repl-buffer)
                  (and (boundp 'jupyter-current-client)
                       (eq jupyter-current-client client)
                       (bound-and-true-p jupyter-repl-interaction-mode)))
          (force-mode-line-update))))))

;;; 2. Redefine timer functions to be client-aware

(defun jupyter-repl--start-execution-timer ()
  "Start the execution timer for live modeline updates."
  (when jupyter-repl-show-execution-time
    (jupyter-repl--stop-execution-timer) ; Ensure no duplicate timers
    ;; Pass current client to the timer
    (setq jupyter-repl--execution-timer
          (run-with-timer 0.1 0.1 #'jupyter-repl--update-execution-display
                          jupyter-current-client))))

(defun jupyter-repl--stop-execution-timer ()
  "Stop the execution timer."
  (when jupyter-repl--execution-timer
    (cancel-timer jupyter-repl--execution-timer)
    (setq jupyter-repl--execution-timer nil)))

(defun jupyter-repl--update-execution-display (client)
  "Called by timer to update modeline during execution."
  ;; Robust check: update if the client's buffer is still alive
  (when (and (object-of-class-p client 'jupyter-repl-client)
             (slot-boundp client 'buffer)
             (buffer-live-p (oref client buffer)))
    (jupyter-repl--update-client-modelines client)))

(defun jupyter-repl--on-execution-start ()
  "Called when a cell execution starts."
  (let* ((client (and (boundp 'jupyter-current-client) jupyter-current-client))
         (target (jupyter-repl--execution-state-buffer client)))
    (with-current-buffer target
      (setq jupyter-repl--execution-start-time (float-time))
      (setq jupyter-repl--last-completion-status nil)
      (setq jupyter-repl--completion-timestamp nil)
      (jupyter-repl--start-execution-timer))))

(defun jupyter-repl--on-execution-complete (&optional success)
  "Called when execution completes.  SUCCESS indicates if execution succeeded."
  (let* ((client (and (boundp 'jupyter-current-client) jupyter-current-client))
         (target (jupyter-repl--execution-state-buffer client)))
    (with-current-buffer target
      (jupyter-repl--stop-execution-timer)
      (setq jupyter-repl--last-completion-status (if success 'success 'error))
      (when jupyter-repl--execution-start-time
        (setq jupyter-repl--last-duration (- (float-time) jupyter-repl--execution-start-time)))
      (setq jupyter-repl--completion-timestamp (float-time))
      (setq jupyter-repl--execution-start-time nil))
    ;; Schedule return to idle display, passing the client
    (run-with-timer jupyter-repl-completion-flash-duration nil
                    #'jupyter-repl--clear-completion-flash
                    client)))

(defun jupyter-repl--clear-completion-flash (client)
  "Clear the completion flash effect."
  (when (and (object-of-class-p client 'jupyter-repl-client)
             (slot-boundp client 'buffer)
             (buffer-live-p (oref client buffer)))
    (with-current-buffer (oref client buffer)
      (setq jupyter-repl--completion-timestamp nil)
      (setq jupyter-repl--last-completion-status nil))
    (jupyter-repl--update-client-modelines client)))

;;; 3. Redefine display function to read from REPL buffer

(defun jupyter-repl-interaction-mode-line ()
  "Return a mode line string with detailed kernel status and execution timing.
Shows execution timer during busy state, completion status with ✓/✗,
or standard idle/busy/disconnected indicators."
  (pcase jupyter-current-client
    ((and client (cl-type jupyter-repl-client))
     (let* ((repl-buffer (oref client buffer))
            ;; Fetch state from REPL buffer to ensure source buffers see it
            (start-time (buffer-local-value 'jupyter-repl--execution-start-time repl-buffer))
            (completion-status (buffer-local-value 'jupyter-repl--last-completion-status repl-buffer))
            (completion-timestamp (buffer-local-value 'jupyter-repl--completion-timestamp repl-buffer))
            (last-duration (buffer-local-value 'jupyter-repl--last-duration repl-buffer))
            (disconnected (not (jupyter-hb-beating-p client)))
            (exec-state (jupyter-execution-state client))
            (busy (equal exec-state "busy")))

       (cond
        ;; Disconnected state
        (disconnected
         (format jupyter-repl-interaction-mode-line-format "x"))

        ;; Busy with timer - show execution time
        ((and busy jupyter-repl-show-execution-time start-time)
         (let* ((elapsed (- (float-time) start-time))
                (formatted-time (jupyter-repl--format-execution-time elapsed)))
           (format " JuPy[%s]" formatted-time)))

        ;; Busy without timer (fallback)
        (busy
         (format jupyter-repl-interaction-mode-line-format "*"))

        ;; Recently completed - show flash
        ((and completion-timestamp
              (< (- (float-time) completion-timestamp)
                 jupyter-repl-completion-flash-duration))
         (pcase completion-status
           ('success " Jupyter ✓")
           ('error " Jupyter ✗")
           (_ (format jupyter-repl-interaction-mode-line-format "-"))))

        ;; Idle
        (t
         (let ((idle-str (format jupyter-repl-interaction-mode-line-format "-")))
           (if last-duration
               (concat idle-str (format " [%s]" (jupyter-repl--format-execution-time last-duration)))
             idle-str))))))))

;;; 4. Header Line Setup

(defun jupyter-timer-display ()
  "Wrapper for jupyter-repl-interaction-mode-line for use in header-line.
Ensures we don't crash if something is missing."
  (condition-case nil
      (jupyter-repl-interaction-mode-line)
    (error "")))

(defun jupyter-timer-setup-header-line ()
  "Configure header line to show Jupyter status."
  ;; For REPL buffers
  (when (derived-mode-p 'jupyter-repl-mode)
    (setq header-line-format '(:eval (jupyter-timer-display))))
  ;; For source buffers with interaction mode
  (when (and (boundp 'jupyter-repl-interaction-mode)
             jupyter-repl-interaction-mode)
    (setq header-line-format '(:eval (jupyter-timer-display)))))

;; Hook into modes
(add-hook 'jupyter-repl-mode-hook #'jupyter-timer-setup-header-line)
(add-hook 'jupyter-repl-interaction-mode-hook #'jupyter-timer-setup-header-line)

;; Apply to existing buffers immediately
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (or (derived-mode-p 'jupyter-repl-mode)
              (and (boundp 'jupyter-repl-interaction-mode)
                   jupyter-repl-interaction-mode))
      (jupyter-timer-setup-header-line))))

;;; 5. Patch Handlers to update all buffers AND detect status changes

(defun jupyter-timer-fix--handle-status-around (orig-fun client req msg)
  "Advice to handle status transitions and update all client modelines.
This ensures the timer starts even if jupyter-repl.el is outdated."
  
  (let ((new-state (condition-case nil
                       (jupyter-with-message-content msg (execution_state)
                         execution_state)
                     (error nil))))

    ;; FORCE FIX: If state is busy, ensure timer is started
    (when (and (equal new-state "busy")
               (object-of-class-p client 'jupyter-repl-client))
      (jupyter-with-repl-buffer client
        (unless jupyter-repl--execution-start-time
          (jupyter-repl--on-execution-start))))

    ;; FORCE FIX: If state is idle, ensure timer is stopped
    (when (and (equal new-state "idle")
               (object-of-class-p client 'jupyter-repl-client))
      (jupyter-with-repl-buffer client
        (when jupyter-repl--execution-start-time
          ;; If completion status wasn't set by execute_reply, default to success
          (unless jupyter-repl--last-completion-status
            (jupyter-repl--on-execution-complete t)))))

    ;; Call original function
    (let ((res (funcall orig-fun client req msg)))
      
      ;; Force update all associated buffers
      (jupyter-repl--update-client-modelines client)
      res)))

(defun jupyter-timer-fix--update-reply-after (orig-fun client req msg)
  "Advice to update all client modelines after execute reply."
  (let ((res (funcall orig-fun client req msg)))
    (jupyter-repl--update-client-modelines client)
    res))

;;; 6. Fallback: handle status in jupyter-handle-message (req may be nil)

(defun jupyter-timer-fix--handle-message-around (orig-fun client channel msg)
  "Ensure status transitions start/stop the timer even if no handler runs."
  (let ((res (funcall orig-fun client channel msg)))
    (when (and (object-of-class-p client 'jupyter-repl-client)
               (listp msg)
               (string= (jupyter-message-type msg) "status"))
      (let ((new-state (jupyter-message-get msg :execution_state)))
        (cond
         ((equal new-state "busy")
          (jupyter-with-repl-buffer client
            (unless jupyter-repl--execution-start-time
              (jupyter-repl--on-execution-start))))
         ((equal new-state "idle")
          (jupyter-with-repl-buffer client
            (when jupyter-repl--execution-start-time
              (unless jupyter-repl--last-completion-status
                (jupyter-repl--on-execution-complete t))))))))
    res))

;; Remove old advice if exists (cleanup from previous attempts)
(advice-remove 'jupyter-handle-status #'jupyter-timer-fix--update-status-after)
;; Apply new advice
(advice-add 'jupyter-handle-status :around #'jupyter-timer-fix--handle-status-around)
(advice-add 'jupyter-handle-execute-reply :around #'jupyter-timer-fix--update-reply-after)
(advice-add 'jupyter-handle-message :around #'jupyter-timer-fix--handle-message-around)

(message "Loaded jupyter-timer-fix.el (FORCE FIX Version)")
(provide 'jupyter-timer-fix)
