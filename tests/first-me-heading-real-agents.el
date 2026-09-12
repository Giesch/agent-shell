;;; first-me-heading-real-agents.el --- Real-agent first-prompt checks -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Graphical AC1/AC2 checks against the configured real agents (Codex,
;; Pi, Claude Code) on the dedicated candidate server: a brand-new shell
;; must show exactly one colored `Me' heading on its own row above the
;; composing marker, before and while typing, matching the fixture's
;; geometry.  Synthetic input only; nothing is ever submitted, so no
;; agent API receives a prompt.  Buffer dumps are capped to keep agent
;; banners out of reports.

;;; Code:

(require 'map)
(require 'seq)

(defvar agent-shell--state)
(declare-function agent-shell--start "agent-shell")

(defun first-me-heading-real-agents--me-summary ()
  "Summarize the `Me' overlays in the current buffer."
  (mapcar (lambda (overlay)
            (list (cons 'tag (overlay-get overlay 'agent-shell-chat--tag))
                  (cons 'beg (overlay-start overlay))
                  (cons 'end (overlay-end overlay))
                  (cons 'before
                        (and (overlay-get overlay 'before-string)
                             (substring-no-properties
                              (overlay-get overlay 'before-string))))
                  (cons 'display
                        (and (overlay-get overlay 'display)
                             (substring-no-properties
                              (overlay-get overlay 'display))))))
          (seq-filter (lambda (overlay)
                        (memq (overlay-get overlay 'agent-shell-chat--tag)
                              '(me me-label me-draft)))
                      (overlays-in (point-min) (point-max)))))

(defun first-me-heading-real-agents--snapshot (label)
  "Return an alist describing the current buffer's headings as LABEL."
  (list (cons 'label label)
        (cons 'me-count
              (seq-count (lambda (overlay)
                           (eq (overlay-get overlay 'agent-shell-chat--tag)
                               'me))
                         (overlays-in (point-min) (point-max))))
        (cons 'overlays (first-me-heading-real-agents--me-summary))
        (cons 'text-length (- (point-max) (point-min)))))

(defun first-me-heading-real-agents--check (name config-maker)
  "Start the agent from CONFIG-MAKER as NAME and report first-prompt geometry.

  Waits up to 20 seconds for its prompt, snapshots the empty and typed
  first input, deletes the input back to empty, and always kills the
  shell (nothing is submitted)."
  (let ((buffer nil)
        (outcome nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq buffer (agent-shell--start :config (funcall config-maker)
                                               :session-strategy 'new))
              (let ((deadline (+ (float-time) 20)))
                (while (and (< (float-time) deadline)
                            (not (and (buffer-live-p buffer)
                                      (buffer-local-value
                                       'comint-last-prompt buffer))))
                  (sleep-for 0.2)
                  (accept-process-output nil 0.1)))
              (sleep-for 1)
              (with-current-buffer buffer
                (setq outcome
                      (list (cons 'name name)
                            (cons 'buffer (buffer-name))
                            (cons 'prompt-arrived
                                  (and comint-last-prompt t))))
                (goto-char (point-max))
                (push (cons 'empty (first-me-heading-real-agents--snapshot
                                   "empty"))
                      outcome)
                (let ((inhibit-read-only t))
                  (insert "synthetic draft"))
                (redisplay)
                (push (cons 'typed (first-me-heading-real-agents--snapshot
                                   "typed"))
                      outcome)
                (let ((process (get-buffer-process (current-buffer))))
                  (let ((inhibit-read-only t))
                    (delete-region (marker-position (process-mark process))
                                   (point-max))))
                (redisplay)
                (push (cons 'deleted (first-me-heading-real-agents--snapshot
                                      "deleted"))
                      outcome)
                (setq outcome (nreverse outcome))))
          (error (setq outcome (list (cons 'name name)
                                     (cons 'error
                                           (error-message-string err))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((process (get-buffer-process (current-buffer))))
            (when process
              (set-process-query-on-exit-flag process nil))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))
    outcome))

(defun first-me-heading-real-agents--run ()
  "Check every configured real agent's first prompt."
  (require 'agent-shell-openai)
  (require 'agent-shell-pi)
  (require 'agent-shell-anthropic)
  (list (first-me-heading-real-agents--check
         "Codex" #'agent-shell-openai-make-codex-config)
        (first-me-heading-real-agents--check
         "Pi" #'agent-shell-pi-make-agent-config)
        (first-me-heading-real-agents--check
         "Claude" #'agent-shell-anthropic-make-claude-code-config)))

(provide 'first-me-heading-real-agents)
;;; first-me-heading-real-agents.el ends here
