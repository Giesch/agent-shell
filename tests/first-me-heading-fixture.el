;;; first-me-heading-fixture.el --- First Me heading lifecycle fixture -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Test-only reproduction and verification fixture for the first user
;; turn's `Me' heading.  Drives a real fake-agent shell through the
;; new/type/delete/submit/second-turn/clear/post-clear lifecycle on a
;; dedicated Emacs server, recording overlay composition, rendered row
;; geometry, vertical-motion reachability, and symbol provenance.
;; Loads only the fakes harness (never candidate agent-shell sources),
;; so the shell under test is whatever the host Emacs loaded.  Synthetic
;; input only; never reads personal conversation data.
;;
;;   emacsclient -s SOCKET -e '(progn (load ".../tests/first-me-heading-fixture.el") (first-me-heading-visual-lifecycle 100))'

;;; Code:

(require 'map)
(require 'seq)

(defconst first-me-heading-fixture--tests-dir
  (expand-file-name
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory holding this fixture (the checkout's tests directory).")

(defvar agent-shell--state)
(declare-function agent-shell-fakes-start-agent "agent-shell-fakes")
(declare-function agent-shell-fakes--settle "agent-shell-fakes")
(declare-function acp-shutdown "acp")

(defun first-me-heading-fixture--load-harness ()
  "Load the fakes harness from this checkout, leaving agent-shell alone."
  (load (expand-file-name "agent-shell-fakes.el"
                          first-me-heading-fixture--tests-dir)
        nil t))

(defun first-me-heading-fixture--provenance ()
  "Return an alist of the running definitions this fixture exercises."
  (list (cons :emacs emacs-version)
        (cons :graphical (display-graphic-p))
        (cons :agent-shell-submit (symbol-file 'agent-shell-submit))
        (cons :chat--label-prompts (symbol-file 'agent-shell-chat--label-prompts))
        (cons :shell-maker-submit (symbol-file 'shell-maker-submit))
        (cons :acp-send-request (symbol-file 'acp-send-request))
        (cons :evil (featurep 'evil))
        (cons :doom-agent-shell
              (and (fboundp '+agent-shell-next-item-or-input) t))))

(defun first-me-heading-fixture--messages ()
  "Return literal traffic for a fresh session with three reply turns."
  '((( :direction . outgoing) (:kind . request)
     (:object (jsonrpc . "2.0") (method . "initialize") (id . 1)
              (params (protocolVersion . 1))))
    ((:direction . incoming) (:kind . response)
     (:object (jsonrpc . "2.0") (id . 1)
              (result (protocolVersion . 1)
                      (agentCapabilities
                       (loadSession . :false)
                       (promptCapabilities (image) (audio)
                                           (embeddedContext . t))))))
    ((:direction . outgoing) (:kind . request)
     (:object (jsonrpc . "2.0") (method . "session/new") (id . 2)
              (params (cwd . "/tmp") (mcpServers . []))))
    ((:direction . incoming) (:kind . response)
     (:object (jsonrpc . "2.0") (id . 2)
              (result (sessionId . "first-me-heading-fixture"))))
    ((:direction . outgoing) (:kind . request)
     (:object (jsonrpc . "2.0") (method . "session/prompt") (id . 3)
              (params (sessionId . "first-me-heading-fixture")
                      (prompt . [((type . "text")
                                  (text . "recorded one"))]))))
    ((:direction . incoming) (:kind . notification)
     (:object (jsonrpc . "2.0") (method . "session/update")
              (params (sessionId . "first-me-heading-fixture")
                      (update (sessionUpdate . "agent_message_chunk")
                              (content (type . "text")
                                       (text . "Reply 1."))))))
    ((:direction . incoming) (:kind . response)
     (:object (jsonrpc . "2.0") (id . 3)
              (result (stopReason . "end_turn"))))
    ((:direction . outgoing) (:kind . request)
     (:object (jsonrpc . "2.0") (method . "session/prompt") (id . 5)
              (params (sessionId . "first-me-heading-fixture")
                      (prompt . [((type . "text")
                                  (text . "recorded two"))]))))
    ((:direction . incoming) (:kind . notification)
     (:object (jsonrpc . "2.0") (method . "session/update")
              (params (sessionId . "first-me-heading-fixture")
                      (update (sessionUpdate . "agent_message_chunk")
                              (content (type . "text")
                                       (text . "Reply 2."))))))
    ((:direction . incoming) (:kind . response)
     (:object (jsonrpc . "2.0") (id . 5)
              (result (stopReason . "end_turn"))))
    ((:direction . outgoing) (:kind . request)
     (:object (jsonrpc . "2.0") (method . "session/prompt") (id . 7)
              (params (sessionId . "first-me-heading-fixture")
                      (prompt . [((type . "text")
                                  (text . "recorded three"))]))))
    ((:direction . incoming) (:kind . notification)
     (:object (jsonrpc . "2.0") (method . "session/update")
              (params (sessionId . "first-me-heading-fixture")
                      (update (sessionUpdate . "agent_message_chunk")
                              (content (type . "text")
                                       (text . "Reply 3."))))))
    ((:direction . incoming) (:kind . response)
     (:object (jsonrpc . "2.0") (id . 7)
              (result (stopReason . "end_turn"))))))

(defun first-me-heading-fixture--overlays ()
  "Return an alist description of every chat overlay in the current buffer."
  (mapcar (lambda (overlay)
            (list (cons 'tag (overlay-get overlay 'agent-shell-chat--tag))
                  (cons 'beg (overlay-start overlay))
                  (cons 'end (overlay-end overlay))
                  (cons 'before (overlay-get overlay 'before-string))
                  (cons 'display (overlay-get overlay 'display))
                  (cons 'line-prefix (overlay-get overlay 'line-prefix))
                  (cons 'wrap-prefix (overlay-get overlay 'wrap-prefix))
                  (cons 'after (overlay-get overlay 'after-string))))
          (seq-filter (lambda (overlay)
                        (overlay-get overlay 'agent-shell-chat--tag))
                      (overlays-in (point-min) (point-max)))))

(defun first-me-heading-fixture--me-label-face ()
  "Return the face on the first `Me' label string in the current buffer.

The label travels on the `me' overlay's own strings, or on the separate
`me-label' overlay riding the newline above the prompt, so both are
read."
  (when-let* ((me (seq-find (lambda (overlay)
                              (eq (overlay-get overlay 'agent-shell-chat--tag)
                                  'me))
                            (overlays-in (point-min) (point-max))))
              (string (seq-some
                       (lambda (overlay)
                         (when-let* ((text (overlay-get overlay
                                                        'before-string)))
                           (and (string-match-p "Me" text) text)))
                       (cons me
                             (overlays-in (max (point-min)
                                               (1- (overlay-start me)))
                                          (1+ (overlay-start me)))))))
    (get-text-property (string-match "Me" string) 'face string)))

(defun first-me-heading-fixture--snapshot (label window)
  "Return an alist describing the shell's state under LABEL in WINDOW."
  (list (cons 'label label)
        (cons 'chat-mode (bound-and-true-p agent-shell-chat-mode))
        (cons 'labeled agent-shell-chat--labeled)
        (cons 'window-width (window-body-width window))
        (cons 'rows-to-input
              (count-screen-lines (point-min) (window-point window) t window))
        (cons 'input-row-col
              (when-let* ((position (window-point window))
                          (posn (posn-at-point position window)))
                (posn-col-row posn)))
        ;; Positions reached by moving up a display row at a time from
        ;; the input: equal neighbours mark a row vertical motion
        ;; cannot land on (one drawn entirely from overlay strings).
        (cons 'rows-above-input
              (save-excursion
                (goto-char (window-point window))
                (list (point)
                      (progn (vertical-motion -1) (point))
                      (progn (vertical-motion -1) (point))
                      (progn (vertical-motion -1) (point)))))
        (cons 'me-count
              (seq-count (lambda (overlay)
                           (eq (overlay-get overlay 'agent-shell-chat--tag)
                               'me))
                         (overlays-in (point-min) (point-max))))
        (cons 'me-label-face (first-me-heading-fixture--me-label-face))
        (cons 'text (buffer-substring-no-properties (point-min) (point-max)))
        (cons 'overlays (first-me-heading-fixture--overlays))))

(defun first-me-heading-fixture--type (text)
  "Type TEXT into the live input of the current shell buffer.

Binds `inhibit-read-only', as comint's own writers do: the prompt text
preceding the input region is read-only."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert text)))

(defun first-me-heading-fixture--delete-input ()
  "Delete the live input back to the process mark."
  (let ((process (get-buffer-process (current-buffer))))
    (let ((inhibit-read-only t))
      (delete-region (marker-position (process-mark process)) (point-max)))))

(defun first-me-heading-fixture--submit (input)
  "Type INPUT into the live prompt and submit it, as RET would.

  Going through `agent-shell-submit' (the command RET is bound to, on
  the buffer's typed input) is what a keystroke submits, payload and
  all."
  (first-me-heading-fixture--delete-input)
  (first-me-heading-fixture--type input)
  (call-interactively #'agent-shell-submit)
  (agent-shell-fakes--settle))

(defun first-me-heading-fixture--window (buffer width)
  "Return the window showing BUFFER, alone, with WIDTH text columns.

  The shell's `agent-shell-display-action' may split the frame; this
  gives the shell the whole frame and sizes it through the frame.
  WIDTH nil keeps the current size."
  (let ((window (get-buffer-window buffer)))
    (select-window window)
    (delete-other-windows window)
    (when width
      (let ((guard 0))
        (while (and (/= width (window-body-width window)) (< guard 8))
          (setq guard (1+ guard))
          (set-frame-width (window-frame window)
                           (+ (frame-width (window-frame window))
                              (- width (window-body-width window)))))))
    window))

(defun first-me-heading-visual-lifecycle (&optional width)
  "Drive the first-heading lifecycle and return snapshots of each stage.

  Stages: fresh empty prompt, typing, typing multiline, deleted back to
  empty, first submission, second prompt typing, second submission,
  `clear', and the cleared shell's own first turn.  WIDTH is the text
  area width to run at (default the current window's).  Returns an
  alist of provenance, width, stage log, failure, and one snapshot per
  stage, with the shell torn down afterwards."
  (first-me-heading-fixture--load-harness)
  (let ((window-buffer (window-buffer (selected-window)))
        (buffer nil)
        (client nil)
        (snapshots nil)
        (stage-log nil)
        (failure nil)
        (window nil))
    (unwind-protect
        (condition-case err
            (progn
              (setq buffer (agent-shell-fakes-start-agent
                            (first-me-heading-fixture--messages)))
              (agent-shell-fakes--settle)
              (setq client (map-elt (buffer-local-value 'agent-shell--state
                                                        buffer)
                                    :client))
              (set-window-buffer (selected-window) buffer)
              (setq window (first-me-heading-fixture--window buffer width))
              (dolist (stage
                       (list (cons "fresh-empty"
                                   (lambda () nil))
                             (cons "type"
                                   (lambda ()
                                     (first-me-heading-fixture--type
                                      "hello world")))
                             (cons "type-multiline"
                                   (lambda ()
                                     (first-me-heading-fixture--type
                                      (format "\n%s"
                                              (make-string 90 ?w)))))
                             (cons "delete-input"
                                   (lambda ()
                                     (first-me-heading-fixture--delete-input)))
                             (cons "submit"
                                   (lambda ()
                                     (first-me-heading-fixture--submit
                                      (format "hello world\n%s"
                                              (make-string 90 ?w)))))
                             (cons "second-type"
                                   (lambda ()
                                     (first-me-heading-fixture--type
                                      "second turn")))
                             (cons "second-submit"
                                   (lambda ()
                                     (first-me-heading-fixture--submit
                                      "second turn")))
                             (cons "clear"
                                   (lambda ()
                                     (first-me-heading-fixture--submit
                                      "clear")))
                             (cons "cleared-type"
                                   (lambda ()
                                     (first-me-heading-fixture--type
                                      "fresh after clear")))
                             (cons "cleared-submit"
                                   (lambda ()
                                     (first-me-heading-fixture--submit
                                      (format "fresh after clear\n%s"
                                              (make-string 90 ?w)))))))
                (push (car stage) stage-log)
                (with-current-buffer buffer
                  (funcall (cdr stage))
                  (goto-char (point-max))
                  (set-window-point window (point-max)))
                (redisplay)
                (push (first-me-heading-fixture--snapshot
                       (car stage) window)
                      snapshots)))
          (error (setq failure (list (car err) (error-message-string err)))))
      (set-window-buffer (selected-window) window-buffer)
      (when client
        (ignore-errors (acp-shutdown :client client)))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))
    (list (cons 'provenance (first-me-heading-fixture--provenance))
          (cons 'width (and window (window-body-width window)))
          (cons 'stage-log (nreverse stage-log))
          (cons 'failure failure)
          (cons 'snapshots (nreverse snapshots)))))

(provide 'first-me-heading-fixture)
;;; first-me-heading-fixture.el ends here
