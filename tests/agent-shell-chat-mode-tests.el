;;; agent-shell-chat-mode-tests.el --- Tests for agent-shell-chat-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-chat-mode)

;;; Code:

;; Declared special so tests can dynamically bind it (the prompt bar need
;; not be loaded); chat-mode reads it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

;; A self-contained `:extend' background face, standing in for a code
;; block panel (whose real face lives in `agent-shell-markdown', not
;; loaded here).  `:extend' is what chat-mode keys off, not the color.
(defface agent-shell-chat-mode-tests--panel
  '((t :extend t :background "gray20"))
  "An `:extend' background face for tests."
  :group 'agent-shell)

(defface agent-shell-chat-mode-tests--plain
  '((t :extend nil))
  "A face that does not extend, for tests."
  :group 'agent-shell)

(defun agent-shell-chat-mode-tests--me-overlays ()
  "Return the `Me' label overlays in the current buffer, ordered by position."
  (sort (seq-filter (lambda (overlay)
                      (eq (overlay-get overlay 'category) 'agent-shell-chat-me))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun agent-shell-chat-mode-tests--agent-overlays ()
  "Return the agent label overlays in the current buffer."
  (seq-filter (lambda (overlay)
                (eq (overlay-get overlay 'category) 'agent-shell-chat-agent))
              (overlays-in (point-min) (point-max))))

(defun agent-shell-chat-mode-tests--prompt (text)
  "Insert a shell prompt run displaying TEXT, as shell-maker fontifies it."
  (insert (propertize text 'font-lock-face
                      '(comint-highlight-prompt comint-highlight-prompt))))

(defun agent-shell-chat-mode-tests--marker ()
  "Insert the invisible `<shell-maker-end-of-prompt>' marker.
Carries `shell-maker--marker', as shell-maker's real marker does."
  (insert (propertize "<shell-maker-end-of-prompt>"
                      'invisible t 'shell-maker--marker t)))

(defmacro agent-shell-chat-mode-tests--with-shell (&rest body)
  "Run BODY in a labeled shell buffer named \"Claude\"."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local agent-shell--state '((:agent-config . ((:mode-line-name . "Claude"))))
                 agent-shell-chat--labeled t)
     ,@body))

(ert-deftest agent-shell-chat-prompt-face-p-test ()
  "Prompt runs are recognized whether the face is a symbol or a list."
  (should (agent-shell-chat--prompt-face-p 'comint-highlight-prompt))
  (should (agent-shell-chat--prompt-face-p '(comint-highlight-prompt comint-highlight-prompt)))
  (should-not (agent-shell-chat--prompt-face-p 'default))
  (should-not (agent-shell-chat--prompt-face-p nil)))

(ert-deftest agent-shell-chat-agent-name-test ()
  "The agent label uses `:mode-line-name', falling back to \"Agent\"."
  (agent-shell-chat-mode-tests--with-shell
    (should (equal "Claude" (agent-shell-chat--agent-name))))
  (with-temp-buffer
    (setq-local agent-shell--state nil)
    (should (equal "Agent" (agent-shell-chat--agent-name)))))

(ert-deftest agent-shell-chat-labels-submitted-turn-test ()
  "A submitted turn boxes the prompt as `Me' and the response as the agent."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi there\n")
    (agent-shell-chat--relabel)
    (let ((me (agent-shell-chat-mode-tests--me-overlays))
          (agent (agent-shell-chat-mode-tests--agent-overlays)))
      (should (= 1 (length me)))
      (should (string-match-p "Me" (overlay-get (car me) 'before-string)))
      (should (= 1 (length agent)))
      (should (string-match-p "Claude" (overlay-get (car agent) 'before-string))))))

(ert-deftest agent-shell-chat-live-prompt-hidden-with-bar-test ()
  "The empty live prompt shows `Me', or is hidden when the bar is on."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel)
      (should (string-match-p
               "Me" (overlay-get (car (agent-shell-chat-mode-tests--me-overlays)) 'before-string))))
    (let ((agent-shell-prompt-bar-mode t))
      (agent-shell-chat--relabel)
      (should (equal
               "" (overlay-get (car (agent-shell-chat-mode-tests--me-overlays)) 'before-string))))))

(ert-deftest agent-shell-chat-agent-keeps-input-terminator-test ()
  "The agent overlay leaves the input's line terminator visible.
Hiding that newline with `display' would merge the input line into the
response for line motion such as `end-of-visual-line'."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello")
    (let ((terminator (point)))
      (insert "\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "reply\n")
      (agent-shell-chat--relabel)
      (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
        ;; The overlay begins past the terminator, so it is not covered.
        (should (> (overlay-start agent) terminator))
        (should-not (get-char-property terminator 'display))))))

(ert-deftest agent-shell-chat-agent-keeps-terminator-restored-test ()
  "A restored turn keeps the input terminator that follows the marker.
Restored input abuts the marker (\"input<marker>\\n\") rather than
preceding it (\"input\\n<marker>\"); the terminator after the marker must
stay visible so line motion does not merge the input into the response."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "restored input?")
    (agent-shell-chat-mode-tests--marker)
    (let ((terminator (point)))
      (insert "\n\nreply\n")
      (agent-shell-chat--relabel)
      (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
        ;; The overlay begins past the terminator, leaving it visible.
        (should (> (overlay-start agent) terminator))
        (should-not (get-char-property terminator 'display))))))

(ert-deftest agent-shell-chat-me-keeps-response-terminator-test ()
  "The `Me' overlay after a response keeps the response's last line terminator.
Hiding it would merge the last output line into the label for line motion
such as `end-of-visual-line'."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "the reply")
    (let ((terminator (point)))
      (insert "\n\n")
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (agent-shell-chat--relabel)
      ;; The live prompt's overlay begins past the response terminator.
      (let ((me (car (last (agent-shell-chat-mode-tests--me-overlays)))))
        (should (> (overlay-start me) terminator))
        (should-not (get-char-property terminator 'display))))))

(ert-deftest agent-shell-chat-label-is-before-string-test ()
  "The `Me' label renders as a `before-string' with an empty `display'.
Like the agent label, this keeps the cursor from landing on it during
vertical motion (a `display' string is backed by buffer positions)."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "reply\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    (dolist (me (agent-shell-chat-mode-tests--me-overlays))
      (should (equal "" (overlay-get me 'display)))
      (should (string-match-p "Me" (overlay-get me 'before-string))))))

(ert-deftest agent-shell-chat-errors-outside-agent-shell-test ()
  "Enabling the mode outside an `agent-shell' buffer signals a `user-error'.
The mode stays off after the failed attempt."
  (with-temp-buffer
    (should-error (agent-shell-chat-mode 1) :type 'user-error)
    (should-not agent-shell-chat-mode)))

(ert-deftest agent-shell-chat-preserves-code-block-top-padding-test ()
  "A response opening with a code block keeps the panel's tinted top padding.
The agent overlay stops before the `:extend' padding instead of hiding it
with `display'."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    ;; Response opens directly with a code block panel's tinted top padding.
    (insert (propertize "\n" 'face 'agent-shell-chat-mode-tests--panel))
    (let ((panel-top (1- (point))))
      (insert (propertize "elisp\ncode\n" 'face 'agent-shell-chat-mode-tests--panel))
      (agent-shell-chat--relabel)
      (let ((agent (car (agent-shell-chat-mode-tests--agent-overlays))))
        ;; The overlay ends before the panel padding, leaving it visible.
        (should (<= (overlay-end agent) panel-top))
        (should-not (get-char-property panel-top 'display))))))

(ert-deftest agent-shell-chat-no-duplicate-agent-overlays-test ()
  "Relabeling a restored turn does not accumulate duplicate agent overlays.
A restored turn's overlay starts past the marker, so the reuse search must
cover the whole span rather than the marker alone."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "restored input?")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n\nthe reply\n")
    (agent-shell-chat--relabel)
    (agent-shell-chat--relabel)
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-relabel-idempotent-test ()
  "Relabeling twice does not duplicate overlays."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi\n")
    (agent-shell-chat--relabel)
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--me-overlays))))
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-absorbs-leading-blank-lines-test ()
  "A submitted prompt's overlay swallows the input's leading blank lines."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((input-start (point)))
      (insert "\n\nPadded\n")
      (agent-shell-chat-mode-tests--marker)
      (insert "reply\n")
      (agent-shell-chat--relabel)
      (should (= (overlay-end (car (agent-shell-chat-mode-tests--me-overlays)))
                 (save-excursion
                   (goto-char input-start)
                   (skip-chars-forward " \t\n")
                   (point)))))))

(ert-deftest agent-shell-chat-live-prompt-shows-marker-test ()
  "The live prompt shows the `❯' marker, faced `default' (not the prompt face)."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel)
      (let ((display (overlay-get (car (agent-shell-chat-mode-tests--me-overlays)) 'before-string)))
        (should (string-match-p "❯" display))
        ;; The marker must not inherit the covered prompt face.
        (should (eq 'default
                    (get-text-property (string-match "❯" display) 'face display)))))))

(ert-deftest agent-shell-chat-live-prompt-keeps-marker-while-typing-test ()
  "The live prompt keeps the `❯' marker after text is typed into it.
The marker is keyed off being the last (live) prompt, not off empty input,
so it does not vanish mid-type when a relabel runs."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "half-typed input")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (car (agent-shell-chat-mode-tests--me-overlays))))
      (should (string-match-p "❯" (overlay-get me 'before-string)))
      ;; No indent overlay claims the live prompt's in-progress input.
      (should-not (seq-filter (lambda (overlay)
                                (eq (overlay-get overlay 'category)
                                    'agent-shell-chat-me-input))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest agent-shell-chat-submitted-input-indented-test ()
  "A submitted turn's input carries the response body indent, the live one none."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi there\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    (let ((input (seq-filter (lambda (overlay)
                               (eq (overlay-get overlay 'category)
                                   'agent-shell-chat-me-input))
                             (overlays-in (point-min) (point-max)))))
      ;; Only the submitted turn gets an indent overlay; the live prompt does not.
      (should (= 1 (length input)))
      (should (equal agent-shell-chat--body-indent
                     (overlay-get (car input) 'line-prefix))))))

(ert-deftest agent-shell-chat-empty-submission-hidden-test ()
  "An empty submission (a prompt with another below it) is not labeled.
Only the live prompt shows an empty `Me', and neither claims input."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (agent-shell-chat-mode-tests--me-overlays)))
      (should (= 2 (length me)))
      ;; Empty submission: hidden.
      (should (equal "" (overlay-get (nth 0 me) 'before-string)))
      ;; Live prompt: `Me' and `❯'.
      (should (string-match-p "❯" (overlay-get (nth 1 me) 'before-string)))
      ;; Neither empty prompt claims input.
      (should-not (seq-filter (lambda (overlay)
                                (eq (overlay-get overlay 'category)
                                    'agent-shell-chat-me-input))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest agent-shell-chat-stacked-empty-submissions-hidden-test ()
  "Consecutive empty submissions are all hidden; only the live prompt shows."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel))
    (let ((me (agent-shell-chat-mode-tests--me-overlays)))
      (should (equal "" (overlay-get (nth 0 me) 'before-string)))
      (should (equal "" (overlay-get (nth 1 me) 'before-string)))
      ;; Only the live prompt is labeled.
      (should (string-match-p "❯" (overlay-get (nth 2 me) 'before-string))))))

(ert-deftest agent-shell-chat-empty-response-not-labeled-test ()
  "A completed turn with no response text is not given an agent label."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "\n\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (agent-shell-chat--relabel)
    ;; The empty response gets no agent label.
    (should-not (agent-shell-chat-mode-tests--agent-overlays))))

(ert-deftest agent-shell-chat-active-turn-labeled-test ()
  "A just-submitted turn is labeled before its output streams.
The marker sits at end of buffer with no response yet; it is the active
turn, not a completed empty one, so the agent label shows immediately."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (agent-shell-chat--relabel)
    (should (= 1 (length (agent-shell-chat-mode-tests--agent-overlays))))))

(ert-deftest agent-shell-chat-code-block-padding-preserved-test ()
  "A prompt after a code block panel keeps the panel's tinted padding.
The overlay starts past the `:extend' background and drops its `line-prefix'."
  (agent-shell-chat-mode-tests--with-shell
    (insert (propertize "code line" 'face 'agent-shell-chat-mode-tests--panel))
    (let ((panel-end (point)))
      ;; Tinted padding newlines (part of the panel).
      (insert (propertize "\n\n" 'face 'agent-shell-chat-mode-tests--panel))
      (insert "\n")
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (agent-shell-chat--relabel)
      (let ((me (car (agent-shell-chat-mode-tests--me-overlays))))
        ;; The overlay does not swallow the panel's tinted padding.
        (should (>= (overlay-start me) panel-end))
        (should-not (agent-shell-chat--extends-bg-p
                     (get-text-property (overlay-start me) 'face)))
        ;; It drops any inherited tinted gutter.
        (should (equal "" (overlay-get me 'line-prefix)))))))

(ert-deftest agent-shell-chat-extends-bg-p-test ()
  "`agent-shell-chat--extends-bg-p' recognizes an `:extend' background face."
  (should (agent-shell-chat--extends-bg-p 'agent-shell-chat-mode-tests--panel))
  (should (agent-shell-chat--extends-bg-p '(agent-shell-chat-mode-tests--panel)))
  (should-not (agent-shell-chat--extends-bg-p 'agent-shell-chat-mode-tests--plain))
  (should-not (agent-shell-chat--extends-bg-p nil)))

(ert-deftest agent-shell-chat-prompt-runs-test ()
  "`agent-shell-chat--prompt-runs' collects each prompt run in buffer order."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (insert "hello\n")
    (agent-shell-chat-mode-tests--marker)
    (insert "hi\n")
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((runs (agent-shell-chat--prompt-runs)))
      (should (= 2 (length runs)))
      (should (< (cdr (nth 0 runs)) (car (nth 1 runs)))))))

(ert-deftest agent-shell-chat-unfaced-prompt-loses-label-test ()
  "A prompt that loses its face (shell-maker reprinted it) loses its label.
Only the reprinted prompt is labeled, as the live one."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (let ((agent-shell-prompt-bar-mode nil))
      (agent-shell-chat--relabel)
      (should (= 1 (length (agent-shell-chat-mode-tests--me-overlays))))
      (remove-text-properties (point-min) (point-max) '(font-lock-face nil))
      (insert "\n")
      (agent-shell-chat-mode-tests--prompt "Claude> ")
      (agent-shell-chat--relabel))
    (let ((me (agent-shell-chat-mode-tests--me-overlays)))
      (should (= 1 (length me)))
      (should (= (overlay-end (car me)) (point-max)))
      (should (string-match-p "❯" (overlay-get (car me) 'before-string))))))

(ert-deftest agent-shell-chat-after-output-filter-schedules-relabel-test ()
  "A moved `comint-last-prompt' schedules one relabel; an unmoved one none."
  (agent-shell-chat-mode-tests--with-shell
    (agent-shell-chat-mode-tests--prompt "Claude> ")
    (setq-local comint-last-prompt (cons (copy-marker (point-min))
                                         (copy-marker (point-max))))
    (let ((buffer (current-buffer)))
      (cl-letf (((symbol-function 'process-buffer) (lambda (_) buffer)))
        (agent-shell-chat--after-output-filter 'fake-process "")
        (should (timerp agent-shell-chat--relabel-timer))
        (cancel-timer agent-shell-chat--relabel-timer)
        (setq agent-shell-chat--relabel-timer nil)
        (agent-shell-chat--after-output-filter 'fake-process "")
        (should-not agent-shell-chat--relabel-timer)
        (insert "\n")
        (agent-shell-chat-mode-tests--prompt "Claude> ")
        (setq-local comint-last-prompt (cons (copy-marker (- (point-max) 8))
                                             (copy-marker (point-max))))
        (agent-shell-chat--after-output-filter 'fake-process "")
        (should (timerp agent-shell-chat--relabel-timer))
        (cancel-timer agent-shell-chat--relabel-timer)))))

(provide 'agent-shell-chat-mode-tests)
;;; agent-shell-chat-mode-tests.el ends here
