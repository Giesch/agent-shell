;;; agent-shell-completion-tests.el --- Tests for agent-shell completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell-completion)

;;; Code:

(ert-deftest agent-shell--completion-bounds-ignores-path-separators-test ()
  "Test `/` in file paths does not trigger command completion."
  (let ((command-chars "[:alnum:]_-")
        (path-chars "[:alnum:]/_.-"))
    (with-temp-buffer
      (insert "@path/abc")
      (goto-char (point-max))
      (should-not (agent-shell--completion-bounds command-chars ?/))
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should bounds)
        (should (equal (map-elt bounds :start) 2))
        (should (equal (map-elt bounds :end) 10)))))

  (with-temp-buffer
    (insert " /help")
    (goto-char (point-max))
    (let ((bounds (agent-shell--completion-bounds "[:alnum:]_-" ?/)))
      (should bounds)
      (should (equal (map-elt bounds :start) 3))
      (should (equal (map-elt bounds :end) 7)))))

(ert-deftest agent-shell-completion-setup-queued-prompt-test ()
  "The queued-prompt hook enables completion for the event's shell.
Reached through `agent-shell-prompt-queue-setup-minibuffer-functions', so
the queue does not have to know completion exists."
  (let ((shell (generate-new-buffer " *agent-shell-completion-test*")))
    (unwind-protect
        (progn
          (with-current-buffer shell (agent-shell-completion-mode 1))
          (with-temp-buffer
            (agent-shell-completion--setup-queued-prompt
             `((:shell-buffer . ,shell)))
            (should (eq shell agent-shell-completion--shell-buffer))
            (should (memq #'agent-shell--file-completion-at-point
                          completion-at-point-functions))))
      (kill-buffer shell))))

(ert-deftest agent-shell-completion-setup-queued-prompt-without-mode-test ()
  "A shell without completion enabled leaves the minibuffer alone."
  (let ((shell (generate-new-buffer " *agent-shell-completion-test*")))
    (unwind-protect
        (with-temp-buffer
          (agent-shell-completion--setup-queued-prompt
           `((:shell-buffer . ,shell)))
          (should-not (memq #'agent-shell--file-completion-at-point
                            completion-at-point-functions)))
      (kill-buffer shell))))

(provide 'agent-shell-completion-tests)
;;; agent-shell-completion-tests.el ends here
