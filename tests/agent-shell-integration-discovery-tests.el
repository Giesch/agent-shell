;;; agent-shell-integration-discovery-tests.el --- Discovery checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise runner checks with fixtures, without running nested test suites.
;; Actual required integration results are checked by the runner after ERT.

;;; Code:

(require 'ert)
(require 'run-batch)

(ert-deftest agent-shell-integration-discovery-duplicate-test ()
  "Reject a deliberate duplicate before either definition is evaluated."
  (let* ((fixture "(ert-deftest agent-shell-fixture () (error \"must not run\"))\n")
         (entries (agent-shell-tests--definitions (concat fixture fixture) "fixture")))
    (should (= 2 (length entries)))
    (should-error (agent-shell-tests--check-duplicates entries)
                  :type 'error)
    (should-error
     (agent-shell-tests--check-duplicates
      (append (agent-shell-tests--definitions fixture "first.el")
              (agent-shell-tests--definitions fixture "second.el")))
     :type 'error)))

(ert-deftest agent-shell-integration-discovery-reader-test ()
  "Ignore commented and quoted examples, but find nested test definitions."
  (should
   (equal '(agent-shell-real)
          (mapcar #'car
                  (agent-shell-tests--definitions
                   "; (ert-deftest agent-shell-comment () nil)\n\
'(ert-deftest agent-shell-quoted () nil)\n\
(progn (ert-deftest agent-shell-real () (let ((value (key . value))) t)))" "fixture"))))
  (should-error (agent-shell-tests--definitions "(ert-deftest" "broken")))

(ert-deftest agent-shell-integration-discovery-parent-union-test ()
  "Both parents' names are mandatory unless equivalent coverage is mapped."
  (let ((parents '((agent-shell-old . "fork") (agent-shell-upstream . "upstream")))
        (candidate '((agent-shell-new . "candidate") (agent-shell-upstream . "candidate"))))
    (should-error (agent-shell-tests--check-coverage parents candidate))
    (should-not
     (agent-shell-tests--check-coverage
      parents candidate
      '((agent-shell-old agent-shell-new
         "The replacement asserts the same request and cancellation behavior."))))
    (should-error
     (agent-shell-tests--check-coverage
      parents candidate '((agent-shell-old agent-shell-absent "Missing target is invalid."))))
    (should-error
     (agent-shell-tests--check-coverage
      parents candidate '((agent-shell-old agent-shell-new "renamed"))))))

(ert-deftest agent-shell-integration-discovery-exact-source-test ()
  "A directory prefix is not sufficient proof of source provenance."
  (let ((source (symbol-file 'agent-shell-tests--assert-source 'defun)))
    (should (agent-shell-tests--assert-source 'agent-shell-tests--assert-source source))
    (should-error
     (agent-shell-tests--assert-source 'agent-shell-tests--assert-source
                                      (file-name-directory source)))))

(ert-deftest agent-shell-integration-discovery-results-test ()
  "Require selected, completed, non-skipped results from the current run."
  (let ((test (make-ert-test :name 'agent-shell-required :body (lambda () t)))
        (passed (make-ert-test-passed))
        ;; Condition-bearing ERT results require all three metadata fields.
        (skipped (make-ert-test-skipped :condition '(ert-test-skipped "fixture")
                                        :backtrace nil :infos nil)))
    (should-not (agent-shell-tests--check-results
                 (list test) (list passed) '(agent-shell-required)))
    (should-error (agent-shell-tests--check-results
                   (list test) (list passed) '(agent-shell-absent)))
    (should-error (agent-shell-tests--check-results
                   (list test) '(nil) '(agent-shell-required)))
    (should-error (agent-shell-tests--check-results
                   (list test) (list skipped) '(agent-shell-required)))
    (should-error (agent-shell-tests--check-results nil nil nil))))

(provide 'agent-shell-integration-discovery-tests)
;;; agent-shell-integration-discovery-tests.el ends here
