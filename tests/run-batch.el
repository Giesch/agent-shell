;;; run-batch.el --- Non-exiting test runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this file, then call `agent-shell-tests-run-batch' with the checkout
;; root, an optional ERT selector, and optional dependency directories.
;; Dependencies otherwise come only from the existing `load-path'.
;; Both parent revisions must be available in the checkout's Git database.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lisp-mode)
(require 'seq)
(require 'subr-x)

(defconst agent-shell-tests--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst agent-shell-tests--parent-revisions '("dc4764c" "80887a9")
  "Fork and pinned upstream revisions whose tests must be retained.")

(defconst agent-shell-tests--required-tests
  '(agent-shell-integration-elicitation-default-test
    agent-shell-integration-elicitation-dispatch-test)
  "Tests which must be selected, completed, and not skipped.")

(defvar agent-shell-tests--superseded-tests
  '((agent-shell-chat-label-is-before-string-test
     agent-shell-chat-label-below-content-renders-once-test
     "Checks one heading below its content with both position-backed rows and fallback strings; the old assertion required before-string storage.")
    (agent-shell--render-deferred-images-test
     agent-shell--render-deferred-markup-test
     "Retains deferred image rendering, collapsed-image invisibility, and exclusion of non-body regions, and extends completion coverage to lists, blockquotes, and headings."))
  "Explicit coverage mappings: (OLD NEW REASON).
NEW must be a candidate test and REASON must explain the equivalent coverage.
Do not use this to waive missing coverage.")

(defun agent-shell-tests--definitions (source origin)
  "Read SOURCE without evaluating it; return (NAME . ORIGIN) test entries.
Reject malformed definitions.  ORIGIN identifies the source in diagnostics."
  (with-temp-buffer
    (insert source)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (let (entries)
      (cl-labels ((walk (form)
                   (when (consp form)
                     (cond
                      ((eq (car form) 'ert-deftest)
                       (unless (and (symbolp (cadr form)) (cadr form))
                         (error "Nonliteral test name in %s" origin))
                       (push (cons (cadr form) origin) entries))
                      ((memq (car form) '(quote function)) nil)
                      (t (walk (car form))
                         (walk (cdr form)))))))
        (goto-char (point-min))
        (while (progn (forward-comment (point-max)) (not (eobp)))
          (walk (read (current-buffer)))))
      (nreverse entries))))

(defun agent-shell-tests--check-duplicates (entries)
  "Reject repeated test names in ENTRIES before ERT can overwrite them."
  (let (seen)
    (dolist (entry entries)
      (when-let* ((previous (assq (car entry) seen)))
        (error "Duplicate ert-deftest %s in %s and %s"
               (car entry) (cdr previous) (cdr entry)))
      (push entry seen)))
  entries)

(defun agent-shell-tests--git (root &rest args)
  "Return Git output for ARGS in ROOT, or fail with diagnostics."
  (with-temp-buffer
    (let ((default-directory root))
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "Git %S failed: %s" args (buffer-string))))
    (buffer-string)))

(defun agent-shell-tests--parent-definitions (root)
  "Collect test definitions from both fixed parent revisions at ROOT."
  (let (entries)
    (dolist (revision agent-shell-tests--parent-revisions)
      (dolist (file (split-string
                     (agent-shell-tests--git root "ls-tree" "-r" "--name-only"
                                             revision "--" "tests") "\n" t))
        (when (string-match-p "-tests\\.el\\'" file)
          (setq entries
                (append entries
                        (agent-shell-tests--definitions
                         (agent-shell-tests--git root "show" (concat revision ":" file))
                         (concat revision ":" file)))))))
    entries))

(defun agent-shell-tests--check-coverage (parents candidates &optional mappings)
  "Require PARENTS' test names in CANDIDATES or justified MAPPINGS.
MAPPINGS contains (OLD NEW REASON) entries.  Return missing names or signal."
  (let ((names (mapcar #'car candidates)) missing)
    (dolist (mapping mappings)
      (unless (and (= (length mapping) 3)
                   (assq (car mapping) parents)
                   (not (memq (car mapping) names))
                   (memq (cadr mapping) names)
                   (stringp (nth 2 mapping))
                   (> (length (string-trim (nth 2 mapping))) 20))
        (error "Invalid coverage mapping: %S" mapping)))
    (dolist (entry parents)
      (unless (or (memq (car entry) names) (assq (car entry) mappings))
        (push (car entry) missing)))
    (when missing
      (error "Missing parent tests (retain or explicitly map equivalent coverage): %S"
             (seq-uniq (nreverse missing))))))

(defun agent-shell-tests--discover (files)
  "Read FILES and reject duplicate test names without loading any file."
  (agent-shell-tests--check-duplicates
   (mapcan (lambda (file)
             (with-temp-buffer
               (insert-file-contents file)
               (agent-shell-tests--definitions (buffer-string) file)))
           files)))

(defun agent-shell-tests--assert-source (symbol expected)
  "Require SYMBOL to be defined by the exact source file EXPECTED."
  (let ((actual (symbol-file symbol 'defun)))
    (unless (and actual (equal (file-truename actual) (file-truename expected)))
      (error "Symbol %s loaded from %s, expected exactly %s" symbol actual expected))
    (cons symbol (file-truename actual))))

(defun agent-shell-tests--load-candidates (root)
  "Load exact candidate sources at ROOT using dependencies on `load-path'."
  (let (dependencies)
    (dolist (library '("compat" "acp" "acp-traffic" "acp-fakes" "shell-maker"))
      (let ((source (locate-library (concat library ".el"))))
        (unless source (error "Missing dependency source %s on load-path" library))
        (push (cons library source) dependencies)
        (load source nil t t)))
    (dolist (file (directory-files root t "\\`agent-shell.*\\.el\\'"))
      (load file nil t t))
    (load (expand-file-name "tests/agent-shell-fakes.el" root) nil t t)
    (append
     (mapcar (lambda (entry)
               (agent-shell-tests--assert-source
                (car entry) (expand-file-name (cdr entry) root)))
             '((agent-shell-submit . "agent-shell.el")
               (agent-shell--render-pending-restore . "agent-shell.el")))
     (mapcar (lambda (entry)
               (agent-shell-tests--assert-source
                (car entry) (cdr (assoc (cdr entry) dependencies))))
             '((acp-send-request . "acp") (acp-fakes-pump . "acp-fakes")
               (shell-maker-submit . "shell-maker"))))))

(defun agent-shell-tests--check-results (tests results required)
  "Validate REQUIRED membership and completed, non-skipped RESULTS.
TESTS supplies the selected ERT objects.
Call only after ERT returns; results must belong to this run, not prior runs."
  (dolist (name required)
    (let ((index (seq-position tests name
                               (lambda (test name) (eq (ert-test-name test) name)))))
      (unless index (error "Required test not selected: %s" name))
      (let ((result (nth index results)))
        (unless result (error "Required test did not complete: %s" name))
        (when (ert-test-skipped-p result)
          (error "Required test skipped: %s" name)))))
  (unless (and tests (= (length tests) (seq-count #'identity results)))
    (error "Empty or incomplete test run")))

(defun agent-shell-tests-run-batch (&optional root selector dependency-directories required-tests)
  "Run candidate suite at ROOT with SELECTOR, without exiting Emacs.
DEPENDENCY-DIRECTORIES, when supplied, precede the existing `load-path'.
Check duplicate definitions and parent coverage before loading tests.  After
ERT returns, verify REQUIRED-TESTS were selected and completed without skipping.
The default full suite requires `agent-shell-tests--required-tests'; explicit
narrow selectors require only REQUIRED-TESTS, when supplied.  Return counts,
coverage mappings, and source provenance."
  (interactive)
  (let* ((enable-local-variables nil)
         (enable-dir-local-variables nil)
         (root (file-name-as-directory (expand-file-name (or root agent-shell-tests--root))))
         (load-path (append (list root) dependency-directories load-path))
         (files (sort (directory-files (expand-file-name "tests" root)
                                       t "-tests\\.el\\'") #'string<))
         (definitions (agent-shell-tests--discover files))
         (parents (agent-shell-tests--parent-definitions root))
         (required-tests (seq-uniq
                          (append required-tests
                                  (when (or (null selector) (equal selector "^agent-shell"))
                                    agent-shell-tests--required-tests)))))
    (agent-shell-tests--check-coverage parents definitions agent-shell-tests--superseded-tests)
    (dolist (name required-tests)
      (unless (assq name definitions) (error "Missing required test definition: %s" name)))
    (let ((provenance (agent-shell-tests--load-candidates root)))
      ;; A reused daemon must not contribute tests removed from the checkout.
      (dolist (test (ert-select-tests "^agent-shell" t))
        (ert-delete-test (ert-test-name test)))
      (let ((load-path (cons (expand-file-name "tests" root) load-path)))
        (dolist (file files) (load file nil t t)))
      (dolist (entry definitions)
        (unless (ert-test-boundp (car entry))
          (error "Discovered test was not registered: %s" (car entry))))
      ;; Prevent results left by an earlier invocation from satisfying completion.
      (dolist (test (ert-select-tests (or selector "^agent-shell") t))
        (setf (ert-test-most-recent-result test) nil))
      (let* ((stats (ert-run-tests-batch (or selector "^agent-shell")))
             (tests (append (ert--stats-tests stats) nil))
             (results (mapcar #'ert-test-most-recent-result tests))
             (pairs (seq-mapn #'cons tests results)))
        (agent-shell-tests--check-results tests results required-tests)
        (format "SUITE=agent-shell TOTAL=%d COMPLETED=%d UNEXPECTED=%d SKIPPED=%d PARENT_TESTS=%d MAPPINGS=%S PROVENANCE=%S %s"
                (length tests) (seq-count #'identity results)
                (seq-count (lambda (pair)
                             (not (ert-test-result-expected-p (car pair) (cdr pair)))) pairs)
                (seq-count #'ert-test-skipped-p results)
                (length (seq-uniq (mapcar #'car parents)))
                agent-shell-tests--superseded-tests provenance
                (mapconcat (lambda (pair)
                             (format "%s=%s" (ert-test-name (car pair))
                                     (agent-shell-tests--result-message (cdr pair))))
                           (seq-filter (lambda (pair)
                                         (or (ert-test-skipped-p (cdr pair))
                                             (not (ert-test-result-expected-p
                                                   (car pair) (cdr pair))))) pairs)
                           " "))))))

(defun agent-shell-tests--result-message (result)
  "Return a one-line description of a test RESULT."
  (cond ((ert-test-skipped-p result)
         (format "skipped: %S" (ert-test-result-with-condition-condition result)))
        ((ert-test-result-with-condition-p result)
         (replace-regexp-in-string
          "\n" "; " (format "%S" (ert-test-result-with-condition-condition result))))
        (t "unexpected result")))

(provide 'run-batch)
;;; run-batch.el ends here
