;;; run-batch.el --- Batch test runner for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Runs the suite with `ert-run-tests-batch' and returns the counts as a
;; string, so callers over `emacsclient -e' get total, completed,
;; unexpected, and skipped numbers without batch-and-exit semantics:
;;
;;   emacsclient -s SOCKET -e '(load "tests/run-batch.el")'
;;   emacsclient -s SOCKET -e '(agent-shell-tests-run-batch "DIR")'
;;
;; DIR is the repository root; it defaults to the directory one level up
;; from this file.  Dependencies load from sibling checkouts of the
;; repository root (../acp or ../acp.el, ../shell-maker, ../compat),
;; falling back to `locate-library'.

;;; Code:

(require 'ert)
(require 'map)
(require 'seq)

(defun agent-shell-tests--dep-file (root dirs library)
  "Return the source file for LIBRARY near ROOT.

Checks each directory name in DIRS as a sibling of ROOT, then falls
back to `locate-library'.  Signals an error when LIBRARY is missing."
  (or (seq-some (lambda (dir)
                  (let ((file (expand-file-name
                               (format "../%s/%s.el" dir library) root)))
                    (when (file-exists-p file)
                      file)))
                dirs)
      (locate-library (concat library ".el"))
      (error "Cannot locate dependency %s near %s" library root)))

(defun agent-shell-tests--load-candidates (root)
  "Load the candidate sources for the suite at ROOT.

An Emacs with another acp, shell-maker, or agent-shell already loaded
would satisfy `require' from that copy; load every candidate file
explicitly so the checkout wins."
  (let ((acp (agent-shell-tests--dep-file root '("acp" "acp.el") "acp"))
        (shell-maker (agent-shell-tests--dep-file
                      root '("shell-maker") "shell-maker"))
        (compat (agent-shell-tests--dep-file root '("compat") "compat")))
    (dolist (dir (list (file-name-directory acp)
                       (file-name-directory shell-maker)
                       (file-name-directory compat)
                       (expand-file-name root)))
      (add-to-list 'load-path dir))
    (load compat nil t)
    (dolist (file '("acp" "acp-traffic" "acp-fakes"))
      (load (expand-file-name (concat file ".el")
                              (file-name-directory acp))
            nil t))
    (load shell-maker nil t)
    (dolist (file (directory-files root t "\\`agent-shell.*\\.el\\'"))
      (load file nil t))
    (load (expand-file-name "tests/agent-shell-fakes.el" root) nil t)))

(defun agent-shell-tests--check-provenance (root)
  "Signal an error unless key symbols come from the checkouts near ROOT.

Returns an alist of symbol names to source file truenames."
  (mapcar
   (lambda (entry)
     (let* ((symbol (car entry))
            (expected (cdr entry))
            (file (symbol-file symbol 'defun))
            (truename (and file (file-truename file))))
       (unless (and truename
                    (string-prefix-p (file-truename expected) truename))
         (error "Symbol %s loaded from %s, not under %s"
                symbol truename expected))
       (cons symbol truename)))
   (list (cons 'agent-shell-submit (expand-file-name root))
         (cons 'agent-shell--render-pending-restore (expand-file-name root))
         (cons 'acp-send-request
               (file-name-directory
                (agent-shell-tests--dep-file root '("acp" "acp.el") "acp")))
         (cons 'acp-fakes-pump
               (file-name-directory
                (agent-shell-tests--dep-file root '("acp" "acp.el") "acp")))
         (cons 'shell-maker-submit
               (file-name-directory
                (file-truename
                 (agent-shell-tests--dep-file
                  root '("shell-maker") "shell-maker")))))))

(defun agent-shell-tests-run-batch (&optional root selector)
  "Run the agent-shell suite from ROOT with SELECTOR.

Loads the candidate checkout and its dependencies, then the fakes and
every tests/*-tests.el file, and runs `ert-run-tests-batch'.  SELECTOR
defaults to \"^agent-shell\".  Returns a summary string with total,
completed, unexpected, and skipped counts plus symbol provenance."
  (interactive)
  (let* ((root (or root
                   (expand-file-name ".." (file-name-directory
                                           (or load-file-name
                                               buffer-file-name
                                               default-directory)))))
         (selector (or selector "^agent-shell")))
    (agent-shell-tests--load-candidates root)
    (let ((provenance (agent-shell-tests--check-provenance root)))
      (dolist (file (sort (directory-files (expand-file-name "tests" root)
                                           t "-tests\\.el\\'")
                          #'string<))
        (load file nil t))
      ;; Failure reporting visits test files for source locations; a
      ;; dir-locals prompt there would hang a frameless daemon.
      (let* ((enable-local-variables nil)
             (enable-dir-local-variables nil)
             (stats (ert-run-tests-batch selector))
             (tests (ert--stats-tests stats))
             (results (mapcar #'ert-test-most-recent-result tests))
             (pairs (seq-mapn #'cons tests results)))
        (format "SUITE=agent-shell TOTAL=%d COMPLETED=%d UNEXPECTED=%d SKIPPED=%d PROVENANCE=%S %s"
                (length tests)
                (cl-count-if #'identity results)
                (cl-count-if (lambda (pair)
                               (not (ert-test-result-expected-p (car pair)
                                                                (cdr pair))))
                             pairs)
                (cl-count-if #'ert-test-skipped-p results)
                provenance
                (mapconcat (lambda (pair)
                             (format "%s=%s"
                                     (ert-test-name (car pair))
                                     (agent-shell-tests--result-message
                                      (cdr pair))))
                           (seq-filter (lambda (pair)
                                         (or (ert-test-skipped-p (cdr pair))
                                             (not (ert-test-result-expected-p
                                                   (car pair) (cdr pair)))))
                                       pairs)
                           " "))))))

(defun agent-shell-tests--result-message (result)
  "Return a one-line description of a test RESULT."
  (cond ((ert-test-skipped-p result)
         (format "skipped: %S"
                 (ert-test-result-with-condition-condition result)))
        ((ert-test-result-with-condition-p result)
         (replace-regexp-in-string
          "\n" "; "
          (format "%S" (ert-test-result-with-condition-condition result))))
        (t "unexpected result")))

(provide 'run-batch)
;;; run-batch.el ends here
