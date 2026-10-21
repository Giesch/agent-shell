;;; agent-shell-elicitation-default-safety-tests.el --- Companion defaults -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Defaults use the same Other precedence as edited answers.  Raw defaults
;; belong in pending answers, not public fields, controls, logs, or events.

;;; Code:

(require 'ert)
(require 'cl-lib)

(declare-function agent-shell-elicitation-tests--button "agent-shell-elicitation-tests")
(declare-function agent-shell-elicitation-tests--call "agent-shell-elicitation-tests")

(defun agent-shell-default-tests--schema (provider multi default &optional parent secret reverse-order)
  "Make a PROVIDER companion schema with DEFAULT.
MULTI selects array choices; PARENT adds a parent default.  SECRET marks
Other sensitive, and REVERSE-ORDER puts Other before its parent."
  (let ((properties
         `((q ,@(if multi
                    '((type . "array") (items (enum . ["Red" "Blue"])))
                  '((type . "string") (enum . ["Red" "Blue"])))
              ,@(when parent `((default . ,(if multi ["Red"] "Red")))))
           (other (type . "string") (default . ,default)
                  (_meta ,@(if (eq provider 'claude)
                               '((_askUserQuestionCustomAnswer (questionId . "q")))
                             `((codex (questionId . "q") (isOtherAnswer . t)
                                      ,@(when secret '((isSecret . t))))))
                         ,@(when (and secret (eq provider 'claude))
                             '((codex (isSecret . t)))))))))
    `((type . "object") (properties . ,(if reverse-order (reverse properties) properties)))))

(cl-defmacro agent-shell-default-tests--with-form ((state sent bodies events) schema &rest body)
  "Run BODY with SCHEMA in STATE, capturing SENT, BODIES and EVENTS."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (let* ((,state (list (cons :buffer (current-buffer)) (cons :client 'default-test-client)
                         (cons :elicitations nil) (cons :active-requests nil)
                         (cons :tool-calls nil)))
            (,sent nil) (,bodies nil) (,events nil)
            (agent-shell-elicitation-capability t))
       (cl-letf (((symbol-function 'agent-shell--state) (lambda () ,state))
                 ((symbol-function 'agent-shell--update-fragment)
                  (lambda (&rest args) (push (plist-get args :body) ,bodies)))
                 ((symbol-function 'agent-shell--delete-fragment) #'ignore)
                 ((symbol-function 'agent-shell-ui-delete-fragment) #'ignore)
                 ((symbol-function 'agent-shell-viewport--buffer) (lambda (&rest _) nil))
                 ((symbol-function 'agent-shell--emit-event)
                  (lambda (&rest args) (push args ,events)))
                 ((symbol-function 'agent-shell--start-idle-timer) #'ignore)
                 ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
                 ((symbol-function 'acp-send-response)
                  (lambda (&rest args) (push (plist-get args :response) ,sent))))
         (agent-shell--on-elicitation-create-request
          :state ,state :acp-request
          (list '(id . 51)
                (cons 'params (list '(mode . "form") '(sessionId . "defaults")
                                    '(message . "Pick a colour")
                                    (cons 'requestedSchema ,schema)))))
         ,@body))))

(ert-deftest agent-shell-elicitation-companion-default-validation-test ()
  "Both providers reject invalid defaults even when Other is folded."
  (dolist (provider '(claude codex))
    (dolist (value '(42 nil :false ["teal"]))
      (should (eq (map-elt (agent-shell--normalize-elicitation
                           (agent-shell-default-tests--schema provider nil value)) :reason)
                  'invalid-default)))
    (dolist (value '("teal" ""))
      (should-not (map-elt (agent-shell--normalize-elicitation
                           (agent-shell-default-tests--schema provider nil value)) :reason)))))

(ert-deftest agent-shell-elicitation-companion-default-precedence-test ()
  "Nonempty Other wins single-select; multi-select preserves both defaults."
  (dolist (provider '(claude codex))
    (dolist (multi '(nil t))
      (dolist (parent '(nil t))
        (dolist (reverse-order '(nil t))
          (dolist (default '("teal" ""))
            (agent-shell-default-tests--with-form (state sent bodies events)
                (agent-shell-default-tests--schema provider multi default parent nil reverse-order)
              (let* ((record (map-nested-elt state '(:elicitations 51)))
                     (answers (map-elt record :answers))
                     (typed (not (string-empty-p default)))
                     (picked (and parent (or multi (not typed)))))
                (should record)
                (should (equal (map-elt answers 'other) (and typed default)))
                (should (equal (map-elt answers 'q) (and picked (if multi ["Red"] "Red"))))
                (should-not (map-elt record :properties))
                (should-not (agent-shell--elicitation-invalid-field record answers))
                (agent-shell--submit-elicitation :state state :request-id 51 :client 'default-test-client)
                (should (= (length sent) 1))
                (let ((content (map-nested-elt (car sent) '(:result content))))
                  (should (equal (map-elt content 'other)
                                 (and typed (if (and multi parent (eq provider 'claude))
                                                "Red, teal" default))))
                  (should (equal (map-elt content 'q)
                                 (and picked (not (and typed (eq provider 'claude)))
                                      (if multi ["Red"] "Red")))))
                (should-not (map-elt state :elicitations))))))))))

(ert-deftest agent-shell-elicitation-secret-companion-default-request-test ()
  "Creating a seeded secret form exposes no raw default in public data."
  (dolist (provider '(claude codex))
    (let ((sentinel "REQUEST_DEFAULT_SECRET_61728"))
      (agent-shell-default-tests--with-form (state sent bodies events)
          (agent-shell-default-tests--schema provider nil sentinel nil t)
        (let* ((record (map-nested-elt state '(:elicitations 51)))
               (print-circle t) (print-level nil) (print-length nil))
          (should (equal (map-elt (map-elt record :answers) 'other) sentinel))
          (should-not (string-search sentinel (prin1-to-string (map-elt record :fields))))
          (should-not (string-search sentinel (prin1-to-string bodies)))
          (should (seq-some (lambda (event) (eq (plist-get event :event) 'elicitation-request)) events))
          (should-not (string-search sentinel (prin1-to-string events))))))))

(ert-deftest agent-shell-elicitation-secret-companion-default-transport-test ()
  "Secret defaults reach the wire, but not controls, logs, events or settlement."
  (dolist (provider '(claude codex))
    (let* ((sentinel "DEFAULT_SECRET_SENTINEL_81273")
           (schema (agent-shell-default-tests--schema provider t sentinel t t)))
      (agent-shell-elicitation-tests--call
       schema
       (lambda (state)
         (let* ((client (map-elt state :client))
                (record (map-nested-elt state '(:elicitations 101)))
                (acp-logging-enabled t)
                wire events)
           (should (equal (map-elt (map-elt record :answers) 'other) sentinel))
           (should-not (map-elt record :properties))
           (should-not (string-search sentinel (format "%S" (map-elt record :fields))))
           ;; Printing the propertized string also inspects captured control data.
           (let ((print-circle t) (print-level nil) (print-length nil))
             (should-not (string-search sentinel (prin1-to-string (buffer-string)))))
           ;; Fake replay normally records responses without serializing them.
           ;; Use the real sender to exercise wire/log separation.
           (setf (map-elt client :response-sender)
                 (lambda (&rest args) (apply #'acp--response-sender args)))
           (cl-letf (((symbol-function 'process-send-string)
                      (lambda (_process json) (push json wire)))
                     ((symbol-function 'agent-shell--emit-event)
                      (lambda (&rest args) (push args events))))
             (funcall (agent-shell-elicitation-tests--button "Submit")))
           (should (= (length wire) 1))
           (let* ((response (json-parse-string (car wire) :object-type 'alist))
                  (content (map-nested-elt response '(result content))))
             (should (equal (map-elt content 'other)
                            (if (eq provider 'claude) (concat "Red, " sentinel) sentinel)))
             (should (equal (map-elt content 'q) (and (eq provider 'codex) ["Red"]))))
           (should (seq-some (lambda (event) (eq (plist-get event :event) 'elicitation-response)) events))
           (should (string-search "[redacted]" (format "%S" events)))
           (should-not (string-search sentinel (format "%S" events)))
           (should-not (map-elt record :answers))
           (should-not (map-elt state :elicitations))
           (should (string-search "[redacted]" (buffer-string)))
           (let ((print-circle t) (print-level nil) (print-length nil))
             (should-not (string-search sentinel (prin1-to-string (buffer-string)))))
           (with-current-buffer (acp-traffic-buffer :client client)
             (should-not (string-search sentinel (format "%S" (acp-traffic--objects))))
             (should-not (string-search sentinel (buffer-string))))))))))

(provide 'agent-shell-elicitation-default-safety-tests)
;;; agent-shell-elicitation-default-safety-tests.el ends here
