;;; agent-shell-elicitation-upstream-tests.el --- Upstream form contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream ERT names not covered by agent-shell-elicitation-tests.el.
;; Exercise the unified form implementation, not the removed widget API.
;; Unsupported constraints reject the entire schema; unsupported fields are
;; never silently omitted.  Skip is the unified form's decline control.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'agent-shell)
(require 'agent-shell-elicitation)

(defconst agent-shell-upstream-tests--text
  '((type . "object") (properties (note (type . "string") (title . "Note")))))

(defconst agent-shell-upstream-tests--ask
  '((type . "object")
    (properties
     (question_0 (type . "string") (title . "Colour")
                 (oneOf . [((const . "Red")) ((const . "Blue"))]))
     (question_0_custom (type . "string") (title . "Other")
                        (_meta (_askUserQuestionCustomAnswer
                                (questionId . "question_0") (isCustomAnswer . t)))))))

(defconst agent-shell-upstream-tests--preview
  '((type . "object")
    (properties
     (q (type . "string") (title . "Approach")
        (oneOf . [((const . "a") (title . "Refactor first")
                   (_meta (_claude/askUserQuestionOption
                           (preview . "mockup one\nmockup two"))))
                  ((const . "b") (title . "Add first"))])))))

(defconst agent-shell-upstream-tests--questionnaire
  '((questions . [((question . "Which colour?") (header . "Colour")
                    (options . [((label . "Red")) ((label . "Blue"))]))])))

(defun agent-shell-upstream-tests--multi ()
  "Return the upstream multi-select companion schema."
  (copy-tree
   '((type . "object")
     (properties
      (question_0 (type . "array") (title . "Colours")
                  (items (anyOf . [((const . "Red")) ((const . "Blue"))
                                    ((const . "Green"))])))
      (question_0_custom (type . "string") (title . "Other")
                         (_meta (_askUserQuestionCustomAnswer
                                 (questionId . "question_0") (isCustomAnswer . t))))))
   t))

(cl-defun agent-shell-upstream-tests--request (&key (id 5) (mode "form")
                                                  tool-call-id (message "Pick one")
                                                  (schema agent-shell-upstream-tests--text))
  "Make a native ACP request using ID, MODE, TOOL-CALL-ID, MESSAGE and SCHEMA."
  `((id . ,id) (method . "elicitation/create")
    (params (sessionId . "session-1") (mode . ,mode) (message . ,message)
            (toolCallId . ,tool-call-id) (requestedSchema . ,schema))))

(cl-defmacro agent-shell-upstream-tests--with-shell ((state sent bodies deleted) &rest body)
  "Run BODY with STATE, captured SENT responses, BODIES and DELETED blocks."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let* ((,state (list (cons :buffer (current-buffer)) (cons :client 'test-client)
                         (cons :tool-calls nil) (cons :elicitations nil)
                         (cons :active-requests nil) (cons :event-subscriptions nil)
                         (cons :idle-timer nil) (cons :last-entry-type nil)))
            (,sent nil) (,bodies nil) (,deleted nil))
       (cl-letf (((symbol-function 'agent-shell--state) (lambda () ,state))
                 ((symbol-function 'agent-shell--update-fragment)
                  (lambda (&rest args) (push (plist-get args :body) ,bodies)))
                 ((symbol-function 'agent-shell--delete-fragment)
                  (lambda (&rest args) (push (plist-get args :block-id) ,deleted)))
                 ((symbol-function 'agent-shell-ui-delete-fragment) #'ignore)
                 ((symbol-function 'agent-shell-viewport--buffer) (lambda (&rest _) nil))
                 ((symbol-function 'agent-shell--append-transcript) #'ignore)
                 ((symbol-function 'agent-shell--emit-event) #'ignore)
                 ((symbol-function 'agent-shell--start-idle-timer) #'ignore)
                 ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
                 ((symbol-function 'acp-send-response)
                  (lambda (&rest args) (push (plist-get args :response) ,sent))))
         ,@body))))

(defun agent-shell-upstream-tests--get (state &optional id)
  "Return ID's pending record in STATE; ID defaults to 5."
  (map-nested-elt state (list :elicitations (or id 5))))

(defun agent-shell-upstream-tests--create (state &optional schema)
  "Dispatch SCHEMA as request 5 in STATE."
  (agent-shell--on-request :state state :acp-request
                           (agent-shell-upstream-tests--request
                            :schema (or schema agent-shell-upstream-tests--text))))

(defun agent-shell-upstream-tests--set (state name value &optional clear)
  "Record NAME's VALUE in STATE, optionally clearing CLEAR."
  (agent-shell--answer-elicitation :state state :request-id 5 :client 'test-client
                                    :field-name name :value value :clear clear))

(defun agent-shell-upstream-tests--submit (state)
  "Submit request 5 in STATE."
  (agent-shell--submit-elicitation :state state :request-id 5 :client 'test-client))

(defun agent-shell-upstream-tests--finish (state action)
  "Finish request 5 in STATE with ACTION."
  (agent-shell--finish-elicitation
   :state state :request-id 5 :client 'test-client :action action
   :content (when (equal action "accept")
              (map-elt (agent-shell-upstream-tests--get state) :answers))))

(defun agent-shell-upstream-tests--position (text label)
  "Return the position inside TEXT's button containing LABEL."
  (let ((start (string-match (regexp-quote label) text)))
    (should start)
    start))

(defun agent-shell-upstream-tests--command (text label &optional key)
  "Return TEXT button LABEL's command for KEY, defaulting to RET."
  (let* ((position (agent-shell-upstream-tests--position text label))
         (map (get-text-property position 'keymap text))
         (command (lookup-key map (kbd (or key "RET")))))
    (should (commandp command))
    command))

(ert-deftest agent-shell-elicitation-parse-schema-keeps-property-order-test ()
  (let ((model (agent-shell--normalize-elicitation
                '((type . "object") (properties (zebra (type . "string"))
                                                (apple (type . "string"))
                                                (mango (type . "string")))))))
    (should-not (map-elt model :reason))
    (should (equal (mapcar (lambda (field) (map-elt field :name)) (map-elt model :fields))
                   '(zebra apple mango)))))

(ert-deftest agent-shell-elicitation-parse-schema-marks-required-test ()
  (let ((model (agent-shell--normalize-elicitation
                '((type . "object") (properties (a (type . "string")) (b (type . "string")))
                  (required . ["b"])))))
    (should (equal (map-elt model :required) '(b)))
    (should (agent-shell--elicitation-invalid-field model '((a . "x"))))
    (should-not (agent-shell--elicitation-invalid-field model '((b . "x"))))))

(ert-deftest agent-shell-elicitation-parse-schema-seeds-defaults-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create
     state '((type . "object")
             (properties (pick (type . "string") (enum . ["a" "b"]) (default . "b"))
                         (tags (type . "array") (items (enum . ["a" "b"])) (default . ["a" "b"]))
                         (n (type . "integer") (default . 3))
                         (flag (type . "boolean") (default . t)) (note (type . "string")))))
    (let ((answers (map-elt (agent-shell-upstream-tests--get state) :answers)))
      (should (equal (map-elt answers 'pick) "b"))
      (should (equal (map-elt answers 'tags) ["a" "b"]))
      (should (equal (map-elt answers 'n) 3))
      (should (eq (map-elt answers 'flag) t))
      (should-not (assq 'note answers)))))

(ert-deftest agent-shell-elicitation-checkbox-answers-only-when-it-means-something-test ()
  (dolist (case '((nil nil nil nil) (nil nil t t) (nil nil :false nil)
                  (t nil nil :false) (t nil t t) (nil t nil t) (nil t :false :false)))
    (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
      (agent-shell-upstream-tests--create
       state `((type . "object")
               (properties (flag (type . "boolean") ,@(when (nth 1 case) '((default . t)))))
               ,@(when (car case) '((required . ["flag"])))))
      (when (nth 2 case) (agent-shell-upstream-tests--set state 'flag (nth 2 case)))
      (agent-shell-upstream-tests--submit state)
      (should (= (length sent) 1))
      (let ((content (map-nested-elt (car sent) '(:result content))))
        (if (nth 3 case)
            (should (equal (map-elt content 'flag) (nth 3 case)))
          (should-not (map-contains-key content 'flag)))))))

(ert-deftest agent-shell-elicitation-folds-custom-answer-into-its-select-test ()
  (let* ((model (agent-shell--normalize-elicitation agent-shell-upstream-tests--ask))
         (fields (map-elt model :fields)))
    (should-not (map-elt model :reason))
    (should (= (length fields) 1))
    (should (eq (map-nested-elt (car fields) '(:custom :name)) 'question_0_custom)))
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--ask)
    (should (string-match-p "Red" (car bodies)))
    (should (string-match-p "Other" (car bodies)))
    (should-not (string-match-p "Other: (empty)" (car bodies)))))

(ert-deftest agent-shell-elicitation-option-and-custom-answer-replace-each-other-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--ask)
    (funcall (agent-shell-upstream-tests--command (car bodies) "Red"))
    (should (equal (map-elt (agent-shell-upstream-tests--get state) :answers) '((question_0 . "Red"))))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "teal")))
      (funcall (agent-shell-upstream-tests--command (car bodies) "Other")))
    (should (equal (map-elt (agent-shell-upstream-tests--get state) :answers)
                   '((question_0_custom . "teal"))))
    (should (string-match-p "Other: teal" (car bodies)))
    (funcall (agent-shell-upstream-tests--command (car bodies) "Blue"))
    (should (equal (map-elt (agent-shell-upstream-tests--get state) :answers) '((question_0 . "Blue"))))))

(ert-deftest agent-shell-elicitation-multi-select-keeps-ticks-and-typed-answer-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state (agent-shell-upstream-tests--multi))
    (funcall (agent-shell-upstream-tests--command (car bodies) "Red"))
    (funcall (agent-shell-upstream-tests--command (car bodies) "Blue"))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "teal")))
      (funcall (agent-shell-upstream-tests--command (car bodies) "Other")))
    (let ((answers (map-elt (agent-shell-upstream-tests--get state) :answers)))
      (should (equal (map-elt answers 'question_0) ["Red" "Blue"]))
      (should (equal (map-elt answers 'question_0_custom) "teal")))
    (dolist (label '("Red" "Blue" "Other: teal"))
      (should (string-match-p (regexp-quote label) (car bodies))))))

(ert-deftest agent-shell-elicitation-multi-select-wire-encoding-test ()
  (dolist (custom '(nil "teal"))
    (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
      (agent-shell-upstream-tests--create state (agent-shell-upstream-tests--multi))
      (agent-shell-upstream-tests--set state 'question_0 ["Red" "Blue"])
      (when custom (agent-shell-upstream-tests--set state 'question_0_custom custom))
      (agent-shell-upstream-tests--submit state)
      (should (equal (map-nested-elt (car sent) '(:result content))
                     (if custom '((question_0_custom . "Red, Blue, teal"))
                       '((question_0 . ["Red" "Blue"])))))
      ;; The settled record must describe the transmitted answer once.
      (should (= 1 (seq-count (lambda (line) (string-match-p "Colours:" line))
                              (split-string (substring-no-properties (car bodies)) "\n")))))))

(ert-deftest agent-shell-elicitation-sends-custom-answer-alone-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--ask)
    (agent-shell-upstream-tests--set state 'question_0_custom "teal" '(question_0))
    (agent-shell-upstream-tests--submit state)
    (should (equal (map-nested-elt (car sent) '(:result content)) '((question_0_custom . "teal"))))
    (should (seq-some (lambda (body) (string-match-p "Colour: teal" body)) bodies))))

(ert-deftest agent-shell-elicitation-custom-answer-satisfies-required-select-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create
     state (append agent-shell-upstream-tests--ask '((required . ["question_0"])) ))
    (should (agent-shell-upstream-tests--get state))
    (agent-shell-upstream-tests--submit state)
    (should-not sent)
    (agent-shell-upstream-tests--set state 'question_0_custom "teal")
    (agent-shell-upstream-tests--submit state)
    (should (equal (map-nested-elt (car sent) '(:result content)) '((question_0_custom . "teal"))))))

(ert-deftest agent-shell-elicitation-dangling-custom-answer-stays-standalone-test ()
  (dolist (target '("nonexistent" "plain"))
    (let* ((model (agent-shell--normalize-elicitation
                   `((type . "object")
                     (properties (plain (type . "string"))
                                 (stray (type . "string") (title . "Other")
                                        (_meta (_askUserQuestionCustomAnswer (questionId . ,target))))))))
           (fields (map-elt model :fields)))
      (should-not (map-elt model :reason))
      (should (equal (mapcar (lambda (field) (map-elt field :name)) fields) '(plain stray)))
      (should-not (seq-some (lambda (field) (map-elt field :custom)) fields)))))

(ert-deftest agent-shell-elicitation-encodes-content-for-the-wire-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create
     state '((type . "object") (required . ["no"])
             (properties (yes (type . "boolean")) (no (type . "boolean"))
                         (n (type . "integer")) (x (type . "number"))
                         (tags (type . "array") (items (enum . ["a" "b"])))
                         (note (type . "string")))))
    (dolist (entry '((yes . t) (no . :false) (n . 1) (x . 1.5) (tags . ["a" "b"]) (note . "hi")))
      (agent-shell-upstream-tests--set state (car entry) (cdr entry)))
    (agent-shell-upstream-tests--submit state)
    (let ((decoded (json-parse-string (json-serialize (map-nested-elt (car sent) '(:result content)))
                                      :object-type 'alist :false-object :false)))
      (should (eq (map-elt decoded 'yes) t))
      (should (eq (map-elt decoded 'no) :false))
      (should (equal (map-elt decoded 'n) 1))
      (should (equal (map-elt decoded 'x) 1.5))
      (should (equal (map-elt decoded 'tags) ["a" "b"]))
      (should (equal (map-elt decoded 'note) "hi")))))

(ert-deftest agent-shell-elicitation-omits-unanswered-fields-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create
     state '((type . "object") (properties (a (type . "string")) (b (type . "string")))))
    (agent-shell-upstream-tests--set state 'a "kept")
    (agent-shell-upstream-tests--submit state)
    (should (equal (map-nested-elt (car sent) '(:result content)) '((a . "kept")))))
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state)
    (agent-shell-upstream-tests--submit state)
    (should (= (length sent) 1))
    ;; An empty accepted form still carries a JSON object, never null or
    ;; an omitted content member, regardless of ACP helper representation.
    (should (equal (json-serialize (map-elt (car sent) :result))
                   "{\"action\":\"accept\",\"content\":{}}")))
  ;; The unified fork rejects unsupported constraints instead of hiding fields.
  (should (map-elt (agent-shell--normalize-elicitation
                    '((type . "object") (properties (a (type . "string") (pattern . ".*"))))) :reason)))

(ert-deftest agent-shell-elicitation-make-response-shapes-test ()
  (dolist (action '("accept" "decline" "cancel"))
    (let ((response (acp-make-elicitation-create-response
                     :request-id 3 :action action :content (when (equal action "accept") '((a . "x"))))))
      (should (equal (map-elt response :request-id) 3))
      (should (equal (map-nested-elt response '(:result action)) action))
      (if (equal action "accept")
          (should (equal (map-nested-elt response '(:result content)) '((a . "x"))))
        (should-not (map-contains-key (map-elt response :result) 'content)))))
  (should-error (acp-make-elicitation-create-response :action "cancel")))

(ert-deftest agent-shell-elicitation-renders-and-blocks-the-shell-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--ask)
    (should-not sent)
    (should (eq (map-elt (agent-shell-upstream-tests--get state) :status) 'pending))
    (cl-letf (((symbol-function 'shell-maker-busy) (lambda (&rest _) t)))
      (should (eq (agent-shell-status) 'blocked)))
    (dolist (label '("Pick one" "Red" "Blue" "Submit" "Skip"))
      (should (string-match-p label (car bodies))))))

(ert-deftest agent-shell-elicitation-accept-sends-the-answers-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--ask)
    (agent-shell-upstream-tests--set state 'question_0 "Red")
    (agent-shell-upstream-tests--submit state)
    (should (equal (map-nested-elt (car sent) '(:result action)) "accept"))
    (should (equal (map-nested-elt (car sent) '(:result content)) '((question_0 . "Red"))))
    (should-not (agent-shell-upstream-tests--get state))
    (should deleted)
    (should-not (map-elt state :active-requests))))

(ert-deftest agent-shell-elicitation-decline-sends-decline-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state)
    (funcall (agent-shell-upstream-tests--command (car bodies) "Skip"))
    (should (equal (map-elt (car sent) :result) '((action . "decline"))))
    (should-not (agent-shell-upstream-tests--get state))))

(ert-deftest agent-shell-elicitation-interrupt-cancels-pending-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state)
    (agent-shell--retire-elicitations state 'test-client t)
    (should (equal (map-elt (car sent) :result) '((action . "cancel"))))
    (should-not (agent-shell-upstream-tests--get state))))

(ert-deftest agent-shell-elicitation-responds-once-per-request-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state)
    (agent-shell-upstream-tests--finish state "accept")
    (agent-shell-upstream-tests--finish state "cancel")
    (agent-shell--retire-elicitations state 'test-client t)
    (should (= (length sent) 1))
    (should-not (agent-shell-upstream-tests--get state))))

(ert-deftest agent-shell-elicitation-rejects-unadvertised-mode-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell--on-request :state state :acp-request
                             (agent-shell-upstream-tests--request :mode "url"))
    (should (equal (map-elt (car sent) :request-id) 5))
    (should (equal (map-nested-elt (car sent) '(:error code)) -32602))
    (should-not (agent-shell-upstream-tests--get state))))

(ert-deftest agent-shell-elicitation-disabled-answers-method-not-found-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (let ((agent-shell-elicitation-capability nil))
      (agent-shell-upstream-tests--create state))
    (should (equal (map-nested-elt (car sent) '(:error code)) -32601))
    (should-not (agent-shell-upstream-tests--get state))))

(ert-deftest agent-shell-elicitation-submit-gate-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create
     state (append agent-shell-upstream-tests--text '((required . ["note"]))))
    (should (commandp (agent-shell-upstream-tests--command (car bodies) "Submit")))
    (agent-shell-upstream-tests--submit state)
    (should-not sent)
    (should (eq (map-nested-elt (agent-shell-upstream-tests--get state) '(:validation :field)) 'note))
    (agent-shell-upstream-tests--set state 'note "filled in")
    (agent-shell-upstream-tests--submit state)
    (should (equal (map-nested-elt (car sent) '(:result content)) '((note . "filled in")))))
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create
     state '((type . "object") (properties (gizmo (type . "gizmo"))) (required . ["gizmo"])))
    (should-not (agent-shell-upstream-tests--get state))
    (should (equal (map-nested-elt (car sent) '(:result action)) "decline"))
    (should (seq-some (lambda (body) (string-match-p "cannot display\\|not support" body)) bodies))))

(ert-deftest agent-shell-elicitation-skips-question-already-shown-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (setf (map-elt state :tool-calls) '(("tc-1" . ((:title . "Which approach?")))))
    (agent-shell--on-request :state state :acp-request
                             (agent-shell-upstream-tests--request :tool-call-id "tc-1" :message "Which approach?"))
    (should-not (string-match-p "Which approach\\?" (car bodies)))
    (agent-shell--on-request :state state :acp-request
                             (agent-shell-upstream-tests--request :id 6 :tool-call-id "tc-1" :message "Something else?"))
    (should (string-match-p "Something else\\?" (car bodies)))))

(ert-deftest agent-shell-elicitation-preview-costs-no-extra-tab-stop-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--preview)
    (with-temp-buffer
      (insert (car bodies))
      (goto-char (point-min))
      (let ((count 0))
        (while (agent-shell-next-permission-button) (cl-incf count))
        (should (= count 4))))))

(ert-deftest agent-shell-elicitation-preview-is-readable-without-opening-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--preview)
    (let* ((body (car bodies))
           (preview (agent-shell-upstream-tests--position body "Refactor first"))
           (plain (agent-shell-upstream-tests--position body "Add first")))
      (should (equal (get-text-property preview 'help-echo body) "mockup one\nmockup two"))
      (should-not (lookup-key (get-text-property plain 'keymap body) (kbd "p")))
      (should-not (map-elt (agent-shell-upstream-tests--get state) :previews)))))

(ert-deftest agent-shell-elicitation-tool-call-reads-as-a-question-test ()
  (dolist (case `((,agent-shell-upstream-tests--questionnaire . "✓ Question")
                  (((path . "/tmp/x")) . "✓ Other")))
    (let* ((state (list (cons :tool-calls
                             (list (cons "t1" (list (cons :status "completed") (cons :kind "other")
                                                    (cons :title "Which colour?") (cons :raw-input (car case))))))))
           (label (agent-shell-make-tool-call-label state "t1")))
      (should (equal (substring-no-properties (map-elt label :status)) (cdr case))))))

(ert-deftest agent-shell-elicitation-settles-into-its-tool-call-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (setf (map-elt state :tool-calls)
          (list (cons "t1" (list (cons :raw-input agent-shell-upstream-tests--questionnaire)))))
    (agent-shell--on-request :state state :acp-request
                             (agent-shell-upstream-tests--request :tool-call-id "t1"))
    (let ((block (map-elt (agent-shell-upstream-tests--get state) :block-id)))
      (agent-shell-upstream-tests--finish state "accept")
      (should (member block deleted))
      (should (= (length sent) 1))
      (should-not (agent-shell-upstream-tests--get state)))))

(ert-deftest agent-shell-elicitation-without-a-questionnaire-keeps-its-form-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (setf (map-elt state :tool-calls) '(("t1" . ((:raw-input . ((path . "/tmp/x")))))))
    (agent-shell--on-request :state state :acp-request
                             (agent-shell-upstream-tests--request :tool-call-id "t1"))
    (agent-shell-upstream-tests--set state 'note "kept")
    (agent-shell-upstream-tests--finish state "accept")
    ;; Pending controls must go; a readable, settled account must remain.
    (should-not (agent-shell-upstream-tests--get state))
    (should (seq-some (lambda (body) (string-match-p "Answered" body)) bodies))
    (should (seq-some (lambda (body) (string-match-p "kept" body)) bodies))))

(ert-deftest agent-shell-elicitation-capability-advertises-form-only-test ()
  (cl-flet ((capability ()
              (let (sent)
                (with-temp-buffer
                  (let ((agent-shell--state (list (cons :buffer (current-buffer)))))
                    (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore)
                              ((symbol-function 'agent-shell--send-request)
                               (lambda (&rest args) (setq sent (plist-get args :request)))))
                      (agent-shell--initiate-handshake :shell-buffer (current-buffer) :on-initiated #'ignore))))
                (map-nested-elt sent '(:params clientCapabilities elicitation)))))
    ;; The unified fork always advertises the form capability it implements.
    (should (equal (json-serialize (capability)) "{\"form\":{}}"))))

(ert-deftest agent-shell-elicitation-arms-and-cancels-the-idle-timer-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (let (armed cancelled)
      (cl-letf (((symbol-function 'agent-shell--start-idle-timer) (lambda (&rest args) (setq armed args)))
                ((symbol-function 'agent-shell--cancel-idle-timer) (lambda () (setq cancelled t))))
        (agent-shell-upstream-tests--create state)
        (should (eq (plist-get armed :event) 'elicitation-request))
        (should (equal (map-elt (plist-get armed :data) :request-id) 5))
        (should-not cancelled)
        (agent-shell-upstream-tests--finish state "decline")
        (should cancelled)))))

(ert-deftest agent-shell-elicitation-keys-drive-the-whole-form-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (cl-letf (((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest args)
                   (let ((inhibit-read-only t))
                     (erase-buffer) (insert (plist-get args :body))))))
        (agent-shell-upstream-tests--create state agent-shell-upstream-tests--ask)
        (cl-flet ((press (label keys)
                    (goto-char (1+ (agent-shell-upstream-tests--position (buffer-string) label)))
                    (execute-kbd-macro keys)))
          (press "Red" (kbd "RET"))
          (should (equal (map-elt (agent-shell-upstream-tests--get state) :answers) '((question_0 . "Red"))))
          (press "Other" (vconcat (kbd "RET") (string-to-vector "teal") (kbd "RET")))
          (should (equal (map-elt (agent-shell-upstream-tests--get state) :answers) '((question_0_custom . "teal"))))
          (press "Submit" (kbd "RET"))
          (should (equal (map-nested-elt (car sent) '(:result content)) '((question_0_custom . "teal")))))))))

(ert-deftest agent-shell-elicitation-keys-reach-controls-only-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--ask)
    (insert (car bodies) "plain")
    (goto-char (1+ (agent-shell-upstream-tests--position (car bodies) "Red")))
    (should (commandp (key-binding (kbd "RET"))))
    (should-not (eq (key-binding (kbd "RET")) #'newline))
    (goto-char (point-max))
    (should (eq (key-binding (kbd "RET")) #'newline))
    (should (eq (key-binding "x") #'self-insert-command))))

(ert-deftest agent-shell-elicitation-navigation-walks-controls-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state agent-shell-upstream-tests--preview)
    (insert (car bodies))
    (goto-char (point-min))
    (let (positions)
      (while (agent-shell-next-permission-button) (push (point) positions))
      (should (= (length positions) 4))
      (dolist (position positions)
        (should (get-text-property position 'button))
        (should-not (get-text-property (1- position) 'button)))
      (should (agent-shell-previous-permission-button)))))

(ert-deftest agent-shell-elicitation-jump-lands-at-the-top-of-the-form-test ()
  (dolist (message '("Which approach should I take?" nil))
    (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
      (agent-shell--on-request :state state :acp-request
                               (agent-shell-upstream-tests--request :message message :schema agent-shell-upstream-tests--preview))
      (insert "earlier transcript\n\n" (car bodies))
      (goto-char (point-max))
      (should (agent-shell-elicitation-jump-to-latest-form))
      (should (string-match-p (regexp-quote (or message "Approach"))
                              (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (should (agent-shell-next-permission-button)))))

(ert-deftest agent-shell-elicitation-goto-control-survives-rerender-test ()
  (agent-shell-upstream-tests--with-shell (state sent bodies deleted)
    (agent-shell-upstream-tests--create state (agent-shell-upstream-tests--multi))
    (funcall (agent-shell-upstream-tests--command (car bodies) "Blue"))
    (insert (car bodies))
    (goto-char (point-min))
    (agent-shell--jump-to-focus-button)
    ;; The unified focus property must identify the option just toggled,
    ;; irrespective of its changed selected glyph.
    (should (get-text-property (point) 'agent-shell-button-focus))
    (should (string-match-p "Blue"
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
    (should-not sent)
    (erase-buffer)
    (insert "settled transcript")
    (goto-char (point-min))
    (let ((before (point)))
      (should-not (agent-shell--jump-to-focus-button))
      (should (= (point) before)))))

(provide 'agent-shell-elicitation-upstream-tests)
;;; agent-shell-elicitation-upstream-tests.el ends here
