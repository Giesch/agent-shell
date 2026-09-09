;;; agent-shell-elicitation-tests.el --- Form contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises schema validation, real rendering, dispatch, and serialization.

;;; Code:

(require 'ert)
(require 'agent-shell)
(require 'acp-fakes)

(defconst agent-shell-elicitation-tests--schema
  '((type . "object") (required . ["text"])
    (properties . ((text . ((type . "string") (title . "Text")))
                   (choice . ((type . "string") (oneOf . [((const . "") (title . "Empty")) ((const . "B"))])))
                   (multi . ((type . "array") (items . ((anyOf . [((const . "A")) ((const . "B"))]))))))))
  "A required text question and optional choices.")

(defun agent-shell-elicitation-tests--call (schema function &optional native-request)
  "Render SCHEMA through fake process dispatch and call FUNCTION with state.

NATIVE-REQUEST optionally supplies the exact incoming request envelope."
  (let* ((messages (acp-traffic-read-file
                    (expand-file-name "elicitation-single-select.traffic"
                                      (file-name-directory (symbol-file 'agent-shell-fakes-start-agent)))))
         (request (seq-find (lambda (message)
                              (equal (map-nested-elt message '(:object method)) "elicitation/create")) messages))
         buffer client)
    (when native-request (setf (map-elt request :object) (copy-tree native-request t)))
    (let ((params (map-nested-elt request '(:object params))))
      (setf (map-elt params 'requestedSchema) (copy-tree schema t)))
    (save-window-excursion
      (unwind-protect
          (progn
            (setq buffer (agent-shell-fakes-start-agent messages))
            (agent-shell-fakes--settle)
            (with-current-buffer buffer
              (setq client (map-elt (agent-shell--state) :client))
              (shell-maker-submit :input "Question test")
              (agent-shell-fakes--settle)
              (funcall function (agent-shell--state))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (agent-shell--cancel-idle-timer))
          (kill-buffer buffer))
        (when client
          (ignore-errors (acp-shutdown :client client))
          (dolist (buffer (list (acp-traffic-buffer :client client) (acp-logs-buffer :client client)))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun agent-shell-elicitation-tests--button (label &optional chord)
  "Return LABEL's real rendered command, using CHORD when non-nil."
  (save-excursion
    (goto-char (point-min))
    (let (command)
      (while (and (not command) (search-forward label nil t))
        (when (get-text-property (1- (point)) 'button)
          (setq command (lookup-key (get-text-property (1- (point)) 'keymap)
                                    (kbd (or chord "RET"))))))
      (should (functionp command))
      command)))

(defun agent-shell-elicitation-tests--cancel (state id)
  "Replay remote cancellation for ID through ACP routing into STATE."
  (acp--route-incoming-message
   :client (map-elt state :client)
   :message (acp--make-message :object `((jsonrpc . "2.0") (method . "$/cancel_request") (params . ((requestId . ,id)))))
   :on-request (lambda (request) (agent-shell--on-request :state state :acp-request request))
   :on-notification (lambda (notification) (agent-shell--on-notification :state state :acp-notification notification))))

(ert-deftest agent-shell-phase2-schema-values ()
  "Validate wire values, required keys, and the supported schema subset."
  (let ((model (agent-shell--normalize-elicitation agent-shell-elicitation-tests--schema)))
    (should-not (map-elt model :reason))
    (should (agent-shell--elicitation-invalid-field model nil))
    (dolist (value '("" " " "verbatim"))
      (should-not (agent-shell--elicitation-invalid-field model `((text . ,value) (choice . "") (multi . [])))))
    (dolist (content '(((text . nil)) ((text . 0)) ((text . "ok") (extra . "x"))
                       ((text . "ok") (choice . "bad")) ((text . "ok") (multi . ["A" "A"]))
                       ((text . "ok") (multi . ["bad"])) ((text . "ok") (multi . ("A")))))
      (should (agent-shell--elicitation-invalid-field model content))))
  (dolist (required '([] ["text"] ["text" "choice"]))
    (let ((schema (copy-tree agent-shell-elicitation-tests--schema t)))
      (setf (map-elt schema 'required) required)
      (should-not (map-elt (agent-shell--normalize-elicitation schema) :reason))))
  (dolist (required '(nil ["unknown"] [42] ["text" "text"]))
    (let ((schema (copy-tree agent-shell-elicitation-tests--schema t)))
      (setf (map-elt schema 'required) required)
      (should (map-elt (agent-shell--normalize-elicitation schema) :reason))))
  (dolist (constraint '((minLength . 1) (pattern . "x") (format . "email") (default . "x") (allOf . []) ($ref . "x")))
    (let ((schema (copy-tree agent-shell-elicitation-tests--schema t)))
      (push constraint (cdr (assq 'text (map-elt schema 'properties))))
      (should (map-elt (agent-shell--normalize-elicitation schema) :reason)))))

(ert-deftest agent-shell-phase2-other-metadata ()
  "Normalize both providers, reject malformed and exclusive required branches."
  (dolist (meta '(((_askUserQuestionCustomAnswer . ((questionId . "q") (isCustomAnswer . t))))
                  ((codex . ((questionId . "q") (isOtherAnswer . t) (isSecret . :false))))
                  ((_askUserQuestionCustomAnswer . ((questionId . "q")))
                   (codex . ((questionId . "q") (isOtherAnswer . t))))))
    (dolist (reverse '(nil t))
      (let* ((properties `((q . ((type . "string") (oneOf . [((const . "Exact"))])))
                           (other . ((type . "string") (_meta . ,meta)))))
             (schema `((type . "object") (properties . ,(if reverse (reverse properties) properties)))))
        (should-not (map-elt (agent-shell--normalize-elicitation schema) :reason))
        (dolist (required '(["q"] ["other"] ["q" "other"]))
          (should (eq 'required-exclusive-branch
                      (map-elt (agent-shell--normalize-elicitation (cons (cons 'required required) schema)) :reason)))))))
  (dolist (meta '(((codex . ((questionId . "q") (isOtherAnswer . :false))))
                  ((codex . ((questionId . "missing") (isOtherAnswer . t))))
                  ((_askUserQuestionCustomAnswer . ((questionId . "q")))
                   (codex . ((questionId . "different") (isOtherAnswer . t))))
                  ((codex . ((isSecret . "yes"))))
                  ((unknown . ((isSecret . t))))))
    (should (map-elt (agent-shell--normalize-elicitation
                      `((type . "object")
                        (properties . ((q . ((type . "string") (oneOf . [((const . "a"))])))
                                       (other . ((type . "string") (_meta . ,meta))))))) :reason))))

(ert-deftest agent-shell-phase2-text-edit-validate-clear ()
  "Edit, abort, clear and submit through real form controls."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let ((client (map-elt state :client)))
       (funcall (agent-shell-elicitation-tests--button "Submit"))
       (should-not (acp-fakes-sent-responses client))
       (should (map-nested-elt state '(:elicitations 101 :validation)))
       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  exact  ")))
         (funcall (agent-shell-elicitation-tests--button "Edit" "C-<return>")))
       (should (equal "  exact  " (map-nested-elt state '(:elicitations 101 :answers text))))
       (should-not (acp-fakes-sent-responses client))
       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (signal 'quit nil))))
         (condition-case nil (funcall (agent-shell-elicitation-tests--button "Edit")) (quit nil)))
       (should (equal "  exact  " (map-nested-elt state '(:elicitations 101 :answers text))))
       (funcall (agent-shell-elicitation-tests--button "Clear"))
       (should-not (map-nested-elt state '(:elicitations 101 :answers)))
       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
         (funcall (agent-shell-elicitation-tests--button "Edit" "C-<return>")))
       (should-not (acp-fakes-sent-responses client))
       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "final")))
         (funcall (agent-shell-elicitation-tests--button "Edit")))
       (funcall (agent-shell-elicitation-tests--button "Submit" "C-<return>"))
       (should (equal "final" (map-nested-elt (car (acp-fakes-sent-responses client)) '(:result content text))))
       (should (= 1 (length (seq-filter (lambda (request) (equal (map-elt request :method) "session/prompt"))
                                        (acp-fakes-sent-requests client)))))))))

(ert-deftest agent-shell-phase2-cancellation-races ()
  "Remote cancellation wins once, preserves ID types and invalidates edits."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let* ((client (map-elt state :client))
            (record (map-nested-elt state '(:elicitations 101)))
            (submit (agent-shell-elicitation-tests--button "Submit"))
            (edit (agent-shell-elicitation-tests--button "Edit")))
       (agent-shell-elicitation-tests--cancel state "101")
       (should (map-nested-elt state '(:elicitations 101)))
       (cl-letf (((symbol-function 'read-string)
                  (lambda (&rest _) (agent-shell-elicitation-tests--cancel state 101) "stale")))
         (funcall edit))
       (funcall submit)
       (funcall edit)
       (agent-shell-elicitation-tests--cancel state 101)
       (should-not (map-elt record :answers))
       (should-not (map-elt state :elicitations))
       (should (= 1 (length (acp-fakes-sent-responses client))))
       (should (= -32800 (map-nested-elt (car (acp-fakes-sent-responses client)) '(:error code))))
       (should-not (text-property-any (point-min) (point-max) 'agent-shell-elicitation-token (map-elt record :token)))))))

(ert-deftest agent-shell-phase2-secret-transport-and-errors ()
  "Secrets reach only transport, even with reentry and a post-write error."
  (dolist (failure '(nil before after))
    (agent-shell-elicitation-tests--call
     '((type . "object") (properties . ((secret . ((type . "string") (_meta . ((codex . ((isSecret . t))))))))))
     (lambda (state)
       (let* ((client (map-elt state :client))
              (record (map-nested-elt state '(:elicitations 101)))
              (acp-logging-enabled t)
              (sentinel "PHASE2_SYNTHETIC_SECRET_7739")
              wire events (attempts 0))
         (cl-letf (((symbol-function 'read-passwd) (lambda (&rest args) (should (= 1 (length args))) sentinel)))
           (funcall (agent-shell-elicitation-tests--button "Edit")))
         (should (string-search "[secret entered]" (buffer-string)))
         (should-not (string-search sentinel (buffer-string)))
         (let ((print-circle t) (print-level nil) (print-length nil))
           (should-not (string-search sentinel (prin1-to-string (buffer-string)))))
         (setf (map-elt client :response-sender)
               (lambda (&rest args)
                 (setq attempts (1+ attempts))
                 (agent-shell-elicitation-tests--cancel state 101)
                 (when (eq failure 'before) (error "%s" sentinel))
                 (apply #'acp--response-sender args)
                 (when (eq failure 'after) (error "%s" sentinel))))
         (cl-letf (((symbol-function 'process-send-string) (lambda (_process json) (push json wire)))
                   ((symbol-function 'agent-shell--emit-event) (lambda (&rest args) (push args events))))
           (funcall (agent-shell-elicitation-tests--button "Submit")))
         (should (= attempts 1))
         (if (eq failure 'before) (should-not wire)
           (should (string-search sentinel (car wire))))
         (should-not (string-search sentinel (format "%S" events)))
         (should-not (map-elt record :answers))
         (should-not (map-elt state :elicitations))
         (with-current-buffer (acp-traffic-buffer :client client)
           (should-not (string-search sentinel (format "%S" (acp-traffic--objects))))
           (should-not (string-search sentinel (buffer-string))))
         (should-not (string-search sentinel (format "%S" minibuffer-history)))
         (when failure (should (string-search "delivery is unknown" (buffer-string)))))))))

(ert-deftest agent-shell-phase2-unsupported-is-persistent ()
  "Unsupported forms decline once and retain a safe client limitation notice."
  (agent-shell-elicitation-tests--call
   '((type . "object") (properties . ((q . ((type . "string") (pattern . "PRIVATE_PATTERN"))))))
   (lambda (state)
     (let ((responses (acp-fakes-sent-responses (map-elt state :client))))
       (should (= 1 (length responses)))
       (should (equal "decline" (map-nested-elt (car responses) '(:result action))))
       (should (string-search "agent-shell cannot display" (buffer-string)))
       (should-not (string-search "PRIVATE_PATTERN" (buffer-string)))
       (should-not (map-elt state :elicitations))))))

(ert-deftest agent-shell-phase2-disconnect-cleans-secret ()
  "Disconnect while editing retires answers and discards resumed input."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let ((client (map-elt state :client)) (record (map-nested-elt state '(:elicitations 101))))
       (cl-letf (((symbol-function 'read-string)
                  (lambda (&rest _)
                    (dolist (handler (map-elt client :disconnect-handlers)) (funcall handler))
                    "stale")))
         (funcall (agent-shell-elicitation-tests--button "Edit")))
       (should-not (map-elt state :elicitations))
       (should-not (map-elt record :answers))
       (should-not (acp-fakes-sent-responses client))))))

(defun agent-shell-elicitation-tests-native-response (input output mode)
  "Answer native INPUT through controls; serialize to OUTPUT using MODE.

MODE selects option, other, or text entry.  Values are synthetic."
  (let* ((request (with-temp-buffer
                    (insert-file-contents input)
                    (acp--parse-json (buffer-string))))
         (id (map-elt request 'id)))
    (agent-shell-elicitation-tests--call
     (map-nested-elt request '(params requestedSchema))
     (lambda (state)
       (let ((client (map-elt state :client)) wire)
         (should (map-nested-elt state (list :elicitations id)))
         (dolist (field (map-nested-elt state (list :elicitations id :fields)))
           (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "PHASE2_OTHER_ANSWER"))
                     ((symbol-function 'read-passwd) (lambda (&rest _) "PHASE2_SYNTHETIC_SECRET")))
             (funcall
              (cond ((eq (map-elt field :kind) 'text)
                     (agent-shell--elicitation-control :state state :request-id id :client client
                                                       :field field :operation 'edit))
                    ((and (equal mode "other") (map-elt field :custom))
                     (agent-shell--make-elicitation-custom-command
                      :state state :request-id id :client client :field field))
                    (t (agent-shell--make-elicitation-answer-command
                        :state state :request-id id :client client :field field
                        :value (map-elt (car (map-elt field :options)) :value)))))))
         (setf (map-elt client :response-sender)
               (lambda (&rest args) (apply #'acp--response-sender args)))
         (cl-letf (((symbol-function 'process-send-string) (lambda (_process json) (push json wire))))
           (funcall (agent-shell-elicitation-tests--button "Submit")))
         (should (= 1 (length wire)))
         (with-temp-file output (insert (car wire)))))
     request)))

(ert-deftest agent-shell-phase2-native-fixtures ()
  "Replay both native provider schemas through real controls and wire JSON."
  (dolist (provider '("claude" "codex"))
    (dolist (mode '("option" "other"))
      (let* ((directory (file-name-directory (symbol-file 'agent-shell-elicitation-tests--call)))
             (fixture (with-temp-buffer
                        (insert-file-contents (expand-file-name (format "phase2-native-%s.json" provider) directory))
                        (json-parse-buffer :object-type 'alist :false-object :false)))
             (input (make-temp-file "phase2-native"))
             (output (make-temp-file "phase2-wire")))
        (unwind-protect
            (progn
              (with-temp-file input (insert (json-serialize (map-elt fixture 'request) :false-object :false)))
              (agent-shell-elicitation-tests-native-response input output mode)
              (with-temp-buffer
                (insert-file-contents output)
                (let* ((wire (json-parse-buffer :object-type 'alist))
                       (content (map-nested-elt wire '(result content))))
                  (should (equal (map-elt wire 'id) (map-nested-elt fixture '(request id))))
                  (should (equal (cdar content) (if (equal mode "other") "PHASE2_OTHER_ANSWER" "Alpha")))
                  (should (= 1 (length content))))))
          (delete-file input) (delete-file output))))))

(ert-deftest agent-shell-phase2-duplicates-and-second-form ()
  "Active and terminal duplicates cannot replace forms or send again."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let* ((client (map-elt state :client))
            (record (map-nested-elt state '(:elicitations 101)))
            (request `((id . 101) (method . "elicitation/create")
                       (params . ((mode . "form") (sessionId . "fake-session-id")
                                  (requestedSchema . ,agent-shell-elicitation-tests--schema))))))
       (agent-shell--on-request :state state :acp-request request)
       (should (eq record (map-nested-elt state '(:elicitations 101))))
       (should-not (acp-fakes-sent-responses client))
       (setf (map-elt request 'id) "101")
       (agent-shell--on-request :state state :acp-request request)
       (let ((second (map-nested-elt state '(:elicitations "101"))))
         (should second)
         (agent-shell-elicitation-tests--cancel state 101)
         (should (eq second (map-nested-elt state '(:elicitations "101"))))
         (should (eq (map-elt second :token) (map-elt state :elicitation-idle-token)))
         (should (text-property-any (point-min) (point-max) 'agent-shell-elicitation-token (map-elt second :token))))
       (setf (map-elt request 'id) 101)
       (agent-shell--on-request :state state :acp-request request)
       (should-not (map-nested-elt state '(:elicitations 101)))
       (should (= 1 (length (acp-fakes-sent-responses client))))))))

(ert-deftest agent-shell-phase2-late-viewport-cleanup ()
  "Cancellation removes controls from a viewport opened after rendering."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let* ((record (map-nested-elt state '(:elicitations 101)))
            (token (map-elt record :token))
            (viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer))))
       (unwind-protect
           (progn
             (with-current-buffer viewport
               (agent-shell-viewport-view-last)
               (should (text-property-any (point-min) (point-max) 'agent-shell-elicitation-token token)))
             (should-not (memq viewport (map-elt record :render-buffers)))
             (agent-shell-elicitation-tests--cancel state 101)
             (with-current-buffer viewport
               (should-not (text-property-any (point-min) (point-max) 'agent-shell-elicitation-token token))))
         (when (buffer-live-p viewport) (kill-buffer viewport)))))))

(ert-deftest agent-shell-phase2-replacement-invalidates-controls ()
  "Old controls cannot edit the same numeric ID on a replacement client."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let* ((old (map-elt state :client))
            (edit (agent-shell-elicitation-tests--button "Edit"))
            (record (map-nested-elt state '(:elicitations 101)))
            (new (acp-make-client :command "fake" :response-sender #'ignore)))
       (setf (map-elt state :client) new)
       (agent-shell--subscribe-to-client-events :state state)
       (should-not (map-elt record :answers))
       (should-not (map-elt state :elicitations))
       (agent-shell--on-request
        :state state :acp-request
        `((id . 101) (method . "elicitation/create")
          (params . ((sessionId . "new") (mode . "form")
                     (requestedSchema . ,agent-shell-elicitation-tests--schema)))))
       (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (ert-fail "Stale editor ran"))))
         (funcall edit))
       (should-not (map-nested-elt state '(:elicitations 101 :answers)))
       (dolist (handler (map-elt old :disconnect-handlers)) (funcall handler))
       (should (map-nested-elt state '(:elicitations 101)))
       (agent-shell--retire-elicitations state new)))))

(ert-deftest agent-shell-phase2-buffer-kill-and-hook-errors ()
  "Buffer destruction and failing hooks cannot keep answers or send twice."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let ((record (map-nested-elt state '(:elicitations 101)))
           (client (map-elt state :client)))
       (cl-letf (((symbol-function 'read-string)
                  (lambda (&rest _) (kill-buffer (map-elt state :buffer)) "discard")))
         (funcall (agent-shell-elicitation-tests--button "Edit")))
       (should-not (map-elt record :answers))
       (should-not (map-elt state :elicitations))
       (should (= 1 (length (acp-fakes-sent-responses client)))))))
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (let ((client (map-elt state :client)) (attempts 0))
       (cl-letf (((symbol-function 'agent-shell--emit-event)
                  (lambda (&rest _) (setq attempts (1+ attempts)) (error "Synthetic hook failure")))
                 ((symbol-function 'agent-shell--delete-fragment) (lambda (&rest _) (error "Synthetic cleanup failure"))))
         (funcall (agent-shell-elicitation-tests--button "Skip")))
       (should (= attempts 2))
       (should (= 1 (length (acp-fakes-sent-responses client))))
       (should-not (map-elt state :elicitations))))))

(ert-deftest agent-shell-phase2-secret-other-clear-and-abort ()
  "A secret companion makes the entire group sensitive and clears safely."
  (agent-shell-elicitation-tests--call
   '((type . "object")
     (properties . ((q . ((type . "string") (oneOf . [((const . "A"))])))
                    (other . ((type . "string")
                              (_meta . ((codex . ((questionId . "q") (isOtherAnswer . t) (isSecret . t))))))))))
   (lambda (state)
     (let* ((record (map-nested-elt state '(:elicitations 101)))
            (client (map-elt state :client))
            (field (car (map-elt record :fields))))
       (should (map-elt field :secret))
       (should (equal '(other q) (map-elt record :sensitive)))
       (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "SECRET_OTHER_SENTINEL")))
         (funcall (agent-shell--make-elicitation-custom-command :state state :request-id 101 :client client :field field)))
       (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (signal 'quit nil))))
         (condition-case nil
             (funcall (agent-shell--make-elicitation-custom-command :state state :request-id 101 :client client :field field))
           (quit nil)))
       (should (equal "SECRET_OTHER_SENTINEL" (map-nested-elt record '(:answers other))))
       (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "")))
         (funcall (agent-shell--make-elicitation-custom-command :state state :request-id 101 :client client :field field :submit t)))
       (should-not (map-elt record :answers))
       (should-not (acp-fakes-sent-responses client))
       (let ((print-circle t))
         (should-not (string-search "SECRET_OTHER_SENTINEL" (prin1-to-string (buffer-string)))))))))

(ert-deftest agent-shell-phase2-property-identity-nil ()
  "The property named nil is a real key, including required and Other links."
  (let ((model (agent-shell--normalize-elicitation
                '((type . "object") (required . ["nil"])
                  (properties . ((nil . ((type . "string")))))))))
    (should-not (map-elt model :reason))
    (should (agent-shell--elicitation-invalid-field model nil))
    (should-not (agent-shell--elicitation-invalid-field model '((nil . "answer")))))
  (should (equal '((q . "new") (nil . "keep"))
                 (agent-shell--elicitation-record-answer '((nil . "keep")) 'q "new" nil)))
  (let* ((model (agent-shell--normalize-elicitation
                 '((type . "object")
                   (properties . ((nil . ((type . "string") (oneOf . [((const . "A"))])))
                                  (other . ((type . "string")
                                            (_meta . ((codex . ((questionId . "nil") (isOtherAnswer . t))))))))))))
         (field (car (map-elt model :fields))))
    (should (equal 'other (map-nested-elt field '(:custom :name))))
    (should (agent-shell--elicitation-field-answered-p '((other . "answer")) field))
    (should-not (agent-shell--elicitation-invalid-field model '((other . "answer"))))))

(ert-deftest agent-shell-phase2-source-text-fixture ()
  "Replay version-attributed Codex standalone and secret text through JSON."
  (let* ((fixture (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "phase2-source-codex-text.json"
                                       (file-name-directory (symbol-file 'agent-shell-elicitation-tests--call))))
                    (json-parse-buffer :object-type 'alist :false-object :false)))
         (input (make-temp-file "phase2-source")) (output (make-temp-file "phase2-output")))
    (unwind-protect
        (progn
          (with-temp-file input (insert (json-serialize (map-elt fixture 'request) :false-object :false)))
          (agent-shell-elicitation-tests-native-response input output "text")
          (with-temp-buffer
            (insert-file-contents output)
            (let ((wire (json-parse-buffer :object-type 'alist)))
              (should (equal "source-text" (map-elt wire 'id)))
              (should (equal "PHASE2_OTHER_ANSWER" (map-nested-elt wire '(result content text))))
              (should (equal "PHASE2_SYNTHETIC_SECRET" (map-nested-elt wire '(result content password)))))))
      (delete-file input) (delete-file output))))

(ert-deftest agent-shell-phase2-editor-error-is-sanitized ()
  "An editor error terminates once without disclosing its exception payload."
  (agent-shell-elicitation-tests--call
   agent-shell-elicitation-tests--schema
   (lambda (state)
     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (error "PRIVATE_EDITOR_ERROR"))))
       (funcall (agent-shell-elicitation-tests--button "Edit")))
     (let ((responses (acp-fakes-sent-responses (map-elt state :client))))
       (should (= 1 (length responses)))
       (should (= -32603 (map-nested-elt (car responses) '(:error code))))
       (should-not (string-search "PRIVATE_EDITOR_ERROR" (format "%S" responses))))
     (should-not (map-elt state :elicitations)))))

(provide 'agent-shell-elicitation-tests)
;;; agent-shell-elicitation-tests.el ends here
