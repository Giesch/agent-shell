;;; agent-shell-elicitation.el --- ACP question forms -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Normalization, editing, and completion of ACP elicitation forms.

;;; Code:

(require 'map)
(require 'seq)
(require 'acp)

(defvar agent-shell-elicitation-capability)
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")

(defun agent-shell--elicitation-put! (record key value)
  "Set KEY to VALUE in nonempty RECORD, preserving its identity."
  (if-let* ((entry (assq key record)))
      (setcdr entry value)
    (nconc record (list (cons key value)))))

(defun agent-shell--elicitation-interactive-command (function)
  "Wrap FUNCTION as an interactive command without retaining editor state."
  (lambda () (interactive) (funcall function)))

;;; Elicitations

(defun agent-shell--elicitation-block-id (request-id)
  "Return the fragment block id for elicitation REQUEST-ID."
  (format "elicitation-%s" request-id))

(define-error 'agent-shell-elicitation-unsupported "Unsupported ACP form")

(defun agent-shell--elicitation-reject (reason)
  "Reject a schema with the safe symbol REASON."
  (signal 'agent-shell-elicitation-unsupported (list reason)))

(defun agent-shell--elicitation-object (object allowed)
  "Validate OBJECT keys against ALLOWED, including presentation types."
  (unless (and (listp object) (seq-every-p #'consp object))
    (agent-shell--elicitation-reject 'malformed-object))
  (let ((keys (map-keys object)))
    (unless (= (length keys) (length (seq-uniq keys #'equal)))
      (agent-shell--elicitation-reject 'duplicate-property)))
  (map-do (lambda (key value)
            (unless (memq key allowed)
              (agent-shell--elicitation-reject 'unsupported-keyword))
            (when (and (memq key '(title description)) (not (stringp value)))
              (agent-shell--elicitation-reject 'malformed-presentation)))
          object))

(defun agent-shell--elicitation-metadata (spec)
  "Validate SPEC metadata and return companion identity and sensitivity.

Only `_meta.codex.isSecret' marks a supported sensitive property."
  (let* ((meta (map-elt spec '_meta))
         (claude (map-elt meta '_askUserQuestionCustomAnswer))
         (codex (map-elt meta 'codex))
         (claude-id (map-elt claude 'questionId))
         (codex-id (map-elt codex 'questionId)))
    (when (map-contains-key spec '_meta)
      (unless (and (listp meta) (seq-every-p #'consp meta))
        (agent-shell--elicitation-reject 'malformed-metadata)))
    (when (map-contains-key meta '_askUserQuestionCustomAnswer)
      (agent-shell--elicitation-object claude '(questionId isCustomAnswer))
      (unless (and (stringp claude-id)
                   (or (not (map-contains-key claude 'isCustomAnswer))
                       (eq (map-elt claude 'isCustomAnswer) t)))
        (agent-shell--elicitation-reject 'malformed-companion)))
    (when (map-contains-key meta 'codex)
      (agent-shell--elicitation-object codex '(questionId isOtherAnswer isOther isSecret))
      (dolist (key '(isOtherAnswer isOther isSecret))
        (when (and (map-contains-key codex key)
                   (not (memq (map-elt codex key) '(t :false))))
          (agent-shell--elicitation-reject 'malformed-sensitivity-or-metadata)))
      (when (or (map-contains-key codex 'questionId)
                (map-contains-key codex 'isOtherAnswer))
        (unless (and (stringp codex-id) (eq (map-elt codex 'isOtherAnswer) t))
          (agent-shell--elicitation-reject 'malformed-companion))))
    (when (and claude-id codex-id (not (equal claude-id codex-id)))
      (agent-shell--elicitation-reject 'contradictory-companion))
    (map-do
     (lambda (key value)
       (unless (memq key '(codex _askUserQuestionCustomAnswer))
         (when (agent-shell--elicitation-sensitive-metadata-p value)
           (agent-shell--elicitation-reject 'unknown-sensitive-metadata))))
     meta)
    (when (map-contains-key meta 'isSecret)
      (agent-shell--elicitation-reject 'unknown-sensitive-metadata))
    (list (cons :companion (and (or claude-id codex-id) t))
          (cons :question (when-let* ((id (or claude-id codex-id))) (intern id)))
          (cons :secret (eq (map-elt codex 'isSecret) t)))))

(defun agent-shell--elicitation-sensitive-metadata-p (value)
  "Check VALUE for an unhandled sensitivity marker."
  (if (vectorp value)
      (seq-some #'agent-shell--elicitation-sensitive-metadata-p value)
    (and (consp value)
         (or (eq (car value) 'isSecret)
             (agent-shell--elicitation-sensitive-metadata-p (car value))
             (agent-shell--elicitation-sensitive-metadata-p (cdr value))))))

(defun agent-shell--elicitation-option (acp-option)
  "Normalize ACP-OPTION, preserving its string constant exactly."
  (agent-shell--elicitation-object acp-option '(const title description _meta))
  (when (seq-some #'cdr (agent-shell--elicitation-metadata acp-option))
    (agent-shell--elicitation-reject 'unsupported-option-metadata))
  (unless (stringp (map-elt acp-option 'const))
    (agent-shell--elicitation-reject 'malformed-choice))
  (list (cons :value (map-elt acp-option 'const))
        (cons :title (or (map-elt acp-option 'title) (map-elt acp-option 'const)))
        (cons :description (map-elt acp-option 'description))
        (cons :preview (agent-shell-elicitation--option-preview acp-option))))

(defun agent-shell--elicitation-options (acp-options)
  "Normalize the nonempty ACP-OPTIONS vector, rejecting duplicates."
  (unless (and (vectorp acp-options) (> (length acp-options) 0))
    (agent-shell--elicitation-reject 'malformed-choices))
  (let* ((options (mapcar #'agent-shell--elicitation-option acp-options))
         (values (mapcar (lambda (option) (map-elt option :value)) options)))
    (unless (= (length values) (length (seq-uniq values #'equal)))
      (agent-shell--elicitation-reject 'duplicate-choice))
    options))

(defun agent-shell--elicitation-custom-field (name spec)
  "Normalize the Other companion NAME described by SPEC."
  (let ((metadata (agent-shell--elicitation-metadata spec)))
    (when (map-elt metadata :companion)
      (unless (and (equal (map-elt spec 'type) "string")
                   (not (map-contains-key spec 'oneOf))
                   (not (map-contains-key spec 'enum)))
        (agent-shell--elicitation-reject 'malformed-companion))
      (when (and (map-contains-key spec 'default)
                 (not (stringp (map-elt spec 'default))))
        (agent-shell--elicitation-reject 'invalid-default))
      (list (cons :name name)
            (cons :join-multi (map-contains-key (map-elt spec '_meta) '_askUserQuestionCustomAnswer))
            (cons :question (map-elt metadata :question))
            (cons :title (or (map-elt spec 'title) "Other"))
            (cons :description (map-elt spec 'description))))))

(defun agent-shell--elicitation-field (name spec custom)
  "Normalize field NAME from SPEC and attach its CUSTOM companion."
  (let* ((type (map-elt spec 'type))
         (multi (equal type "array"))
         (choice-spec (if multi (map-elt spec 'items) spec))
         (choices (or (map-elt choice-spec (if multi 'anyOf 'oneOf))
                      (when (map-contains-key choice-spec 'enum)
                        (let ((values (map-elt choice-spec 'enum)))
                          (unless (and (vectorp values) (seq-every-p #'stringp values))
                            (agent-shell--elicitation-reject 'malformed-choices))
                          (vconcat (mapcar (lambda (value) `((const . ,value))) values))))))
         (text (and (not multi) (not choices)))
         (options (unless text (agent-shell--elicitation-options choices)))
         (kind (cond ((member type '("boolean" "number" "integer")) (intern type))
                     (text 'text) (multi 'multi-choice) (t 'choice))))
    (unless (member type '("string" "array" "boolean" "number" "integer"))
      (agent-shell--elicitation-reject 'unsupported-type))
    (when (and text custom)
      (agent-shell--elicitation-reject 'companion-without-choices))
    (let ((field (list (cons :name name) (cons :title (map-elt spec 'title))
                       (cons :description (map-elt spec 'description))
                       (cons :multi multi) (cons :options options) (cons :custom custom)
                       (cons :kind kind)
                       (cons :has-default (map-contains-key spec 'default))
                       (cons :schema (seq-filter (lambda (entry) (memq (car entry) '(minimum maximum))) spec))
                       (cons :secret (map-elt (agent-shell--elicitation-metadata spec) :secret)))))
      (dolist (bound '(minimum maximum))
        (when (and (map-contains-key spec bound) (not (agent-shell--elicitation-finite-number-p (map-elt spec bound))))
          (agent-shell--elicitation-reject 'malformed-bound)))
      (when (and (map-contains-key spec 'minimum) (map-contains-key spec 'maximum)
                 (> (map-elt spec 'minimum) (map-elt spec 'maximum)))
        (agent-shell--elicitation-reject 'contradictory-bounds))
      (when (and (map-contains-key spec 'default)
                 (not (agent-shell--elicitation-value-valid-p field (map-elt spec 'default))))
        (agent-shell--elicitation-reject 'invalid-default))
      field)))

(defun agent-shell--elicitation-read-value (field prompt old operation)
  "Read a typed FIELD value with PROMPT and OLD for OPERATION.

Invalid numeric input stays in the editor loop and is never coerced."
  (if (or (eq operation 'other) (not (memq (map-elt field :kind) '(boolean number integer))))
      (if (map-elt field :secret) (read-passwd prompt) (read-string prompt old))
    (if (eq (map-elt field :kind) 'boolean)
        (if (y-or-n-p prompt) t :false)
      (catch 'value
        (let ((initial (when old (format "%s" old))))
          (while t
            (let* ((text (if (map-elt field :secret) (read-passwd prompt)
                           (read-string prompt initial)))
                   (value (and (string-match-p
                                (if (eq (map-elt field :kind) 'integer)
                                    "\\`[+-]?[0-9]+\\'"
                                  "\\`[+-]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\'") text)
                               (string-to-number text))))
              (when (equal text "") (throw 'value ""))
              (when (and value (agent-shell--elicitation-value-valid-p field value))
                (throw 'value value))
              (setq initial text)
              (message "Enter a valid number within the field's limits"))))))))

(defun agent-shell--elicitation-finite-number-p (value)
  "Return non-nil for finite numeric VALUE, including arbitrary integers."
  (and (numberp value)
       (or (integerp value)
           (and (= value value) (= (- value value) 0.0)))))

(defun agent-shell--elicitation-value-valid-p (field value)
  "Check VALUE against the complete normalized FIELD contract."
  (let* ((spec (map-elt field :schema))
         (options (mapcar (lambda (option) (map-elt option :value)) (map-elt field :options))))
    (pcase (or (map-elt field :kind) (if (map-elt field :multi) 'multi-choice 'choice))
      ('text (stringp value))
      ('boolean (memq value '(t :false)))
      ((or 'number 'integer)
       (and (if (eq (map-elt field :kind) 'integer) (integerp value) (numberp value))
            (agent-shell--elicitation-finite-number-p value)
            (or (not (map-contains-key spec 'minimum)) (>= value (map-elt spec 'minimum)))
            (or (not (map-contains-key spec 'maximum)) (<= value (map-elt spec 'maximum)))))
      ('multi-choice
       (and (vectorp value) (= (length value) (length (seq-uniq value #'equal)))
            (seq-every-p (lambda (choice) (member choice options)) value)))
      (_ (and (stringp value) (member value options))))))

(defun agent-shell--normalize-elicitation (schema)
  "Return a complete model for SCHEMA or a safe :reason alist."
  (condition-case err
      (progn
        (agent-shell--elicitation-object
         schema '(type properties required additionalProperties title description _meta))
        (unless (equal (map-elt schema 'type) "object")
          (agent-shell--elicitation-reject 'unsupported-type))
        (when (and (map-contains-key schema 'additionalProperties)
                   (not (memq (map-elt schema 'additionalProperties) '(t :false))))
          (agent-shell--elicitation-reject 'unsupported-additional-properties))
        (when (seq-some #'cdr (agent-shell--elicitation-metadata schema))
          (agent-shell--elicitation-reject 'unsupported-object-metadata))
        (let ((properties (map-elt schema 'properties))
              (required (map-elt schema 'required))
              fields customs sensitive)
          (unless (and (listp properties) properties
                       (seq-every-p (lambda (entry) (and (consp entry) (symbolp (car entry)))) properties)
                       (= (length properties) (length (seq-uniq (map-keys properties)))))
            (agent-shell--elicitation-reject 'malformed-properties))
          (when (map-contains-key schema 'required)
            (unless (and (vectorp required) (seq-every-p #'stringp required)
                         (= (length required) (length (seq-uniq required #'equal)))
                         (seq-every-p (lambda (name) (map-contains-key properties (intern name))) required))
              (agent-shell--elicitation-reject 'malformed-required)))
          (map-do
           (lambda (name spec)
             (agent-shell--elicitation-object
              spec (cond ((equal (map-elt spec 'type) "string")
                          '(type title description _meta oneOf enum default))
                         ((member (map-elt spec 'type) '("number" "integer"))
                          '(type title description _meta default minimum maximum))
                         ((equal (map-elt spec 'type) "boolean")
                          '(type title description _meta default))
                         ((equal (map-elt spec 'type) "array")
                          '(type title description _meta items default))
                         (t (agent-shell--elicitation-reject 'unsupported-type))))
             (when (equal (map-elt spec 'type) "array")
               (agent-shell--elicitation-object (map-elt spec 'items) '(anyOf enum))
               (when (and (map-contains-key (map-elt spec 'items) 'anyOf)
                          (map-contains-key (map-elt spec 'items) 'enum))
                 (agent-shell--elicitation-reject 'contradictory-choices)))
             (when (and (map-contains-key spec 'oneOf) (map-contains-key spec 'enum))
               (agent-shell--elicitation-reject 'contradictory-choices))
             (when-let* ((custom (agent-shell--elicitation-custom-field name spec)))
               (when (seq-find (lambda (other) (eq (map-elt other :question) (map-elt custom :question))) customs)
                 (agent-shell--elicitation-reject 'duplicate-companion))
               (push custom customs)))
           properties)
          (dolist (custom customs)
            (let ((next custom) seen)
              (while next
                (when (memq (map-elt next :name) seen)
                  (agent-shell--elicitation-reject 'cyclic-companion))
                (push (map-elt next :name) seen)
                (setq next (seq-find
                            (lambda (candidate)
                              (eq (map-elt candidate :name) (map-elt next :question)))
                            customs)))))
          (setq customs
                (seq-filter (lambda (custom)
                              (let* ((target (map-elt properties (map-elt custom :question)))
                                     (items (map-elt target 'items)))
                                (or (map-contains-key target 'oneOf)
                                    (map-contains-key target 'enum)
                                    (map-contains-key items 'anyOf)
                                    (map-contains-key items 'enum))))
                            customs))
          (map-do
           (lambda (name spec)
             (unless (seq-find (lambda (custom) (eq (map-elt custom :name) name)) customs)
               (let* ((custom (seq-find (lambda (entry) (eq (map-elt entry :question) name)) customs))
                      (field (agent-shell--elicitation-field name spec custom)))
                 (when custom
                   (when (map-elt (agent-shell--elicitation-metadata (map-elt properties (map-elt custom :name))) :secret)
                     (agent-shell--elicitation-put! field :secret t)))
                 (when (map-elt field :secret)
                   (push name sensitive)
                   (when custom (push (map-elt custom :name) sensitive)))
                 (push field fields))))
           properties)
          (unless fields (agent-shell--elicitation-reject 'empty-form))
          (list (cons :fields (nreverse fields)) (cons :properties properties)
                (cons :required (mapcar #'intern required)) (cons :sensitive sensitive))))
    (agent-shell-elicitation-unsupported (list (cons :reason (cadr err))))
    (error '((:reason . malformed-schema)))))

(defun agent-shell--elicitation-form-fields (requested-schema)
  "Return normalized REQUESTED-SCHEMA fields, or `unsupported'."
  (let ((model (agent-shell--normalize-elicitation requested-schema)))
    (if (map-elt model :reason) 'unsupported (map-elt model :fields))))

(defun agent-shell--elicitation-invalid-field (elicitation content)
  "Return a safe validation failure for ELICITATION and CONTENT, or nil.

The result contains :reason and :field; it never contains answer values."
  (catch 'invalid
    (let ((fields (map-elt elicitation :fields)) allowed)
      (dolist (field fields)
        (let* ((name (map-elt field :name))
               (companion (map-elt field :custom))
               (custom (map-elt companion :name))
               (value (map-elt content name)))
          (push name allowed)
          (when companion (push custom allowed))
          (when (or (and (not (map-elt field :multi))
                          (map-contains-key content name) companion (map-contains-key content custom))
                    (and companion (map-contains-key content custom) (not (stringp (map-elt content custom))))
                    (and (map-contains-key content name)
                         (not (agent-shell--elicitation-value-valid-p field value)))
                    (seq-some (lambda (key) (and (memq key (map-elt elicitation :required))
                                                 (not (agent-shell--elicitation-field-answered-p content field))))
                              (append (list name) (when companion (list custom)))))
            (throw 'invalid (list (cons :reason 'invalid-or-required-answer) (cons :field name))))))
      (unless (and (seq-every-p (lambda (entry) (memq (car entry) allowed)) content)
                   (= (length content) (length (seq-uniq (map-keys content)))))
        (throw 'invalid (list (cons :reason 'unknown-or-duplicate-answer)
                              (cons :field (map-elt (car fields) :name))))))))

(defun agent-shell--elicitation-field-answered-p (answers field)
  "Return non-nil when ANSWERS carries an answer to FIELD.

A question is answered through its options or through its Other box.  The
agent prefers a non-empty Other box over the options, so either one
answers the question."
  (or (map-contains-key answers (map-elt field :name))
      (when (map-elt field :custom)
        (map-contains-key answers (map-nested-elt field '(:custom :name))))))

(defun agent-shell--elicitation-pending-field (elicitation)
  "Return the first unanswered field of ELICITATION, or nil."
  (let ((answers (map-elt elicitation :answers)))
    (seq-find (lambda (field)
                (not (agent-shell--elicitation-field-answered-p answers field)))
              (map-elt elicitation :fields))))

(defun agent-shell--elicitation-answer-completes-p (elicitation field)
  "Return non-nil when answering FIELD completes ELICITATION.

Every other field of ELICITATION already carries an answer.  A
one-question form always satisfies this."
  (let ((answers (map-elt elicitation :answers)))
    (seq-every-p (lambda (other)
                   (or (eq other field)
                       (agent-shell--elicitation-field-answered-p answers other)))
                 (map-elt elicitation :fields))))

(defun agent-shell--elicitation-field-label (field fallback)
  "Return the question text for FIELD, or FALLBACK when it carries none.

A field's description is the question.  A single-question schema leaves
the description out and carries the question in the request's message
instead, which the caller passes as FALLBACK.  A field's title is a short
label for the answer (\"Auth\"), not the question, so it comes last."
  (or (map-elt field :description)
      fallback
      (map-elt field :title)
      (format "%s" (map-elt field :name))))

(defun agent-shell--elicitation-toggle-selection (field selection value)
  "Add VALUE to SELECTION, or remove it when SELECTION already carries it.

FIELD is a multi-select field and SELECTION is its current answer: a
vector of option labels, or nil when the question is unanswered.

Returns a vector in FIELD's option order rather than in the order the
options were pressed, so the answer reads the way the question does.
Returns nil when the toggle leaves nothing selected, which
`agent-shell--answer-elicitation' records as unanswered."
  (let* ((chosen (if (seq-contains-p selection value)
                     (seq-remove (lambda (each) (equal each value)) selection)
                   (append selection (list value))))
         (ordered (seq-filter (lambda (option)
                                (seq-contains-p chosen (map-elt option :value)))
                              (map-elt field :options))))
    (when ordered
      (vconcat (mapcar (lambda (option) (map-elt option :value)) ordered)))))

(cl-defun agent-shell--elicitation-control (&key state request-id client field value submit operation)
  "Make a control for REQUEST-ID in STATE using CLIENT.

FIELD, VALUE, SUBMIT and OPERATION describe the edit.  The control retains
only public field data and a token; answers are read from pending storage."
  (ignore client)
  (agent-shell--elicitation-interactive-command
   (apply-partially #'agent-shell--activate-elicitation
                    (map-elt state :buffer) request-id
                    (map-nested-elt state (list :elicitations request-id :token))
                    field value submit operation)))

(defun agent-shell--activate-elicitation (buffer request-id token field value submit operation)
  "Activate a form control in BUFFER for REQUEST-ID and TOKEN.

FIELD, VALUE, SUBMIT and OPERATION select the edit or terminal action."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((state (agent-shell--state))
             (client (map-elt state :client)))
        (when-let* ((record (agent-shell--elicitation-current state request-id client token)))
          (condition-case nil
              (pcase operation
                ('cancel (agent-shell--finish-elicitation :state state :request-id request-id :client client
                                                          :token token :action "cancel" :reason 'locally-canceled))
                ('skip (agent-shell--finish-elicitation :state state :request-id request-id :client client
                                                        :token token :action "decline" :reason 'user-skipped))
                ('submit (agent-shell--submit-elicitation :state state :request-id request-id :client client))
                ('preview
                 (let ((key (cons (map-elt field :name) value)))
                   (agent-shell--elicitation-put!
                    record :previews
                    (if (member key (map-elt record :previews))
                        (remove key (map-elt record :previews))
                      (cons key (map-elt record :previews))))
                   (agent-shell--elicitation-put! record :focus-option key)
                   (agent-shell--render-elicitation
                    :state state :request-id request-id :client client)))
                ((or 'edit 'other)
                 (let* ((name (if (eq operation 'other) (map-nested-elt field '(:custom :name)) (map-elt field :name)))
                        (old (map-elt (map-elt record :answers) name))
                        (prompt (if (eq operation 'other) (agent-shell--elicitation-custom-prompt field) "Answer: "))
                        (text (agent-shell--elicitation-read-value field prompt old operation)))
                   (when (and (agent-shell--elicitation-current state request-id client token)
                              (agent-shell--elicitation-connected-p state client))
                     (agent-shell--answer-elicitation
                      :state state :request-id request-id :client client :field-name name
                      :value (unless (equal text "") text)
                      :clear (when (and (eq operation 'other) (not (map-elt field :multi)))
                                (list (map-elt field :name)))
                      :submit (and submit (not (equal text "")))))))
                (_
                 (agent-shell--answer-elicitation
                  :state state :request-id request-id :client client
                  :field-name (if (eq operation 'clear-other) (map-nested-elt field '(:custom :name)) (map-elt field :name))
                  :clear (when (and (not (map-elt field :multi))
                                    (not (memq operation '(clear clear-other))) (map-elt field :custom))
                           (list (map-nested-elt field '(:custom :name))))
                  :value (pcase operation
                           ((or 'clear 'clear-other) nil)
                           ('toggle (agent-shell--elicitation-toggle-selection
                                     field (map-elt (map-elt record :answers) (map-elt field :name)) value))
                           (_ value))
                  :focus-option (when (and (eq operation 'toggle) (not submit)) (cons (map-elt field :name) value))
                  :submit submit)))
            (error
             (agent-shell--finish-elicitation
              :state state :request-id request-id :client client :token token :reason 'failed
              :error (acp-make-error :code -32603 :message "Unable to edit ACP form")))))))))

(cl-defun agent-shell--make-elicitation-answer-command (&key state request-id client field value submit)
  "Make a selection control for FIELD and VALUE in STATE's REQUEST-ID.

CLIENT sends the response; SUBMIT requests automatic completion."
  (agent-shell--elicitation-control :state state :request-id request-id :client client
                                    :field field :value value :submit submit :operation 'choice))

(cl-defun agent-shell--make-elicitation-toggle-command (&key state request-id client field value submit)
  "Make a toggle control for FIELD and VALUE in STATE's REQUEST-ID.

CLIENT sends the response; SUBMIT requests automatic completion."
  (agent-shell--elicitation-control :state state :request-id request-id :client client
                                    :field field :value value :submit submit :operation 'toggle))

(defun agent-shell--elicitation-custom-prompt (field)
  "Return the edit prompt for FIELD's Other companion."
  (if (map-elt field :title)
      (format "%s (%s): " (map-nested-elt field '(:custom :title)) (map-elt field :title))
    (format "%s: " (map-nested-elt field '(:custom :title)))))

(cl-defun agent-shell--make-elicitation-custom-command (&key state request-id client field submit)
  "Make the Other editor for FIELD in STATE's REQUEST-ID through CLIENT.

SUBMIT requests automatic completion after committing nonempty text."
  (agent-shell--elicitation-control :state state :request-id request-id :client client
                                    :field field :submit submit :operation 'other))

(cl-defun agent-shell--make-elicitation-edit-buttons (&key state request-id client field keymap focus other)
  "Make text controls for FIELD in STATE's REQUEST-ID using CLIENT.

KEYMAP is inherited.  FOCUS marks Edit; OTHER selects the companion Clear."
  (let ((edit-map (make-sparse-keymap)))
    (set-keymap-parent edit-map keymap)
    (define-key edit-map (kbd "C-<return>")
                (agent-shell--elicitation-control :state state :request-id request-id :client client
                                                  :field field :operation 'edit :submit t))
    (append
     (unless other
       (list (agent-shell--make-permission-button
              :text "Edit (e)" :help "Edit answer" :char "e" :navigatable t :focus focus
              :keymap edit-map :option "edit"
              :action (agent-shell--elicitation-control :state state :request-id request-id :client client
                                                        :field field :operation 'edit))))
     (list (agent-shell--make-permission-button
            :text "Clear" :help "Clear answer" :keymap keymap :navigatable t :option "clear"
            :action (agent-shell--elicitation-control :state state :request-id request-id :client client
                                                      :field field :operation (if other 'clear-other 'clear)))))))

(defun agent-shell--elicitation-glyph (multi selected)
  "Return the mark an option carries, SELECTED or not.

A radio marks a question that takes one answer, a checkbox one that takes
several, so MULTI picks the pair.

A glyph rather than a face: the button frame is a box in a graphical
frame and brackets in a terminal, so a background colour would not read
the same in both."
  (cond ((and multi selected) agent-shell-elicitation-checked-icon)
        (multi agent-shell-elicitation-unchecked-icon)
        (selected agent-shell-elicitation-selected-icon)
        (t agent-shell-elicitation-unselected-icon)))

(cl-defun agent-shell--make-elicitation-custom-button (&key state request-id client field answer completes keymap)
  "Create the Other button of FIELD, carrying ANSWER.

ANSWER is the free text FIELD's Other box already holds, or nil.  The
button shows it, truncated, so the answer is readable before the form is
submitted.

COMPLETES is non-nil when answering FIELD completes the form, which is
what `C-<return>' needs to submit.  KEYMAP is the question's keymap,
which the button's own keymap extends.  STATE, REQUEST-ID, and CLIENT
identify the elicitation to update.

The button carries the glyph of its question rather than a radio in every
form, so one row reads as one kind of question.  Recording free text
drops the question's selection, so the cleared options already show that
the box replaces them.

For example:

  \"[ ● Other: use OAuth (o) ]\""
  (let ((button-keymap (make-sparse-keymap)))
    (set-keymap-parent button-keymap keymap)
    ;; "C-<return>" acts on the button point sits on, so it binds below
    ;; the question's "o".
    (define-key button-keymap (kbd "C-<return>")
                (agent-shell--make-elicitation-custom-command
                 :state state
                 :request-id request-id
                 :client client
                 :field field
                 :submit t))
    (agent-shell--make-permission-button
     ;; The hotkey stays last: `agent-shell--make-permission-button'
     ;; derives the navigatable character from the trailing "(x)".
     :text (format "%s %s%s (o)"
                   (agent-shell--elicitation-glyph (map-elt field :multi) answer)
                   (map-nested-elt field '(:custom :title))
                   (if answer
                       (if (map-elt field :secret) ": [secret entered]"
                         (format ": %s" (truncate-string-to-width answer 32 0 nil t)))
                     ""))
     :help (or (map-nested-elt field '(:custom :description))
               "Type your own answer")
     :action (agent-shell--make-elicitation-custom-command
              :state state
              :request-id request-id
              :client client
              :field field
              :submit nil)
     :keymap button-keymap
     :navigatable t
     :char "o"
     :option (if completes
                 "type your own answer, or C-RET to type it and submit"
               "type your own answer"))))

(cl-defun agent-shell--make-elicitation-text (&key elicitation request-id client state)
  "Create text to render the question form for ELICITATION.

REQUEST-ID identifies the elicitation within STATE.  CLIENT is the ACP
client used to send the response.

Every question renders its options as buttons, numbered for hotkey
access.  A single-select question renders radios and a press replaces its
answer.  A multi-select question renders checkboxes and a press toggles
one option.  A question whose schema offers a free-text box renders an
Other button after its options, and a press reads the text from the
minibuffer.  The form carries a Submit button and sends the answers when
it is pressed.

`C-<return>' on an option or on Other records that answer and submits the
form.  It submits only when every other question already carries an
answer, a multi-select toggle that clears the last selected option never
submits, and neither does an Other box left empty.  Elsewhere it records
the answer alone, so the chord never sends a partial accept.  A terminal
sends the same byte for `RET' and `C-<return>', so the chord records the
answer alone there too.

Point lands on the option a multi-select toggle just acted on, else on
the first option of the first unanswered question, else on Submit.  See
`agent-shell--jump-to-focus-button'.

For example:

   ╭─

       ⁇ Question ⁇


       I need two decisions first.


       Which auth approach?

       [ ● JWT (1) ]
       [ ○ Session (2) ]
       [ ○ Other (o) ]

       Which areas should the tests cover?

       [ ✓ Parsing (1) ]
       [ □ Rendering (2) ]
       [ ✓ Errors (3) ]
       [ □ Other (o) ]

       [ Submit (c) ] [ Skip (s) ]


   ╰─"
  (let* ((message (let ((text (map-elt elicitation :message))
                        (title (map-nested-elt state (list :tool-calls (map-elt elicitation :tool-call-id) :title))))
                    (unless (and title (equal title text)) text)))
         (fields (map-elt elicitation :fields))
         (answers (agent-shell--elicitation-redact
                   (map-elt elicitation :answers) (map-elt elicitation :sensitive)))
         (pending (or (seq-find (lambda (field)
                                  (eq (map-elt field :name) (map-nested-elt elicitation '(:validation :field)))) fields)
                      (agent-shell--elicitation-pending-field elicitation)))
         ;; The option a multi-select toggle just acted on.  Point stays
         ;; there rather than moving to the next question, so the question's
         ;; own digit hotkeys stay under point for the next toggle.
         (focus-option (map-elt elicitation :focus-option))
         ;; With one question the message is the question itself.  With
         ;; several it is a generic lead-in, rendered once as a header.
         (single (= (length fields) 1))
         (skip-action (agent-shell--elicitation-control
                       :state state :request-id request-id :client client :operation 'skip))
         (submit-action (agent-shell--elicitation-control
                         :state state :request-id request-id :client client :operation 'submit))
         ;; Every question is answerable at once, so a digit is only
         ;; unambiguous within one question.  Each question gets a child
         ;; of this map binding its own digits.  The map rides on the
         ;; button as a text property, so point picks the question.
         (keymap (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "TAB") #'agent-shell-elicitation-next-field)
                   (define-key map (kbd "<backtab>") #'agent-shell-elicitation-previous-field)
                   (define-key map (kbd "s") skip-action)
                   (define-key map (kbd "c") submit-action)
                   (define-key map (kbd "C-<return>") submit-action)
                   (define-key map (kbd "C-c C-c")
                               (agent-shell--elicitation-control :state state :request-id request-id :client client :operation 'cancel))
                   map))
         (rows (mapcar
                (lambda (field)
                  (let* ((multi (map-elt field :multi))
                         (label (propertize (agent-shell--elicitation-field-label
                                             field (when single message))
                                            'font-lock-face 'agent-shell-input))
                         (answer (map-elt answers (map-elt field :name)))
                         (custom-answer (map-elt answers (map-nested-elt field '(:custom :name))))
                         (completes (agent-shell--elicitation-answer-completes-p
                                     elicitation field))
                         (field-keymap (make-sparse-keymap))
                         ;; A single-select press replaces the answer.  A
                         ;; multi-select press toggles one option of it.
                         (option-command
                          (lambda (option submit)
                            (if multi
                                (agent-shell--make-elicitation-toggle-command
                                 :state state
                                 :request-id request-id
                                 :client client
                                 :field field
                                 :value (map-elt option :value)
                                 :submit submit)
                              (agent-shell--make-elicitation-answer-command
                               :state state
                               :request-id request-id
                               :client client
                               :field field
                               :value (map-elt option :value)
                               :submit submit)))))
                    (set-keymap-parent field-keymap keymap)
                    (seq-do-indexed
                     (lambda (option index)
                       (when (< index 9)
                         (define-key field-keymap (kbd (number-to-string (1+ index)))
                                     (funcall option-command option nil))))
                     (map-elt field :options))
                    (when (map-elt field :custom)
                      (define-key field-keymap (kbd "o")
                                  (agent-shell--make-elicitation-custom-command
                                   :state state
                                   :request-id request-id
                                   :client client
                                   :field field
                                   :submit nil)))
                    (format "%s\n\n    %s" label
                            (string-join
                             (append
                              (seq-map-indexed
                               (lambda (option index)
                                 (let* ((hotkey (when (< index 9)
                                                  (number-to-string (1+ index))))
                                        (value (map-elt option :value))
                                        (selected (if multi
                                                      (seq-contains-p answer value)
                                                    (equal answer value)))
                                        (verb (cond ((not multi) "answer")
                                                    (selected "unselect")
                                                    (t "select")))
                                        ;; A multi-select press that clears the
                                        ;; last selected option leaves the
                                        ;; question unanswered, so the chord
                                        ;; toggles alone rather than submitting
                                        ;; a form it just emptied.
                                        (chord-submits
                                         (and completes
                                              (or (not multi)
                                                  (and (agent-shell--elicitation-toggle-selection
                                                        field answer value)
                                                       t))))
                                        ;; "C-<return>" acts on the option
                                        ;; point sits on, so it binds below
                                        ;; the question's digits.
                                        (option-keymap (make-sparse-keymap)))
                                   (set-keymap-parent option-keymap field-keymap)
                                   (define-key option-keymap (kbd "C-<return>")
                                               (funcall option-command option t))
                                   (when (map-elt option :preview)
                                     (define-key option-keymap (kbd "p")
                                                 (agent-shell--elicitation-control
                                                  :state state :request-id request-id :client client
                                                  :field field :value value :operation 'preview)))
                                   (concat
                                    (agent-shell--make-permission-button
                                    ;; The hotkey stays last:
                                    ;; `agent-shell--make-permission-button'
                                    ;; derives the navigatable character
                                    ;; from the trailing "(x)".
                                    :text (if hotkey
                                              (format "%s %s (%s)"
                                                      (agent-shell--elicitation-glyph multi selected)
                                                      (map-elt option :title) hotkey)
                                            (format "%s %s"
                                                    (agent-shell--elicitation-glyph multi selected)
                                                    (map-elt option :title)))
                                    :help (or (map-elt option :preview)
                                              (map-elt option :description)
                                              (map-elt option :title))
                                    :action (funcall option-command option nil)
                                    :keymap option-keymap
                                    :navigatable t
                                    :char hotkey
                                    :focus (if focus-option
                                               (and (equal (car focus-option) (map-elt field :name))
                                                    (equal (cdr focus-option) value))
                                             (and (eq field pending) (= index 0)))
                                    :option (if chord-submits
                                                (format "%s %s, or C-RET to %s and submit"
                                                        verb (map-elt option :title) verb)
                                              (format "%s %s" verb (map-elt option :title))))
                                    (when-let* ((preview (map-elt option :preview)))
                                      (if (member (cons (map-elt field :name) value)
                                                  (map-elt elicitation :previews))
                                          (concat "\n        "
                                                  (replace-regexp-in-string "\n" "\n        " preview t t))
                                        "  (p: preview)")))))
                               (map-elt field :options))
                              (when (memq (map-elt field :kind) '(text boolean number integer))
                                (append
                                 (when answer (list (if (map-elt field :secret) "[secret entered]" (format "%s" answer))))
                                 (agent-shell--make-elicitation-edit-buttons
                                  :state state :request-id request-id :client client :field field
                                  :keymap field-keymap :focus (eq field pending))))
                              (when (map-elt field :custom)
                                (append (agent-shell--make-elicitation-edit-buttons
                                         :state state :request-id request-id :client client :field field
                                         :keymap field-keymap :other t)
                                        (list (agent-shell--make-elicitation-custom-button
                                               :state state
                                               :request-id request-id
                                               :client client
                                               :field field
                                               :answer custom-answer
                                               :completes completes
                                               :keymap field-keymap)))))
                             "\n    "))))
                fields)))
    (format "╭─

    %s %s %s%s


    %s


╰─"
            (propertize agent-shell-elicitation-icon
                        'font-lock-face 'agent-shell-warning)
            (propertize "Question" 'font-lock-face 'agent-shell-permission-title)
            (propertize agent-shell-elicitation-icon
                        'font-lock-face 'agent-shell-warning)
            (if single
                ""
              (format "\n\n\n    %s"
                      (propertize (or message "") 'font-lock-face 'agent-shell-input)))
            (string-join
             (append rows
                     (when (map-elt elicitation :validation)
                       (list "Answer the required field or correct its value before submitting."))
                     (list (string-join
                            (list (agent-shell--make-permission-button
                                   :text "Submit (c)"
                                   :help "Press c to submit"
                                   :action submit-action
                                   :keymap keymap
                                   :navigatable t
                                   :char "c"
                                   :focus (and (null focus-option) (null pending))
                                   :option "submit")
                                  (agent-shell--make-permission-button
                                   :text "Skip (s)"
                                   :help "Press s to skip"
                                   :action skip-action
                                   :keymap keymap
                                   :navigatable t
                                   :char "s"
                                   :option "skip"))
                            " ")))
             "\n\n    "))))

(cl-defun agent-shell--render-elicitation (&key state request-id client)
  "Render the form for elicitation REQUEST-ID in STATE.

CLIENT is the ACP client used to send the response.  Re-rendering reuses
the same block id, so a recorded answer replaces the form in place."
  (when-let* ((elicitation (map-elt (map-elt state :elicitations) request-id)))
    (agent-shell--elicitation-put! elicitation :render-buffers
                                   (delq nil (list (map-elt state :buffer)
                                                   (agent-shell-viewport--buffer :shell-buffer (map-elt state :buffer) :existing-only t))))
    (agent-shell--update-fragment
     :state state
     :namespace-id (map-elt elicitation :namespace-id)
     :block-id (or (map-elt elicitation :block-id) (agent-shell--elicitation-block-id request-id))
     :body (propertize
            (with-current-buffer (map-elt state :buffer)
              (agent-shell--elicitation-mark-control-starts
               (agent-shell--make-elicitation-text
                :elicitation elicitation
                :request-id request-id
                :client client
                :state state)))
            'agent-shell-elicitation-token (map-elt elicitation :token)
            'agent-shell-elicitation-form-start t)
     :expanded t
     :navigation 'never
     :above-last-prompt (not (agent-shell--active-requests-p state)))
    (with-current-buffer (map-elt state :buffer)
      (agent-shell--jump-to-focus-button))
    (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                  :shell-buffer (map-elt state :buffer)
                                  :existing-only t)))
      (with-current-buffer viewport-buffer
        (agent-shell--jump-to-focus-button)))))

(defun agent-shell--elicitation-record-answer (answers field-name value clear)
  "Return ANSWERS with VALUE recorded as the answer to FIELD-NAME.

A nil VALUE drops FIELD-NAME, so the question is unanswered again.

CLEAR names the field FIELD-NAME answers instead of, or lists its key.
A question's options and its Other box are exclusive: both answer it and the
agent prefers the Other box, so recording one drops the other.  A nil
VALUE drops nothing else, so emptying the Other box leaves the option
standing.

For example:

  (agent-shell--elicitation-record-answer
   \\='((question_0_custom . \"OAuth\")) \\='question_0 \"JWT\" \\='question_0_custom)
  => ((question_0 . \"JWT\"))"
  (if value
      (cons (cons field-name value)
            (seq-remove (lambda (entry)
                          (or (eq (car entry) field-name)
                              (memq (car entry) (if (listp clear) clear (list clear))))) answers))
    (map-delete (copy-alist answers) field-name)))

(cl-defun agent-shell--answer-elicitation (&key state request-id client field-name value clear submit focus-option)
  "Commit VALUE for FIELD-NAME in STATE's REQUEST-ID through CLIENT.

CLEAR names the exclusive sibling.  SUBMIT completes only a fully answered,
valid form.  FOCUS-OPTION retains a multi-select editing position."
  (when-let* ((record (agent-shell--elicitation-current state request-id client)))
    (agent-shell--elicitation-put! record :answers
                                   (agent-shell--elicitation-record-answer (map-elt record :answers) field-name value clear))
    (agent-shell--elicitation-put! record :focus-option focus-option)
    (agent-shell--elicitation-put! record :validation nil)
    (if (and submit (not (agent-shell--elicitation-pending-field record)))
        (agent-shell--submit-elicitation :state state :request-id request-id :client client)
      (condition-case nil
          (agent-shell--render-elicitation :state state :request-id request-id :client client)
        (error (agent-shell--finish-elicitation
                :state state :request-id request-id :client client :reason 'failed
                :error (acp-make-error :code -32603 :message "Unable to update ACP form")))))))

(cl-defun agent-shell--submit-elicitation (&key state request-id client)
  "Validate and submit STATE's REQUEST-ID through CLIENT.

Optional unanswered fields are omitted.  Invalid content remains editable."
  (when-let* ((record (agent-shell--elicitation-current state request-id client)))
    (condition-case nil
        (let ((invalid (agent-shell--elicitation-invalid-field record (map-elt record :answers))))
          (if invalid
              (progn
                (agent-shell--elicitation-put! record :validation invalid)
                (agent-shell--elicitation-put! record :focus-option nil)
                (agent-shell--render-elicitation :state state :request-id request-id :client client))
            (agent-shell--finish-elicitation
             :state state :request-id request-id :client client :action "accept"
             :content (map-elt record :answers) :reason 'accepted)))
      (error (agent-shell--finish-elicitation
              :state state :request-id request-id :client client :reason 'failed
              :error (acp-make-error :code -32603 :message "Unable to validate ACP form"))))))

(defun agent-shell--elicitation-current (state request-id client &optional token)
  "Return the pending request in STATE matching REQUEST-ID, CLIENT and TOKEN."
  (when-let* ((record (map-elt (map-elt state :elicitations) request-id))
              ((or (null (map-elt record :status)) (eq (map-elt record :status) 'pending)))
              ((eq client (map-elt state :client)))
              ((or (null (map-elt record :client)) (eq client (map-elt record :client))))
              ((or (null token) (eq token (map-elt record :token))))
              ((or (null (map-elt record :generation))
                   (eq (map-elt record :generation) (map-elt client :connection-generation))))
              )
    record))

(defun agent-shell--elicitation-redact (content sensitive)
  "Copy CONTENT, replacing values at SENSITIVE property identities."
  (mapcar (lambda (entry)
            (cons (car entry) (if (memq (car entry) sensitive)
                                  "[redacted]" (copy-tree (cdr entry) t))))
          content))

(defun agent-shell--elicitation-connected-p (state client)
  "Return non-nil when STATE still owns a usable CLIENT transport."
  (and (eq client (map-elt state :client))
       (or (not (listp client))
           (if (eq (map-elt client :response-sender) #'acp--response-sender)
               (and (processp (map-elt client :process))
                    (process-live-p (map-elt client :process)))
             (not (map-elt client :closed))))))

(defun agent-shell--elicitation-notice (state record text)
  "Persist safe TEXT for RECORD in STATE's transcript."
  (when (buffer-live-p (map-elt state :buffer))
    (agent-shell--update-fragment
     :state state :namespace-id (map-elt record :namespace-id) :block-id (concat (map-elt record :block-id) "-notice")
     :body text :expanded t :navigation 'never
     :above-last-prompt (not (agent-shell--active-requests-p state)))))

(defun agent-shell--elicitation-remove-controls (state record)
  "Remove RECORD controls from every live rendering buffer in STATE."
  (dolist (buffer (seq-uniq
                  (append (list (map-elt state :buffer)
                                (when (buffer-live-p (map-elt state :buffer))
                                  (agent-shell-viewport--buffer
                                   :shell-buffer (map-elt state :buffer) :existing-only t)))
                          (map-elt record :render-buffers))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (condition-case nil
            (agent-shell-ui-delete-fragment
             :namespace-id (map-elt record :namespace-id) :block-id (map-elt record :block-id) :no-undo t)
          (error nil))
        (when-let* ((token (map-elt record :token)))
          (let ((inhibit-read-only t) (buffer-undo-list t) start)
            (while (setq start (text-property-any (point-min) (point-max) 'agent-shell-elicitation-token token))
              (delete-region start (next-single-property-change start 'agent-shell-elicitation-token nil (point-max))))))))))

(cl-defun agent-shell--finish-elicitation (&key state request-id client token action error reason content disconnected)
  "Claim and retire REQUEST-ID in STATE once, optionally sending via CLIENT.

TOKEN rejects stale callbacks.  ACTION or ERROR selects the wire response;
REASON describes the terminal outcome.  CONTENT is validated before claim.
A send failure has unknown delivery and is never retried.
DISCONNECTED retires an old connection without sending a response."
  (when-let* ((record (if disconnected
                          (let ((entry (map-nested-elt state (list :elicitations request-id))))
                            (when (and (eq (map-elt entry :client) client)
                                       (eq (map-elt entry :status) 'pending)) entry))
                        (agent-shell--elicitation-current state request-id client token))))
    (let* ((answers (when (equal action "accept") content))
           (redacted (agent-shell--elicitation-redact answers (map-elt record :sensitive)))
           (response (when (or action error)
                       (acp-make-elicitation-create-response
                        :request-id request-id :action action :content answers :error error)))
           (sent nil)
           (delivery 'not-attempted))
      (when (and (equal action "accept") (agent-shell--elicitation-invalid-field record content))
        (error "Invalid ACP form content"))
      (when (equal action "accept")
        (setq answers (copy-tree answers t))
        (dolist (field (map-elt record :fields))
          (let* ((name (map-elt field :name))
                 (custom (map-nested-elt field '(:custom :name)))
                 (typed (and custom (map-elt answers custom))))
            (when (and (eq (map-elt field :kind) 'boolean)
                       (not (eq (map-elt answers name) t))
                       (not (memq name (map-elt record :required)))
                       (not (map-elt field :has-default)))
              (setq answers (assq-delete-all name answers)))
            (when (and (map-elt field :multi) (map-nested-elt field '(:custom :join-multi))
                       (stringp typed) (not (string-empty-p typed)))
              (let ((combined (string-join (append (map-elt answers name) (list typed)) ", ")))
                (setq answers (assq-delete-all name answers))
                (setcdr (assq custom answers) combined)))))
        (setq redacted (agent-shell--elicitation-redact answers (map-elt record :sensitive))
              response (acp-make-elicitation-create-response
                        :request-id request-id :action action
                        :content (or answers (make-hash-table :test #'equal)))))
      (when response
        (agent-shell--elicitation-put! response :log-result
                                       (map-elt (acp-make-elicitation-create-response
                                                 :request-id request-id :action action
                                                  :content (if (equal action "accept")
                                                               (or redacted (make-hash-table :test #'equal))
                                                             redacted)
                                                  :error error) :result)))
      (agent-shell--elicitation-put! record :status 'finishing)
      (unwind-protect
          (when (and (not disconnected) response (agent-shell--elicitation-connected-p state client))
            (setq delivery 'unknown)
            (condition-case nil
                (progn (acp-send-response :client client :response response)
                       (setq sent t delivery 'sent))
              (error nil)))
        (setq answers nil content nil response nil)
        (agent-shell--elicitation-put! record :answers nil)
        (agent-shell--elicitation-put! record :status 'terminal)
        (agent-shell--delete-elicitation state request-id)
        (agent-shell--elicitation-put! state :elicitation-terminal-ids
                                       (cons (list client (map-elt record :generation)
                                                   (map-elt record :session-id) request-id)
                                             (map-elt state :elicitation-terminal-ids)))
        (condition-case nil
            (agent-shell--elicitation-remove-controls state record)
          (error nil))
        (when (buffer-live-p (map-elt state :buffer))
          (with-current-buffer (map-elt state :buffer)
            (condition-case nil
                (agent-shell--delete-fragment
                 :state state :block-id (or (map-elt record :block-id)
                                            (agent-shell--elicitation-block-id request-id)))
              (error nil))
            (when (and sent action)
              (condition-case nil
                  (agent-shell--elicitation-settle-record state record action redacted)
                (error nil)))
            (when (eq (map-elt state :elicitation-idle-token) (map-elt record :token))
              (condition-case nil (agent-shell--cancel-idle-timer) (error nil))
              (agent-shell--elicitation-put! state :elicitation-idle-token nil))
            (condition-case nil
                (when (and sent action)
                  (agent-shell--emit-event
                   :event 'elicitation-response
                   :data (list (cons :request-id request-id) (cons :tool-call-id (map-elt record :tool-call-id))
                               (cons :action action) (cons :answers redacted))))
              (error nil))
            (condition-case nil
                (agent-shell--emit-event
                 :event 'elicitation-terminal
                 :data (list (cons :request-id request-id) (cons :reason reason) (cons :delivery delivery)))
              (error nil))
            (when (eq delivery 'unknown)
              (condition-case nil
                  (agent-shell--elicitation-notice state record
                                                   "ACP form response delivery is unknown after a send failure; it will not be retried.")
                (error nil)))
            (condition-case nil
                (or (agent-shell-jump-to-latest-permission-button-row) (goto-char (point-max)))
              (error nil))))))))

(cl-defun agent-shell--send-elicitation-response (&key client request-id action content state)
  "Complete STATE's REQUEST-ID with ACTION and CONTENT through CLIENT."
  (agent-shell--finish-elicitation
   :client client :request-id request-id :action action :content content :state state
   :reason (pcase action ("accept" 'accepted) ("decline" 'user-skipped) (_ 'locally-canceled))))

(defun agent-shell--retire-elicitations (state client &optional orderly)
  "Retire CLIENT requests in STATE; send local cancel only when ORDERLY."
  (dolist (entry (copy-sequence (map-elt state :elicitations)))
    (when (eq client (map-elt (cdr entry) :client))
      (agent-shell--finish-elicitation
       :state state :request-id (car entry) :client client
       :token (map-elt (cdr entry) :token) :disconnected (not orderly)
       :action (when orderly "cancel") :reason (if orderly 'locally-canceled 'disconnected))))
  (agent-shell--elicitation-put! state :elicitation-terminal-ids
                                 (seq-remove (lambda (identity) (eq (car identity) client)) (map-elt state :elicitation-terminal-ids))))

(cl-defun agent-shell--on-elicitation-create-request (&key state acp-request)
  "Normalize and display ACP-REQUEST in STATE, terminating early errors once."
  (let* ((request-id (map-elt acp-request 'id))
         (client (map-elt state :client))
         (session (map-nested-elt acp-request '(params sessionId)))
         (generation (and (listp client) (map-elt client :connection-generation)))
         (identity (list client generation session request-id)))
    (when-let* ((active (map-nested-elt state (list :elicitations request-id))))
      (unless (map-elt active :duplicate-noticed)
        (agent-shell--elicitation-put! active :duplicate-noticed t)
        (condition-case nil
            (agent-shell--elicitation-notice state active
                                             "Duplicate ACP form request ignored; the existing form remains active.")
          (error nil))))
    (unless (or (map-contains-key (map-elt state :elicitations) request-id)
                (member identity (map-elt state :elicitation-terminal-ids)))
      (let* ((token (make-symbol "elicitation"))
             (record (list (cons :token token) (cons :client client) (cons :generation generation)
                           (cons :session-id session) (cons :namespace-id (map-elt state :request-count)) (cons :status 'pending) (cons :answers nil)
                           (cons :block-id (format "elicitation-%s" (symbol-name (gensym)))))))
        (agent-shell--save-elicitation state request-id record)
        (condition-case nil
            (let* ((mode (map-nested-elt acp-request '(params mode)))
                   (protocol-error
                    (cond ((not agent-shell-elicitation-capability)
                           (acp-make-error :code -32601 :message "ACP elicitation is disabled"))
                          ((not (equal mode "form"))
                           (acp-make-error :code -32602 :message "Unadvertised ACP elicitation mode"))))
                   (model (if (equal mode "form")
                              (agent-shell--normalize-elicitation (map-nested-elt acp-request '(params requestedSchema)))
                            '((:reason . unsupported-mode))))
                   (reason (or (and protocol-error 'protocol-error) (map-elt model :reason))))
              (if reason
                  (progn
                    (agent-shell--elicitation-notice
                     state record
                     (format "agent-shell cannot display this form because its ACP form support does not support this schema feature (%s; %s)."
                             reason (map-elt record :block-id)))
                    (agent-shell--finish-elicitation :state state :request-id request-id :client client
                                                     :token token :action (unless protocol-error "decline")
                                                     :error protocol-error :reason 'unsupported))
                (map-do (lambda (key value) (agent-shell--elicitation-put! record key value)) model)
                (agent-shell--elicitation-put!
                 record :answers
                 (seq-mapcat
                  (lambda (field)
                    (let* ((name (map-elt field :name))
                           (custom (map-nested-elt field '(:custom :name)))
                           (spec (map-elt (map-elt record :properties) name))
                           (custom-spec (and custom (map-elt (map-elt record :properties) custom)))
                           (custom-default (map-elt custom-spec 'default))
                           ;; Match the provider's Other precedence without
                           ;; retaining defaults in public fields or controls.
                           (custom-answer (and (stringp custom-default)
                                               (not (string-empty-p custom-default))
                                               (cons custom (copy-sequence custom-default))))
                           (answer
                            (cond
                             ((map-contains-key spec 'default)
                              (cons name (copy-tree (map-elt spec 'default) t)))
                             ((and (eq (map-elt field :kind) 'boolean)
                                   (memq name (map-elt record :required)))
                              (cons name :false)))))
                      (delq nil (list (unless (and custom-answer (not (map-elt field :multi))) answer)
                                      custom-answer))))
                  (map-elt record :fields)))
                (agent-shell--elicitation-put! record :properties nil)
                (agent-shell--elicitation-put! record :message (map-nested-elt acp-request '(params message)))
                (agent-shell--elicitation-put! record :tool-call-id (map-nested-elt acp-request '(params toolCallId)))
                (agent-shell--render-elicitation :state state :request-id request-id :client client)
                (when (agent-shell--elicitation-current state request-id client token)
                  (let ((data (list (cons :request-id request-id)
                                    (cons :tool-call-id (map-elt record :tool-call-id))
                                    (cons :fields (copy-tree (map-elt record :fields) t)))))
                    (agent-shell--emit-event :event 'elicitation-request :data data)
                    (when (agent-shell--elicitation-current state request-id client token)
                      (agent-shell--start-idle-timer :event 'elicitation-request :data data)
                      (agent-shell--elicitation-put! state :elicitation-idle-token token)))
                  (agent-shell--elicitation-put! state :last-entry-type "elicitation/create"))))
          (error
           (agent-shell--finish-elicitation
            :state state :request-id request-id :client client :token token :reason 'failed
            :error (acp-make-error :code -32603 :message "Unable to display ACP form"))))))))

(defun agent-shell--elicitation-mark-control-starts (text)
  "Move TEXT's navigation stops to each control's beginning, retaining focus."
  (let ((position 0))
    (while (< position (length text))
      (let ((next (next-single-property-change position 'button text (length text))))
        (when (get-text-property position 'button text)
          (remove-text-properties position next '(agent-shell-permission-button nil) text)
          (put-text-property position (1+ position) 'agent-shell-permission-button t text))
        (setq position next))))
  text)

;;; Navigation

(defun agent-shell-elicitation--move-control (backward)
  "Move to the next form control, or the previous one when BACKWARD."
  (let ((position (point)) found)
    (while (and (not found)
                (setq position
                      (if backward
                          (previous-single-property-change position 'agent-shell-permission-button)
                        (next-single-property-change position 'agent-shell-permission-button))))
      (when (and (get-text-property position 'agent-shell-permission-button)
                 (get-text-property position 'agent-shell-elicitation-token))
        (setq found position)))
    (when found (deactivate-mark) (goto-char found) found)))

(defun agent-shell-elicitation-next-field ()
  "Move to the next interactive form control."
  (interactive)
  (agent-shell-elicitation--move-control nil))

(defun agent-shell-elicitation-previous-field ()
  "Move to the previous interactive form control."
  (interactive)
  (agent-shell-elicitation--move-control t))

(defun agent-shell-elicitation-jump-to-latest-form ()
  "Reveal the beginning of the latest pending form in every window."
  (interactive)
  (when-let* ((position (save-excursion
                         (goto-char (point-max))
                         (let ((match (text-property-search-backward
                                       'agent-shell-elicitation-form-start t t)))
                           (when match (prop-match-beginning match))))))
    (goto-char position)
    (let* ((token (get-text-property position 'agent-shell-elicitation-token))
           (record (seq-find
                    (lambda (entry) (eq token (map-elt (cdr entry) :token)))
                    (map-elt (agent-shell--state) :elicitations)))
           (form (cdr record))
           (fields (map-elt form :fields))
           (heading (if (= (length fields) 1)
                        (agent-shell--elicitation-field-label (car fields) (map-elt form :message))
                      (map-elt form :message)))
           (end (or (next-single-property-change position 'agent-shell-elicitation-token)
                    (point-max))))
      (when (and heading (search-forward heading end t))
        (goto-char (match-beginning 0))))
    (deactivate-mark)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window (point)))
    t))

(defun agent-shell-elicitation-interrupt ()
  "Cancel pending forms through the shell's guarded completion path."
  (interactive)
  (let* ((shell (if (derived-mode-p 'agent-shell-mode) (current-buffer)
                  (agent-shell-viewport--shell-buffer)))
         (state (and (buffer-live-p shell)
                     (with-current-buffer shell (agent-shell--state)))))
    (when state
      (dolist (entry (copy-sequence (map-elt state :elicitations)))
        (agent-shell--finish-elicitation
         :state state :request-id (car entry) :client (map-elt state :client)
         :token (map-elt (cdr entry) :token) :action "cancel" :reason 'locally-canceled)))))

(defun agent-shell-elicitation-act ()
  "Activate the form control at point."
  (interactive)
  (when-let* ((map (get-text-property (point) 'keymap))
              (action (lookup-key map (kbd "RET")))
              ((commandp action)))
    (call-interactively action)))

;;; Tool-call integration

(defun agent-shell--elicitation-settle-record (state record action redacted)
  "Leave a safe summary for RECORD in STATE after ACTION with REDACTED answers."
  (let ((tool-call (map-elt (map-elt state :tool-calls) (map-elt record :tool-call-id))))
    (unless (agent-shell-elicitation--questionnaire-p (map-elt tool-call :raw-input))
      (agent-shell--update-fragment
       :state state :namespace-id (map-elt record :namespace-id)
       :block-id (concat (map-elt record :block-id) "-settled")
       :body (string-join
              (append (list (or (map-elt record :message) "Question")
                            (pcase action ("accept" "Answered") ("decline" "Skipped") (_ "Canceled")))
                      (mapcar
                       (lambda (entry)
                         (let ((field (seq-find
                                       (lambda (field)
                                         (or (eq (car entry) (map-elt field :name))
                                             (eq (car entry) (map-nested-elt field '(:custom :name)))))
                                       (map-elt record :fields))))
                           (format "%s: %s"
                                   (or (map-elt field :title) (map-elt field :name) (car entry))
                                   (cdr entry))))
                       redacted))
              "\n")
       :expanded t :navigation 'never
       :above-last-prompt (not (agent-shell--active-requests-p state))))))

(cl-defun agent-shell-elicitation--tool-call-records-p (&key state id)
  "Return non-nil when ID's tool call in STATE records its questionnaire."
  (when-let* ((tool-call-id (map-nested-elt state (list :elicitations id :tool-call-id))))
    (agent-shell-elicitation--questionnaire-p
     (map-nested-elt state (list :tool-calls tool-call-id :raw-input)))))

(defconst agent-shell-elicitation--option-meta-key '_claude/askUserQuestionOption
  "ACP option metadata key containing preview text.")

(defun agent-shell-elicitation--option-preview (option)
  "Return validated preview text attached to OPTION, or nil."
  (let* ((meta (map-elt option '_meta))
         (preview-meta (map-elt meta agent-shell-elicitation--option-meta-key)))
    (when (map-contains-key meta agent-shell-elicitation--option-meta-key)
      (agent-shell--elicitation-object preview-meta '(preview))
      (unless (stringp (map-elt preview-meta 'preview))
        (agent-shell--elicitation-reject 'malformed-preview))
      (map-elt preview-meta 'preview))))

(defconst agent-shell-elicitation--advertised-modes '("form")
  "ACP elicitation modes supported by the form renderer.")

(defun agent-shell-elicitation--questionnaire-p (raw-input)
  "Return non-nil for RAW-INPUT with a nonempty questionnaire."
  (let ((questions (and (listp raw-input) (map-elt raw-input 'questions))))
    (and (or (vectorp questions) (listp questions))
         (not (seq-empty-p questions))
         (seq-every-p (lambda (question)
                        (and (listp question)
                             (stringp (map-elt question 'question))))
                      questions))))

(cl-defun agent-shell-elicitation--pending-for-tool-call-p (&key state tool-call-id)
  "Return the pending form in STATE associated with TOOL-CALL-ID."
  (when tool-call-id
    (seq-find (lambda (entry)
                (let ((record (cdr entry)))
                  (and (eq (map-elt record :status) 'pending)
                       (equal (map-elt record :tool-call-id) tool-call-id)
                       (agent-shell--elicitation-current
                        state (car entry) (map-elt state :client)))))
              (map-elt state :elicitations))))

(cl-defun agent-shell-elicitation--clear (&key state)
  "Drop settled forms from STATE without orphaning pending requests.

The normal completion path already removes settled forms.  Preserve pending
records and their active requests if reset runs before cancellation.  Keep
terminal request identities until connection retirement to reject replays."
  (agent-shell--elicitation-put!
   state :elicitations
   (seq-filter (lambda (entry)
                 (memq (map-elt (cdr entry) :status) '(nil pending finishing)))
               (map-elt state :elicitations)))
  (agent-shell--elicitation-put!
   state :active-requests
   (seq-remove
    (lambda (request)
      (and (equal (map-elt request :method) "elicitation/create")
           (not (map-contains-key (map-elt state :elicitations)
                                  (map-elt request :request-id)))))
    (map-elt state :active-requests))))

(provide 'agent-shell-elicitation)
;;; agent-shell-elicitation.el ends here
