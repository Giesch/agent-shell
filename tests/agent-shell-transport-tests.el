;;; agent-shell-transport-tests.el --- Transport boundary tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Integration tests that drive real agent-shell state, comint input
;; handling, and request construction against a fake ACP transport, and
;; assert on the traffic the client actually sent.

;;; Code:

(require 'ert)
(require 'map)
(require 'seq)
(require 'agent-shell)

(declare-function acp-fakes-barrier "acp-fakes")
(declare-function acp-fakes-skip-barrier "acp-fakes")
(declare-function acp-fakes-sent-requests "acp-fakes")
(declare-function acp-fakes-sent-responses "acp-fakes")
(declare-function acp-fakes-sent-notifications "acp-fakes")
(declare-function acp-fakes-exhausted-p "acp-fakes")
(declare-function agent-shell-fakes-start-agent "agent-shell-fakes")
(declare-function agent-shell-fakes--settle "agent-shell-fakes")

(defun agent-shell-transport-tests--traffic (file)
  "Read traffic FILE from the tests directory."
  (acp-traffic-read-file
   (expand-file-name file (file-name-directory
                           (symbol-file 'agent-shell-fakes-start-agent
                                        'defun)))))

(defun agent-shell-transport-tests--prompts (client session-id)
  "Return CLIENT's sent \"session/prompt\" requests for SESSION-ID."
  (seq-filter (lambda (request)
                (and (equal (map-elt request :method) "session/prompt")
                     (equal (map-nested-elt request '(:params sessionId))
                            session-id)))
              (acp-fakes-sent-requests client)))

(cl-defun agent-shell-transport-tests--restore-and-submit
    (&key traffic-file session-id insert draft history)
  "Restore a session while editing the live input, then submit for real.

TRAFFIC-FILE is a capture whose `session/load' completion is held at a
recorded \"test/hold\" barrier.  SESSION-ID is the restored session.
INSERT is `typed', `context', or `none'.  DRAFT is the text placed at
the live input during the hold window; with INSERT `none' it is the
fresh prompt typed after restoration.  HISTORY lists strings that the
replayed turns must render and the outgoing prompt must not contain."
  (let ((agent-shell-session-restore-verbosity 'full)
        (buffer nil)
        (client nil))
    (unwind-protect
        (progn
          (setq buffer (agent-shell-fakes-start-agent
                        (agent-shell-transport-tests--traffic traffic-file)))
          (agent-shell-fakes--settle)
          (let ((state (buffer-local-value 'agent-shell--state buffer)))
            (setq client (map-elt state :client))
            ;; The load is in flight and its completion is held.
            (should (equal (map-nested-elt (acp-fakes-barrier client)
                                           '(:object method))
                           "test/hold"))
            (should (map-elt state :pending-restore)))
          (pcase insert
            ('typed (with-current-buffer buffer
                      (goto-char (point-max))
                      (insert draft)))
            ('context (agent-shell--insert-to-shell-buffer
                       :shell-buffer buffer
                       :text draft
                       :no-focus t))
            ('none nil))
          (acp-fakes-skip-barrier client)
          (agent-shell-fakes--settle)
          (should-not (map-elt (buffer-local-value 'agent-shell--state buffer)
                               :pending-restore))
          (let ((submitted (if (eq insert 'none)
                               draft
                             (concat draft "-edited"))))
            (with-current-buffer buffer
              (let* ((mark (marker-position
                            (process-mark (shell-maker--process))))
                     (above (buffer-substring-no-properties (point-min) mark))
                     (below (buffer-substring-no-properties mark (point-max))))
                (dolist (chunk history)
                  (should (string-search chunk above)))
                (unless (eq insert 'none)
                  (should-not (string-search draft above))
                  (should (string-search draft below))))
              ;; The live input stays editable after the replay.
              (goto-char (point-max))
              (insert (if (eq insert 'none) draft "-edited"))
              (agent-shell-submit))
            (agent-shell-fakes--settle)
            (let ((prompts (agent-shell-transport-tests--prompts
                            client session-id)))
              (should (= 1 (length prompts)))
              (let ((serialized (json-serialize
                                 (map-elt (car prompts) :params))))
                (should (string-search submitted serialized))
                (should-not (string-search "Fake> " serialized))
                (dolist (chunk history)
                  (should-not (string-search chunk serialized)))))
            (should (acp-fakes-exhausted-p client))
            ;; Deferred callbacks must not send a duplicate prompt.
            (agent-shell-fakes--settle)
            (should (= 1 (length (agent-shell-transport-tests--prompts
                                  client session-id))))))
      (when client
        (ignore-errors (acp-shutdown :client client)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (ignore-errors (agent-shell--cancel-idle-timer)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(ert-deftest agent-shell-transport-typed-draft-agent-turn-restore-submit-test ()
  "A draft typed during restore survives replay ending in an agent turn."
  (agent-shell-transport-tests--restore-and-submit
   :traffic-file "session-load-agent-turn.traffic"
   :session-id "restore-agent-1"
   :insert 'typed
   :draft "unique-typed-draft-a1"
   :history '("What is the restored question?" "The restored answer.")))

(ert-deftest agent-shell-transport-typed-draft-user-turn-restore-submit-test ()
  "A draft typed during restore survives replay ending in a user turn."
  (agent-shell-transport-tests--restore-and-submit
   :traffic-file "session-load-user-turn.traffic"
   :session-id "restore-user-1"
   :insert 'typed
   :draft "unique-typed-draft-u1"
   :history '("restored first question"
              "restored first answer"
              "restored interrupted question")))

(ert-deftest agent-shell-transport-inserted-context-agent-turn-restore-submit-test ()
  "Context inserted during restore survives replay ending in an agent turn."
  (agent-shell-transport-tests--restore-and-submit
   :traffic-file "session-load-agent-turn.traffic"
   :session-id "restore-agent-1"
   :insert 'context
   :draft "unique-context-a1"
   :history '("What is the restored question?" "The restored answer.")))

(ert-deftest agent-shell-transport-inserted-context-user-turn-restore-submit-test ()
  "Context inserted during restore survives replay ending in a user turn."
  (agent-shell-transport-tests--restore-and-submit
   :traffic-file "session-load-user-turn.traffic"
   :session-id "restore-user-1"
   :insert 'context
   :draft "unique-context-u1"
   :history '("restored first question"
              "restored first answer"
              "restored interrupted question")))

(ert-deftest agent-shell-transport-empty-draft-restore-then-prompt-test ()
  "An empty input during restore accepts an ordinary prompt afterwards."
  (agent-shell-transport-tests--restore-and-submit
   :traffic-file "session-load-agent-turn.traffic"
   :session-id "restore-agent-1"
   :insert 'none
   :draft "fresh-prompt-after-restore"
   :history '("What is the restored question?" "The restored answer.")))

(defun agent-shell-transport-tests--permission-responses (client)
  "Return CLIENT's sent responses answering permission request 10."
  (seq-filter (lambda (response)
                (equal (map-elt response :request-id) 10))
              (acp-fakes-sent-responses client)))

(cl-defun agent-shell-transport-tests--permission-case
    (&key button-label key expected-option-id expect-interrupt)
  "Answer a rendered permission request and assert on the sent response.

Renders the dialog from the shared-kind capture, then activates it: with
KEY, through the effective hotkey command at a permission button; without
KEY, through the RET binding of the button whose text starts with
BUTTON-LABEL.  EXPECTED-OPTION-ID is the advertised id the recorded
response must carry.  EXPECT-INTERRUPT asserts a \"session/cancel\"
notification follows the rejection."
  (let ((agent-shell-permission-responder-function nil)
        (agent-shell-permission-option-chars '(("auto" . "a")))
        (buffer nil)
        (client nil))
    (unwind-protect
        (progn
          (setq buffer (agent-shell-fakes-start-agent
                        (agent-shell-transport-tests--traffic
                         "permission-shared-kind.traffic")))
          (agent-shell-fakes--settle)
          (setq client (map-elt (buffer-local-value 'agent-shell--state buffer)
                                :client))
          (should (equal (map-nested-elt (acp-fakes-barrier client)
                                         '(:object method))
                         "session/prompt"))
          (with-current-buffer buffer
            (shell-maker-submit :input "please edit the file"))
          (agent-shell-fakes--settle)
          ;; The replay holds at the recorded permission response with
          ;; the dialog on screen.
          (should (equal (map-nested-elt (acp-fakes-barrier client)
                                         '(:object id))
                         10))
          (should-not (agent-shell-transport-tests--permission-responses
                       client))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward (concat "[ " button-label))
            (let* ((keymap (get-text-property (1- (point)) 'keymap))
                   (command (lookup-key keymap
                                        (if key (kbd key) (kbd "RET")))))
              (should (commandp command))
              (call-interactively command)))
          (agent-shell-fakes--settle)
          ;; The turn is still in flight, held at the second barrier.
          (should (equal (map-nested-elt (acp-fakes-barrier client)
                                         '(:object method))
                         "test/hold"))
          (let ((responses (agent-shell-transport-tests--permission-responses
                            client)))
            (should (= 1 (length responses)))
            (let ((serialized (json-serialize
                               (map-elt (car responses) :result))))
              (should (string-search "\"outcome\":\"selected\"" serialized))
              (should (string-search (format "\"optionId\":%S" expected-option-id)
                                     serialized))))
          ;; The request is retired while the tool call lives on, and no
          ;; interactive controls are left in the buffer.
          (let ((tool-call (map-nested-elt
                            (buffer-local-value 'agent-shell--state buffer)
                            '(:tool-calls "tool-call-77"))))
            (should tool-call)
            (should-not (map-elt tool-call :permission-request-id)))
          (with-current-buffer buffer
            (should-not (text-property-not-all (point-min) (point-max)
                                               'agent-shell-permission-button
                                               nil)))
          (if expect-interrupt
              (should (seq-find (lambda (notification)
                                  (equal (map-elt notification :method)
                                         "session/cancel"))
                                (acp-fakes-sent-notifications client)))
            (should-not (acp-fakes-sent-notifications client)))
          ;; Completing the turn must not produce another response.
          (acp-fakes-skip-barrier client)
          (agent-shell-fakes--settle)
          (should (acp-fakes-exhausted-p client))
          (should (= 1 (length (agent-shell-transport-tests--permission-responses
                                client)))))
      (when client
        (ignore-errors (acp-shutdown :client client)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (ignore-errors (agent-shell--cancel-idle-timer)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(ert-deftest agent-shell-transport-permission-hotkey-auto-response-test ()
  "The configured hotkey answers with the advertised \"auto\" option id."
  (agent-shell-transport-tests--permission-case
   :button-label "Always allow (a)"
   :key "a"
   :expected-option-id "auto"))

(ert-deftest agent-shell-transport-permission-button-shared-kind-response-test ()
  "Button activation answers with the second option of a shared kind."
  (agent-shell-transport-tests--permission-case
   :button-label "Always allow edits"
   :expected-option-id "acceptEdits"))

(ert-deftest agent-shell-transport-permission-reject-response-test ()
  "Rejection answers with the advertised id; the interrupt stays separate."
  (agent-shell-transport-tests--permission-case
   :button-label "Reject"
   :expected-option-id "reject-once"
   :expect-interrupt t))

(provide 'agent-shell-transport-tests)
;;; agent-shell-transport-tests.el ends here
