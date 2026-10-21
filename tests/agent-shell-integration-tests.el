;;; agent-shell-integration-tests.el --- Combined protocol contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises initialization and incoming forms across the merged implementation.

;;; Code:

(require 'ert)
(require 'agent-shell)
(require 'acp-fakes)

(declare-function agent-shell-fakes--settle "agent-shell-fakes")
(declare-function agent-shell-elicitation-tests--button "agent-shell-elicitation-tests")
(declare-function agent-shell-elicitation-tests--call "agent-shell-elicitation-tests")

(ert-deftest agent-shell-integration-elicitation-default-test ()
  "Advertise form support through the default initialization request."
  (with-temp-buffer
    (setq-local agent-shell--state (agent-shell--make-state))
    (setf (map-elt agent-shell--state :buffer) (current-buffer))
    (let ((agent-shell-elicitation-capability
           (default-value 'agent-shell-elicitation-capability))
          request)
      (should agent-shell-elicitation-capability)
      (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment)
                 (lambda (&rest _)))
                ((symbol-function 'agent-shell--send-request)
                 (lambda (&rest arguments)
                   (setq request (plist-get arguments :request)))))
        (agent-shell--initiate-handshake
         :shell-buffer (current-buffer) :on-initiated #'ignore))
      (should request)
      (should (equal (map-elt request :method) "initialize"))
      (should (assq 'form
                    (map-nested-elt request '(:params clientCapabilities elicitation)))))))

(ert-deftest agent-shell-integration-elicitation-dispatch-test ()
  "Route upstream and native provider forms once, then remove their controls."
  (let* ((directory (file-name-directory
                     (symbol-file 'agent-shell-elicitation-tests--call 'defun)))
         (requests
          (list '((id . 101) (method . "elicitation/create")
                  (params . ((mode . "form") (sessionId . "fake-session-id")
                             (message . "Choose")
                             (requestedSchema .
                              ((type . "object") (required . ["choice"])
                               (properties .
                                ((choice . ((type . "string")
                                            (enum . ["Alpha" "Beta"])))))))))))))
    (dolist (provider '("claude" "codex"))
      (push (with-temp-buffer
              (insert-file-contents
               (expand-file-name (format "phase2-native-%s.json" provider) directory))
              (map-elt (json-parse-buffer :object-type 'alist :false-object :false)
                       'request))
            requests))
    (dolist (request requests)
      (let ((id (map-elt request 'id)))
        (agent-shell-elicitation-tests--call
         nil
         (lambda (state)
           (let* ((client (map-elt state :client))
                  (record (map-nested-elt state (list :elicitations id))))
             (should record)
             (dolist (field (map-elt record :fields))
               (funcall (agent-shell--make-elicitation-answer-command
                         :state state :request-id id :client client :field field
                         :value "Alpha")))
             (funcall (agent-shell-elicitation-tests--button "Submit"))
             (agent-shell-fakes--settle)
             (let ((responses (seq-filter
                               (lambda (response)
                                 (equal (map-elt response :request-id) id))
                               (acp-fakes-sent-responses client))))
               (should (= 1 (length responses)))
               (should (equal (map-nested-elt (car responses) '(:result action))
                              "accept"))
               (let ((content (map-nested-elt (car responses) '(:result content))))
                 (should (= 1 (length content)))
                 (should (equal (cdar content) "Alpha"))))
             (should-not (map-elt state :elicitations))
             (should-not (map-elt record :answers))
             (should-not (text-property-any
                          (point-min) (point-max) 'agent-shell-elicitation-token
                          (map-elt record :token)))
             (agent-shell-fakes--settle)
             (should (= 1 (length (acp-fakes-sent-responses client))))))
         request)))))

(provide 'agent-shell-integration-tests)
;;; agent-shell-integration-tests.el ends here
