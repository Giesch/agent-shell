;;; agent-shell-permission-lifecycle-tests.el --- Permission lifecycle tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)

(defmacro agent-shell-permission-lifecycle--with-request (&rest body)
  "Run BODY with a pending request and captured permission callback.
Bind state, client, callback, responses, fragments, events, diff-kills,
idle-cancels, transport-hook, and diff-hook for deterministic assertions."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((client (list (cons :name "client")))
            (state (list (cons :buffer (current-buffer))
                         (cons :client client)
                         (cons :tool-calls
                               (list (cons "tool-1"
                                           (list (cons :permission-request-id "request-1")
                                                 (cons :status "pending")
                                                 (cons :title "Keep this tool")
                                                 (cons :diff-buffer 'tracked-diff)))))))
            (callback (lambda (&optional cancel)
                        (agent-shell--send-permission-response
                         :client client :request-id "request-1"
                         :option-id (unless cancel "allow-once") :cancelled cancel
                         :state state :tool-call-id "tool-1")))
            responses fragments events diff-kills idle-cancels
            (transport-hook nil) (diff-hook nil))
       (cl-letf (((symbol-function 'acp-make-session-request-permission-response)
                  (lambda (&rest args) args))
                 ((symbol-function 'acp-send-response)
                  (lambda (&rest args)
                    (push args responses)
                    (when transport-hook (funcall transport-hook))))
                 ((symbol-function 'agent-shell-diff-kill-buffer)
                  (lambda (buffer)
                    (push buffer diff-kills)
                    (when diff-hook (funcall diff-hook))))
                 ((symbol-function 'agent-shell--delete-fragment)
                  (lambda (&rest args) (push args fragments)))
                 ((symbol-function 'agent-shell--emit-event)
                  (lambda (&rest args) (push args events)))
                 ((symbol-function 'agent-shell--cancel-idle-timer)
                  (lambda () (push t idle-cancels)))
                 ((symbol-function 'agent-shell-jump-to-latest-permission-button-row)
                  #'ignore)
                 ((symbol-function 'agent-shell-viewport--buffer) #'ignore))
         ,@body))))

(ert-deftest agent-shell-permission-lifecycle-callback-twice-test ()
  "Selection and cancellation each send and clean up exactly once."
  (dolist (cancel '(nil t))
    (agent-shell-permission-lifecycle--with-request
      (funcall callback cancel)
      (funcall callback cancel)
      (should (equal responses
                     (list (list :client client :response
                                 (list :request-id "request-1" :cancelled cancel
                                       :option-id (unless cancel "allow-once"))))))
      (should (equal fragments
                     (list (list :state state :block-id "permission-tool-1"))))
      (should (equal events
                     (list (list :event 'permission-response
                                 :data (list (cons :request-id "request-1")
                                             (cons :tool-call-id "tool-1")
                                             (cons :option-id (unless cancel "allow-once"))
                                             (cons :cancelled cancel))))))
      (should (equal diff-kills '(tracked-diff)))
      (should (equal idle-cancels '(t)))
      (should-not (map-nested-elt state '(:tool-calls "tool-1" :permission-request-id)))
      (should (equal (map-nested-elt state '(:tool-calls "tool-1" :title))
                     "Keep this tool"))
      (should (equal (map-nested-elt state '(:tool-calls "tool-1" :status))
                     "pending")))))

(ert-deftest agent-shell-permission-lifecycle-reentrant-callback-test ()
  "Claim the request before transport and diff cleanup can reenter."
  (dolist (source '(transport diff))
    (agent-shell-permission-lifecycle--with-request
      (let ((reenter (lambda ()
                       (should-not (map-nested-elt
                                    state '(:tool-calls "tool-1" :permission-request-id)))
                       (funcall callback t))))
        (if (eq source 'transport)
            (setq transport-hook reenter)
          (setq diff-hook reenter)))
      (funcall callback)
      (should (= (length responses) 1))
      (should (= (length fragments) 1))
      (should (= (length events) 1))
      (should (equal diff-kills '(tracked-diff)))
      (should (equal idle-cancels '(t))))))

(ert-deftest agent-shell-permission-lifecycle-stale-request-test ()
  "An old callback cannot resolve a replacement or absent request."
  (dolist (pending '("request-2" nil))
    (agent-shell-permission-lifecycle--with-request
      (let ((tool (map-nested-elt state '(:tool-calls "tool-1"))))
        (map-put! tool :permission-request-id pending))
      (funcall callback)
      (should-not responses)
      (should-not fragments)
      (should-not events)
      (should-not diff-kills)
      (should-not idle-cancels)
      (should (equal (map-nested-elt state '(:tool-calls "tool-1" :permission-request-id))
                     pending)))))

(ert-deftest agent-shell-permission-lifecycle-stale-client-test ()
  "Reject a captured client after replacement, even if structurally equal."
  (agent-shell-permission-lifecycle--with-request
    (map-put! state :client (copy-tree client))
    (should (equal client (map-elt state :client)))
    (should-not (eq client (map-elt state :client)))
    (funcall callback)
    (should-not responses)
    (should-not fragments)
    (should-not events)
    (should-not diff-kills)
    (should-not idle-cancels)
    (should (equal (map-nested-elt state '(:tool-calls "tool-1" :permission-request-id))
                   "request-1"))))

(ert-deftest agent-shell-permission-lifecycle-missing-client-test ()
  "A missing client is not a valid permission-response destination."
  (agent-shell-permission-lifecycle--with-request
    (setq client nil)
    (map-put! state :client nil)
    (funcall callback)
    (should-not responses)
    (should-not fragments)
    (should-not events)
    (should-not diff-kills)
    (should-not idle-cancels)
    (should (equal (map-nested-elt state '(:tool-calls "tool-1" :permission-request-id))
                   "request-1"))))

(ert-deftest agent-shell-permission-lifecycle-transport-error-test ()
  "Release resources on uncertain delivery, without success or retry."
  (agent-shell-permission-lifecycle--with-request
    (let ((diff-buffer (generate-new-buffer " *permission-diff*"))
          notices)
      (unwind-protect
          (progn
            (let ((tool (map-nested-elt state '(:tool-calls "tool-1"))))
              (map-put! tool :diff-buffer diff-buffer))
            ;; The fixture's diff cleanup records the call but leaves the
            ;; buffer alive, exercising the terminal buffer-kill fallback.
            (setq transport-hook (lambda () (error "Secret transport detail")))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) notices))))
              (should (equal (should-error (funcall callback) :type 'error)
                             '(error "Secret transport detail"))))
            (should (equal notices
                           '("Permission response delivery is unknown; the request will not be retried.")))
            (should-not (buffer-live-p diff-buffer))
            (should-not (map-nested-elt state '(:tool-calls "tool-1" :permission-request-id)))
            (should (equal (map-nested-elt state '(:tool-calls "tool-1" :title))
                           "Keep this tool"))
            (should (equal fragments
                           (list (list :state state :block-id "permission-tool-1"))))
            (should (equal diff-kills (list diff-buffer)))
            (should (equal idle-cancels '(t)))
            (should-not events)
            (setq transport-hook nil)
            (funcall callback)
            (should (= (length responses) 1))
            (should (= (length fragments) 1))
            (should (= (length diff-kills) 1))
            (should (= (length idle-cancels) 1)))
        (when (buffer-live-p diff-buffer)
          (kill-buffer diff-buffer))))))

(ert-deftest agent-shell-permission-lifecycle-cleanup-errors-test ()
  "Cleanup errors cannot block other cleanup or mask a transport error."
  (dolist (send-fails '(nil t))
    (agent-shell-permission-lifecycle--with-request
      (let ((diff-buffer (generate-new-buffer " *permission-diff-error*")))
        (unwind-protect
            (progn
              (let ((tool (map-nested-elt state '(:tool-calls "tool-1"))))
                (map-put! tool :diff-buffer diff-buffer))
              (with-current-buffer diff-buffer
                (setq-local agent-shell-diff--on-exit
                            (lambda () (ert-fail "Permission diff callback ran")))
                (add-hook 'kill-buffer-hook
                          (lambda () (error "Diff kill hook failed")) nil t))
              (setq diff-hook (lambda () (error "Diff cleanup failed")))
              (when send-fails
                (setq transport-hook (lambda () (error "Transport failed"))))
              (cl-letf (((symbol-function 'agent-shell--delete-fragment)
                         (lambda (&rest args)
                           (push args fragments)
                           (error "Control cleanup failed"))))
                (should (equal (should-error (funcall callback) :type 'error)
                               (if send-fails
                                   '(error "Transport failed")
                                 '(error "Diff cleanup failed")))))
              (should-not (buffer-live-p diff-buffer))
              (should (= (length responses) 1))
              (should (= (length diff-kills) 1))
              (should (= (length fragments) 1))
              (should (equal idle-cancels '(t)))
              (should (= (length events) (if send-fails 0 1)))
              (funcall callback)
              (should (= (length responses) 1))
              (should (= (length idle-cancels) 1)))
          (when (buffer-live-p diff-buffer)
            (with-current-buffer diff-buffer
              (let ((kill-buffer-hook nil))
                (kill-buffer diff-buffer)))))))))

(provide 'agent-shell-permission-lifecycle-tests)
;;; agent-shell-permission-lifecycle-tests.el ends here
