;; There are two types of transitions:
;; Direct:
;;  Just change to another page, without performing any action (mutation, etc)
;;  before hand.
;; Action:
;;  Perform some action (e.g. mutate the backend, etc.), then send a page.
;; The naming convention is `transition-foo' is a direct transition to the page
;; named `page-foo'. `transition-action' is an action transition that performs
;; the action `action', then sends a page.

(module transitions mzscheme
  (require (lib "unitsig.ss")
           (lib "servlet.ss" "web-server")
           (prefix backend- "backend.ss")
           "scheduler.ss"
           "sigs.ss"
           "data.ss")

  (provide transitions@)

  (define transitions@
    (unit/sig transitions^
      (import pages^)

      ;; Direct transition to the login page.
      (define transition-login
        (lambda (req)
          (send/suspend/callback (page-login))))

      ;; Action transition to the logged-in page.
      ;; Action: check that the username and password pair are correct.
      ;; If the username and password match, send page-logged-in; otherwise
      ;; send page-login with a message.
      (define transition-log-in
        (lambda (req)
          (let* ((rb (request-bindings req))
                 (username (extract-binding/single 'username rb))
                 (password (extract-binding/single 'password rb))
                 (valid? (schedule
                           (lambda ()
                             (backend-valid-username-password?
                               username password)))))
            (if valid?
              (send/suspend/callback
                (page-logged-in (make-session username)
                                "Congrats, you've logged in."))
              (send/suspend/callback
                (page-login "Invalid username or password."))))))

      ;; Direct transition to the login page. This clears the previously-
      ;; stored continuations first.
      (define transition-log-out
        (lambda (req)
          (send/forward/callback (page-login))))

      ;; Direct transition to the change password page.
      (define (transition-change-password session)
        (lambda (req)
          (send/suspend/callback (page-change-password session))))

      ;; Action transition to the change password page.
      ;; Action: Change the password.
      ;; Send the change password page with a message explaining whether it
      ;; worked and, if it failed, why.
      (define (transition-update-password session)
        (lambda (req)
          (let* ((rb (request-bindings req))
                 (username (session-username session))
                 (old-password (extract-binding/single 'old-password rb))
                 (valid? (schedule
                           (lambda ()
                             (backend-valid-username-password?
                               username old-password))))
                 (new-password1 (extract-binding/single 'new-password1 rb))
                 (new-password2 (extract-binding/single 'new-password2 rb)))
            (if valid?
              (if (string=? new-password1 new-password2)
                (begin
                  (schedule
                    (lambda ()
                      (backend-update-password! username new-password1)))
                  (send/suspend/callback (page-logged-in
                                           session
                                           "Your password has been changed.")))
                (send/suspend/callback (page-change-password
                                         session
                                         "New passwords do not match.")))
              (send/suspend/callback (page-change-password
                                       session
                                       "Incorrect old password."))))))



      )))
