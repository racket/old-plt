; vim:lispwords+=,define-transition,define-action-transition,let/bindings
;; There are two types of transitions:
;; Direct:
;;  Just change to another page, without performing any action (mutation, etc)
;;  beforehand.
;; Action:
;;  Perform some action (e.g. mutate the backend, etc.), then send a page.
;; The naming convention is: `transition-foo' is a direct transition to the
;; page named `page-foo'. `transition-action' is an action transition that
;; performs the action described by `action', then sends a page.

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
      (define-transition transition-login
        (send/suspend/callback (page-login)))

      ;; Action transition to the logged-in page.
      ;; ACTION: check that the username and password pair are correct.
      ;; If the username and password match, send page-courses; otherwise
      ;; send page-login with a message.
      (define-action-transition transition-log-in (username password)
        (cond
          ( (not (schedule (lambda () (backend-valid-username-password?
                                        username password))))
            (send/suspend/callback
              (page-login "Invalid username or password.")) )
          ( else 
            (send/suspend/callback
              (page-courses (make-session username #f)
                            (schedule (lambda () (backend-courses username)))
                            "Congrats, you've logged in.")) )))

      ;; Direct transition to the login page. This clears the previously-
      ;; stored continuations first.
      (define-transition transition-log-out
        (send/forward/callback (page-login)))

      ;; Direct transition to the change password page.
      (define-transition (transition-change-password session) 
        (send/suspend/callback (page-change-password session)))

      ;; Action transition to the change password page.
      ;; ACTION: Change the password.
      ;; Send the change password page with a message explaining whether it
      ;; worked and, if it failed, why.
      (define-action-transition (transition-update-password session)
        (old-password new-password1 new-password2)
        (let ((username (session-username session)))
          (cond
            ( (not (string=? new-password1 new-password2))
              (send/suspend/callback
                (page-change-password
                  session "New passwords do not match.")) )
            ( (not (schedule (lambda ()
                               (backend-valid-username-password?
                                 username old-password))))
              (send/suspend/callback
                (page-change-password
                  session "Incorrect old password.")) )
            ( else  
              (schedule-transaction
                (lambda () (backend-update-password! username new-password1)))
              (main session "Your password has been changed.")
               ))))

      ;; Direction transition to the user creation page.
      (define-transition transition-create-user
        (send/suspend/callback (page-create-user)))

      ;; Action transition to the logged-in page.
      ;; ACTION: create a user with a username and password. Check the
      ;; Northeastern ID against the name in a list of ID/name pairs; if they
      ;; match, check the passwords; if those match, and the username is not
      ;; taken, and the user is not already created, then create the account
      ;; and transition to the logged-in page. Otherwise, transition to the
      ;; user creation page with an error explaining which step failed.
      (define-action-transition transition-create-a-user
        (name neu-id username password1 password2)
        (let ((neu-id (string->number neu-id)))
          (cond
            ( (not (string=? password1 password2))
              (send/suspend/callback
                (page-create-user "Passwords do not match")) )
            ( (not (schedule
                     (lambda () (backend-name/neu-id-match? name neu-id))))
              (send/suspend/callback
                (page-create-user "Your Northeastern ID is incorrect")) )
            ( (schedule (lambda () (backend-username-taken? username)))
              (send/suspend/callback (page-create-user "Username taken")) )
            ( (schedule (lambda () (backend-has-username? name neu-id)))
              (send/suspend/callback
                (page-create-user "You already have a username")) )
            ( else
              (schedule-transaction (lambda ()
                                      (backend-create-account!
                                        name neu-id username password1)))
              (send/suspend/callback
                (page-courses (make-session username #f)
                              (schedule (lambda () (backend-courses username)))
                                "Congrats, you've logged in.")) ))))

      ;; Direct transition to the courses selection page.
      (define-transition (transition-courses session)
        (send/suspend/callback
          (page-courses
            session
            (schedule (lambda () (backend-courses
                                   (session-username session)))))))

      ;; Go to the main page for the position.
      (define-transition (transition-main session)
        (main session "Congrats, you've logged in."))

      ;; Direct transition to the main student page.
      (define-transition (transition-student-main session)
        (main session "Congrats, you've logged in as a student."))

      ;; Direct transition to the main non-student page.
      (define-transition (transition-non-student-main session)
        (main session "Congrats, you've logged in as a non-student."))

      ;; Go to the main page for the position.
      (define (main session message)
        (let ((c (session-course session)))
          (if (not c)
            (send/suspend/callback
              (page-courses
                session
                (schedule
                  (lambda () (backend-courses (session-username session))))
                message))
            (let ((p (course-position c)))
              (case p
                ( (student)
                  (send/suspend/callback (page-student-main session message)) )
                ( else 
                  (send/suspend/callback (page-non-student-main session message)) ))))))

      ))

  ;; **************************************************************

  ;; schedule-transaction : ( -> X) -> X
  ;; Wrap a thunk in a database transaction.
  (define (schedule-transaction f)
    (schedule
      (lambda ()
        (with-handlers (( (lambda (x) #t)
                          (lambda (x)
                            (backend-db-rollback)
                            (raise x)) ))
          (backend-db-begin)
          (begin0
            (f)
            (backend-db-commit))))))

  ;; Extract the values from the bindings in the request and use them in
  ;; the bodies.
  (define-syntax let/bindings
    (syntax-rules ()
      ((_ req (name ...) . body)
       (let ((req-var (request-bindings req)))
         (let ((name (extract-binding/single 'name req-var)) ...)
           . body)))))

  ;; A direct transition takes a request, but ignores it.
  (define-syntax define-transition
    (syntax-rules ()
      ( (_ f body ...)
        (define f (lambda _ body ...)))))

  ;; An action transition pulls apart the request.
  (define-syntax define-action-transition
    (syntax-rules ()
      ( (_ f bs body ...)
        (define f
          (lambda (req)
            (let/bindings req bs
              body ...))))))

)
