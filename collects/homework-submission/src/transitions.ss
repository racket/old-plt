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

      ;; Direct transition to the login page. This clears the previously-
      ;; stored continuations first.
      (define-transition transition-log-out
        (send/forward/callback (page-login)))

      ;; Direct transition to the change password page.
      (define-transition (transition-change-password session) 
        (send/suspend/callback (page-change-password session)))

      ;; Direction transition to the user creation page.
      (define-transition transition-create-user
        (send/suspend/callback (page-create-user)))

      ;; Direct transition to the courses selection page.
      (define-transition (transition-courses session)
        (send/suspend/callback
          (with-handlers ((exn:fail? (lambda (e)
                                       (page-courses
                                         session null (exn-message e)))))
            (page-courses
              session
              (schedule (lambda () (backend-courses
                                     (session-username session))))))))

      ;; Direct transition to the main page for the position.
      (define-transition (transition-main session)
        (main session "Congrats, you've logged in."))

      ;; Direct transition to the main student page.
      (define-transition (transition-student-main session)
        (main session "Congrats, you've logged in as a student."))

      ;; Direct transition to the main non-student page.
      (define-transition (transition-non-student-main session)
        (main session "Congrats, you've logged in as a non-student."))

      ;; Direct transition to the student's assignment page.
      (define-transition (transition-student-assignments session)
        (send/suspend/callback
          (page-student-assignments
            session
            (schedule (lambda ()
                        (backend-assignments/due
                          (session-id session)
                          (course-id (session-course session)) '>)))
            (schedule (lambda ()
                        (backend-assignments/due
                          (session-id session)
                          (course-id (session-course session)) '<))))))

      ;; Direct transition to the partnership management page for students.
      (define-transition (transition-student-partners session)
        (send/suspend/callback
          (page-student-partners
            (update-course-partnership?/session session)
            (schedule (lambda ()
                        (backend-partners
                          (session-id session)
                          (course-id (session-course session))))))))

      ;; Direct transition to the description of an assignment.
      (define-transition (transition-view-description a)
        (if (assignment-description-url a)
          (redirect-to (assignment-description-url a))
          (send/suspend/callback (assignment-description a))))

      ;; Direct transition to the submitted assignment.
      (define-transition (transition-view-submission a)
        (send/back (list "text/plain" (read-file (assignment-submission a)))))

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
              (with-handlers ((exn:fail? (lambda (e)
                                           (page-courses
                                             (make-session 
                                               (schedule
                                                 (lambda ()
                                                   (backend-id/username
                                                     username)))
                                               username #f)
                                             null (exn-message e)))))
                (page-courses
                  (make-session 
                    (schedule (lambda () (backend-id/username username)))
                    username #f)
                  (schedule (lambda () (backend-courses username)))
                  "Congrats, you've logged in."))) )))

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
                (with-handlers ((exn:fail? (lambda (e)
                                             (page-courses
                                               (make-session 
                                                 (schedule
                                                   (lambda ()
                                                     (backend-id/username
                                                       username)))
                                                 username #f)
                                               null (exn-message e)))))
                  (page-courses
                    (make-session 
                      (schedule (lambda () (backend-id/username username)))
                      username #f)
                    (schedule (lambda () (backend-courses username)))
                    "Congrats, you've logged in."))) ))))

      ;; Action transition to the partnership management page.
      ;; Action: Add a student to the partnership for this student, if legal.
      ;; Check that this student is not in a partnership of the correct size
      ;; for the course, and that the selected partner entered the correct
      ;; password. Add the selected partner to the partnership for this
      ;; student. Send the partnership management page.
      (define-action-transition (transition-add-partner session)
        (username password)
        (cond
          ( (not (schedule
                   (lambda ()
                     (backend-can-add-partner?
                       (session-id session)
                       (course-id (session-course session))))))
            (send/suspend/callback
              (page-student-partners
                (update-course-partnership?/session session)
                (schedule (lambda ()
                            (backend-partners
                              (session-id session)
                              (course-id (session-course session)))))
                "Correct number of partners")) )
          ( (not
              (or
                (schedule
                  (lambda ()
                    (backend-valid-username-password? username password)))
                (schedule
                  (lambda ()
                    (backend-user-in-course?
                      username (course-id (session-course session)))))))
            (send/suspend/callback
              (page-student-partners
                (update-course-partnership?/session session)
                (schedule (lambda ()
                            (backend-partners
                              (session-id session)
                              (course-id (session-course session)))))
                (format "~s is not in this course" username))) )
          ( else
            (let ((cid (course-id (session-course session)))
                  (sid (session-id session)))
              (schedule-transaction
                (lambda () (backend-add-partner!  sid cid username)))
              (send/suspend/callback
                (page-student-partners
                  (update-course-partnership?/session session)
                  (schedule (lambda () (backend-partners sid cid)))
                  (format "~s is added as a new partner for this course"
                          username)))) )))

      ;; Action transition to the assignments page.
      ;; Action: upload the homework and store it in the database.
      ;; If the student can submit an assignment, store the file on the
      ;; filesystem and update the database. Send the assignments page.
      (define-action-transition (transition-submit-assignment session a)
        (file)
        (cond
          ( (schedule (lambda () (backend-can-submit?
                                   (session-id session)
                                   (course-id (session-course session)))))
            (with-handlers
              ((exn:fail?
                 (lambda (e)
                   (send/suspend/callback
                     (page-student-assignments
                       (update-course-partnership?/session session)
                       (schedule (lambda ()
                                   (backend-assignments/due
                                     (session-id session)
                                     (course-id (session-course session)) '>)))
                       (schedule (lambda ()
                                   (backend-assignments/due
                                     (session-id session)
                                     (course-id (session-course session)) '<)))
                       (exn-message e))))))
              (let ((filename (schedule (lambda ()
                                          (backend-submission-filename
                                            (session-id session)
                                            (course-id (session-course session))
                                            (assignment-id a))))))
                ;; Not scheduled because it doesn't hit the database
                (backend-store-submission/file! filename file)
                (schedule-transaction
                  (lambda () (backend-store-submission/db!
                               (assignment-partner-id a)
                               (assignment-id a)
                               filename))))
              (send/suspend/callback
                (page-student-assignments
                  (update-course-partnership?/session session)
                  (schedule (lambda ()
                              (backend-assignments/due
                                (session-id session)
                                (course-id (session-course session)) '>)))
                  (schedule (lambda ()
                              (backend-assignments/due
                                (session-id session)
                                (course-id (session-course session)) '<)))
                  "Submitted"))) )
          ( else (send/suspend/callback
                   (page-student-assignments
                     (update-course-partnership?/session session)
                     (schedule (lambda ()
                                 (backend-assignments/due
                                   (session-id session)
                                   (course-id (session-course session)) '>)))
                     (schedule (lambda ()
                                 (backend-assignments/due
                                   (session-id session)
                                   (course-id (session-course session)) '<)))
                     (string-append
                       "You cannot submit an assignment. This is probably "
                       "because you do not have the correct number of "
                       "partners."))
                   ))))

      ;; **** Helpers ****

      ;; Go to the main page for the position.
      (define (main session message)
        (let ((c (session-course session)))
          (if (not c)
            (send/suspend/callback
              (with-handlers ((exn:fail? (lambda (e)
                                           (page-courses
                                             session null (exn-message e)))))
                (page-courses
                  session
                  (schedule (lambda () (backend-courses
                                         (session-username session))))
                  message)))
            (case (course-position c)
              ( (student)
                (send/suspend/callback (page-student-main session message)) )
              ( else 
                (send/suspend/callback
                  (page-non-student-main session message)) )))))

      ;; update-course-partnership?/session : (session? . -> . session?)
      ;; Update the can-submit? and partnership-full? fields of a course in a
      ;; session. This is a common action.
      (define (update-course-partnership?/session session)
        (let ((c (session-course session)))
          (make-session
            (session-id session)
            (session-username session)
            (make-course
              (course-id c)
              (course-name c)
              (course-number c)
              (course-position c)
              (schedule (lambda () (backend-can-submit?
                                     (session-id session)
                                     (course-id c))))
              (schedule (lambda () (backend-partnership-full?
                                     (session-id session)
                                     (course-id c))))))))

      ))

  ;; **************************************************************

  ;; read-file : String -> String
  ;; Return the contents of a file.
  (define (read-file filename)
    (with-input-from-file
      filename
      (lambda ()
        (let loop ((acc ""))
          (let ((r (read-line)))
            (if (eof-object? r)
              acc
              (loop (format "~a~n~a" acc r))))))))

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
