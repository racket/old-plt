;; A page is a function that produces a xexpr/callback. page@ is a unit
;; because it has a mutual dependency on transitions@.
;; The naming convention for pages is `page-foo', where `foo' describes that
;; page.
; vim:lispwords+=,page

(module pages mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           "widgets.ss"
           "sigs.ss"
           "data.ss")

  (provide pages@)

  (define pages@
    (unit/sig pages^
      (import transitions^)

      ;; Prompt the user for his or her username and password.
      (define page-login
        (page ()
          "Please Log In"
          (form transition-log-in '()
                (text-input "User name" "username")
                (password-input "password")
                (submit-button "Log in"))
          (p (hyperlink transition-create-user "Create Username"))))

      ;; Confirm the student has logged in.
      (define page-student-main
        (page (session)
          "You Are Logged In As A Student"
          (p (hyperlink (transition-courses session) "Courses"))
          (p (hyperlink (transition-change-password session) "Change Password"))
          (p (hyperlink transition-log-out "Logout"))))

      ;; Confirm the non-student has logged in.
      (define page-non-student-main
        (page (session)
          "You Are Logged In As A Non-Student"
          (p (hyperlink (transition-courses session) "Courses"))
          (p (hyperlink (transition-change-password session) "Change Password"))
          (p (hyperlink transition-log-out "Logout"))))

      ;; Prompt the user for a new password
      (define page-change-password
        (page (session)
              "Change Password"
              (form (transition-update-password session) '()
                    (input "Old password" "old-password" "password")
                    (input "New password" "new-password1" "password")
                    (input "New password (again)" "new-password2" "password")
                    (submit-button "Change"))
              (p (hyperlink (transition-courses session) "Courses"))
              (p (hyperlink transition-log-out "Logout"))))

      ;; Prompt the user for all the information needed to create an account.
      ;; This is: their name as we know it, the last four digits of their
      ;; Northeastern ID, their desired username, and their password (twice).
      (define page-create-user
        (page ()
          "Create A Username"
          (form transition-create-a-user '()
                (text-input "Last name" "name")
                (text-input "Last four digits of Northeastern ID" "neu-id")
                (text-input "Desired username" "username")
                (password-input "password1")
                (password-input "password2")
                (submit-button "Log in"))))


      ;; The courses a user is in.
      (define page-courses
        (page (session courses)
          "Courses"
          (html-table "Courses in which you are enrolled"
                      (list "Name" "Number" "Position")
                      (map
                        (lambda (c)
                          `(tr (td ,(hyperlink
                                      (transition-main 
                                        (make-session 
                                          (session-username session)
                                          c))
                                      (course-name c)))
                               (td ,(course-number c))
                               (td ,(if (symbol=? (course-position c)
                                                  'student)
                                      "Student"
                                      "Non-student"))))
                        courses))
          (p (hyperlink (transition-change-password session) "Change Password"))
          (p (hyperlink transition-log-out "Logout"))))

      ))


      ;; ************************************************************

      ;; page : (Symbol? ...) String? Xexpr ... ->
      ;;         ((Alpha ... [String]) -> Xexpr)
      ;; Produce a function that produces a page with an optional message.
      (define-syntax page
        (syntax-rules ()
          ((_ (args ...) title body ...)
           (opt-lambda (args ... (message #f))
             (build-page
               title
               (if message (p message) "")
               body ...)))))

      )
