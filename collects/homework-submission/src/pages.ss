;; A page is a function that produces a xexpr/callback. page@ is a unit
;; because it has a mutual dependency on transitions@.
;; The naming convention for pages is `page-foo', where `foo' describes that
;; page.

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
                (submit-button "Log in"))))

      ;; Confirm the user has logged in.
      (define page-logged-in
        (page (session)
          "You Are Logged In"
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
              (p (hyperlink transition-log-out "Logout"))))


      ;; ************************************************************

      ;; page : (Symbol? ...) String? Xexpr ... ->
      ;;         ((Alpha ... [String]) -> Xexpr
      ;; Produce a function that produces a page with an optional message.
      (define-syntax page
        (syntax-rules ()
          ((_ (args ...) title body ...)
           (opt-lambda (args ... (message #f))
             (build-page
               title
               (if message (p message) "")
               body ...)))))

      )))
