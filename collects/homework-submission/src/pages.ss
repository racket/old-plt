;; A page is a function that produces a xexpr/callback. page@ is a unit
;; because it has a mutual dependency on transitions@.
;; The naming convention for pages is `page-foo', where `foo' describes that
;; page.

(module pages mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           "widgets.ss"
           "sigs.ss")

  (provide pages@)

  (define pages@
    (unit/sig pages^
      (import transitions^)

      ;; Prompt the user for his or her username and password.
      (define page-login
        (opt-lambda ((message #f))
          (build-page
            "Please Log In"
            (if message (p message) "")
            (form transition-log-in '()
                  (text-input "User name" "username")
                  (password-input "password")
                  (submit-button "Log in")))))

      ;; Confirm the user has logged in.
      (define page-logged-in
        (build-page
          "You Are Logged In"
          (p "Congrats, you are logged in.")
          (p (hyperlink transition-log-out "Logout"))))

      )))
