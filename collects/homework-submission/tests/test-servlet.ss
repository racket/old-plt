;; Automated tests of the use cases. These are meant to be run as a single
;; client, once, against a testing backend. This tests that the servlet has the
;; functionality we want for a single user, once.

(module test-servlet mzscheme
  (require (lib "etc.ss")
           (lib "test.ss" "schemeunit")
           (lib "send-assertions.ss" "web-server" "tools")
           
           "../src/pages-transitions.ss"
           (prefix backend: "../src/backend.ss")
           )

  (provide test-servlet)

  (define (the-servlet)
    (transition-login #f)) ;;; #f is usually req

  (define test-servlet
    (make-test-suite
      "Test all the use cases"

      (make-test-case
        "A user enters an invalid username and/or password"
        (assert-output-response/suspended
          the-servlet
          (list (list form->k-url
                      (list (cons 'username "The Test Username")
                            (cons 'password "The Wrong Password"))))
          (login-page "Invalid username or password.")))

      (make-test-case
        "A user logs in, then logs out"
        (assert-output-response/suspended
          the-servlet
          (list (list form->k-url
                      (list (cons 'username "The Test Username")
                            (cons 'password "The Test Password")))
                (list (hyperlink->k-url "Logout") '()))
          (login-page)))

      (make-test-case
        "A user logs in, then closes the Web browser"
        (assert-output-response/suspended
          the-servlet
          (list (list form->k-url
                      (list (cons 'username "The Test Username")
                            (cons 'password "The Test Password"))))
          logged-in-page))

      (make-test-case
        (string-append
          "A user logs in, logs out, goes back to the logged-in page, and "
          "logs out again")
        (assert-output-response/suspended
          the-servlet
          (list (list form->k-url
                      (list (cons 'username "The Test Username")
                            (cons 'password "The Test Password")))
                (list (hyperlink->k-url "Logout") '())
                'back
                'forward)
          restart-session-page))

      (make-test-case
        (string-append
          "A user logs in, sucessfully changes his or her password, then logs "
          "out")
        (assert-output-response/suspended
          the-servlet
          (list (list form->k-url
                      (list (cons 'username "The Test Username")
                            (cons 'password "The Test Password")))
                (list (hyperlink->k-url "Change Password") '())
                (list form->k-url
                      (list (cons 'old-password "The Test Password")
                            (cons 'new-password1 "The New Password")
                            (cons 'new-password2 "The New Password")))
                (list (hyperlink->k-url "Logout") '()))
          (login-page))
        (void)
        (backend:update-password! "The Test Username" "The Test Password"))

      (make-test-case
        (string-append
          "A user logs in, attempts to change his or her password, enters an "
          "invalid old password, then enters mismatched new passwords, then "
          "sucessfully changes his or her password, then logs out")
        (assert-output-response/suspended
          the-servlet
          (list (list form->k-url
                      (list (cons 'username "The Test Username")
                            (cons 'password "The Test Password")))
                (list (hyperlink->k-url "Change Password") '())
                (list form->k-url
                      (list (cons 'old-password "The Wrong Password")
                            (cons 'new-password1 "The New Password")
                            (cons 'new-password2 "The New Password")))
                (list form->k-url
                      (list (cons 'old-password "The Test Password")
                            (cons 'new-password1 "The New Password1")
                            (cons 'new-password2 "The New Password2")))
                (list form->k-url
                      (list (cons 'old-password "The Test Password")
                            (cons 'new-password1 "The New Password")
                            (cons 'new-password2 "The New Password")))                
                (list (hyperlink->k-url "Logout") '()))
          (login-page))
        (void)
        (backend:update-password! "The Test Username" "The Test Password"))

      ))


  ;; login-page : [string] -> xexpr/callback
  ;; The login page, with an optional error message.
  (define login-page
    (opt-lambda ((message #f))
      `(html () (head () (title () "Please Log In"))
             (body () (h1 () "Please Log In")
                   ,(if message `(p () ,message) "")
                   (form ((action ,(make-unknown)))
                         (p () (label ((for "username")) "User name")
                            (input ((name "username") (id "username")
                                                      (type "text"))))
                         (p () (label ((for "password")) "Password")
                            (input ((name "password") (id "password")
                                                      (type "password"))))
                         (p () (input ((type "submit") (value "Log in")))))))))

  ;; logged-in-page : xexpr/callback
  ;; The page after logging in.
  (define logged-in-page
    `(html () (head () (title () "You Are Logged In"))
           (body () (h1 () "You Are Logged In")
                 (p "Congrats, you've logged in.")
                 (p (a ((href ,(make-unknown))) "Logout")))))

  ;; restart-session-page : xexpr/callback
  ;; The session has timed out.
  (define restart-session-page
    `(html () (head () (title () "Timeout"))
           (body () (p () ,(string-append
                             "The transaction referred to by this url is no "
                             "longer active.  Please ")
                       ,(make-unknown) " the transaction."))))

  )
