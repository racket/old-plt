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
           "sigs.ss")

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
                 (valid? (backend-valid-username-password? username password)))
            (if valid?
              (send/suspend/callback page-logged-in)
              (send/suspend/callback
                (page-login "Invalid username or password."))))))

      ;; Direct transition to the login page. This clears the previously-
      ;; stored continuations first.
      (define transition-log-out
        (lambda (req)
          (send/forward/callback (page-login))))

      )))
