;; Data structures common to transitions and pages.
(module data mzscheme
  (require (lib "contract.ss"))

  (provide/contract
    (struct session ((username string?)))
    )

  ;; A Session is a
  ;; (make-session String)
  (define-struct session (username))

  )
