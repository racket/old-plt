;; The signatures for units used in the homework submission servlet.
(module sigs mzscheme
  (require (lib "unitsig.ss"))

  (provide pages^ transitions^)

  (define-signature pages^
    (page-login
     page-logged-in
     page-change-password))

  (define-signature transitions^
    (transition-login
     transition-log-in
     transition-log-out
     transition-change-password
     transition-update-password))

  )
