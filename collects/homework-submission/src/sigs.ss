;; The signatures for units used in the homework submission servlet.
(module sigs mzscheme
  (require (lib "unitsig.ss"))

  (provide pages^ transitions^)

  (define-signature pages^
    (page-login
     page-change-password
     page-create-user
     page-courses
     page-student-main
     page-non-student-main))

  (define-signature transitions^
    (transition-login
     transition-main
     transition-student-main
     transition-non-student-main
     transition-log-in
     transition-log-out
     transition-change-password
     transition-update-password
     transition-create-user
     transition-create-a-user
     transition-courses))

  )
