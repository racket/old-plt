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
     page-student-partners
     page-non-student-main
     page-instructor-main
     page-student-assignments
     page-instructor-manage-students
     page-instructor-manage-partnerships
     ))

  (define-signature transitions^
    (transition-login
     transition-main
     transition-student-main
     transition-student-partners
     transition-non-student-main
     transition-log-in
     transition-log-out
     transition-change-password
     transition-update-password
     transition-create-user
     transition-create-a-user
     transition-courses
     transition-add-partner
     transition-view-description
     transition-view-submission
     transition-student-assignments
     transition-submit-assignment
     transition-manage-students
     transition-add-a-student
     transition-add-students
     transition-drop-students
     transition-update-partnerships
     transition-manage-partnerships
     ))

  )
