(module websubmit-machine mzscheme
  (require (lib "unitsig.ss")
           "websubmit-sig.scm"
           "websubmit-states.scm"
           "websubmit-strings.scm")
  
  
  (provide websubmit-machine@)
  
  (define machine-unit@
    (unit/sig websubmit-machine^
      (import websubmit-states^)
         
      ;; ********************************************************************************
      (define q-login
        (letrec
            ([q-login (lambda args (apply (make-q-login q-login q-create-account q-selection q-staff-lobby) args))]
             [q-create-account (lambda args (apply (make-q-create-account q-create-account q-login q-choose-credentials) args))]
             [q-selection (lambda args (apply (make-q-selection q-manage-account q-select-grader q-login q-upload) args))]
             [q-upload (lambda args (apply (make-q-upload q-upload q-confirm) args))]
             [q-confirm (lambda args (apply (make-q-confirm q-selection q-file-saved q-overdue) args))]
             [q-file-saved (lambda args (apply (make-q-file-saved q-selection) args))]
             [q-overdue (lambda args (apply (make-q-overdue q-selection) args))]
             [q-choose-credentials (lambda args (apply (make-q-choose-credentials q-choose-credentials q-add-partner-or-proceed) args))]
             [q-manage-account (lambda args (apply (make-q-manage-account q-pass-entry q-select-grader q-new-partner q-login q-selection) args))]
             [q-select-grader (lambda args (apply (make-q-select-grader q-manage-account) args))]
             [q-add-partner-or-proceed (lambda args (apply (make-q-add-partner-or-proceed q-partner-login q-account-summary) args))]
             [q-pass-entry (lambda args (apply (make-q-pass-entry q-pass-entry q-manage-account) args))]
             [q-new-partner (lambda args (apply (make-q-new-partner q-new-partner q-manage-account) args))]
             [q-partner-login (lambda args (apply (make-q-partner-login q-partner-login q-add-partner-or-proceed q-initial-select-grader) args))]
             [q-account-summary (lambda args (apply (make-q-account-summary q-login) args))]
             [q-initial-select-grader (lambda args (apply (make-q-initial-select-grader q-account-summary) args))]
             
             ;; staff pages
             [q-staff-lobby (lambda args (apply (make-q-staff-lobby q-staff-change-pass
                                                                    q-staff-create-student
                                                                    q-staff-find-student
                                                                    q-login)
                                                args))]
             [q-staff-change-pass (lambda args (apply (make-q-staff-change-pass q-staff-change-pass q-staff-lobby) args))]
             [q-staff-create-student (lambda args (apply (make-q-staff-create-student q-staff-lobby
                                                                                      q-staff-review-new-student)
                                                         args))]
             [q-staff-review-new-student (lambda args (apply (make-q-staff-review-new-student q-staff-create-student q-staff-lobby) args))]
             [q-staff-find-student (lambda args (apply (make-q-staff-find-student q-staff-find-student
                                                                                  q-staff-lobby
                                                                                  q-staff-edit-student
                                                                                  q-staff-edit-full-student)
                                                       args))]
             [q-staff-edit-student (lambda args (apply (make-q-staff-edit-student q-staff-edit-student
                                                                                  q-staff-lobby
                                                                                  q-staff-review-student)
                                                       args))]
             [q-staff-review-student (lambda args (apply (make-q-staff-review-student q-staff-lobby
                                                                                      q-staff-edit-student)
                                                         args))]
             [q-staff-edit-full-student (lambda args (apply (make-q-staff-edit-full-student q-staff-edit-full-student
                                                                                            q-staff-lobby
                                                                                            q-staff-review-full-student)
                                                            args))]
             [q-staff-review-full-student (lambda args (apply (make-q-staff-review-full-student q-staff-lobby
                                                                                           q-staff-edit-full-student)
                                                              args))]
             )
          q-login))
      
      (define (start-machine) (q-login initial-login-message))
      
      ))
  
  (define websubmit-machine@
    (compound-unit/sig
      (import (CNF : websubmit-configuration^)
              (PGS : websubmit-pages^))
      (link
       [STS : websubmit-states^ (websubmit-states@ CNF PGS)]
       [MCHN : websubmit-machine^ (machine-unit@ STS)])
      (export (open MCHN))))
  )

