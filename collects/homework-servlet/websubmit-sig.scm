(module websubmit-sig mzscheme
  (require (lib "unitsig.ss")
           "websubmit-macros.scm")
  (provide websubmit-machine^
           websubmit-configuration^
           websubmit-states^
           course-specific^)

  (define-signature websubmit-machine^ (start-machine))
  
  (define-signature websubmit-configuration^ (max-partners min-partners))
  
  (define-signature course-specific^ (course-title course-number))
  
  (define-signature websubmit-states^ (make-q-login
                                       make-q-create-account
                                       make-q-selection
                                       make-q-upload
                                       make-q-confirm
                                       make-q-file-saved
                                       make-q-overdue
                                       make-q-choose-credentials
                                       make-q-manage-account
                                       make-q-select-grader
                                       make-q-add-partner-or-proceed
                                       make-q-pass-entry
                                       make-q-new-partner
                                       make-q-partner-login
                                       make-q-account-summary
                                       make-q-initial-select-grader
                                       
                                       ;; staff pages
                                       make-q-staff-lobby
                                       make-q-staff-change-pass
                                       make-q-staff-create-student
                                       make-q-staff-review-new-student
                                       make-q-staff-find-student
                                       make-q-staff-edit-student
                                       make-q-staff-review-student
                                       make-q-staff-edit-full-student
                                       make-q-staff-review-full-student
                                       ))

  (define-structs-and-sig/provide websubmit-pages^
    
    ;; ********************************************************************************
    ;; used by the pages interface
    
    ;; **************************************************
    ;; staff pages structures
    
    ;; (make-staff-lobby-req boolean boolean boolean boolean)
    (struct/sig staff-lobby-req (changepass? create-student? edit-student? logout?) (make-inspector))
  
    ;; (make-simple-student-req string string boolean)
    (struct simple-student-req (name id continue?) (make-inspector))
    (struct/sig (simple-student-req/create simple-student-req) () (make-inspector))
    (struct/sig (simple-student-req/edit simple-student-req) () (make-inspector))
    
    ;; (make-review-student-req boolean boolean boolean)
    (struct review-student-req (edit? create? cancel?) (make-inspector))
    (struct/sig (review-simple-student-req/create review-student-req) () (make-inspector))
    (struct/sig (review-simple-student-req/edit review-student-req) () (make-inspector))
    (struct/sig (review-full-student-req/edit review-student-req) () (make-inspector))
  
    ;; (make-student-to-edit-req string boolean)
    (struct/sig student-to-edit-req (id continue?) (make-inspector))
    
    ;; (make-full-student-req/edit string string string string boolean boolean boolean)
    (struct/sig full-student-req/edit (name id login-name grader
                                            clear-password?
                                            clear-partnership?
                                            continue?) (make-inspector))
    
    ;; **************************************************
    ;; Account Creation Pages structures
    
    ;; (make-create-account-req string)
    (struct/sig create-account-req (id) (make-inspector))
    
    ;; (make-account-summary-req)
    (struct/sig account-summary-req () (make-inspector))
    
    ;; (make-add-partner-or-proceed-req boolean)
    (struct/sig add-partner-or-proceed-req (add?) (make-inspector))
    
    ;; (make-choose-credentials-req string string string)
    (struct/sig choose-credentials-req (login-name first second) (make-inspector))
    
    ;; **************************************************
    ;; Login Pages structures
    
    ;; (make-login-req string string)
    (struct login-req (login-name password) (make-inspector))
    (struct/sig (login-req/initial login-req) (create?) (make-inspector))
    (struct/sig (login-req/partner login-req) () (make-inspector))
    
    ;; **************************************************
    ;; continue requests
    
    ;; (make-continue-req ())
    (struct continue-req () (make-inspector))
    (struct/sig (already-exists-req continue-req) () (make-inspector))
    (struct/sig (need-partner-req continue-req) () (make-inspector))
    (struct/sig (partner-already-paired-req continue-req) () (make-inspector))
    (struct/sig (sorry-charlie-req continue-req) () (make-inspector))
    (struct/sig (upload-complete-req continue-req) () (make-inspector))
    
    ;; **************************************************
    ;; Password Entry structs
    
    ;; (make-pass-entry-req string string)
    (struct/sig pass-entry-req (first second) (make-inspector))
    (struct/sig (pass-entry-req/staff pass-entry-req) () (make-inspector))
    
    ;; **************************************************
    
    ;; (make-grader-selection-req string)
    (struct/sig grader-selection-req (grader) (make-inspector))
    
    ;; (make-manage-account-req (boolean boolean boolean))
    (struct/sig manage-account-req (changepass? addpartner? logout?) (make-inspector))
    
    ;; (make-select-assignment-req string boolean boolean)
    (struct/sig select-assignment-req (assignment manage? logout?) (make-inspector))
    
    ;; (make-upload-confirm-req boolean)
    (struct/sig upload-confirm-req (confirm?) (make-inspector))
    
    ;; (make-upload-file-req string string)
    (struct/sig upload-file-req (definitions interactions) (make-inspector))
    ))