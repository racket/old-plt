(module websubmit-states mzscheme
  (require (lib "unitsig.ss")
	   "websubmit-sig.scm"
	   "websubmit-lib.scm"
	   "websubmit-strings.scm"
	   "websubmit-structs.scm"
	   "parsing.scm"
           "md5.ss"
	   )
  
  (provide websubmit-states@)
  
  (define websubmit-states@
    (unit/sig websubmit-states^
      (import websubmit-configuration^ websubmit-pages^)
      
      ;; ********************************************************************************
      ;; account creation
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-create-account:  (string->) (string->) (student-account string ->) -> (string ->)
      (define make-q-create-account
        (lambda (q-create-account q-login q-choose-credentials)
          (lambda (message)
            (let* ([id (create-account-req-id (get-create-account-req message))]
                   [acct (lookup-student-account/id id (read-accounts))])
              (cond
                [(not acct) (q-create-account cant-find-id-message)]
                [(full-student-account? acct)
                 (get-already-exists-req)
                 (q-login initial-login-message)]
                [else
                 (q-choose-credentials acct initial-choose-credentials-message)])))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-choose-credentials: (student-account string . string ->) (student-account ->) -> (student-account string . string ->)
      (define make-q-choose-credentials
        (lambda (q-choose-credentials q-add-partner-or-proceed)
          (lambda (acct message . username)
            (let ([req (apply get-choose-credentials-req (cons acct (cons message username)))])
              (cond
                [(not (valid-login-name? (choose-credentials-req-login-name req)))
                 (q-choose-credentials
                  acct
                  (format "The username you entered, ~a, is invalid. Please enter a different one"))]
                [(not (string=? (choose-credentials-req-first req)
                                (choose-credentials-req-second req)))
                 (q-choose-credentials
                  acct
                  "The passwords did not match. Please try again"
                  (choose-credentials-req-login-name req))]
                [(create-new-account acct
                                     (choose-credentials-req-login-name req)
                                     (choose-credentials-req-first req))
                 => q-add-partner-or-proceed]
                
                ;; THIS IS WHERE THE QUICK FIX WAS
                ;              [(create-new-account acct
                ;                                   (choose-credentials-request-username req)
                ;                                   (choose-credentials-request-first req))
                ;               => q-select-grader-quick-fix]
                [else
                 (q-choose-credentials
                  acct
                  (format "The username you entered, ~a, already exists. Please enter a different one"
                          (choose-credentials-req-login-name req)))])))))
      
      
      ;; make-q-select-grader-quick-fix: (student-account ->) -> (student-account ->)
      ;; single student selects grader only for himself.
      (define make-q-select-grader-quick-fix
        (lambda (q-account-summary)
          (lambda (acct1)
            (let ([new-grader (grader-selection-req-grader
                               (apply get-grader-selection-req (get-grader-selection-args acct1)))])
              (update-partners! (list acct1) new-grader (full-student-account-group-id acct1))
              (q-account-summary acct1)))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-add-partner-or-proceed: (student-account string ->) (student-account ->) -> (student-account ->)
      (define make-q-add-partner-or-proceed
        (lambda (q-partner-login q-account-summary)
          (lambda (acct)
            (let ([req (get-add-partner-or-proceed-req acct)])
              (if (add-partner-or-proceed-req-add? req)
                  (q-partner-login acct initial-partner-login-message)
                  (q-account-summary acct))))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-partner-login: (student-account string ->) (student-account ->) (student-account student-account ->) -> (student-account string ->)
      (define make-q-partner-login
        (lambda (q-partner-login q-add-partner-or-proceed q-initial-select-grader)
          (lambda (acct1 message)
            (let ([req (get-login-req/partner (format message 
                                                      (format-partner-list (cons acct1 (find-partners acct1)))))])
              (let ([acct2 (request->account (login-req-login-name req)
                                             (login-req-password req))])
                (cond
                  [(not acct2)
                   (q-partner-login acct1 partner-invalid-credentials-message)]
                  [(staff-account? acct2)
                   (q-partner-login acct1 partner-invalid-credentials-message)]
                  ;; security use partner-invalid-credentials for both cases above, otherwise we
                  ;; have a tool for testing if credentials belong to staff.
                  [else
                   (let ([partners (find-partners acct2)])
                     (cond
                       ;[(null? partners) (q-initial-select-grader acct1 acct2)]
                       [(< (length partners) max-partners) (q-initial-select-grader acct1 acct2)]
                       [else
                        (get-partner-already-paired-req acct2 partners)
                        (q-add-partner-or-proceed acct1)]))]))))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-initial-select-grader: (student-account ->) -> (student-account student-account ->)
      ;; NOTE: We select the grader and then commit both the partnership *and* the grader.
      ;;       this way we know that all partners have the same grader.
      (define make-q-initial-select-grader
        (lambda (q-account-summary)
          (lambda (acct1 acct2)
            (let ([new-grader
                   (grader-selection-req-grader (apply get-grader-selection-req (get-grader-selection-args acct1)))])
              (let ([partners (cons acct1 (cons acct2 (find-partners acct2)))])
                (update-partners! partners new-grader (full-student-account-group-id acct1))
                (q-account-summary acct1))))))
      
      ;; get-grader-selecton-args: student-account -> (list (listof grader) string string . (listof student-account))
      (define get-grader-selection-args
        (lambda (acct)
          (let ([graders (read-graders)])
            `(,graders
              ,@(if (has-grader? acct graders)
                    (let ([current-grader (full-student-account-grader acct)])
                      (list current-grader (format current-grader-info current-grader)))
                    (list (car graders) no-current-grader-info))
              ,@(find-partners acct)))))
      
      ;; update-partners!: (listof full-student-account) string symbol ->
      ;; mutate all student-accounts to have the same grader and group-symbol
      ;; save all student-accounts.
      (define update-partners!
        (lambda (partners grader group-id)
          (for-each
           (lambda (acct)
             (set-full-student-account-grader! acct grader)
             (set-full-student-account-group-id! acct group-id)
             (save-account acct))
           partners)))
      
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-account-summary: (student-account ->) -> (student-account ->)
      (define make-q-account-summary
        (lambda (q-login)
          (lambda (acct)
            (get-account-summary-req acct (find-partners acct) (find-grader acct) min-partners)
            (q-login initial-login-message))))
      
      ;; find-grader: student-account -> string
      (define find-grader
        (lambda (acct)
          (let ([graders (read-graders)])
            (if (has-grader? acct graders)
                (full-student-account-grader acct)
                ""))))
      
      ;; ********************************************************************************
      ;; states
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-login: (string ->) (string ->) (student-account ->) (staff-account ->) -> (string ->)
      ;; The student needs to provide a name and password.
      (define make-q-login
        (lambda (q-login q-create-account q-selection q-staff-lobby)
          (lambda (message)
            (let ([req (get-login-req/initial message)])
              (if (login-req/initial-create? req)
                  (q-create-account initial-create-account-message)
                  (let ([acct
                         (request->account
                          (login-req-login-name req)
                          (login-req-password req))])
                    (cond
                      [(not acct) (q-login invalid-credentials-message)]
                      [(student-account? acct) (q-selection acct)]
                      [(staff-account? acct) (q-staff-lobby acct)]
                      [else (q-login invalid-credentials-message)])))))))

      
      ;; request->student-account: request -> student-account | #f
      ;; get a login request and then lookup the student-account
      ;; a login-token in this context is either a student-account or #f
      (define request->account
        (lambda (username password)
          (lookup-account username password (read-accounts))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-selection: (student-account (listof student-account) ->)
      ;;              (student-account (listof student-account) ->)
      ;;              (string ->)  
      ;;              (student-account (listof student-account) full-assignment string ->)
      ;;               -> (student-account ->)
      ;; The student has successfully logged in and must select an assignment to hand in,
      ;; choose to manage his/her account, log out.
      ;; If the student doesn't have (a) valid homework partner(s) then it
      ;; will redirect to the Manage Account page.
      (define make-q-selection
        (lambda (q-manage-account q-select-grader q-login q-upload)
          (lambda (acct)
            (let ([partners (find-partners acct)])
              (cond
                [(< (length partners) min-partners)
                 (get-need-partner-req) ;; redirect to the Manage Account Page.
                 (q-manage-account acct partners)]
                [(not (has-grader? acct (read-graders)))               
                 (q-select-grader acct partners)] ;; redirect to the choose grader page.
                [else
                 (let ([assmts (read-assignment-list acct)]) ;; Note: read-assignment-list returns a (listof full-assignment)
                   (let ([request (get-select-assignment-req acct partners assmts)])
                     (cond
                       [(select-assignment-req-manage? request)
                        (q-manage-account acct partners)]
                       [(select-assignment-req-logout? request) (q-login initial-login-message)]
                       [else
                        (q-upload acct partners
                                  (select-assignment (select-assignment-req-assignment request) assmts)
                                  initial-upload-message)])))])))))
      
      (define (has-grader? acct graders)
        (ormap
         (lambda (a-grader)
           (and (full-student-account? acct)
                (string=? (full-student-account-grader acct) a-grader)))
         graders))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-upload: (student-account (listof student-account) full-assignment string ->)
      ;;                (student-account (listof student-account) full-assignment upload-token ->)
      ;;                -> (student-account (listof student-account) full-assignment string ->)
      ;; The student has selected an assignment to hand in and must provide the file to upload
      (define make-q-upload
        (lambda (q-upload q-confirm)
          (lambda (acct partners assmt upload-message)
            (let ([upload-tok (get-upload-token acct partners assmt upload-message)])
              (if (invalid-upload? upload-tok)
                  (q-upload acct partners assmt
                            (get-upload-message upload-tok))
                  (q-confirm acct partners assmt upload-tok))))))
      
      ;; get-upload-token: student-account (listof student-account) homework-assignment string -> upload-token
      ;; get an upload given an assignment selection
      (define get-upload-token
        (lambda (acct partners assmt upload-message)
          (let ([request (get-upload-file-req acct partners (homework-assignment-title assmt) upload-message)])
            (make-upload-token (upload-file-req-definitions request)
                               (upload-file-req-interactions request)))))
      
      ;; get-upload-message: upload-token -> string
      ;; generate a message for the upload page
      (define get-upload-message
        (lambda (tok)
          (cond
            [(and (string=? "" (upload-token-definitions tok))
                  (string=? "" (upload-token-testcases tok)))
             missing-both-uploads-message]
            [(string=? "" (upload-token-definitions tok))
             missing-def-upload-message]
            [else missing-int-upload-message])))
      
      ;; invalid-upload?: upload-token -> boolean
      ;; Are either of the defitions or the tests missing.
      (define invalid-upload?
        (lambda (tok)
          (string=? "" (upload-token-definitions tok))))
      ;          (or (string=? "" (upload-token-definitions tok))
      ;              (string=? "" (upload-token-testcases tok)))))
      
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-confirm: (student-account ->)
      ;;                 (student-account (listof student-account) full-assignment upload-token ->)
      ;;                 (student-account full-assignment ->)
      ;;                 -> (student-account (listof student-account) full-assignment upload-token ->)
      ;; The student has provided the homework file contents and has one last chance to cancel
      ;; Perhaps the student has tampered with the URL or by some other exploit provided
      ;; content for an assignment that is overdue.
      (define make-q-confirm
        (lambda (q-selection q-file-saved q-overdue)
          (lambda (acct partners assmt upload-tok)
            (let ([upload-confirm-request (get-upload-confirm-req acct partners (homework-assignment-title assmt))])
              (cond
                [(cancel-save? upload-confirm-request) (q-selection acct)]
                [(is-current? assmt)
                 (q-file-saved acct partners assmt upload-tok)]
                [else
                 (q-overdue acct assmt)])))))
      
      (define (cancel-save? req)
        (not (upload-confirm-req-confirm? req)))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-overdue: (student-account ->) -> (student-account full-assignment ->)
      ;; The student has tampered with the URL or by some other exploit provided content for an
      ;; assignment that is overdue. 
      (define make-q-overdue
        (lambda (q-selection)
          (lambda (acct assmt)
            (log-sorry-charlie assmt acct)
            (get-sorry-charlie-req (homework-assignment-title assmt))
            (q-selection acct))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-file-saved:(student-account ->)
      ;;                   -> (student-account (listof student-account) full-assignment upload-token ->)
      ;; The student has confirmed the intention to save the homework file upload and will continue
      ;; to the selection page
      (define make-q-file-saved
        (lambda (q-selection)
          (lambda (acct partners assmt upload-tok)
            (save-file-and-continue acct partners assmt upload-tok)
            (q-selection acct))))
      
      ;; get-file-saved-continue-token: student-account (listof student-account) full-assignment upload-token -> request
      ;; save the file for the assignment, student account and file contents
      ;; log the file upload for the assignment and account
      ;; get the continue request given the assignment
      (define save-file-and-continue
        (lambda (acct partners assmt content)
          (save-file-content assmt acct
                             (upload-token-definitions content)
                             (upload-token-testcases content))
          (let ([assmts (read-assignment-list acct)])
            (let ([assmt (select-assignment (homework-assignment-title assmt) assmts)])
              (log-file-upload assmt acct)
              (set-full-assignment-date!
               assmt
               (homework-submission-date acct (full-assignment-dir assmt)))
              (get-upload-complete-req acct partners assmt)))))
      
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-manage-account: (student-account (listof student-account) string ->)
      ;;                        (student-account (listof student-account) ->)
      ;;                        (student-account string ->)
      ;;                        (student-account ->)
      ;;                        -> (student-account (listof student-account ->)
      (define make-q-manage-account
        (lambda (q-pass-entry q-select-grader q-new-partner q-login q-selection)
          (lambda (acct partners)
            (let ([req (get-manage-account-req acct partners max-partners)])
              (cond
                [(manage-account-req-changepass? req) (q-pass-entry acct partners initial-changepass-message)]
                ;[(manage-account-req-choose-grader? req) (q-select-grader acct partners)]
                [(and (manage-account-req-addpartner? req)
                      (< (length partners) 3)) ;; double check in case a clever student manufactures the bindings.
                 (q-new-partner acct initial-login-message)]
                [(manage-account-req-logout? req) (q-login initial-login-message)]
                [else (q-selection acct)])))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-select-grader: (student-account (listof student-account) ->) -> (student-account (listof student-account) ->)
      ;; The student can choose a grader
      (define make-q-select-grader
        (lambda (q-manage-account)
          (lambda (acct partners)
            (let ([new-grader 
                   (grader-selection-req-grader (apply get-grader-selection-req (get-grader-selection-args acct)))])
              (update-partners! (cons acct partners) new-grader (full-student-account-group-id acct))
              (q-manage-account acct partners)))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-new-partner: (student-account string ->) (student-account (listof student-account) ->)
      ;;                     --> (student-account string ->)
      ;; The first student has chosen to add a partner (second student.)
      ;; The second student must now login in order to be added
      (define make-q-new-partner
        (lambda (q-new-partner q-manage-account)
          (lambda (acct1 message)
            (let ([req (get-login-req/partner message)])
              (let ([acct2 (request->account (login-req-login-name req)
                                                     (login-req-password req))])
                (cond
                  [(not acct2)
                   (q-new-partner acct1 invalid-credentials-message)]
                  [(staff-account? acct2)
                   (q-new-partner acct1 invalid-credentials-message)]
                  [else
                   (let ([acct2-partners (find-partners acct2)]
                         [acct1-partners (find-partners acct1)])
                     (cond
                       [(< (+ (length acct1-partners) 
                              (length acct2-partners)) max-partners)
                        (update-partners! `(,acct1 ,acct2 ,@acct1-partners ,@acct2-partners)
                                          (full-student-account-grader acct1)
                                          (full-student-account-group-id acct1))
                        (q-manage-account acct1 `(,acct2 ,@acct1-partners ,@acct2-partners))]
                       [else
                        (get-partner-already-paired-req acct2 acct2-partners)
                        (q-manage-account acct1 acct1-partners)]))]))))))
      
      ; **************************************************
      ;; **************************************************
      ;; make-q-pass-entry: (student-account (listof student-account) string ->)
      ;;                    (student-account (listof student-account) ->)
      ;;                    -> (student-account (listof student-account) string ->)
      ;; The student has successfully logged in and has chosen to change her password
      (define make-q-pass-entry
        (lambda (q-pass-entry q-manage-account)
          (lambda (acct partners message)
            (let ([pass-entry-request (get-pass-entry-req acct partners message)])
              (cond
                [(password-mismatch? pass-entry-request)
                 (q-pass-entry acct partners password-mismatch-message)]
                [else
                 (q-manage-account (update-account-password acct pass-entry-request) partners)])))))
      
      ;; password-mismatch?: request -> boolean
      ;; determine if the passwords match
      (define password-mismatch?
        (lambda (req)
          (not (string=? (pass-entry-req-first req)
                         (pass-entry-req-second req)))))
      
      ;; update-account-password: full-student-account pass-entry -> student-account
      (define update-account-password
        (lambda (acct req)
          (set-full-student-account-password! acct (md5 (pass-entry-req-first req)))
          (save-account acct)
          acct))
      
      ;; ********************************************************************************
      ;; ********************************************************************************
      ;; ********************************************************************************
      ;; staff accounts
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-lobby: (staff-account string ->)
      ;;                     (staff-account string ->)
      ;;                     (staff-account string ->)
      ;;                     (string ->) -> (staff-account ->)
      ;; The staff member has logged in may do one of the following
      ;; 1. change password
      ;; 2. create a student account
      ;; 3. edit a student account
      ;; 4. logout
      (define make-q-staff-lobby
        (lambda (q-staff-change-pass q-staff-create-student q-staff-find-student q-login)
          (lambda (acct)
            (let ([req (get-staff-lobby-req (staff-account-login-name acct))])
              (cond
                [(staff-lobby-req-changepass? req) (q-staff-change-pass acct initial-changepass-message)]
                [(staff-lobby-req-create-student? req) (q-staff-create-student acct "" "" initial-create-student-message)]
                [(staff-lobby-req-edit-student? req) (q-staff-find-student acct initial-student-to-edit-message)]
                [(staff-lobby-req-logout? req) (q-login initial-login-message)]
                [else (error "unrecognized request" req)])))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-change-pass (staff-account string ->) (staff-account ->) -> (staff-account string ->)
      ;; The staff member types a new password twice.
      (define make-q-staff-change-pass
        (lambda (q-staff-change-pass q-staff-lobby)
          (lambda (acct message)
            (let ([req (get-pass-entry-req/staff (staff-account-login-name acct) message)])
              (cond
                [(password-mismatch? req)
                 (q-staff-change-pass acct password-mismatch-message)]
                [else
                 (q-staff-lobby (update-staff-account-password acct req))])))))
      
      ;; update-staff-account-password: staff-account pass-entry -> staff-account
      (define update-staff-account-password
        (lambda (acct req)
          (set-staff-account-password! acct (md5 (pass-entry-req-first req)))
          (save-account acct)
          acct))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-create-student: (staff-account string string string ->)
      ;;                              (staff-account ->)
      ;;                              (staff-account student-account ->)
      ;;                              -> (staff-account string string string ->)
      (define make-q-staff-create-student
        (lambda (q-staff-lobby q-staff-review-new-student)
          (lambda (acct name id message)
            (let ([req (get-simple-student-req/create name id message)])
              (cond
                [(simple-student-req-continue? req)
                 (q-staff-review-new-student acct (simple-student-req-id req) (simple-student-req-name req))]
                [else (q-staff-lobby acct)])))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-review-new-student: (staff-account string string string ->)
      ;;                                  (staff-account ->)
      ;;                                   -> (staff-account string string ->)
      (define make-q-staff-review-new-student
        (lambda (q-staff-create-student q-staff-lobby)
          (lambda (acct id name)
            (let ([req (get-review-simple-student-req/create name id)])
              (cond
                [(review-student-req-create? req)
                 (let ([new-acct (staff-create-new-student-account id name)])
                   (if new-acct
                       (q-staff-lobby acct)
                       (q-staff-create-student acct name id student-id-exists-message)))]
                [(review-student-req-edit? req)
                 (q-staff-create-student acct name id initial-create-student-message)]
                [else (q-staff-lobby acct)])))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-find-student: (staff-account string ->)
      ;;                            (staff-account ->)
      ;;                            (staff-account student-account string ->)
      ;;                            (staff-account string full-student-account full-student-account boolean boolean ->)
      ;;                             -> (staff-account string ->)
      (define make-q-staff-find-student
        (lambda (q-staff-find-student q-staff-lobby q-staff-edit-student q-staff-edit-full-student)
          (lambda (acct message)
            (let ([req (get-student-to-edit-req message)])
              (let ([s-acct (lookup-student-account/id (student-to-edit-req-id req) (read-accounts))])
                (cond
                  [(not (student-to-edit-req-continue? req))
                   (q-staff-lobby acct)]
                  [(full-student-account? s-acct)
                   (q-staff-edit-full-student acct
                                              initial-edit-student-message
                                              s-acct s-acct #f #f)]
                  [(student-account? s-acct)
                   (q-staff-edit-student acct s-acct
                                         (student-account-name s-acct)
                                         (student-account-id s-acct)
                                         initial-edit-student-message)]
                  [else (q-staff-find-student acct cant-find-students-id-message)]))))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-edit-student: (staff-account student-account string string string ->)
      ;;                            (staff-account ->)
      ;;                            (staff-account student-account student-account ->)
      ;;                            -> (staff-account student-account string string string ->)
      (define make-q-staff-edit-student
        (lambda (q-staff-edit-student q-staff-lobby q-staff-review-student)
          (lambda (acct s-acct name id message)
            (let ([req (get-simple-student-req/edit name id message)])
              (cond
                [(simple-student-req-continue? req)
                 (q-staff-review-student
                  acct
                  s-acct
                  (make-student-account (simple-student-req-name req) (simple-student-req-id req)))]
                [else (q-staff-lobby acct)])))))      
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-review-student: (staff-account ->)
      ;;                              (staff-account student-account string ->)
      ;;                              -> (staff-account student-account student-account ->)
      (define make-q-staff-review-student
        (lambda (q-staff-lobby q-staff-edit-student)
          (lambda (acct old-s-acct new-s-acct)
            (let ([req (get-review-simple-student-req/edit (student-account-name new-s-acct)
                                                        (student-account-id new-s-acct))])
              (cond
                [(review-student-req-cancel? req) (q-staff-lobby acct)]
                [(review-student-req-edit? req)
                 (q-staff-edit-student acct old-s-acct
                                       (student-account-name new-s-acct)
                                       (student-account-id new-s-acct)
                                       initial-edit-student-message)]
                [(string=? (student-account-id old-s-acct)
                           (student-account-id new-s-acct))
                 (save-account new-s-acct)
                 (q-staff-lobby acct)]
                [else
                 (let ([s-acct (staff-create-new-student-account (student-account-id new-s-acct)
                                                                 (student-account-name new-s-acct))])
                   (cond
                     [s-acct
                      (staff-delete-account (student-account-id old-s-acct))
                      (q-staff-lobby acct)]
                     [else
                      (q-staff-edit-student acct old-s-acct
                                            (student-account-name new-s-acct)
                                            (student-account-id new-s-acct)
                                            student-id-exists-message)]))])))))
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-edit-full-student:
      ;; (staff-account string full-student-account full-student-account boolean boolean ->)
      ;; (staff-account ->)
      ;; (staff-account full-student-account full-student-account ->)
      ;; -> (staff-account string full-student-account full-student-account boolean boolean ->)
      (define make-q-staff-edit-full-student
        (lambda (q-staff-edit-full-student q-staff-lobby q-staff-review-full-student)
          (lambda (acct message s-acct-old s-acct-new clear-password? clear-partnership?)
            (let ([req (get-full-student-req/edit
                        message
                        (read-graders)
                        s-acct-new
                        (find-partners s-acct-old) clear-password? clear-partnership?)])
              (cond
                [(full-student-req/edit-continue? req)
                 (q-staff-review-full-student
                  acct
                  s-acct-old
                  (make-full-student-account
                   (full-student-req/edit-name req)
                   (full-student-req/edit-id req)
                   (full-student-req/edit-login-name req)
                   (if (full-student-req/edit-clear-password? req)
                       (md5 (full-student-req/edit-id req))
                       (full-student-account-password s-acct-old))
                   (full-student-req/edit-grader req)
                   (if (full-student-req/edit-clear-partnership? req)
                       #t
                       (full-student-account-group-id s-acct-old))))]
                [else 
                 (q-staff-lobby acct)
                      
                      ])))))
                  
      
      ;; **************************************************
      ;; **************************************************
      ;; make-q-staff-review-full-student: (staff-account ->)
      ;;                                   (staff-account string (listof string) full-student-account full-student-account boolean boolean ->)
      ;;                                   -> (staff-account full-student-account full-student-account ->)
      ;; NOTE: if group-id of s-acct-new is #t, then clear the partnership.
      (define make-q-staff-review-full-student
        (lambda (q-staff-lobby q-staff-edit-full-student)
          (lambda (acct s-acct-old s-acct-new)
            (let ([clear-password? (not (string=? (full-student-account-password s-acct-old)
                                                  (full-student-account-password s-acct-new)))]
                  [clear-partnership? (boolean? (full-student-account-group-id s-acct-new))])
              (let ([req (get-review-full-student-req/edit
                          s-acct-new 
                          (find-partners s-acct-old)
                          clear-password?
                          clear-partnership?)])
                (let ([call-edit (lambda (message)
                                   (q-staff-edit-full-student acct
                                              message
                                              s-acct-old s-acct-new
                                              clear-password?
                                              clear-partnership?))])
                  
                  ;; NOTE: Essentially just using the group-id field of the structure as a parameter.
                  ;;       Yuck! This is bad. I'm a fucking C-programmer. Shame on me.
                  (set-full-student-account-group-id! s-acct-new (full-student-account-group-id s-acct-old))
                  (cond
                    [(review-student-req-cancel? req)
                     (q-staff-lobby acct)]
                    [(review-student-req-edit? req) (call-edit initial-edit-student-message)]
                    [(string=? (student-account-id s-acct-old)
                               (student-account-id s-acct-new))
                     (cond
                       [(staff-attempt-save-full-student (student-account-id s-acct-old) s-acct-new)
                        (and clear-partnership? (staff-clear-partnership s-acct-new))
                        (q-staff-lobby acct)]
                       [else (call-edit login-name-exists-message)])]
                    [else
                     (let ([s-acct (staff-create-new-student-account (student-account-id s-acct-new)
                                                                     (student-account-name s-acct-new))])
                       (if s-acct
                           (cond
                             [(staff-attempt-save-full-student (student-account-id s-acct-old) s-acct-new)
                              (and clear-partnership? (staff-clear-partnership s-acct-new))
                              (q-staff-lobby acct)]
                             [else
                              (staff-delete-account (student-account-id s-acct))
                              (call-edit login-name-exists-message)])
                           (call-edit student-id-exists-message)))])))))))
      ))
  )