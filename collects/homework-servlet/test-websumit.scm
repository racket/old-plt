(require (lib "servlet-sig.ss" "web-server")
         "md5.ss"
         "websubmit-sig.scm"
         "websubmit-test-harness.scm"
         "websubmit-machine.scm"
         "parsing.scm"
         "websubmit-lib.scm"
         )

(define config@
  (unit/sig websubmit-configuration^
    (import)
    (define min-partners 1)
    (define max-partners 2)
    
    
    ;; QUICK FIX WAS HERE: (define min-partners 0)
    ))

(define dummy-websubmit-pages@ (dummy-unit-from-sig websubmit-pages^))

(define-values/invoke-unit/sig
 test-machine^
 (compound-unit/sig
   (import)
   (link
    [TS : token-stream^ (token-stream@)]
    [CFG : websubmit-configuration^ (config@)]
    [DWP : websubmit-pages^ (dummy-websubmit-pages@ TS)]
    [WSM : websubmit-machine^ (websubmit-machine@ CFG DWP)])
   (export (var (WSM start-machine))
           (var (TS reset-token-stream!))
           (var (TS get-result)))))

;; run-test: (listof request-struct) -> boolean
(define run-test
  (lambda (ls)
    (reset-token-stream! ls)
    (with-handlers ([exn-finished? (lambda (exn) (void))])
      (start-machine))
    (equal? ls (get-result))))

(define the-test-directory (build-path "/Users" "gregp" "projects" "websubmit-test"))

(current-directory the-test-directory)

;; ********************************************************************************
;; some tests for websubmit-lib.scm

;; dupe-test: (listof student-account) -> boolean
;; test with various kinds of duplicates
(define (dupe-test accts grep-str)
  (save-all-accounts accts)
  (let ([str
         (with-handlers ([exn? (lambda (the-exn) (exn-message the-exn))])
           (read-accounts))])
    (and (regexp-match grep-str str)
         (regexp-match "duplicate" str)
         #t)))

(dupe-test (list
            (make-student-account "one" "000000001")
            (make-student-account "two" "000000001"))
           "000000001")

(dupe-test (list
            (make-full-student-account "Just a Name" "000000006" "same-login" (md5 "4tlotw") "A-TA" 'foo)
            (make-full-student-account "Just a Name" "000000007" "same-login" (md5 "4tlotw") "A-TA" 'foo))
           "same-login")

(dupe-test (list
            (make-full-student-account "Just a Name" "000000006" "same-login" (md5 "4tlotw") "A-TA" 'foo)
            (make-staff-account "same-login" (md5 "who cares")))
           "same-login")

;; ********************************************************************************
;; tests for websubmit-machine

(define lil-test-database
  (list
   (make-student-account "create-without-partner"   "000000001")
   (make-student-account "create-with-one-partner"  "000000002")
   (make-student-account "create-with-one-partner2" "000000003")
   (make-student-account "create-with-two-partners" "000000004")
   (make-student-account "attempt-create-with-three-partners" "000000005")
   
   (make-student-account "group-id-coincidence" "000000018")
   (make-student-account "group-id-coincidence" "000000019")
   (make-student-account "first-new-account" "000000020")
   (make-student-account "second-new-account" "000000021")
   
   (make-full-student-account "account-without-partner1" "000000006" "account-without-partner1" (md5 "4tlotw") "A-TA" 'account-without-partner1)
   (make-full-student-account "account-without-partner2" "000000007" "account-without-partner2" (md5 "4tlotw") "A-TA" 'account-without-partner2)
   (make-full-student-account "account-without-partner3" "000000008" "account-without-partner3" (md5 "4tlotw") "A-TA" 'account-without-partner3)
   (make-full-student-account "account-without-partner4" "000000009" "account-without-partner4" (md5 "4tlotw") "A-TA" 'account-without-partner4)
   (make-full-student-account "account-without-partner5" "000000010" "account-without-partner5" (md5 "4tlotw") "A-TA" 'account-without-partner5)
   (make-full-student-account "account-without-partner6" "000000011" "account-without-partner6" (md5 "4tlotw") "A-TA" 'account-without-partner6)

   (make-full-student-account "account-with-two-partners1" "000000012" "account-with-two-partners1" (md5 "4tlotw") "A-TA" 'account-with-two-partners1)
   (make-full-student-account "account-with-two-partners1-partner" "000000013" "account-with-two-partners1-partner" (md5 "4tlotw") "A-TA" 'account-with-two-partners1)
   
   (make-full-student-account "account-with-two-partners2" "000000014" "account-with-two-partners2" (md5 "4tlotw") "A-TA" 'account-with-two-partners2)
   (make-full-student-account "account-with-two-partners2-partner" "000000015" "account-with-two-partners2-partner" (md5 "4tlotw") "A-TA" 'account-with-two-partners2)
   

   (make-full-student-account "has-partner-needs-grader1" "000000016" "has-partner-needs-grader1" (md5 "4tlotw") "no-grader" 'has-partner-needs-grader1)
   (make-full-student-account "has-partner-needs-grader2" "000000017" "has-partner-needs-grader2" (md5 "4tlotw") "no-grader" 'has-partner-needs-grader1)
   
   (make-full-student-account "has-partner-and-grader1" "000000022" "has-partner-and-grader1" (md5 "4tlotw") "A-TA" 'has-partner-and-grader)
   (make-full-student-account "has-partner-and-grader2" "000000023" "has-partner-and-grader2" (md5 "4tlotw") "A-TA" 'has-partner-and-grader)
   
   (make-student-account "will-attempt-to-add-staff1" "000000200")
   (make-full-student-account "willie1" "000000201" "will-attempt-partner" (md5 "4tlotw") "A-TA" 'will)
   (make-full-student-account "willie2" "000000202" "will-attempt2" (md5 "4tlotw") "A-TA" 'willie2)
   (make-full-student-account "willie3" "000000203" "will-attempt3" (md5 "4tlotw") "A-TA" 'willie3)
   
   ;; used for staff account tests
   (make-student-account "simple-student" "000000107")
   (make-full-student-account "full-student1" "000000108" "log-me-in1" (md5 "4tlotw") "A-TA" 'full-student)
   (make-full-student-account "full-student2" "000000109" "log-me-in2" (md5 "4tlotw") "A-TA" 'full-student)
   
   (make-staff-account "staff-infection" (md5 "4tlotw"))
   
   ))

(save-all-accounts lil-test-database)

(call-with-output-file "graders"
  (lambda (o-port)
    (write (list "A-TA" "B-TA" "C-TA" "D-TA") o-port))
  'replace)


(define change-pass-request (make-manage-account-req #t #f #f))
(define manage-account-logout-request (make-manage-account-req #f #f #t))
(define add-partner-request (make-manage-account-req #f #t #f))
(define choose-grader-request (make-manage-account-req #f #f #f))
(define back-to-selection-request (make-manage-account-req #f #f #f))
(define select-assignment-logout (make-select-assignment-req #f #f #t))
(define manage-account-request (make-select-assignment-req #f #t #f))


;; check-accounts: (listof string) string -> boolean
;; check for existence of accounts based on name and password
(define check-accounts
  (lambda (acct-names password)
    (let ([accts (read-accounts)])
      (and
       (andmap
        (lambda (acct-name)
          (lookup-account acct-name password accts))
        acct-names)
       #t))))

;; check-accounts/id (listof (cons string string)) -> boolean
;; check for existence of accounts based on id
(define check-accounts/id
  (lambda (ids)
    (let ([accts (read-accounts)])
      (and
       (andmap
        (lambda (id)
          (lookup-student-account/id id accts))
        ids)
       #t))))

;; check-account/id/name: string string -> boolean
;; check for the existence of a single account with a give name and id
(define check-account/id/name
  (lambda (id name)
    (let ([accts (read-accounts)])
      (let ([acct (lookup-student-account/id id accts)])
        (and acct
             (string=? (student-account-name acct) name))))))

;; check-partnerships: (listof string) (string) -> boolean
(define check-partnerships
  (lambda (acct-names password)
    (let* ([accts (read-accounts)]
           [acct (lookup-account (car acct-names) password accts)]
           [group-id (full-student-account-group-id acct)]
           [grader (full-student-account-grader acct)])
      (and
       (andmap
        (lambda (acct-name)
          (let ([acct (lookup-account acct-name password accts)])
            (and (eqv? group-id (full-student-account-group-id acct))
                 (string=? grader (full-student-account-grader acct)))))
        acct-names)
       #t))))


;; **************************************************************************
;; Rizza's bug: New student get's the same group name as a different student.
;; **************************************************************************
;; create-account --> valid-id --> valid-credentials --> proceed --> continue
(run-test
 (list
  (make-login-req/initial "name" "pass" #t)
  (make-create-account-req "000000018")
  (make-choose-credentials-req "group-id-coincidence1" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #f)
  (make-account-summary-req)))

(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000019")
  (make-choose-credentials-req "group-id-coincidence2" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #f)
  (make-account-summary-req)))

;; check some post conditions
(check-accounts (list "group-id-coincidence1" "group-id-coincidence2") "4tlotw")


;; ********************************************************************************
;; ********************************************************************************
;; Some error cases

;; First Create an Account 
;; ********************************************************************************
;; create-account --> valid-id --> valid-credentials --> proceed --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000020")
  (make-choose-credentials-req "first-new-account" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #f)
  (make-account-summary-req)))


;; Try to create it again
;; ********************************************************************************
;; create-account --> already-exists --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000020")
  (make-already-exists-req)))

;; Try a blank name and pass. Try creating the account again just for fun.
;; ********************************************************************************
;; blank-name-and-pass --> create-account --> already-exists --> continue
(run-test
 (list
  (make-login-req/initial "" "" #f)
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000020")
  (make-already-exists-req)))


;; ********************************************************************************
;; create-account --> no-such-id --> valid-id --> valid-credentials --> proceed
;; --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "666666666")
  (make-create-account-req "000000021")
  (make-choose-credentials-req "second-new-account" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #f)
  (make-account-summary-req)))

(check-accounts (list "first-new-account" "second-new-account") "4tlotw")


;; ********************************************************************************
;; ********************************************************************************
;; ADDING PARTNERS DURING ACCOUNT CREATION

;; Create four accounts.
;; The first account does not add a partner.
;; The second account adds the first as a partner during creation.
;; The third account adds the first two as partners during creation.
;; The fourth account attempts to add the first three as partners during creation, but fails.

;; Create First Acount:
;; create-account --> valid-id --> valid-credentials --> proceed --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000001")
  (make-choose-credentials-req "create-without-partner" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #f)
  (make-account-summary-req)))

;; Create Second Account:
;; ********************************************************************************
;; create-account --> valid-id --> valid-credentials --> add-partner --> valid-login
;; --> choose-grader --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000002")
  (make-choose-credentials-req "create-with-one-partner" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #t)
  (make-login-req/partner "create-without-partner" "4tlotw")
  (make-grader-selection-req "A-TA")
  (make-account-summary-req)))

(check-partnerships
 '("create-without-partner" "create-with-one-partner") "4tlotw")

;; Create Third Account
;; ********************************************************************************
;; create-account --> valid-id --> valid-credentials --> add-partner --> valid-login
;; --> choose-grader --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000004")
  (make-choose-credentials-req "create-with-two-partners" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #t)
  (make-login-req/partner "create-with-one-partner" "4tlotw")
  (make-grader-selection-req "A-TA")
  (make-account-summary-req)))

(check-partnerships
 '("create-without-partner" "create-with-one-partner" "create-with-two-partners") "4tlotw")


;; Create Fourth Account
;; ********************************************************************************
;; create-account --> valid-id --> valid-credentials 
;; --> add-partner --> valid-login --> already-has-partner
;; --> add-partner --> valid-login --> already-has-partner
;; --> add-partner --> valid-login --> already-has-partner
;; --> proceed --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000005")
  (make-choose-credentials-req "attempt-create-with-three-partners" "4tlotw" "4tlotw")
  
  (make-add-partner-or-proceed-req #t)
  (make-login-req/partner "create-without-partner" "4tlotw")
  (make-partner-already-paired-req)
  
  (make-add-partner-or-proceed-req #t)
  (make-login-req/partner "create-with-one-partner" "4tlotw")
  (make-partner-already-paired-req)
  
  (make-add-partner-or-proceed-req #t)
  (make-login-req/partner "create-with-two-partners" "4tlotw")
  (make-partner-already-paired-req)
  
  (make-add-partner-or-proceed-req #f)
  (make-account-summary-req)
  ))

(not (check-partnerships
      '("create-without-partner" "attempt-create-with-three-partners") "4tlotw"))


;; *********************************************************
;; MORE ADDING PARTNERS DURING ACCOUNT CREATION: ERROR CASES

;; Precondition: The database contains an account that has no partner: account-without-partner1
;; Create a new account and make some mistakes when trying to add a partner
  
;; Invalid Partner Login
;; ********************************************************************************
;; create-account --> valid-id --> valid-credentials --> add-partner --> invalid-login
;; --> valid-login --> choose-grader --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000003")
  (make-choose-credentials-req "create-with-one-partner2" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #t)
  (make-login-req/partner "this-account-name-doesnt-exist" "foo")
  (make-login-req/partner "" "foo")
  (make-login-req/partner " " "foo")
  (make-login-req/partner "account-without-partner1" "foo")
  (make-login-req/partner "account-without-partner1" " ")
  (make-login-req/partner "account-without-partner1" "")
  (make-login-req/partner "account-without-partner1" "4tlotw")
  (make-grader-selection-req "A-TA")
  (make-account-summary-req)))

(check-partnerships
 '("account-without-partner1" "create-with-one-partner2") "4tlotw")


;; student attempts to add staff
;; ********************************************************************************
;; create-account --> valid-id --> valid-credentials --> add-partner --> login (staff credentials)
;; --> valid-login --> choose-grader --> continue
(run-test
 (list
  (make-login-req/initial #f #f #t)
  (make-create-account-req "000000200")
  (make-choose-credentials-req "will-attempt" "4tlotw" "4tlotw")
  (make-add-partner-or-proceed-req #t)
  (make-login-req/partner "staff-infection" "4tlotw")
  (make-login-req/partner "will-attempt-partner" "4tlotw")
  (make-grader-selection-req "A-TA")
  (make-account-summary-req)))

(check-partnerships
 (list "will-attempt" "will-attempt-partner") "4tlotw")

;; ********************************************************************************
;; ********************************************************************************
;; ********************************************************************************
;; Account without partners (after creation)

;; Precondition: database has an account which has no partners: account-without-partner2

;; Test a bad login and a good login and then logout
;; ********************************************************************************
;; bad-login -->  good-login --> need-partner-continue --> logout
;;
(run-test
 (list
  (make-login-req/initial "foo" "bar" #f)
  (make-login-req/initial "account-without-partner2" "4tlotw" #f)
  (make-need-partner-req)
  manage-account-logout-request))

;; Change password and stuff.
;; ********************************************************************************
; good-login --> need-partner-continue --> change-password --> mismatch --> valid --> logout
; --> good-login --> need-partner-continue --> change-password --> valid --> logout
; --> good-login --> need-partner-continue --> logout
;
(run-test
 (list
  (make-login-req/initial "account-without-partner2" "4tlotw" #f)
  (make-need-partner-req)
  change-pass-request
  (make-pass-entry-req "foo" "bar")
  (make-pass-entry-req "foo" "foo")
  manage-account-logout-request
  (make-login-req/initial "account-without-partner2" "foo" #f)
  (make-need-partner-req)
  change-pass-request
  (make-pass-entry-req "4tlotw" "4tlotw")
  manage-account-logout-request
  (make-login-req/initial "account-without-partner2" "4tlotw" #f)
  (make-need-partner-req)
  manage-account-logout-request
  ))

;; ********************************************************************************
;; ********************************************************************************
;; ********************************************************************************
;; TEST PARTNERING AFTER ACCOUNT CREATION

;; precondition: database has four accounts:
;; account-without-partner3
;; account-without-partner4
;; account-without-partner5
;; account-without-partner6

;; Add account-without-partner3 to account-without-partner4 (partnership will contain 2)
;; ********************************************************************************
;; good-login --> need-partner-continue --> add-partner --> valid --> logout
;;
(run-test
 (list
  (make-login-req/initial "account-without-partner4" "4tlotw" #f)
  (make-need-partner-req)
  add-partner-request
  (make-login-req/partner "account-without-partner3" "4tlotw")
  manage-account-logout-request))

(check-partnerships (list "account-without-partner3" "account-without-partner4") "4tlotw")

;; Add account-without-partner5 to account-without-partner4 (partnership will contain 3)
;; ********************************************************************************
;; good-login --> manage-account --> add-partner --> valid --> logout
;;
(run-test
 (list
  (make-login-req/initial "account-without-partner4" "4tlotw" #f)
  manage-account-request
  add-partner-request
  (make-login-req/partner "account-without-partner5" "4tlotw")
  manage-account-logout-request))

(check-partnerships (list "account-without-partner3"
                          "account-without-partner4"
                          "account-without-partner5") "4tlotw")

;; Attempt to add account-without-partner[3, 4, 5] to account-without-partner6 (partnership cannot contain 4)
;; ********************************************************************************
;; good-login --> need-partner-continue 
;; --> add-partner --> login --> has-partners
;; --> add-partner --> login --> has-partners
;; --> add-partner --> login --> has-partners
;; --> continue --> logout
(run-test
 (list
  (make-login-req/initial "account-without-partner6" "4tlotw" #f)
  (make-need-partner-req)
 
  add-partner-request
  (make-login-req/partner "account-without-partner3" "4tlotw")
  (make-partner-already-paired-req)
  
  add-partner-request
  (make-login-req/partner "account-without-partner4" "4tlotw")
  (make-partner-already-paired-req)
  
  add-partner-request
  (make-login-req/partner "account-without-partner5" "4tlotw")
  (make-partner-already-paired-req)
  
  manage-account-logout-request))

(not (check-partnerships (list "account-without-partner3" "account-without-partner6") "4tlotw"))
(check-partnerships (list "account-without-partner3"
                          "account-without-partner4"
                          "account-without-partner5") "4tlotw")

;; precondition: database has two accounts:
;; account-with-two-partners1
;; account-with-two-partners2

;; Attempt to add account-with-two-partners2 to account-with-two-partners1 (partnership cannot contain 4)
;; ********************************************************************************
;; good-login --> need-partner-continue
;; --> add-partner --> login --> has-partners
;; --> continue --> logout
(run-test
 (list
  (make-login-req/initial "account-with-two-partners1" "4tlotw" #f)
  manage-account-request
  
  add-partner-request
  (make-login-req/partner "account-with-two-partners2" "4tlotw")
  (make-partner-already-paired-req)
  
  manage-account-logout-request))

(check-partnerships (list "account-with-two-partners1" "account-with-two-partners1-partner") "4tlotw")
(check-partnerships (list "account-with-two-partners2" "account-with-two-partners2-partner") "4tlotw")
(not
 (check-partnerships (list "account-with-two-partners1" "account-with-two-partners2") "4tlotw"))

;; student attempts to add staff
;; ********************************************************************************
;; good-login --> need-partner-continue
;; --> add-partner --> login (staff name and pass) --> can't-add-staff
;; --> continue --> logout
(run-test
 (list
  (make-login-req/initial "will-attempt2" "4tlotw" #f)
  (make-need-partner-req)
  add-partner-request
  (make-login-req/partner "staff-infection" "4tlotw")
  (make-login-req/partner "will-attempt3" "4tlotw")
  
  manage-account-logout-request
  ))

(check-partnerships (list "will-attempt2" "will-attempt3") "4tlotw")


;; ********************************************************************************
;; ********************************************************************************


;; add a grader to has-partner-needs-grader1
;; ********************************************************************************
;; good-login --> choose-grader --> logout
;;
(run-test
 (list
  (make-login-req/initial "has-partner-needs-grader1" "4tlotw" #f)
  (make-grader-selection-req "A-TA")
  manage-account-logout-request))

(let* ([accts (read-accounts)]
       [acct (lookup-account "has-partner-needs-grader1" "4tlotw" accts)])
  (string=? (full-student-account-grader acct) "A-TA"))
  

;; ********************************************************************************
;; ********************************************************************************

;; Precondition: There exist accounts called:
;; has-partner-and-grader1
;; has-partner-and-grader2
;; in the database that each have a partner and a grader.

;; ********************************************************************************
;; good-login --> manage-account --> logout
;;
(run-test
 (list
  (make-login-req/initial "has-partner-and-grader1" "4tlotw" #f)
  manage-account-request
  manage-account-logout-request))

;; ********************************************************************************
;; good-login --> manage-account --> back-to-selection-page --> logout
;;
(run-test
 (list
  (make-login-req/initial "has-partner-and-grader1" "4tlotw" #f)
  manage-account-request
  back-to-selection-request
  select-assignment-logout))

;; ********************************************************************************
;; good-login --> selection --> invalid-upload --> valid-upload --> confirmation --> continue --> logout
;;
(run-test
 (list
  (make-login-req/initial "has-partner-and-grader1" "4tlotw" #f)
  (make-select-assignment-req "Test Assignment" #f #f)
  (make-upload-file-req "" "foo")
  (make-upload-file-req "this is a string" "this is a string")
  (make-upload-confirm-req #t)
  (make-upload-complete-req)
  select-assignment-logout))

;; ********************************************************************************
;; good-login --> selection --> valid-upload --> confirmation --> continue --> logout
;;
(run-test
 (list
  (make-login-req/initial "has-partner-and-grader1" "4tlotw" #f)
  (make-select-assignment-req "Test Assignment" #f #f)
  (make-upload-file-req "this is a string" "this is a string")
  (make-upload-confirm-req #t)
  (make-upload-complete-req)
  select-assignment-logout))

;; ********************************************************************************
;; good-login --> selection --> valid-upload --> confirmation (cancel) --> logout
;;
(run-test
 (list
  (make-login-req/initial "has-partner-and-grader1" "4tlotw" #f)
  (make-select-assignment-req "Test Assignment" #f #f)
  (make-upload-file-req "this is a string" "this is a string")
  (make-upload-confirm-req #f)
  select-assignment-logout))

;; ********************************************************************************
;; good-login --> overdue-selection --> valid-upload --> overdue --> sorry-charlie-continue --> logout
;;
(run-test
 (list
  (make-login-req/initial "has-partner-and-grader1" "4tlotw" #f)
  (make-select-assignment-req "Overdue Assignment" #f #f)
  (make-upload-file-req "this is a string" "this is a string")
  (make-upload-confirm-req #t)
  (make-sorry-charlie-req)
  select-assignment-logout))


;; ********************************************************************************
;; ********************************************************************************
;; ********************************************************************************
;; Staff Pages

(define staff-login (make-login-req/initial "staff-infection" "4tlotw" #f))

(define staff-logout (make-staff-lobby-req #f #f #f #t))
(define staff-change-pass (make-staff-lobby-req #t #f #f #f))
(define staff-create-student (make-staff-lobby-req #f #t #f #f))
(define staff-edit-account (make-staff-lobby-req #f #f #t #f))

(define new-review-edit (make-review-simple-student-req/create #t #f #f))
(define new-review-create (make-review-simple-student-req/create #f #t #f))
(define new-review-cancel (make-review-simple-student-req/create #f #f #t))

(define edit-review-edit (make-review-simple-student-req/edit #t #f #f))
(define edit-review-create (make-review-simple-student-req/edit #f #t #f))
(define edit-review-cancel (make-review-simple-student-req/edit #f #f #t))

(define edit-review-full-edit (make-review-full-student-req/edit #t #f #f))
(define edit-review-full-create (make-review-full-student-req/edit #f #t #f))
(define edit-review-full-cancel (make-review-full-student-req/edit #f #f #t))


;; staff login and logout
;; ********************************************************************************
;; staff-login --> lgout
(run-test
 (list
  staff-login
  staff-logout))

;; logging in and out, changing password
;; ********************************************************************************
;; staff-login --> staff-change-pass --> mismatch --> passwords-match
;; --> logout --> staff-login --> staff-change-pass --> passwords-match
;; --> logout --> staff-login --> logout
(run-test
 (list
  staff-login
  staff-change-pass
  (make-pass-entry-req/staff "foo" "bar")
  (make-pass-entry-req/staff "foo" "foo")
  staff-logout
  
  (make-login-req/initial "staff-infection" "foo" #f)
  staff-change-pass
  (make-pass-entry-req/staff "4tlotw" "4tlotw")
  staff-logout
  staff-login
  staff-logout))

;; create new student, cancel right away
;; ********************************************************************************
;; staff-login --> create-new-student --> cancel --> logout
(run-test
 (list
  staff-login
  staff-create-student
  (make-simple-student-req/create "new name" "000000001" #f)
  staff-logout
  ))

;; create a new student, student exists
;; ********************************************************************************
;; staff-login --> create-new-student --> create(id-exists) --> review-student(create)
;; --> create(cancel) --> logout

;; check precondition
(check-accounts/id (list "000000001"))

(run-test
 (list
  staff-login
  staff-create-student
  (make-simple-student-req/create "new name" "000000001" #t)
  new-review-create
  (make-simple-student-req/create "" "" #f)
  staff-logout))


;; create a new student, but cancel, student doesn't exist
;; ********************************************************************************
;; staff-login --> create-new-student --> create(no-such-id) --> review (cancel)
;; --> logout

;; check precondition
(not (check-accounts/id (list "000000100")))

(run-test
 (list
  staff-login
  staff-create-student
  (make-simple-student-req/create "new name" "000000100" #t)
  new-review-cancel
  staff-logout))

;; check postcondition
(not (check-accounts (list "000000100") "4tlotw"))

;; create a new student, don't cancel
;; ********************************************************************************
;; staff-login --> create-new-student --> create(no-such-id) --> review (create)
;; --> logout

;; check precondition
(not (check-accounts/id (list "000000101")))

(run-test
 (list
  staff-login
  staff-create-student
  (make-simple-student-req/create "a-new-account" "000000101" #t)
  new-review-create
  staff-logout))


;; check postcondition
(check-account/id/name "000000101" "a-new-account")

;; create a new student, edit, then cancel
;; ********************************************************************************
;; staff-login --> create-new-student --> create(no-such-id) --> review (edit)
;; --> cancel --> logout

;; check precondition
(not (check-accounts/id (list "000000102")))

(run-test
 (list
  staff-login
  staff-create-student
  (make-simple-student-req/create "a-new-account" "000000102" #t)
  new-review-edit
  (make-simple-student-req/create "who cares" "000000102" #f)
  staff-logout))

;; check postcondition
(not (check-accounts/id (list "000000102")))


;; create a new student, edit, don't cancel
;; ********************************************************************************
;; staff-login --> create-new-student --> create(no-such-id) --> review (edit)
;; create (edit name) --> review (create) --> logout

;; check precondition
(not (check-accounts/id (list "000000103")))

(run-test
 (list
  staff-login
  staff-create-student
  (make-simple-student-req/create "not-the-name" "000000103" #t)
  new-review-edit
  (make-simple-student-req/create "a-new-account" "000000103" #t)
  new-review-create
  staff-logout))

;; check postcondition
(check-account/id/name "000000103" "a-new-account")

;; create a new student, edit, don't cancel (this time edit id too)
;; ********************************************************************************
;; staff-login --> create-new-student --> create(no-such-id) --> review (edit)
;; --> create(new-id) --> review (create) --> logout

;; check precondition
(not (check-accounts/id (list "000000104")))
(not (check-accounts/id (list "000000105")))

(run-test
 (list
  staff-login
  staff-create-student
  (make-simple-student-req/create "a-new-account" "000000104" #t)
  new-review-edit
  (make-simple-student-req/create "a-new-account" "000000105" #t)
  new-review-create
  staff-logout))

;; check postcondition
(not (check-accounts/id (list "000000104")))
(check-account/id/name "000000105" "a-new-account")

;; edit a student-account, no-such id, cancel
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (no-such-id) --> enter-id (cancel)
;; --> logout

;; check precondition
(not (check-accounts/id (list "000000106")))

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000106" #t)
  (make-student-to-edit-req "000000106" #f)
  staff-logout))


;; edit a student-account, student-account(not full), cancel
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (cancel)
;; --> logout

;; check precondition
(check-account/id/name "000000107" "simple-student")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000107" #t)
  (make-simple-student-req/edit "new-name" "000000107" #f)
  staff-logout))

;; check postcondition
(check-account/id/name "000000107" "simple-student")

;; now actually edit simple-student
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-name)
;; --> review-page(confirm) --> logout

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000107" #t)
  (make-simple-student-req/edit "new-name" "000000107" #t)
  edit-review-create
  staff-logout))

;; check postcondition
(check-account/id/name "000000107" "new-name")

;; attempt to edit new-name's id using an existing id
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-id, exists)
;; --> review-page(confirm) --> edit-page (cancel) --> logout

;; check precondition
(check-accounts/id (list "000000107" "000000001"))

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000107" #t)
  (make-simple-student-req/edit "new-name" "000000001" #t)
  edit-review-create
  (make-simple-student-req/edit "whatever" "foo" #f)
  staff-logout))

;; check postcondition
(check-account/id/name "000000107" "new-name")


;; edit new-name's id using a new id
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-id, unique)
;; --> review (commit) --> logout

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000107" #t)
  (make-simple-student-req/edit "new-name" "000000777" #t)
  edit-review-create
  staff-logout))

;; check postcondition
(check-account/id/name "000000777" "new-name")


;; edit a full-student, but cancel
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists, full) --> edit-page (cancel)
;; --> logout

;; check precondition
(check-account/id/name "000000108" "full-student1")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000108" #t)
  (make-full-student-req/edit "new-name" "000000108" "new-login-name" "new-grader" #f #f #f)
  staff-logout))

;; edit full-student1's name but cancel
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id --> edit-page (change name, confirm)
;; --> review (cancel) --> logout

;; check precondition
(check-account/id/name "000000108" "full-student1")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000108" #t)
  (make-full-student-req/edit "new-name" "000000108" "new-login-name" "new-grader" #f #t #t)
  edit-review-full-cancel
  staff-logout))
  

;; check precondition
(check-account/id/name "000000108" "full-student1")

;; now actually edit full-student1's name
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-name)
;; --> review-page(confirm) --> logout

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000108" #t)
  (make-full-student-req/edit "full-student1-new-name" "000000108" "log-me-in1" "A-TA" #f #f #t)
  edit-review-full-create
  staff-logout))

;; check postcondition
(check-account/id/name "000000108" "full-student1-new-name")

;; now actually edit full-student1's name again
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-name)
;; --> review-page(edit) --> edit-page (change-name) --> review-page (confirm)
;; --> logout

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000108" #t)
  (make-full-student-req/edit "full-student1-new-name2" "000000108" "log-me-in1" "A-TA" #f #f #t)
  edit-review-full-edit
  (make-full-student-req/edit "full-student1-again" "000000108" "log-me-in1" "A-TA" #f #f #t)
  edit-review-full-create
  staff-logout))

;; check postcondition
(check-account/id/name "000000108" "full-student1-again")


;; attempt to edit full-student1's id using an existing id
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-id, exists)
;; --> review-page (confirm) --> edit-page(cancel) --> logout

;; check precondition
(check-accounts/id (list "000000108" "000000001"))

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000108" #t)
  (make-full-student-req/edit "full-student1-again" "000000001" "log-me-in1" "A-TA" #f #f #t)
  edit-review-full-create
  (make-full-student-req/edit "full-student1-again" "000000108" "log-me-in1" "A-TA" #f #f #f)
  staff-logout))

;; check postcondition
(check-account/id/name "000000108" "full-student1-again")

;; edit full-student1's id using a new id
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-id, unique)
;; --> review (commit) --> logout

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000108" #t)
  (make-full-student-req/edit "full-student1-again" "000000888" "log-me-in1" "A-TA" #f #f #t)
  edit-review-full-create
  staff-logout))

;; check postcondition
(check-account/id/name "000000888" "full-student1-again")


;; attempt to edit full-student1's login-name using an existing login-name
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-login-name, exists)
;; --> review (commit) --> edit-page(cancel) --> logout

;; check precondition
(check-accounts (list "log-me-in1" "log-me-in2") "4tlotw")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000888" #t)
  (make-full-student-req/edit "full-student1-again" "000000888" "log-me-in2" "A-TA" #f #f #t)
  edit-review-full-create
  (make-full-student-req/edit "whatever" "000000888" "log-me-in2" "A-TA" #f #f #f)
  staff-logout))

;; check postcondition
(check-accounts (list "log-me-in1" "log-me-in2") "4tlotw")


;; attempt to change full-student1's id to something unique and clobber someone's login-name
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-id, unique; change-login-name, exists)
;; --> review (commit) --> edit-page(cancel) --> logout

;; check precondition
(check-accounts (list "log-me-in1" "log-me-in2") "4tlotw")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000888" #t)
  (make-full-student-req/edit "full-student1-again" "000000999" "log-me-in2" "A-TA" #f #f #t)
  edit-review-full-create
  (make-full-student-req/edit "whatever" "000000888" "log-me-in2" "A-TA" #f #f #f)
  staff-logout))

;; check postcondition
(check-accounts (list "log-me-in1" "log-me-in2") "4tlotw")

;; edit full-student1's login-name using a new login-name
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (change-login-name, unique)
;; --> review (commit) --> logout

;; check precondition (sort of)
(not (check-accounts (list "log-me-in3") "4tlotw")) ;; assumes that "log-me-in3" uses "4tlotw"
(check-accounts (list "log-me-in1") "4tlotw")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000888" #t)
  (make-full-student-req/edit "full-student1-again" "000000888" "log-me-in3" "A-TA" #f #f #t)
  edit-review-full-create
  staff-logout))

;; check postcondition
(check-accounts (list "log-me-in3") "4tlotw")


;; clear a partnership
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (clear partnership)
;; --> review (commit) --> logout

;; check precondition
(check-partnerships (list "log-me-in3" "log-me-in2") "4tlotw")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000888" #t)
  (make-full-student-req/edit "full-student1-again" "000000888" "log-me-in3" "A-TA" #f #t #t)
  edit-review-full-create
  staff-logout))

;; check postcondition
(not (check-partnerships (list "log-me-in3" "log-me-in2") "4tlotw"))
(check-partnerships (list "log-me-in3") "4tlotw")
(check-partnerships (list "log-me-in2") "4tlotw")

;; reset a password
;; ********************************************************************************
;; staff-login --> edit-student --> enter-id (exists) --> edit-page (clear password)
;; --> review (commit) --> logout

;; check precondition
(check-accounts (list "log-me-in3") "4tlotw")

(run-test
 (list
  staff-login
  staff-edit-account
  (make-student-to-edit-req "000000888" #t)
  (make-full-student-req/edit "full-student1-again" "000000888" "log-me-in3" "A-TA" #t #f #t)
  edit-review-full-create
  staff-logout))

;; check postcondition
(check-accounts (list "log-me-in3") "000000888")

