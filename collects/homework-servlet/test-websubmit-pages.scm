(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "cgi.ss" "net")

         "page-tester.scm"
         "websubmit-strings.scm"
         "websubmit-sig.scm"
         "websubmit-structs.scm"
         "parsing.scm"
         "websubmit-pages.scm"
         
         )

(define assignments
  (map
   (lambda (expr)
     (let ([assmt (parse-homework-assignment-expr expr)])
       (make-full-assignment
        (homework-assignment-title assmt)
        (homework-assignment-url assmt)
        (homework-assignment-due-date assmt)
        "this is a string"
        17
        (current-seconds))))
   '((assignment (title "Mission Impossible"
                        "http://www.google.com/search?hl=en&lr=&ie=UTF-8&oe=UTF-8&safe=off&q=%22Mission+Impossible%22&btnG=Google+Search")
                 (due-date (year 2002)
                           (month 5)
                           (day 2)))
     
     (assignment (title "Manhattan Project")
                 (due-date
                  (year 1945)
                  (month 8)
                  (day 6)))
     
     (assignment (title "Alan Parsons Project"
                        "http://www.google.com/search?hl=en&ie=UTF-8&oe=UTF-8&q=%22Alan+Parsons+Project%22&btnG=Google+Search")
                 (due-date
                  (year 2010)
                  (month 7)
                  (day 4)))
     
     (assignment (title "Final Project")
                 (due-date
                  (year 2010)
                  (month 5)
                  (day 22))))))
   
(define partners
  (list
   (make-full-student-account "Mary" "000000001" "mary" "secret" "Newton" 'foo)
   (make-full-student-account "John" "000000002" "john" "secret" "Newton" 'foo)
   (make-full-student-account "Sally" "000000003" "sally" "secret" "Newton" 'foo)))

(define hypothetical-assmt
  (let ([assmt 
         (parse-homework-assignment-expr
          '(assignment (title "Manhattan Project")
                       (due-date
                        (year 1945)
                        (month 8)
                        (day 6))))])
    (make-full-assignment
     (homework-assignment-title assmt)
     (homework-assignment-url assmt)
     (homework-assignment-due-date assmt)
     "this is a string"
     17
     (current-seconds))))


(define test@
  (unit/sig ()
    (import websubmit-pages^ test-pages^)
    
    (test-pages-macro
     ([staff-lobby-title (lambda () (get-staff-lobby-req "Phileas Fogg"))]
      [create-new-student-title
       (lambda () (get-simple-student-req/create "Passporteau" "000000001" initial-create-student-message))]
      [review-new-student-title
       (lambda () (get-review-simple-student-req/create "Passporteau" "000000001"))]
      [get-student-to-edit-title
       (lambda () (get-student-to-edit-req initial-student-to-edit-message))]
      [edit-student-title
       (lambda ()
         (get-full-student-req/edit initial-edit-student-message
                                        (list "Gauss" "Euler" "Newton" "Leibniz" "Cantor" "Cauchy")
                                        (car partners) (cdr partners) #f #f))]
      [edit-student-title
       (lambda ()
         (get-full-student-req/edit initial-edit-student-message
                                        (list "Gauss" "Euler" "Newton" "Leibniz" "Cantor" "Cauchy")
                                        (car partners) (cdr partners) #t #t))]
      [review-edit-student-title
       (lambda () (get-review-full-student-req/edit (car partners) (cdr partners) #t #t))]
      [review-edit-student-title
       (lambda () (get-review-full-student-req/edit (car partners) (cdr partners) #f #f))]
      [staff-change-pass-title
       (lambda () (get-pass-entry-req/staff "Phileas Fogg" initial-changepass-message))]
      
      [create-account-title
       (lambda ()
         (get-create-account-req initial-create-account-message))]
      [choose-credentials-title
       (lambda ()
         (get-choose-credentials-req (car partners) initial-choose-credentials-message))]
      [choose-credentials-title
       (lambda ()
         (get-choose-credentials-req (car partners) password-mismatch-message "a-username"))]
      [add-or-proceed-title
       (lambda () (get-add-partner-or-proceed-req (car partners)))]
      [account-summary-title
       (lambda ()
         (get-account-summary-req (car partners) '() "" 1))]
      [account-summary-title
       (lambda ()
         (get-account-summary-req (car partners) '() "A-TA" 1))]
      [account-summary-title
       (lambda ()
         (get-account-summary-req (car partners) (cdr partners) "" 1))]
      [account-summary-title
       (lambda ()
         (get-account-summary-req (car partners) (cdr partners) "A-TA" 1))]
      [account-summary-title
       (lambda ()
         (get-account-summary-req (car partners) '() "A-TA" 0))]
      
      [select-grader-title
       (lambda ()
         (get-grader-selection-req (list "Mary" "John" "Susan" "Tim" "Sally") "John" (format current-grader-info "John")))]
      [select-grader-title
       (lambda ()
         (get-grader-selection-req (list "Mary" "John" "Susan" "Tim" "Sally") "no-grader" no-current-grader-info))]
      [web-submission-login-title
       (lambda () (get-login-req/initial initial-login-message))]
      [partner-login-title
       (lambda () (get-login-req/partner initial-partner-login-message))]
      [partner-already-paired-title
       (lambda () (get-partner-already-paired-req (car partners) (cdr partners)))]
      [need-partner-title
       get-need-partner-req]
      [select-assignment-title
       (lambda () (get-select-assignment-req (car partners) (cdr partners) assignments))]
      
      [manage-account-title
       (lambda () (get-manage-account-req (car partners) '() 3))]
      [manage-account-title
       (lambda () (get-manage-account-req (car partners) (cddr partners) 3))]
      [manage-account-title
       (lambda () (get-manage-account-req (car partners) (cdr partners) 3))]
      
      [change-pass-title
       (lambda () (get-pass-entry-req (car partners) (cdr partners) initial-changepass-message))]
      [upload-homework-title
       (lambda () (get-upload-file-req (car partners) (cdr partners) "Hypothetical Assignment Name" initial-upload-message))]
      [upload-confirm-title
       (lambda () (get-upload-confirm-req (car partners) (cdr partners) "Hypothetical Assignment Name"))]
      [upload-complete-title
       (lambda () (get-upload-complete-req (car partners) (cdr partners) hypothetical-assmt))]
      [sorry-charlie-title
       (lambda () (get-sorry-charlie-req "Hypothetical Assignment Name"))]))
    
    ))


(define course@
  (unit/sig course-specific^
    (import)
    (define course-title "Introduction to Programming and Computing")
    (define course-number "CSU211")))
    
(invoke-unit/sig
 (compound-unit/sig
   (import (SVT : servlet^))
   (link (CRS : course-specific^ (course@))
         (WSP : websubmit-pages^ (websubmit-pages@ CRS SVT))
         (PT : test-pages^ (test-pages@ SVT))
         (TST : () (test@ WSP PT)))
   (export (open WSP)))
 servlet^)

