(module parsing mzscheme
  (require (lib "date.ss" "mzlib")
           (lib "match.ss" "mzlib")
	   (lib "url.ss" "net"))
  
  ;;************************************************************
  ;; Data definition
  ;;
  ;; A homework-assignment-expr is an s-expression
  ;;
  ;; homework-assignment-expr ::= ('assignment homework-field-expr ...)
  ;;
  ;; homework-field-expr ::=  ('due-date date-field-expr ...)
  ;;                      |   ('title <string>)
  ;;
  ;;  date-field-expr ::= (year <number>)    ;; e.g. 2003
  ;;                   |  (month <number>)   ;; (1 - 12)
  ;;                   |  (day <number>)     ;; (1 - 31)
  ;;                   |  (hour <number>)    ;; (0 - 23)
  ;;                   |  (minute <number>)  ;; (0 - 59)
  ;;
  ;;
  ;;
  ;; Notes: I choose this syntax in anticipation of future fields not yet
  ;;        specified.
  
  ;; a homework-assignment will be a structure
  ;; (make-homework-assignment string (url | #f) date)
  (define-struct homework-assignment (title url due-date))
  
  ;; If there is any problem parsing any of the configuration data
  (define-struct (exn-homework-config exn) ())
  
  ;; raise-exn-homework-config: format-string [format-args] ->
  ;; raise such an exception
  (define raise-exn-homework-config
    (lambda (msg-format . args)
      (raise (make-exn-homework-config (apply format (cons msg-format args)) (current-continuation-marks)))))
  
  ;; parse-homework-assignment-expr: s-expression -> homework-assignment
  ;; Produce a homework-assignment structure by parsing a homework-assignment-expr
  (define parse-homework-assignment-expr
    (lambda (hw-expr)
      (match hw-expr
        [('assignment fields ...)
         (check-homework-assignment (parse-homework-field-expr
                                     (make-homework-assignment #f #f (make-date 0 59 23 #f #f #f #f #f #f #f))
                                     fields))]
        [else (raise-exn-homework-config "invalid assignment expression: ~a" hw-expr)])))
  
  ;; check-homework-assignment: homework-assignment -> homework-assignment
  ;; Produce provided homework-assignment or bust
  (define check-homework-assignment
    (lambda (hw)
      (unless (string? (homework-assignment-title hw))
        (raise-exn-homework-config "missing title field for assignment expression"))
      (unless (date? (check-date (homework-assignment-due-date hw)))
        (raise-exn-homework-config "missing date field for assignment expression"))
      (make-homework-assignment (homework-assignment-title hw)
				(homework-assignment-url hw)
                                (fixup-date (homework-assignment-due-date hw)))))
  
  ;; check-date: date -> date
  ;; Produce provided date or bust
  (define check-date
    (lambda (dt)
      (unless (number? (date-second dt))
        (raise-exn-homework-config "missing second field for date"))
      (unless (number? (date-minute dt))
        (raise-exn-homework-config "missing minute field for date"))
      (unless (number? (date-hour dt))
        (raise-exn-homework-config "missing hour field for date"))
      (unless (number? (date-day dt))
        (raise-exn-homework-config "missing day field for date"))
      (unless (number? (date-month dt))
        (raise-exn-homework-config "missing month field for date"))
      (unless (number? (date-year dt))
        (raise-exn-homework-config "missing year field for date"))
      dt))
  
  ;; fixup-date: date -> date
  ;; create a date structure from the given structure but with details filled in
  (define fixup-date
    (lambda (dt)
      (seconds->date
       (find-seconds (date-second dt) (date-minute dt) (date-hour dt) (date-day dt) (date-month dt) (date-year dt)))))
  
  ;; parse-homework-field-expr: homework-assignment (listof s-expr) -> homework-assignment
  ;; parse the homework field expressions mutating the provided homework-assignment
  (define parse-homework-field-expr
    (lambda (hw fields)
      (cond
        [(null? fields) hw]
        [else (match (car fields)
                [('title (? string? str)) (set-homework-assignment-title! hw str)]
                [('title (? string? str-title) (? string? str-url))
                 (set-homework-assignment-title! hw str-title)
                 (set-homework-assignment-url! hw (string->url str-url))]
                [('due-date date-fields ...) (parse-date-fields! (homework-assignment-due-date hw) date-fields)]
                [else (raise-exn-homework-config "unrecognized homework field: ~a" (car fields))])
              (parse-homework-field-expr hw (cdr fields))])))
  
  ;; parse-date-fields: date (listof s-expr) ->
  ;; parse the date fields mutating the provided date structure
  (define parse-date-fields!
    (lambda (dt fields)
      (unless (null? fields)
        (match (car fields)
          [('year (? number? yr)) (set-date-year! dt yr)]
          [('month (? (lambda (x)
                        (and (number? x)
                             (<= 1 x 12)))
                      mth)) (set-date-month! dt mth)]
          [('day (? (lambda (x)
                      (and (number? x)
                           (<= 1 x 31))) dy)) (set-date-day! dt dy)]
          [('hour (? (lambda (x)
                       (and (number? x)
                            (<= 0 x 23))) hr)) (set-date-hour! dt hr)]
          [('minute (? (lambda (x)
                         (and (number? x)
                              (<= 0 x 59))) min)) (set-date-minute! dt min)]
          [else (raise-exn-homework-config "unrecognized date field: ~a" (car fields))])
        (parse-date-fields! dt (cdr fields)))))
  
  
  ;; ********************************************************************************
  ;; student-accounts
  
  ;; A student-account is either
  ;; (make-student-account string string) 
  ;; (make-full-student-account string string string string string symbol)
  (define-struct student-account (name id))
  (define-struct (full-student-account student-account) (login-name password grader group-id))
  
  ;; parse-student-account: s-expression -> student-account
  ;; parse a student-account
  (define parse-student-account
    (lambda (s-exp)
      (match s-exp
        [((? string? name) (? string? id)) (make-student-account name id)]
        [((? string? name) (? string? id) (? string? login-name) (? string? password) (? string? grader) (? symbol? group-id))
         (make-full-student-account name id login-name password grader group-id)]
        [else (raise-exn-homework-config "Unable to parse the student account: ~s" s-exp)])))
  
  ;; student-account=?: student-account student-account -> boolean
  (define student-account=?
    (lambda (stud1 stud2)
      (or       
       (and (full-student-account? stud1)
            (full-student-account? stud2)
            (string=? (student-account-name stud1)
                      (student-account-name stud2))
            (string=? (student-account-id stud1)
                      (student-account-id stud2))
            (string=? (full-student-account-login-name stud1)
                      (full-student-account-login-name stud2))
            (string=? (full-student-account-password stud1)
                      (full-student-account-password stud2))
            (string=? (full-student-account-grader stud1)
                      (full-student-account-grader stud2))
            (eqv? (full-student-account-group-id stud1)
                  (full-student-account-group-id stud2)))
       
       (and (student-account? stud1)
            (student-account? stud2)
            (string=? (student-account-name stud1)
                      (student-account-name stud2))
            (string=? (student-account-id stud1)
                      (student-account-id stud2))))))
  
  ;; ********************************************************************************
  ;; staff-accounts
  ;; A staff-account is a structure
  ;; (make-staff-account string string)
  (define-struct staff-account (login-name password))
  
  ;; ********************************************************************************
  ;; An account is: (union student-account full-student-account staff-account)

  
  ;; parse-account: s-expression -> account
  ;; parse an account
  (define parse-account
    (lambda (s-exp)
      (match s-exp
        [('staff (? string? login-name) (? string? password))
         (make-staff-account login-name password)]
        [else (parse-student-account s-exp)])))
  
  (provide
   
   exn-homework-config?
   raise-exn-homework-config
   
   parse-homework-assignment-expr
   (struct homework-assignment (title url due-date))
   
   student-account=?
   (struct student-account (name id))
   (struct full-student-account (login-name password grader group-id))
   
   (struct staff-account (login-name password))
   parse-account
      
   ))


