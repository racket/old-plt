(module websubmit-lib mzscheme
  (require (lib "date.ss")
           (lib "list.ss")
           (lib "pretty.ss")
           "md5.ss"
	   "parsing.scm"
	   "websubmit-structs.scm"
	   )
           
  
  (provide is-current?
  
           lookup-account
           read-accounts
           save-account
           
           lookup-student-account/id
                      
           create-new-account
           
           

           read-graders
  
           read-assignment-list
           select-assignment  
  
           valid-login-name?
           

           
           format-partner-list
           

           
           homework-submission-date
           save-file-content
           log-file-upload
           
           find-partners
           
           log-sorry-charlie
           
           ;; **************************************************
           ;; these are used by staff accounts
           staff-create-new-student-account
           staff-delete-account
           staff-attempt-save-full-student
           staff-clear-partnership
           
           ;; **************************************************
           ;; These are used by scripts but not used by the websubmit servlet
           
           make-unique-group-symbol
           save-all-accounts
           )
  
  
  (define submission-dir "submissions")
  (define ACCOUNTS-FILENAME "student-accounts")
  (define ASSIGNMENT-FILENAME "assignment")
  (define HOMEWORK-FILENAME "HOMEWORK-CONTENT")
  (define GRADER-FILENAME "graders")
  (define log-path (build-path (current-directory) "websubmit.log"))
  
  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; everything that accesses the file system goes here
  
  (define account-semaphore (make-semaphore 1))
    
  ;; read-graders: -> (listof string)
  ;; read the list of graders from a file
  (define read-graders
    (lambda ()
      (call-with-input-file GRADER-FILENAME read)))
  
  ;; read-accounts: -> (listof account)
  ;; read the list of accounts from a file
  (define read-accounts
    (lambda ()
      (dynamic-wind
       (lambda () (semaphore-wait account-semaphore))
       (lambda () (parse-accounts/filename ACCOUNTS-FILENAME))
       (lambda () (semaphore-post account-semaphore)))))
  
  ;; read-student-accounts/filename: string -> (listof student-account)
  (define parse-accounts/filename
    (lambda (path)
      (or (check-duplicates
           account=?
           (call-with-input-file path
             (lambda (i-port)
               (map
                parse-account
                (read i-port))))
           (lambda (acct1 acct2)
             (if (same-login-name? acct1 acct2)
                 (error "read-student-accounts: duplicate login-names" (acct->login-name acct1))
                 (error "read-student-accounts: duplicate student-id" (student-account-id acct1))))))))
  
  ;; write-accounts: string (listof student-account) symbol ->
  ;; write a list of student-account to a file
  ;; symbol determines whether to replace the file
  ;; change
  (define write-accounts
    (lambda (filename accts sym)
      (call-with-output-file filename
        (lambda (o-port)
          (pretty-print
           (map account->s-expr accts)
           o-port))
        sym)))
  
  ;; replace-account: account (listof account) -> (listof account)
  ;; replace the single occurence of the account in the list
  ;; if there is no occurence then insert it at the end (presently there should always be an occurence.)
  (define replace-account
    (lambda (acct accts)
      (cond
        [(null? accts) (list acct)]
        [(and (account=? acct (car accts)))
         (cons acct (cdr accts))]
        [else
         (cons (car accts) (replace-account acct (cdr accts)))])))
  
  ;; save-account: student-account ->
  ;; use read-student-accounts/filename which does not mess with the semaphore.
  (define save-account
    (lambda (acct)
      (dynamic-wind
       (lambda () (semaphore-wait account-semaphore))
       (lambda ()
         (let ([accts (parse-accounts/filename ACCOUNTS-FILENAME)])
           (write-accounts
            ACCOUNTS-FILENAME
            (replace-account acct accts)
            'replace)))
       (lambda () (semaphore-post account-semaphore)))))
  
  ;; save-all-accounts: (listof account) ->
  ;; save the whole list of accounts overwriting the accounts file with a new one.
  (define save-all-accounts
    (lambda (accts)
      (dynamic-wind
       (lambda () (semaphore-wait account-semaphore))
       (lambda () (write-accounts ACCOUNTS-FILENAME accts 'replace))
       (lambda () (semaphore-post account-semaphore)))))
      
  ;; read-homework-assignment: -> homework-assignment
  (define read-homework-assignment
    (lambda (filename)
      (call-with-input-file filename
        (lambda (i-port)
          (parse-homework-assignment-expr (read i-port))))))
  
  ;; login-name-exists?: string (listof account) -> boolean
  (define login-name-exists?
    (lambda (lname accts)
      (ormap
       (lambda (acct)
         (let ([login-name (acct->login-name acct)])
           (and (string? login-name)
                (string=? login-name lname))))
       accts)))
  
  ;; staff-create-new-student-account: string string -> student-account
  ;; create a new (non-full) student account
  (define staff-create-new-student-account
    (lambda (id name)
      (let ([new-acct (make-student-account name id)])
        (dynamic-wind
         (lambda () (semaphore-wait account-semaphore))
         (lambda ()
           (let* ([accts (parse-accounts/filename ACCOUNTS-FILENAME)]
                  [acct (lookup-student-account/id id accts)])
             (and (not acct)
                  (write-accounts
                   ACCOUNTS-FILENAME
                   (cons new-acct accts)
                   'replace)
                  new-acct)))
         (lambda () (semaphore-post account-semaphore))))))
  
  ;; staff-attempt-save-full-student: string full-student-account -> boolean
  ;; Load the accounts, find this account, if the login-name has changed, try to find a duplicate
  ;; if there is no duplicate, then save and return #t, otherwise #f
  (define staff-attempt-save-full-student
    (lambda (id new-acct)
      (dynamic-wind
       (lambda () (semaphore-wait account-semaphore))
       (lambda ()
         (let ([accts
                (foldr
                 (lambda (acct rest)
                   (and rest
                        (cond
                          [(staff-account? acct) (cons acct rest)]
                          [(string=? (student-account-id acct)
                                     (student-account-id new-acct))
                           (cons new-acct rest)]
                          [(string=? (student-account-id acct) id) rest]
                          [(and (full-student-account? acct)
                                (string=? (full-student-account-login-name acct)
                                          (full-student-account-login-name new-acct)))
                           #f]
                          [else (cons acct rest)])))
                 '()
                 (parse-accounts/filename ACCOUNTS-FILENAME))])
           (and accts (write-accounts ACCOUNTS-FILENAME accts 'replace))))           
       (lambda () (semaphore-post account-semaphore)))))
  
  ;; staff-delete-account: string ->
  ;; delete an account from the database by ID
  (define staff-delete-account
    (lambda (id)
      (dynamic-wind
         (lambda () (semaphore-wait account-semaphore))
         (lambda ()
           (let ([accts (parse-accounts/filename ACCOUNTS-FILENAME)])
             (write-accounts
              ACCOUNTS-FILENAME
              (foldr
               (lambda (acct rest)
                 (cond
                   [(staff-account? acct) (cons acct rest)]
                   [(string=? (student-account-id acct) id) rest]
                   [else (cons acct rest)]))
               '()
               accts)
              'replace)))
         (lambda () (semaphore-post account-semaphore)))))
  
  ;; set-friendly-group-id!: student-account (listof full-student-account) (listof account) ->
  ;; reset the group-id to one that is based on the student-name and ID.
  (define set-friendly-group-id!
    (lambda (acct accts-to-change accts)
      (and (full-student-account? acct)
           (ormap
            (lambda (acct-to-change)
              (string=? (student-account-id acct-to-change)
                        (student-account-id acct)))
            accts-to-change)
           (set-full-student-account-group-id!
            acct
            (make-unique-group-symbol acct accts)))))
  
  ;; clear-partnership!: symbol (listof account) ->
  ;; clear a paticular partnership
  (define clear-partnership!
    (lambda (group-id accts)
      (for-each
       (lambda (acct)
         (and (full-student-account? acct)
              (eqv? (full-student-account-group-id acct) group-id)
              (set-full-student-account-group-id! acct
                                                  (string->symbol
                                                   (string-append
                                                    (symbol->string (full-student-account-group-id acct))
                                                    (symbol->string (gensym)))))))
       accts)))
  
  ;; clear-partnership/friendly!: (listof full-student-account) (listof account) -> (listof account)
  (define clear-partnership/friendly!
    (lambda (accts-to-change accts)
      (clear-partnership! (full-student-account-group-id (car accts-to-change)) accts)
      (for-each
       (lambda (acct)
         (set-friendly-group-id! acct accts-to-change accts))
       accts)))
  
  ;; staff-clear-partnership: full-student-account ->
  ;; clear the partnership for an account
  (define staff-clear-partnership
    (lambda (s-acct)
      (dynamic-wind
         (lambda () (semaphore-wait account-semaphore))
         (lambda () (let ([accts (parse-accounts/filename ACCOUNTS-FILENAME)])
                      (clear-partnership/friendly!
                       (cons s-acct (find-partners/accts s-acct accts))
                       accts)
                      (write-accounts ACCOUNTS-FILENAME accts 'replace)))
         (lambda () (semaphore-post account-semaphore)))))  
  
  ;; create-new-account: student-account username password
  ;; atomically, read the accounts and search for the username. If it is not
  ;; there then create the account, save it, return it --- otherwise return false.
  ;; use read-accounts/filename which does not mess with the semaphore.
  (define create-new-account
    (lambda (acct login-name password)
      (dynamic-wind
       (lambda () (semaphore-wait account-semaphore))
       (lambda ()
         (let* ([accts (parse-accounts/filename ACCOUNTS-FILENAME)]
                [new-acct (create-new-full-student-account-struct acct accts login-name password)])
           (and (not (login-name-exists? login-name accts))
                (write-accounts
                 ACCOUNTS-FILENAME
                 (replace-account new-acct accts)
                 'replace)
                new-acct)))
       (lambda () (semaphore-post account-semaphore)))))
      
  ;; create-new-account-struct: student-account (listof student-account) string string -> full-student-account
  (define create-new-full-student-account-struct
    (lambda (acct accts login-name password)
      (make-full-student-account
       (student-account-name acct)
       (student-account-id acct)
       login-name
       (md5 password)
       "no grader"
       (make-unique-group-symbol acct accts))))
  
  ;; make-unique-group-symbol: student-account (listof student-account) -> symbol
  (define make-unique-group-symbol
    (lambda (acct accts)
      (let ([base-str (regexp-replace* " " (student-account-name acct) "_")])
        (let loop ([proposed base-str]
                   [i 0])
          (if (exists-group-name? proposed accts)
              (loop (string-append base-str 
                                   (list->string (list (string-ref (student-account-id acct) i))))
                    (+ i 1))
              (string->symbol proposed))))))
  
  
  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; ********************************************************************************
    
  ;; date<: date date -> boolean
  ;; compare to dates to determine if the first comes before the second
  (define date<
    (lambda (current-date due-date)
      (let ([jul-current (date->julian/scalinger current-date)]
	    [jul-due (date->julian/scalinger due-date)])
        (or (< jul-current jul-due)
            (and (= (date->julian/scalinger current-date)
                    (date->julian/scalinger due-date))
                 (< (+ (* 60 60 (date-hour current-date))
                       (* 60 (date-minute current-date))
                       (date-second current-date))
                    (+ (* 60 60 (date-hour due-date))
                       (* 60 (date-minute due-date))
                       (date-second due-date))))))))
  
  ;; is-current?: homework-assignment -> boolean
  ;; determine whether the provided assignment is current
  (define is-current?
    (lambda (assmt)
      (let ([due-date (homework-assignment-due-date assmt)]
            [current-date (seconds->date (current-seconds))])
        (date< current-date due-date))))
  
  ;; ********************************************************************************
  ;; lookup-account: string string (listof account) -> account | #f
  ;; find an account from a list using the login credentials
  (define lookup-account
    (lambda (name pass accts)
      (let ([pass (md5 pass)])
        (let lookup-loop ([accts accts])
          (if (null? accts) #f
              (let* ([first (car accts)]
                     [login-name (acct->login-name first)]
                     [password (acct->password first)])
                (if (and (string? login-name)
                         (string=? login-name name)
                         (string=? password pass))
                    first
                    (lookup-loop (cdr accts)))))))))
  
  ;; lookup-student-account/id: string (listof account) -> student-account | #f
  (define lookup-student-account/id
    (lambda (id accts)
      (cond
        [(null? accts) #f]
        [(and (student-account? (car accts))
              (string=? id (student-account-id (car accts))) (car accts))]
        [else (lookup-student-account/id id (cdr accts))])))
  
  ;; exists-group-name?: string (listof account) -> boolean
  ;; determine if the proposed group-id already exists in a list of accounts
  (define exists-group-name?
    (lambda (proposed accts)
      (and (not (null? accts))
           (or (and (full-student-account? (car accts))
                    (string=? proposed (symbol->string (full-student-account-group-id (car accts)))))
               (exists-group-name? proposed (cdr accts))))))
  
  ;; account=?: account account -> boolean
  ;; determine if two accounts collide:
  ;;   either they have the same ID or the same username.
  (define account=?
    (lambda (std1 std2)
      (or (and (student-account? std1)
               (student-account? std2)
               (string=? (student-account-id std1)
                         (student-account-id std2)))
          (same-login-name? std1 std2))))
  
  ;; acct->login-name: account -> (union string #f)
  ;; the login name or #f
  (define acct->login-name
    (lambda (acct)
      (cond
        [(full-student-account? acct)
         (full-student-account-login-name acct)]
        [(staff-account? acct)
         (staff-account-login-name acct)]
        [else #f])))
  
  ;; acct->password: account -> (union string #f)
  (define acct->password
    (lambda (acct)
      (cond
        [(full-student-account? acct)
         (full-student-account-password acct)]
        [(staff-account? acct)
         (staff-account-password acct)]
        [else #f])))
  
  ;; same-login-name: account account -> boolean
  ;; determine if two accounts have the same login name
  (define same-login-name?
    (lambda (acct1 acct2)
      (let ([login1 (acct->login-name acct1)]
            [login2 (acct->login-name acct2)])
        (and (string? login1)
             (string? login2)
             (string=? login1 login2)))))
  
  ;; valid-login-name?: string -> boolean
  (define valid-login-name?
    (lambda (str)
      (regexp-match "^[^ ]+.*[^ ]+$" str)))
  
  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; Student Accounts
  
  ;; format-partner-list: (listof student-account) -> string
  ;; NOTE: the partner-list is currently limited to be of length three or less!!
  (define format-partner-list
    (lambda (partners)
      (case (length partners)
        [(0) "no one"]
        [(1) (student-account-name (car partners))]
        [(2) (format "~a and ~a"
                     (student-account-name (car partners))
                     (student-account-name (cadr partners)))]
        [(3) (format "~a, ~a, and ~a"
                     (student-account-name (car partners))
                     (student-account-name (cadr partners))
                     (student-account-name (caddr partners)))])))
    
  ;; student-account->s-expr: student-account -> s-expression
  (define account->s-expr
    (lambda (acct)
      (cond
        [(student-account? acct)
         `(,(student-account-name acct)
           ,(student-account-id acct)
           ,@(cond
               [(not (full-student-account? acct)) '()]
               [else
                (list
                 (full-student-account-login-name acct)
                 (full-student-account-password acct)
                 (full-student-account-grader acct)
                 (full-student-account-group-id acct))]))]
        [(staff-account? acct)
         `(staff
           ,(staff-account-login-name acct)
           ,(staff-account-password acct))])))
  
  
  
  ;; ********************************************************************************
  
  ;; find-partners: student-account -> (listof student-account)
  ;; the sublist of all students who have the same group symbol as the given account
  (define find-partners
    (lambda (acct)
      (if (full-student-account? acct)
          (find-partners/accts acct (parse-accounts/filename ACCOUNTS-FILENAME))
          '())))
  
  ;; find-partners/accts: full-student-account (listof account) -> (listof full-student-account)
  (define find-partners/accts
    (lambda (s-acct accts)
      (filter
       (lambda (student)
         (and (full-student-account? student)
              (not (student-account=? student s-acct))
              (eqv? (full-student-account-group-id student)
                    (full-student-account-group-id s-acct))))
       accts)))
  
  ;; map-filter: proc list -> list
  ;; apply proc to each element of the list if the result is not #f keep it
  (define map-filter
    (lambda (proc l)
      (if (null? l) l
          (let ([res (proc (car l))])
            (if res
                (cons res (map-filter proc (cdr l)))
                (map-filter proc (cdr l)))))))
  
  ;; tests for map-filter
  
  ;  (equal? '() (map-filter number? '(this is a list)))
  ;  (equal? '(1 2 3)
  ;          (map-filter
  ;           (lambda (x)
  ;             (and (number? x)
  ;                  x))
  ;           '(1 2 3 a b c)))
  ;  
  ;  (equal? '() (map-filter (lambda (x) x) '()))
  ;  
  ;  (equal? '(a b c 1 2 3) (map-filter (lambda (x) x) '(a b c 1 2 3)))
  ;  
  ;  (equal? '(a b c 1 2 3) (map-filter (lambda (x) x) '(a #f b #f c 1 #f 2 #f 3)))
  
  
  
  ;; assignment=: homework-assignment homework-assignment -> boolean
  ;; determine if two assignments have the same title.
  (define assignment=
    (lambda (hw1 hw2)
      (string=? (homework-assignment-title hw1)
                (homework-assignment-title hw2))))

  
  ;; read-assignment-list: student-account -> (listof full-assignment)
  ;; create a (listof full-assignment) by reading all the assignments
  (define read-assignment-list
    (lambda (acct)
      (quicksort ;; sort based on due date
       (check-duplicates
        assignment=
        (map-filter
         (lambda (file-or-directory)
           (let ([filename (build-path file-or-directory ASSIGNMENT-FILENAME)])
             (and (file-exists? filename)
                  (let ([assmt (read-homework-assignment filename)])
                    (make-full-assignment
                     (homework-assignment-title assmt)
                     (homework-assignment-url assmt)
                     (homework-assignment-due-date assmt)
                     file-or-directory
                     (homework-submission-size acct file-or-directory)
                     (homework-submission-date acct file-or-directory)
                     )))))
         (directory-list))
        (lambda (assmt1 assmt2)
          (error "read-assignment-list: duplicate assignment title" (homework-assignment-title assmt1))))
       (lambda (assmt1 assmt2)
         (date<
          (homework-assignment-due-date assmt1)
          (homework-assignment-due-date assmt2))))))
         
         
  
  ;; homework-submission-size: student-account string -> number | #f
  ;; given the directory for the assignment and the student account
  ;; produce the size of any previous submission or #f
  (define homework-submission-size
    (lambda (acct path)
      (let* ([base-path (student-account->base-path path acct)]
             [oldpath (build-path base-path HOMEWORK-FILENAME)]
             [defpath (build-path base-path (string-append HOMEWORK-FILENAME ".definitions"))]
             [intpath (build-path base-path (string-append HOMEWORK-FILENAME ".testcases"))])
        (or (and (file-exists? oldpath)
                 (file-size oldpath))
            (and (file-exists? defpath)
                 (file-exists? intpath)
                 (+ (file-size defpath)
                    (file-size intpath)))))))
  
  ;; homework-submission-date: student-account string -> number | #f
  ;; given the directory for the assignment and the student account
  ;; produce the date (in seconds) of any previous submission or #f
  (define homework-submission-date
    (lambda (acct path)
      (let* ([base-path (student-account->base-path path acct)]
             [oldpath (build-path base-path HOMEWORK-FILENAME)]
             [defpath (build-path base-path (string-append HOMEWORK-FILENAME ".definitions"))])
        (or (and (file-exists? oldpath)
                 (file-or-directory-modify-seconds oldpath))
            (and (file-exists? defpath)
                 (file-or-directory-modify-seconds defpath))))))
  
  ;; student-account->base-path: string student-account -> string
  (define student-account->base-path
    (lambda (path acct)
      (and (not (full-student-account? acct))
           (error "Violation of invariant: Student who has not setup a full-student-account cannot save homework."))
      (build-path
       path 
       (full-student-account-grader acct)
       (symbol->string (full-student-account-group-id acct)))))
  
  ;; select-assignment: string (listof homework-assignment) -> homework-assignment
  ;; select an assignment from a list given a string
  (define select-assignment
    (lambda (assmt-name assmts)
      (cond
        [(null? assmts) (error "select-assignment: assignment-title not found " assmt-name)]
        [(string=? assmt-name (homework-assignment-title (car assmts))) (car assmts)]
        [else (select-assignment assmt-name (cdr assmts))])))
  
  ;; ********************************************************************************
  
  ;; save-file-content: homework-assignment student-account string string ->
  ;; change to the correct directory and create the file using the content
  ;; GP: maybe just use build-path instead of parameterize.
  (define save-file-content
    (lambda (assmt acct definitions testcases)
      (and (not (full-student-account? acct))
           (error "Violation of invariant: Student who has not setup a full-student-account cannot save homework."))
      (let ([partners (cons acct (find-partners acct))])
        (parameterize ([current-directory (full-assignment-dir assmt)])
          (parameterize ([current-directory (grader->directory (full-student-account-grader acct))])
            (parameterize ([current-directory (account->directory acct)])
              (call-with-output-file (string-append HOMEWORK-FILENAME ".definitions")
                (lambda (o-port)
                  (display definitions o-port))
                'replace)
              (call-with-output-file (string-append HOMEWORK-FILENAME ".testcases")
                (lambda (o-port)
                  (display testcases o-port))
                'replace)
              (call-with-output-file "student-info"
                (lambda (o-port)
                  (map
                   (lambda (an-acct)
                     (fprintf o-port
                              "Student-Name: ~a     Student-ID: ~a~n"
                              (student-account-name an-acct)
                              (substring (student-account-id an-acct) 5)))
                   partners)))
              ))))))
  
  ;; grader->directory: string -> string
  ;; if the directory already exists then just return the string
  ;; otherwise create the directory and return the string
  (define grader->directory
    (lambda (str)
      (if (directory-exists? str) str
          (begin
            (make-directory str)
            str))))
  
  ;; string->directory: student-account -> string
  ;; remove any directory with the given name and create a new one. Return the name.
  (define account->directory
    (lambda (acct)
      (and (not (full-student-account? acct))
           (error "Violation of invariant: Student who has not setup a full-student-account cannot save homework."))
      (let ([str (symbol->string (full-student-account-group-id acct))])
        (when (directory-exists? str)
          (delete-directory-dammit str))
        (make-directory str)
        str)))
  
  
  ;; log-file-upload: homework-assignment student-account ->
  ;; log the upload event
  (define log-file-upload
    (lambda (assmt acct)
      (call-with-output-file log-path
        (lambda (o-port)
          (parameterize ([print-struct #t])
            (fprintf o-port "**** Homework File Upload ****~n~a~n~a~a~n"
                     (date->string (seconds->date (current-seconds)))
                     (format-student-account acct)
                     (format "assignment: ~a~n" (homework-assignment-title assmt)))))
        'append)))
  
  ;; log-sorry-charlie: homework-assignment student-account ->
  ;; log the sorry-charlie event
  (define log-sorry-charlie
    (lambda (assmt acct)
      (call-with-output-file log-path
        (lambda (o-port)
          (parameterize ([print-struct #t])
            (fprintf o-port "**** Sorry Charlie ****~n A student attempted to submit an assignment using an expired link.~n~a~n~a~a~n"
                     (date->string (seconds->date (current-seconds)))
                     (format-student-account acct)
                     (format "assignment: ~a~n" (homework-assignment-title assmt)))))
        'append)))
  
  ;; format-student-account: full-student-account -> string
  ;; create a string suitable for printing in a log file.
  (define format-student-account
    (lambda (acct)
      (format "student-account: ~s~n"
              (list (student-account-name acct)
                    (substring (student-account-id acct) 5)
                    (full-student-account-login-name acct)))))
  
  ;; duplicates?: (x x -> boolean) (listof x) (x x -> alpha) -> (union (listof x) alpha)
  ;; determine if a list has duplicates
  (define check-duplicates
    (lambda (x=? lox dupe-proc)
      (if (null? lox) '()
          (let ([x (car lox)] [rest (check-duplicates x=? (cdr lox) dupe-proc)])
            (let loop ([lox (cdr lox)])
              (cond
                [(null? lox) (cons x rest)]
                [(x=? x (car lox)) (dupe-proc x (car lox))]
                [else (loop (cdr lox))]))))))

;  (null? (check-duplicates = '() error))
;  (equal? '(1 2 3 4 5) (check-duplicates = '(1 2 3 4 5) error)) 
;  (equal? '(3 . 3) (check-duplicates = '(3 1 9 0 -1 3) (lambda (x1 x2) (cons x1 x2))))
  
  ;; delete-directory-dammit:
  ;; Yes I really want to delete this directory, even if contains files.
  (define delete-directory-dammit
    (lambda (str)
      (parameterize ([current-directory str])
        (for-each
         (lambda (file-or-directory)
           (if (directory-exists? file-or-directory)
               (delete-directory-dammit file-or-directory)
               (delete-file file-or-directory)))
         (directory-list)))
      (delete-directory str)))
  
  )
