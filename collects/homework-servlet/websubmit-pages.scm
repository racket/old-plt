(module websubmit-pages mzscheme
  (require (lib "unitsig.ss" "mzlib")
           (lib "servlet-sig.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "date.ss")
           
           "pages-lib.scm"
	   "websubmit-sig.scm"
	   "websubmit-structs.scm"
	   "websubmit-strings.scm"
	   "parsing.scm"
	   "websubmit-lib.scm"
           
           )
  
  (provide websubmit-pages@)
   
  (define websubmit-pages@
    (unit/sig websubmit-pages^
      (import course-specific^ servlet^)
      
      (define notnull? (lambda (x) (not (null? x))))
      (define urlof (lambda (url str) (add-binding-to-url-string url str "yes")))
                      
      
      ;; make-student-info-exprs: student-account (listof student-account) -> (listof x-expression)
      ;; make the student info expressions that go at the top of the page
      (define make-student-info-exprs
        (lambda (acct partners)
          `((h3 ,(student-account-name acct))
            (h3 ,(string-append "You currently belong to a group with "
                                (format-partner-list partners) ".")))))
      
      ;; make-continue-html: string string string  (listof x-expression) -> (-> request)
      ;; make a continue page that allows the user to continue from a message
      (define make-continue-html
        (lambda (action title-str message-str . exprs)
          `(html
            (title ,title-str )
            (body
             ,@exprs
             (h3 ,title-str)
             (p ,message-str)
             (p
              (form ([action ,action] [method "get"])
                    (input ([type "submit"] [name "continue"] [value "Continue"]))))))))
      
      ;; ********************************************************************************
      ;; Staff pages
           
      ;; get-staff-lobby-req: string -> staff-lobby-req
      ;; a staff member chooses between:
      ;;   1. Editing his/her own account password.
      ;;   2. Creating a new student account
      ;;   3. Editing an existing student account
      ;;   4. logout
      (define get-staff-lobby-req
        (lambda (staff-name)
          (send/suspend/struct
           staff-lobby-req ([changepass? notnull?] [create-student? notnull?] [edit-student? notnull?] [logout? notnull?])
           (lambda (url)
             `(html
               (title ,staff-lobby-title)
               (body
                (h3 ,staff-lobby-title)
                (h3 ,(format "Staff options for ~a" staff-name))
                (p "Change my own " (a ([href ,(urlof url staff-lobby-req-changepass?)]) "password") ".")
                (p (a ([href ,(urlof url staff-lobby-req-create-student?)]) "Create") " a new student account.")
                (p (a ([href ,(urlof url staff-lobby-req-edit-student?)]) "Edit") " an existing student account.")
                (p (a ([href ,(urlof url staff-lobby-req-logout?)]) "Logout"))))))))
    
      (define simple-student-req/html
        (lambda (title student-name student-id message NEW-STUDENT-NAME NEW-STUDENT-ID CONTINUE-STR)
          (lambda (action)
            `(html (head (title ,title))
                   (body
                    (table
                     (tr (td (h3 ,title)))
                     (tr (td ,message)))
                    (form ([action ,action] [method "post"][enctype "multipart/form-data"])
                          (table
                           (tr (td "Student Name: ")
                               (td (input ([type "text"] [name ,NEW-STUDENT-NAME] [value ,student-name]))))
                           (tr (td "Student ID: ")
                               (td (input ([type "text"] [name ,NEW-STUDENT-ID] [value ,student-id]))))
                           (tr (td (input ([type "submit"] [name ,CONTINUE-STR] [value "Continue"])))
                               (td (input ([type "submit"] [name "cancel"] [value "Cancel"])))))))))))
      
      ;; get-simple-student-req/create: string string string -> simple-student-req/create
      ;; get the name and id for a new student account
      (define get-simple-student-req/create
        (lambda (student-name student-id message)
          (send/suspend/struct
           simple-student-req/create (name id [continue? (lambda (bindings) (not (null? bindings)))])
           (simple-student-req/html create-new-student-title student-name student-id message
                                    simple-student-req/create-name simple-student-req/create-id
                                    simple-student-req/create-continue?))))
      
      ;; get-simple-student-req/edit: string string string -> simple-student-req/edit
      ;; get the name and id for a new student account
      (define get-simple-student-req/edit
        (lambda (student-name student-id message)
          (send/suspend/struct
           simple-student-req/edit (name id [continue? notnull?])
           (edit-student-title student-name student-id message
                               simple-student-req/edit-name simple-student-req/edit-id
                               simple-student-req/edit-continue?))))
        
      (define review-simple-student-req/html
        (lambda (title student-name student-id EDIT-STR CREATE-STR CANCEL-STR)
          (lambda (action)
            `(html (head (title ,title))
                   (body
                    (table 
                     (tr (td (h3 ,title)))
                     (tr (td "Are you sure you want to create the following student account?:")))
                    (form ([action , action] [method "post"] [enctype "multipart/form-data"])
                          (table
                           (tr (td "Student Name: ") (td ,student-name))
                           (tr (td "Student ID: ") (td ,student-id))
                           (tr (td (input ([type "submit"] [name ,CREATE-STR] [value "Create Student"])))
                               (td (input ([type "submit"] [name ,EDIT-STR] [value "Edit Student"])))
                               (td (input ([type "submit"] [name ,CANCEL-STR] [value "Cancel"])))))))))))
      
      ;; get-review-simple-student-req/create: string string -> simple-student-req/create
      ;; one last chance to look at the student before creating it
      (define get-review-simple-student-req/create
        (lambda (student-name student-id)
          (send/suspend/struct
           review-simple-student-req/create ([edit? notnull?] [create? notnull?] [cancel? notnull?])
           (review-simple-student-req/html review-new-student-title student-name student-id
                                           review-simple-student-req/create-edit?
                                           review-simple-student-req/create-create?
                                           review-simple-student-req/create-cancel?))))
           
      ;; get-review-simple-student-req/edit: string string -> simple-student-req/edit
      ;; one last chance to look at the student before committing changes
      (define get-review-simple-student-req/edit
        (lambda (student-name student-id)
          (send/suspend/struct
           review-simple-student-req/edit ([edit? notnull?] [create? notnull?] [cancel? notnull?])
           (review-simple-student-req/html review-edit-student-title student-name student-id
                                           review-simple-student-req/edit-edit?
                                           review-simple-student-req/edit-create?
                                           review-simple-student-req/edit-cancel?))))
          
      
      ;; get-student-to-edit-req: sring -> student-to-edit-req
      ;; staff enters the id for a student-account to edit
      (define get-student-to-edit-req
        (lambda (message)
          (send/suspend/struct
           student-to-edit-req (id [continue? notnull?])
           (lambda (action)
             `(html (head (title ,get-student-to-edit-title))
                    (body (h3 ,get-student-to-edit-title)
                          (p ,message)
                          (form ([action ,action] [method "post"] [enctype "multipart/form-data"])
                                (table
                                 (tr (td "Student ID: ")
                                     (td (input ([type "password"] [name ,student-to-edit-req-id]))))
                                 (tr (td (input ([type "submit"] [name "whocares"] [value "Cancel"])))
                                     (td (input ([type "submit"] [name ,student-to-edit-req-continue?] [value "Continue"]))))))))))))
      
          
      ;; get-full-student-req/edit:
      ;;   string (listof string) full-student-account (listof full-student-account) boolean boolean -> full-student-req
      ;; get all the changes for a student-account
      (define get-full-student-req/edit
        (lambda (message graders acct partners clear-password? clear-partnership?)
          (send/suspend/struct
           full-student-req/edit (name id login-name 
                                       [grader (lambda (bindings)
                                                 (if (null? bindings)
                                                     "no-ta"
                                                     (car bindings)))]
                                       [clear-password? notnull?]
                                       [clear-partnership? notnull?]
                                       [continue? notnull?])
           (lambda (action)
             `(html (head (title ,edit-student-title))
                    (body
                     (table
                      (tr (td (h3 ,edit-student-title)))
                      (tr (td ,message)))
                     (form ([action ,action] [method "post"][enctype "multipart/form-data"])
                           (table
                            (tr (td "Student Name: ")
                                (td (input ([type "text"] [name ,full-student-req/edit-name]
                                            [value ,(student-account-name acct)]))))
                            (tr (td "Student ID: ")
                                (td (input ([type "text"] [name ,full-student-req/edit-id]
                                            [value ,(student-account-id acct)]))))
                            (tr (td "Clear password?: ")
                                (td (table
                                     (tr
                                      (td (input ([type "radio"] [name ,full-student-req/edit-clear-password?] [value "Yes"]
                                                  ,@(if clear-password? '([checked "yes"]) '()))))
                                      (td "Yes")
                                      (td (input ([type "radio"] [name ,full-student-req/edit-clear-password?] [value "No"]
                                                  ,@(if clear-password? '() '([checked "yes"])))))
                                      (td "No")))))
                            (tr (td "Login Name: ")
                                (td (input ([type "text"] [name ,full-student-req/edit-login-name]
                                            [value ,(full-student-account-login-name acct)]))))
                            (tr (td "Grader: ")
                                (td (table
                                     (tr
                                      ,@(foldr
                                         (lambda (a-grader rest)
                                           `((td (input (,@(if (string=? (full-student-account-grader acct) a-grader)
                                                               '([checked "yes"])
                                                               '())
                                                         [name ,full-student-req/edit-grader][type "radio"][value ,a-grader])))
                                             (td ,a-grader) ,@rest))
                                         '()
                                         graders)))))
                            
                            (tr (td ([colspan "2"]) ,(string-append "This student currently belongs to a group with "
                                                                    (format-partner-list partners) ".")))
                            (tr (td "Clear partnership? ")
                                (td (table
                                     (tr
                                      (td (input ([type "radio"] [name ,full-student-req/edit-clear-partnership?] [value "Yes"]
                                                  ,@(if clear-partnership? '([checked "yes"]) '()))))
                                      (td "Yes")
                                      (td (input ([type "radio"] [name ,full-student-req/edit-clear-partnership?] [value "No"]
                                                  ,@(if clear-partnership? '() '([checked "yes"])))))
                                      (td "No")))))
                            (tr (td (input ([type "submit"] [name ,full-student-req/edit-continue?] [value "Continue"])))
                                (td (input ([type "submit"] [name "cancel"] [value "Cancel"]))))))))))))
      
      ;; get-review-full-student-req/edit: full-student-account (listof full-student-account) boolean boolean -> review-full-student-req/edit
      ;; review the edits to a full student account before comitting
      (define get-review-full-student-req/edit
        (lambda (acct partners clear-password? clear-partnership?)
          (send/suspend/struct
           review-full-student-req/edit ([edit? notnull?] [create? notnull?] [cancel? notnull?])
           (lambda (action)
             `(html (head (title ,review-edit-student-title))
                    (body
                     (table 
                      (tr (td (h3 ,review-edit-student-title)))
                      (tr (td "Are you sure you want to change the following student account?:")))
                     (form ([action , action] [method "post"] [enctype "multipart/form-data"])
                           (table
                            (tr (td "Student Name: ") (td ,(student-account-name acct)))
                            (tr (td "Student ID: ") (td ,(student-account-id acct)))
                            (tr (td "Password: ") (td ,(if clear-password?
                                                           "<Use the student ID as the new password.>"
                                                           "<Use the current password.>")))
                            (tr (td "Login Name: ") (td ,(full-student-account-login-name acct)))
                            (tr (td "TA: ") (td ,(full-student-account-grader acct)))
                            (tr (td "Partners: ") (td ,(if clear-partnership?
                                                           "<clear the partnership that the student currently belongs to>"
                                                           (format-partner-list partners))))
                            (tr (td (input ([type "submit"] [name ,review-full-student-req/edit-create?] [value "Commit Changes"])))
                                (td (input ([type "submit"] [name ,review-full-student-req/edit-edit?] [value "Edit Student"])))
                                (td (input ([type "submit"] [name ,review-full-student-req/edit-cancel?] [value "Cancel"]))))))))))))
      
      
      ;; ********************************************************************************
      ;; Account Creation Pages
      
      ;; get-create-account-req: string -> create-account-req
      ;; get the student id for the account to be created by the student
      (define get-create-account-req
        (lambda (message)
          (send/suspend/struct
           create-account-req (id)
           (lambda (action)
             `(html
               (title ,create-account-title)
               (body
                (h3 ,create-account-title)
                (p ,message)
                (p "Please enter your nine-digit student ID")
                (p
                 (form ([action ,action][method "post"][enctype "multipart/form-data"])
                       (p "ID goes here: " (input ([type "password"] [name ,create-account-req-id])))
                       (input ([type "submit"] [name "login"] [value "Submit"]))))))))))
      
      ;; get-already-exists-req: -> already-exists-req
      (define get-already-exists-req
        (lambda ()
          (send/suspend/struct
           already-exists-req ()
           (lambda (action)
             (make-continue-html action
                                 account-exists-title
                                 (format "Your account has already been created. Please login and use the ~a page to edit your account."
                                         manage-account-title))))))
      
      ;; get-choose-credentials-req: student-account string . string -> request
      ;; get the credentials for a new account
      (define get-choose-credentials-req
        (lambda (acct message . username)
          (send/suspend/struct
           choose-credentials-req (login-name first second)
           (lambda (action)
             `(html (head (title ,choose-credentials-title))
                    (body
                     (table
                      (tr (td (h3 ,(format choose-credentials-custom-title (student-account-name acct)))))
                      (tr (td (h3 (string-append "Currently this is an insecure password and will be stored in plain-text"
                                                 "Choose a password that you don't mind your grader being able to see."))))
                      (tr (td ,message)))
                     (form ([action ,action] [method "post"][enctype "multipart/form-data"])
                           (table
                            (tr (td "Username: ")
                                (td (input ([type "text"]
                                            [name ,choose-credentials-req-login-name]
                                            ,@(if (null? username) '()
                                                  `([value ,(car username)]))
                                            ))))
                            (tr (td "Password: ") (td (input ([type "password"] [name ,choose-credentials-req-first]))))
                            (tr (td "Reenter Password: ") (td (input ([type "password"] [name ,choose-credentials-req-second]))))
                            (tr (td (input ([type "submit"] [name "enter"] [value "Enter"]))))))))))))
      
      ;; get-add-partner-or-proceed-req: student-account -> add-partner-req
      ;; student chooses to either add a partner or proceed to account summary and be added.
      (define get-add-partner-or-proceed-req
        (lambda (acct)
          (send/suspend/struct
           add-partner-or-proceed-req ([add? notnull?])
           (lambda (action)
             `(html (head (title ,add-or-proceed-title))
                    (body
                     (h3 ,(format add-or-proceed-custom-title (student-account-name acct)))
                     (p ,(string-append
                          "You must have a partner in order to submit homework. "
                          "At most three people can be in a group. "
                          "We strongly encourage groups of exactly two people. "
                          "You may either add a partner or proceed to account summary. "
                          "If you proceed to account summary, then your partner must add you. "))
                     (p "I will " (a ([href ,(urlof action add-partner-or-proceed-req-add?)]) "add") " my partner(s) now.")
                     (p "I will " (a ([href ,action]) "proceed") " to account summary and allow my partner to add me")))))))

             
      ;; get-account-summary-req: full-student-account (listof student-account) string numbers -> account-summary-req
      (define get-account-summary-req
        (lambda (acct partners grader min-partners)
          (send/suspend/struct
           account-summary-req ()
           (lambda (url)
             `(html (head (title ,account-summary-title))
                    (body
                     (h3 ,(format account-summary-custom-title (student-account-name acct)))
                     (p ,(format "Account summary for: ~a." (student-account-name acct)))
                     (p ,(format "When you login to submit homework, your login name will be: ~a"
                                 (full-student-account-login-name acct)))
                     (p ,(format "I hope you remember your password, 'cause I won't display it here."))
                     (p ,(string-append "You currently belong to a group with "
                                        (format-partner-list partners) "."))
                     (p ,@(if (< (length partners) min-partners)
                              
                              `((p ,(string-append
                                     "You do not have a homework partner yet. "
                                     "Your partner must add you when he/she creates an account. "
                                     "Alternatively, you or your partner may choose to add a partner from the page called: "
                                     manage-account-title
                                     ", when you login to submit homework.")))
                              '()))
                     (p ,@(if (string=? "" grader)
                              `((p ,(string-append
                                     "You do not have a TA yet. "
                                     "Your partner must choose a TA when he/she creates an account. "
                                     "Alternatively, you or your partner may choose a TA from the page called: "
                                     manage-account-title
                                     ", when you login to submit homework.")))
                              `((p ,(format "Your TA is: ~a." grader)))))
                     (form ([action ,url] [method "post"][enctype "multipart/form-data"])
                           (input ([type "submit"] [name "submit"] [value "Continue"])))))))))
      
      
      
      ;; ********************************************************************************
      ;;  All the pages go here
      
      ;; get-grader-selection-req: (listof string) string string . (listof full-student-account) -> grader-selection-req
      (define get-grader-selection-req
        (lambda (graders grader info-string . accts)
          (send/suspend/struct
           grader-selection-req ([grader (lambda (bindings)
                                           (if (null? bindings) "no-grader"
                                               (car bindings)))])
           (lambda (action)
             `(html (head (title ,select-grader-title))
                    (body (h3 ,(make-select-grader-custom-title accts))
                          (p ,select-grader-instructions)
                          (p ,info-string)
                          (form
                           ([action ,action][method "post"][enctype "multipart/form-data"])
                           (table
                            ,@(map
                               (lambda (a-grader)
                                 `(tr (td (input (,@(if (string=? grader a-grader)
                                                        '([checked "yes"])
                                                        '())
                                                  [name ,grader-selection-req-grader][type "radio"][value ,a-grader])))
                                      (td ,a-grader)))
                               graders))
                           (input ([type "submit"] [name "submit"] [value "Choose TA"])))))))))
      
      ;; make-select-grader-custom-title: (listof student-account) -> string
      (define make-select-grader-custom-title
        (lambda (accts)
          (if (null? accts) select-grader-title
              (format select-grader-custom-title (format-partner-list accts)))))
          
      ;; get-login-html: string (listof x-expr) string string string -> x-expression
      (define get-login-html
        (lambda (button-value banner-expr login-name password create?)
          (lambda (action)
            `(html
              (title ,web-submission-login-title)
              (body
               ,@banner-expr
               (p
                (form ([action ,action][method "post"][enctype "multipart/form-data"])
                      (p "Username: " (input ([type "input"] [name ,login-name])))
                      (br)
                      (p "Password: " (input ([type "password"] [name ,password])))
                      (input ([type "submit"] [name "login"] [value ,button-value]))))
               ,@(if (string=? "" create?) '()
                     `((p "I don't have a name and password yet and would like to "
                          (a ([href ,(add-binding-to-url-string action create? "yes")])
                             "create an account")
                          "."))))))))
      
      (define get-login-req/initial
        (lambda (message)
          (send/forward/struct
           login-req/initial ([login-name
                               (lambda (l)
                                 (unless (null? l)
                                   (car l)))]
                              [password
                               (lambda (l)
                                 (unless (null? l)
                                   (car l)))]
                              [create? notnull?])
           (get-login-html "Login" 
                           `((h2 ,course-title)
                             (h3 ,course-number)
                             (h3 ,web-submission-login-title)
                             (p ,message))
                           login-req/initial-login-name login-req/initial-password login-req/initial-create?))))
      
      (define get-login-req/partner
        (lambda (message)
          (send/suspend/struct
           login-req/partner (login-name password)
           (get-login-html "Join Pair"
                           `((h3 ,partner-login-title)
                             (p "The student who is to be the new partner must now log in.")
                             (p ,message))
                           login-req/partner-login-name login-req/partner-password ""))))
      
      ;; get-partner-already-paired-req: student-account (listof student-account) -> partner-already-paired-req
     (define get-partner-already-paired-req
       (lambda (acct partners)
         (send/suspend/struct
          partner-already-paired-req ()
          (lambda (action)
            (make-continue-html action
                                partner-already-paired-title
                                (format "The student, ~a, already has homework partners: ~a, and cannot join another group."
                                        (student-account-name acct)
                                        (format-partner-list partners)))))))
      
      ;; get-need-partner-req: -> need-partner-continue-req
      (define get-need-partner-req
        (lambda ()
          (send/suspend/struct
           need-partner-req ()
           (lambda (action)
             (make-continue-html action
                                 need-partner-title
                                 "You must have a homework partner before you can submit homework.")))))
      
      
      ;; make-assignment-links: action (listof homework-assignment) -> (listof x-expression)
      ;; make the html for the list of current assignment links
      (define make-assignment-links
        (lambda (bind-str action assmts)
          (map
           (lambda (assmt)
             (let ([new-url (add-binding-to-url-string action bind-str (homework-assignment-title assmt))])
               `(tr (td ,(make-title-link assmt))
                    (td (a ([href ,new-url]) "(upload)"))
                    (td  ,(format "Due on: ~a" (date->string (homework-assignment-due-date assmt) #t)))
                    ,@(size-and-date-annotation assmt))))
           assmts)))

           
      ;; get-select-assignment-req: student-account (listof student-account) (listof homework-assignment) -> select-assignment-req
      ;; get the request for the Select Assignment page given a list of current assignments
      ;; and a list of overdue assignments.
      (define get-select-assignment-req
        (lambda (acct partners assmts)
          (send/suspend/struct
           select-assignment-req ([assignment (lambda (bindings)
                                                (and (notnull? bindings)
                                                     (car bindings)))]
                                  [manage? notnull?] [logout? notnull?])
           (lambda (action)
             `(html
               (title ,select-assignment-title)
               (body
                ,@(make-student-info-exprs acct partners)
                (table
                 (tr
                  (td ([width "30%"]) (h3 ,select-assignment-title))
                  (td ([width "60%"]) "")
                  (td (a ([href ,(urlof action select-assignment-req-logout?)]) "Logout"))
                     (td (a ([href ,(urlof action select-assignment-req-manage?)]) "Manage Account"))))
                (center (h4 "Current Assignments (select to upload):"))
                (table ([cellspacing "10"])
                       ,@(make-assignment-links select-assignment-req-assignment action (filter is-current? assmts)))
                (hr)
                (center (h4 "Past-Due Assignments:"))
                (table
                 ,@(make-assignment-list (filter (lambda (a) (not (is-current? a))) assmts)))
                (hr)
                ,(format "Current time: ~a" (date->string (seconds->date (current-seconds)) #t))))))))
           
      ;; pass-entry-html: string string string string string -> (listof x-expression)
      (define pass-entry-html
        (lambda (action message title first second)
          `((table
             (tr (td (h3 ,title)))
             (tr (td ,message)))
            (form ([action ,action] [method "post"][enctype "multipart/form-data"])
                  (table
                   (tr (td "Password: ") (td (input ([type "password"] [name ,first]))))
                   (tr (td "Reenter Password: ") (td (input ([type "password"] [name ,second]))))
                   (tr (td (input ([type "submit"] [name "enter"] [value "Enter"])))))))))
      
      ;; get-pass-entry-req: student-account (listof student-account) string -> pass-entry-req
      ;; get the request for changing of a password
      (define get-pass-entry-req
        (lambda (acct partners message)
          (send/suspend/struct
           pass-entry-req (first second)
           (lambda (action)
             `(html (head (title ,change-pass-title))
                    (body
                     ,@(make-student-info-exprs acct partners)
                     ,@(pass-entry-html action message change-pass-title pass-entry-req-first pass-entry-req-second)
                     ))))))
      
      ;; get-pass-entry-req/staff string string -> pass-entry-req/staff
      (define get-pass-entry-req/staff
        (lambda (staff-name message)
          (send/suspend/struct
           pass-entry-req/staff (first second)
           (lambda (action)
             `(html (head (title ,staff-change-pass-title))
                    (body
                     (h3 ,(format "Change password for ~a" staff-name))
                     ,@(pass-entry-html action message staff-change-pass-title pass-entry-req/staff-first pass-entry-req/staff-second)))))))
           
      ;; add-binding-to-url-string: string string string -> string
      (define add-binding-to-url-string
        (lambda (url-str name val)
          (let ([url (string->url url-str)])
            (url->string
             (make-url (url-scheme url)
                       (url-host url)
                       (url-port url)
                       (url-path url)
                       (url-params url) ;; This is the continuation web-noise
                       (format "~a=~a" name val)
                       #f)))))
      
      ;; make-title-link: homework-assignment -> x-expression
      ;; make an anchor or a string from the homework-assignment-title and homework-assignment-url
      (define make-title-link
        (lambda (assmt)
          (if (homework-assignment-url assmt)
              `(a ([href ,(url->string (homework-assignment-url assmt))])
                  ,(homework-assignment-title assmt))
              (homework-assignment-title assmt))))
      
      ;; make-assignment-list: action (listof assignment) -> (listof x-expression)
      ;; make the html for the list of overdue assignment links
      (define make-assignment-list
        (lambda (assmts)
          (map
           (lambda (assmt)
             `(tr (td ,(make-title-link assmt) ,@(size-and-date-annotation assmt))
                  (td  ,(format "Due on: ~a" (date->string (homework-assignment-due-date assmt) #t)))))
           assmts)))
      
      ;; size-and-date-annotation: full-assignment -> (listof string)
      ;; produce a list of annotations for this list item
      (define size-and-date-annotation
        (lambda (assmt)
          (if (full-assignment-size assmt)
              (list `(td ,(format " (submitted on: ~a,  size ~a) "
                                                  (date->string (seconds->date (full-assignment-date assmt)) #t)
                                                  (full-assignment-size assmt))))
              '())))
      
      ;; get-upload-file-req: student-account (listof student-account) string string -> upload-file-req
      ;; given the name of the assignment being uploaded get the request for the
      ;; Upload Homework File page.
      (define get-upload-file-req
        (lambda (acct partners assmt-name message)
          (send/suspend/struct
           upload-file-req (definitions interactions)
           (lambda (action)
             `(html
               (title ,upload-homework-title)
               (body
                ,@(make-student-info-exprs acct partners)
                (h3 ,upload-homework-title)
                (p ,message ,assmt-name)
                (p
                 (form ([action ,action] [method "post"][enctype "multipart/form-data"])
                       (p "Your definitions file: ")
                       (input ([type "file"] [name ,upload-file-req-definitions]))
                       (br)
                       (p "Your test cases file: ")
                       (input ([type "file"] [name ,upload-file-req-interactions]))
                       (br)
                       (input ([type "submit"] [name "upload"] [value "Upload"]))))))))))
            
      ;; get-upload-confirm-req: student-account (listof student-account) string -> upload-confirm-req
      ;; given the name of the assignment being uploaded get the request for the
      ;; Upload Confirm page.
      (define get-upload-confirm-req
        (lambda (acct partners assmt-name)
          (send/suspend/struct
           upload-confirm-req ([confirm? notnull?])
           (lambda (action)
             `(html (head (title ,upload-confirm-title))
                    (body
                     ,@(make-student-info-exprs acct partners)
                     (h3 ,upload-confirm-title)
                     (p "Upload file and overwrite any previous submission for assignment: " ,assmt-name "?")
                     (form ([action ,action] [method "post"] [enctype "multipart/form-data"])
                           (input ([type "submit"] [name ,upload-confirm-req-confirm?] [value "Confirm"]))
                           (input ([type "submit"] [name "whocares"] [value "Cancel"]))
                           
                           )
                     ))))))
      
      ;; get-upload-complete-req: student-account (listof student-account) full-assignment -> upload-complete-req
      ;; get the request for the Upload Complete page
      (define get-upload-complete-req
        (lambda (acct partners assmt)
          (send/suspend/struct
           upload-complete-req ()
           (lambda (action)
             (apply make-continue-html
                    (cons action
                          (cons upload-complete-title
                                (cons (format "Successfully uploaded file for assignment: ~a, on ~a (size: ~a)"
                                              (homework-assignment-title assmt)
                                              (date->string (seconds->date (full-assignment-date assmt)) #t)
                                              (full-assignment-size assmt))
                                      (cons '(p "Please print this page in case there are any problems with your submission.")
                                            (make-student-info-exprs acct partners))))))))))
      
      ;; get-sorry-charlie-req: string -> request
      ;; get the request for the Sorry Charlie page
      (define get-sorry-charlie-req
        (lambda (assmt-name)
          (send/suspend/struct
           sorry-charlie-req ()
           (lambda (action)
             (make-continue-html action
                                 sorry-charlie-title
                                 (format "The assignment link you have selected has expired: ~a" assmt-name))))))
    
      ;; get-manage-account-req: student-account (listof student-account) number -> manage-account-req
      ;; display the student's partners
      ;; display a link for changing the student's password
      ;; if not exceed max-partners display a link to add a partner
      (define get-manage-account-req
        (lambda (acct partners max-partners)
          (let ([can-add-partner? (< (length partners) max-partners)])
            (send/suspend/struct
             manage-account-req ([changepass? notnull?]
                                 [addpartner? notnull?]
                                 [logout? notnull?])
             (lambda (action)
               `(html
                 (title ,manage-account-title)
                 (body
                  ,@(make-student-info-exprs acct partners)
                  (h3 ,manage-account-title)
                  (p (a ([href ,(urlof action manage-account-req-changepass?)]) "Change Password"))
                  ,@(if can-add-partner?
                        `((p (a ([href ,(urlof action manage-account-req-addpartner?)]) "Add Partner")))
                        '())
                  (p (a ([href ,action]) "Homework Submission"))
                  (p (a ([href ,(urlof action manage-account-req-logout?)]) "Logout")))))))))
  ))

)
