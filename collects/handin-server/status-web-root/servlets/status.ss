
(module status mzscheme
  (require (lib "file.ss")
	   (lib "list.ss")
	   (lib "date.ss")
	   (lib "unitsig.ss")
	   (lib "servlet-sig.ss" "web-server")
	   (lib "md5.ss" "handin-server"))

  (provide status-servlet)
  
  (define TOPICS-PER-PAGE 25)
  
  (define handin-dir (current-directory))
  (define active-dir (build-path handin-dir "active"))
  (define inactive-dir (build-path handin-dir "inactive"))

  (define (clean-str s)
    (regexp-replace
     " *$" 
     (regexp-replace "^ *" s "")
     ""))

  (define (update-status status tag val)
    (let loop ([status status])
      (cond
       [(null? status) (list (cons tag val))]
       [(eq? (caar status) tag) (cons (cons tag val) (cdr status))]
       [else (cons (car status) (loop (cdr status)))])))

  (define (get-status status tag default)
    (let ([a (assq tag status)])
      (if a
	  (cdr a)
	  default)))

  (define (make-page title . body)
    `(html (head (title ,title))
	   (body ([bgcolor "white"])
		 (p ((align "center"))
		    ,title)
		 ,@body)))

  (define status-servlet
    (unit/sig ()
      (import servlet^)

      (define (make-k k tag)
	(format "~a~atag=~a" k 
		(if (regexp-match #rx"^[^#]*[?]" k)
		    "&"
		    "?")
		tag))
      (define (select-k request)
	(let ([a (assq 'tag (request-bindings request))])
	  (and a (cdr a))))
      (define (link-tag k tag label)
	`(a ((href ,(make-k k tag)))
	    ,label))

      (define (cleanup-name f)
	(format "~a.scm" (regexp-replace #rx"-?0$" f "")))

      (define (handin-link k user hi)
	(let* ([dir (build-path (if (directory-exists? (build-path "active" hi))
				    "active"
				    "inactive")
				hi
				user)]
	       [l (filter (lambda (f)
			    (regexp-match #rx"[^0-9]0$" f))
			  (with-handlers ([not-break-exn? (lambda (x) null)])
			    (directory-list dir)))])
	  (if (pair? l)
	      (cdr
	       (apply
		append
		(map
		 (lambda (i) `((br) ,i))
		 (map (lambda (f)
			 (let ([hi (build-path dir f)])
			   `(font 
			     ()
			     (a ((href ,(make-k k hi)))
				,(cleanup-name f))
			     " ("
			     ,(date->string
			       (seconds->date
				(file-or-directory-modify-seconds hi))
			       #t)
			     ")")))
		      l))))
	      (list (format "No handins for ~a" hi)))))

      (define (one-status-page status for-handin)
	(let ([user (get-status status 'user (lambda () "???"))])
	  (let ([next
		 (send/suspend
		  (lambda (k)
		    (make-page
		     (format "User: ~a, Handin: ~a" user for-handin)
		     `(p ,@(handin-link k user for-handin))
		     `(p (a ((href ,(make-k k "allofthem")))
			    ,(format "All handins for ~a" user))))))])
	    (let ([tag (select-k next)])
	      (if (string=? tag "allofthem")
		  (all-status-page status)
		  (download tag))))))

      (define (all-status-page status)
	(let ([l (quicksort
		  (append (directory-list "active")
			  (with-handlers ([not-break-exn? (lambda (x) null)])
			    (directory-list "inactive")))
		  string<?)]
	      [user (get-status status 'user (lambda () "???"))])
	  (let ([next
		 (send/suspend
		  (lambda (k)
		    (make-page
		     (format "All Handins for ~a" user)
		     `(table
		       ((bgcolor "#ddddff"))
		       ,@(map (lambda (hi)
				`(tr (td ((bgcolor "white")) ,hi)
				     (td ((bgcolor "white")) ,@(handin-link k user hi))))
			      l)))))])
	    (let ([tag (select-k next)])
	      (download tag)))))

      (define (download tag)
	(list "application/data"
	      (with-input-from-file tag
		(lambda ()
		  (read-string (file-size tag))))))

      (define (status-page status for-handin)
	(if for-handin
	    (one-status-page status for-handin)
	    (all-status-page status)))

      (define (login-page status for-handin errmsg)
	(let ([request
	       (send/suspend
		(lambda (k)
		  (make-page
		   "Handin Status Login"
		   `(form ([action ,k] [method "post"])
			  (table
			   ((align "center"))
			   (tr (td ((colspan "2") (align "center"))
				   (font ((color "red"))
					 ,(if errmsg
					      errmsg
					      'nbsp))))
			   (tr (td "Username")
			       (td (input ([type "text"] [name "user"] [size "20"] [value ""]))))
			   (tr (td nbsp))
			   (tr (td "Password")
			       (td (input ([type "password"] [name "passwd"] [size "20"] [value ""]))))
			   (td ((colspan "2") (align "center"))
			       (input ([type "submit"] [name "post"] [value "Login"]))))))))])
	  (let ([user (clean-str (cdr (assq 'user (request-bindings request))))]
		[passwd (cdr (assq 'passwd (request-bindings request)))])
	    (let ([user-data (get-preference (string->symbol user)
					     (lambda () #f)
					     #f
					     "users.ss")])
	      (cond
	       [(and user-data
		     (string? passwd)
		     (equal? (md5 passwd) (car user-data)))
		(status-page (update-status status 'user user) for-handin)]
	       [else
		(login-page status for-handin "Bad username or password")])))))

      (let ([a (assq 'handin (request-bindings initial-request))])
	(login-page null (and a (cdr a)) #f))

      )))

(require status)
status-servlet

