#cs
(module handin-server mzscheme
  (require (lib "thread.ss")
	   (lib "mzssl.ss" "openssl")
	   (lib "file.ss")
	   (lib "date.ss")
	   "md5.ss")

  (define log-port (open-output-file "log.ss" 'append))

  (define current-session (make-parameter 0))

  (define (LOG str . args)
    (fprintf log-port
	     "(~a ~s ~s)~n"
	     (current-session)
	     (parameterize ([date-display-format 'iso-8601])
	       (date->string (seconds->date (current-seconds)) #t))
	     (apply format str args)))

  (define (get-config which default)
    (get-preference which 
		    (lambda () default)
		    #f
		    "config.ss"))

  (define SESSION-TIMEOUT (get-config 'session-timeout 300))
  (define MAX-UPLOAD (get-config 'max-upload 500000))
  (define MAX-UPLOAD-KEEP (get-config 'max-upload-keep 9))
  (define ID-REGEXP (get-config 'id-regexp #rx"^.*$"))
  (define ID-DESC (get-config 'id-desc "anything"))
  (define ALLOW-NEW-USERS? (get-config 'allow-new-users #f))

  (define (check-id s)
    (regexp-match ID-REGEXP s))

  (define (save-submission s part)
    ;; Shift old files:
    (when (file-exists? (format "~a0" part))
      (when (file-exists? (format "~a~a" part MAX-UPLOAD-KEEP))
	(delete-file (format "~a~a" part MAX-UPLOAD-KEEP)))
      (let loop ([n MAX-UPLOAD-KEEP][log? #t])
	(unless (zero? n)
	  (let ([exists? (file-exists? (format "~a~a" part (sub1 n)))])
	    (when exists?
	      (when log?
		(LOG "shifting ~a" (sub1 n)))
	      (rename-file-or-directory 
	       (format "~a~a" part (sub1 n))
	       (format "~a~a" part n)))
	    (loop (sub1 n) (not exists?))))))
    (with-output-to-file (format "~a0" part)
      (lambda () (display s))))

  (define (accept-specific-submission user assignment r r-safe w)
    (parameterize ([current-directory (build-path "active" assignment)])
      (unless (directory-exists? user)
	(make-directory user))
      (parameterize ([current-directory user])
	(let ([len (read r-safe)])
	  (unless (and (number? len)
		       (integer? len)
		       (positive? len))
	    (error 'handin "bad length: ~s" len))
	  (unless (len . < . MAX-UPLOAD)
	    (error 'handin 
		   "max handin file size is ~s bytes, file to handin is too big (~s bytes)"
		   MAX-UPLOAD
		   len))
	  (fprintf w "go\n")
	  (unless (regexp-match #rx"[$]" r-safe)
	    (error 'handin 
		   "did not find start-of-content marker"))
	  (let ([s (read-string len r)])
	    (unless (and (string? s) (= (string-length s) len))
	      (error 'handin 
		     "error uploading (got ~s, expected ~s bytes)"
		     (if (string? s) (string-length s) s)
		     len))
	    (LOG "checking ~a for ~a" assignment user)
	    (let ([part
		   (let ([checker (build-path 'up "checker.ss")])
		     (if (file-exists? checker)
			 ((dynamic-require `(file ,(path->complete-path checker)) 'checker)
			  user
			  s)
			 "handin"))])
	      (fprintf w "confirm\n")
	      (let ([v (read (make-limited-input-port r 50))])
		(if (eq? v 'check)
		    (begin
		      (LOG "saving ~a for ~a" assignment user)
		      (save-submission s part)
		      (fprintf w "done\n"))
		    (error 'handin "upload not confirmed: ~s" v)))))))))

  (define orig-custodian (current-custodian))

  ;; On startup, check that the prefs file is not locked:
  (put-preferences null null 
		   (lambda (f)
		     (delete-file f)
		     (put-preferences null null 
				      (lambda (f)
					(error 'handin-server
					       "unable to clean up lock file: ~s" f))
				      "users.ss"))
		   "users.ss")

  (define (put-user key val)
    ;; Although we don't have to worry about trashing the
    ;;  prefs file, we do have to worry about a thread
    ;;  getting killed while it locks the pref file.
    ;; Avoid the problem by using orig-custodian.
    (call-in-nested-thread
     (lambda ()
       (put-preferences (list key)
			(list val)
			(lambda (f)
			  (error 
			   'handin 
			   "user database busy; please try again, and alert the adminstrator is problems persist"))
			"users.ss"))
     orig-custodian))

  (define (add-new-user username r-safe w)
    (let ([full-name (read r-safe)]
	  [id (read r-safe)]
	  [passwd (read r-safe)])
      (unless (and (string? full-name)
		   (string? id)
		   (string? passwd))
	(error 'handin "bad user-addition request"))
      (unless (check-id id)
	(error 'handin "id has wrong format: ~a; need ~a for id" id ID-DESC))
      (put-user (string->symbol username)
		(list (md5 passwd) id full-name))
      (fprintf w "ok~n")))
  
  (define (change-user-passwd username r-safe w old-user-data)
    (let ([new-passwd (read r-safe)])
      (LOG "change passwd for ~a" username)
      (unless (string? new-passwd)
	(error 'handin "bad password-change request"))
      (put-user (string->symbol username)
		(cons (md5 new-passwd) (cdr old-user-data)))
      (fprintf w "ok~n")))

  (define (accept-submission-or-update active-assignments r r-safe w)
    (fprintf w "~s~n" active-assignments)
    ;; Get username and password:
    (let ([username (read r-safe)]
	  [passwd (read r-safe)])
      (let ([user-data
	     (and (string? username)
		  (get-preference (string->symbol username)
				  (lambda () #f)
				  #f
				  "users.ss"))])
	(cond
	 [(eq? passwd 'create)
	  (when user-data
	    (error 'handin "username already exists: ~a" username))
	  (unless ALLOW-NEW-USERS?
	    (error 'handin "new users not allowed: ~a" username))
	  (LOG "create user: ~a" username)
	  (add-new-user username r-safe w)]
	 [(and user-data
	       (string? passwd)
	       (equal? (md5 passwd) (car user-data)))
	  (LOG "login: ~a" username)
	  (let ([assignment (read r-safe)])
	    (LOG "assignment for ~a: ~a" username assignment)
	    (if (eq? assignment 'change)
		(change-user-passwd username r-safe w user-data)
		(if (member assignment active-assignments)
		    (begin
		      (fprintf w "ok\n")
		      (accept-specific-submission username assignment r r-safe w))
		    (error 'handin "not an active assignment: ~a" assignment))))]
	 [else
	  (LOG "failed login: ~a" username)
	  (error 'handin "bad username or password for ~a" username)]))))
  
  (define assignment-list
    (directory-list "active"))

  (LOG "server started ------------------------------")
  
  (define session-count 0)

  (parameterize ([error-display-handler
		  (lambda (msg exn)
		    (LOG msg))])
    (run-server
     7979
     (lambda (r w)
       (parameterize ([current-session (begin
					 (set! session-count (add1 session-count))
					 session-count)])
	 (let-values ([(here there) (ssl-addresses r)])
	   (LOG "connect from ~a" there))
	 (let ([r-safe (make-limited-input-port r 1024)])
	   (fprintf w "handin\n")
	   ;; Check protocol:
	   (with-handlers ([not-break-exn?
			    (lambda (exn)
			      (let ([msg (if (exn? exn)
					     (exn-message exn)
					     (format "~e" exn))])
				(LOG "ERROR: ~a" msg)
				(fprintf w "(server error: ~s)\n" msg)))])
	     (let ([protocol (read r-safe)])
	       (if (eq? protocol 'original)
		   (fprintf w "original\n")
		   (error 'handin "unknown protocol: ~s" protocol)))
	     (accept-submission-or-update assignment-list r r-safe w)
	     (LOG "normal exit")))))
     SESSION-TIMEOUT
     (lambda (exn)
       (printf "~a~n" (if (exn? exn)
			  (exn-message exn)
			  exn)))
     (lambda (port-k)
       (let ([l (ssl-listen port-k 5 #t)])
	 (ssl-load-certificate-chain! l "server-cert.pem")
	 (ssl-load-private-key! l "private-key.pem")
	 l))
     ssl-close
     ssl-accept
     ssl-accept/enable-break)))
