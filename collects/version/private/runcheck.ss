(module runcheck mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "url.ss" "net"))

  (require "checksigs.ss")

  (provide runcheck@)

  (define runcheck@
    (unit/sig empty^

      (import defs^)	      

      (define check-question 
	"Check for a newer version of DrScheme over the Internet?")
      (define cannot-get-version
	"Unable to obtain latest version from PLT version server")
      (define newer-format
	(string-append 
	 "You are running DrScheme version ~a.~n"
	 "A newer version, ~a, is available.~n"
	 "You may download it from http://www.drscheme.org/"))
      (define latest-format 
	"You are running the latest DrScheme, version ~a")
      
      (define drscheme-url-string
	(format "http://download.plt-scheme.org/cgi-bin/check-version?package=~a&version=~a"
	"drscheme"
	(version)))
      
      (define newer-version? string>=?)

      (define timeout 60)
      
      (define (timer-proc)
	(let loop ([n 0])
	  (when (> n timeout)
		(show-ok "Network timeout" 
			 "Can't connect to PLT version server")
		(exit))
	  (sleep 1)
	  (loop (add1 n))))
      
      (when (eq? 'yes
		 (get-yes-no "Version check" 
			     check-question))
	    (let* ([port (with-handlers 
			  ((void 
			    (lambda _ 
			      (show-error-ok
			       "Network failure" 
			       "Can't connect to PLT version server")
			      (exit))))
			  (get-pure-port (string->url drscheme-url-string)))]
		   [timeout-thread (thread timer-proc)]
		   [responses 
		    (let loop ()
		      (let ([r (read port)])
			(if (eof-object? r)
			    '()
			    (cons r (loop)))))]
		   [_ (kill-thread timeout-thread)]
		   [curr-version (version)])
	      (close-input-port port)

	      ; responses should be one of
	      ;  (need-update n)
	      ;  (up-to-date)
              ;  (error)

	      (let ([conclusion (car responses)])
		(cond 
		 [(eq? conclusion 'update-available)
		  (show-ok "Update available" 
			   (format newer-format 
				   curr-version
				   (cadr responses)))]
		 [(eq? conclusion 'up-to-date)
		  (show-ok "Up to date" 
			   (format latest-format 
				   curr-version))]
		 [(eq? conclusion 'error)
		  (show-error-ok "Server error"
				 cannot-get-version)]
		 [else
		  (show-error-ok "Error"
				 "Unknown response from version server")])))))))
