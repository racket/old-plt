
(module bug-report-server mzscheme
  (require "bug-report-server-info.ss"
           (lib "date.ss")
           (lib "match.ss")
           (lib "smtp.ss" "net")
           (lib "head.ss" "net"))

  ;; run-server : number (inport outport -> void) number -> A
  ;; creates a server on `port-number', 
  ;; calling `handler' in a separate thread for each connection.
  (define (run-server port-number handler connection-timeout)
    (let ([l (tcp-listen port-number)]
	  [can-break? (break-enabled)])
      (dynamic-wind
       void
       (lambda ()
	    ;; loop to handle connections
         (let loop ()
           (with-handlers ([not-break-exn? void])
		;; Make a custodian for the next session:
             (let ([c (make-custodian)])
               (parameterize ([current-custodian c])
		    ;; disabled breaks during session set-up...
                 (parameterize ([break-enabled #f])
		      ;; ... but enabled breaks while blocked on an accept:
                   (let-values ([(r w) ((if can-break?
                                            tcp-accept/enable-break
                                            tcp-accept)
                                        l)])
			;; Handler thread:
                     (let ([t (thread (lambda () 
                                        (when can-break?
                                          (break-enabled #t))
                                        (handler r w)))])
			  ;; Clean-up and timeout thread:
                       (thread (lambda () 
                                 (object-wait-multiple connection-timeout t)
                                 (when (thread-running? t)
				      ;; Only happens if connection-timeout is not #f
                                   (break-thread t))
                                 (object-wait-multiple connection-timeout t)
                                 (custodian-shutdown-all c)))))))))
           (loop)))
       (lambda () (tcp-close l)))))
  
  ;; log-msg : string -> void
  ;; logs errors and other messages
  (define (log-msg x)
    (display "[")
    (display (date->string (seconds->date (current-seconds))))
    (display "] ")
    (display x)
    (newline))
    
  ;; handler : inport outport -> void
  (define (handler in out)
    (with-handlers ([not-break-exn?
                     (lambda (x)
                       (log-msg (exn-message x)))])
      (let ([data (read in)])
        (unless (and (list? data)
                     (= 13 (length data)))
          (error 'bug-report "expected a list of length 13, got ~e" data))
        (match data
          [`(,email
             ,originator
             ,summary
             ,severity
             ,priority
             ,class
             ,release
             ,environment
             ,tools
             ,docs
             ,collections
             ,description
             ,reproduce)
           (check-string 'email email)
           (check-string 'originator originator)
           (check-string 'summary summary)
           (check-string 'severity severity)
           (check-string 'priority priority)
           (check-string 'class class)
           (check-string 'release release)
           (check-string 'environment environment)
           (check-string 'tools tools)
           (check-string 'docs docs)
           (check-string 'collections collections)
           (check-list-of-strings 'description description)
           (check-list-of-strings 'reproduce reproduce)
           
           (log-msg (format "received bug report from ~a" originator))
            
           (smtp-send-message
            "cs.rice.edu"
            email
            (list "plt-gnats")
            (insert-field
             "X-Mailer"
             (format "Bug Report Server (running in mz ~a)" (version))
             (insert-field     
              "Subject" 
              summary
              (insert-field
               "To"
               "plt-gnats@cs.rice.edu"
               (insert-field
                "From"
                email
                empty-header))))
            (append
             (list
              ">Category:       all"
              (format ">Synopsis:       ~a" summary)
              ">Confidential:   no"
              (format ">Severity:       ~a" severity)
              (format ">Priority:       ~a" priority)
              (format ">Class:          ~a" class)
              ">Submitter-Id:   unknown"
              (format ">Originator:     ~a" originator)
              ">Organization:"
              "titan"
              (format ">Release:        ~a" release)
              ">Environment:"
              (format "~a" environment)
              (format "Tools: ~a" tools)
              "Docs Installed:" docs
              "Collections:"
              collections
              ">Fix: ")
             (cons
              ">Description:"
              description)
             (cons
              ">How-To-Repeat:"
              reproduce)))
           
           (log-msg (format "sent bug report from ~a" originator))]))))
  
  ;; check-string : sym TST -> void
  ;; raises an error if `str' is not a string
  (define (check-string name str)
    (unless (string? str)
      (error 'bug-report "expected field ~a to be a string, got ~e" name str)))
  
  ;; check-list-of-strings : sym TST -> void
  ;; raises an error if `strs' is not a list of string
  (define (check-list-of-strings name strs)
    (unless (and (list? strs)
                 (andmap string? strs))
      (error 'bug-report "expected field ~a to be a list of strings, got ~e" name strs)))
               
  ;; start the server
  (log-msg "started the server")
  (run-server bug-report-server-id handler 100))
