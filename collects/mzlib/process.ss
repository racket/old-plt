
(module process mzscheme
  (provide process
	   process*
	   process/ports
	   process*/ports
	   system
	   system*)

  (require (lib "thread.ss"))

  ;; Helpers: ----------------------------------------

  (define (shell-path/args)
    (case (system-type)
      ((unix) '("/bin/sh" "-c"))
      ((windows) (let ([d (find-system-path 'sys-dir)])
		   (let ([cmd (build-path d "cmd.exe")])
		     (if (file-exists? cmd)
			 cmd
			 (build-path d "command.com")))))
      (else (error "don't know what shell to use for ~e." (system-type)))))

  (define (if-stream-out p)
    (if (file-stream-port? p)
	p
	(if (output-port? p)
	    #f
	    (raise-type-error
	     'subprocess
	     "output port"
	     p))))
	
  (define (if-stream-in p)
    (if (file-stream-port? p)
	p
	(if (input-port? p)
	    #f
	    (raise-type-error
	     'subprocess
	     "input port"
	     p))))

  (define (streamify-in cin in)
    (if (and cin (not (file-stream-port? cin)))
	(begin
	  (thread (lambda () 
		    (copy-port cin in)
		    (close-output-port in)))
	  #f)
	in))

  (define (streamify-out cout out)
    (if (and cout (not (file-stream-port? cout)))
	(begin
	  (thread (lambda () (copy-port out cout)))
	  #f)
	out))

  ;; Old-style functions: ----------------------------------------

  (define (process*/ports cout cin cerr exe . args)
    (let-values ([(subp out in err pid) (apply subprocess 
					       (if-stream-out cout)
					       (if-stream-in cin)
					       (if-stream-out cerr)
					       exe args)])      
      (list (streamify-out cout out)
	    (streamify-in cin in)
	    pid
	    (streamify-out cerr err)
	    (letrec ((control
		      (lambda (m)
			(case m
			  ((status) (let ((s (subprocess-status subp)))
				      (cond ((not (integer? s)) s)
					    ((zero? s) 'done-ok)
					    (else 'done-error))))
			  ((wait) (subprocess-wait subp))
			  (else
			   (raise-type-error 'control-process "'status or 'wait" m))))))
	      control))))

  (define (process/ports out in err str)
    (apply process*/ports out in err (append (shell-path/args) (list str))))

  (define (process* exe . args)
    (apply process*/ports #f #f #f exe args))

  (define (process str)
    (apply process* (append (shell-path/args) (list str))))

  ;; Note: these always use current ports
  (define (system* exe . args)
    (let ([cout (current-output-port)]
	  [cin (current-input-port)]
	  [cerr (current-error-port)])
      (let-values ([(subp out in err pid)
		    (apply
		     subprocess
		     (if-stream-out cout)
		     (if-stream-in cin)
		     (if-stream-out cerr)
		     exe args)])
	(streamify-out cout out)
	(streamify-in cin in)
	(streamify-out cerr err)
	(subprocess-wait subp)
	(zero? (subprocess-status subp)))))

  (define (system str)
    (apply system* (append (shell-path/args) (list str)))))
