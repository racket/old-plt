(module gui-defs mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "file.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "launcher.ss" "launcher"))

  (require "checksigs.ss")

  (provide gui-defs@) 

  (define gui-defs@
    (unit/sig defs^
      (import)

      (define progname 
	(let ([fullname
		(file-name-from-path 
		 (mred-program-launcher-path "Check Version"))])
	  (if (eq? (system-type) 'windows)
	      (substring fullname
			 0
			 (- (string-length fullname) 4))
	      fullname)))

      (define (get-yes-no s1 s2)	      
	(message-box s1 s2
	 #f
	 '(yes-no)))

     (define (show-ok s1 s2)
       (message-box s1 s2 #f '(ok)))
   
     (define (show-error-ok s1 s2)
       (show-ok s1 (format "Error: ~a" s2))))))
