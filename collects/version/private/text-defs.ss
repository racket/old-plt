(module text-defs mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "file.ss"))
  (require (lib "launcher.ss" "launcher"))

  (require "checksigs.ss")

  (provide text-defs@) 

  (define text-defs@
    (unit/sig defs^
      (import)

      (define progname 
	(let ([fullname
		(file-name-from-path 
		 (mzscheme-program-launcher-path "Check Version-nw"))])
	  (if (eq? (system-type) 'windows)
		   (substring fullname
			      0
			      (- (string-length fullname) 4))
		   fullname)))

      (define (get-yes-no s1 s2)
	(printf "~a [y/n]: " s2)
	(let loop ()
	  (let ([sym (read)])
	    (cond
	     [(member sym '(y Y yes YES))
	      'yes]
	     [(member sym '(n N no NO))
	      'no]
	     [else
	      (printf "Please answer 'y' or 'n': ")
	      (loop)]))))

      (define (show-ok s1 s2)
	(printf "~a~n" s2))
      
      (define (show-error-ok s1 s2)
	(show-ok s1 (format "Error: ~a" s2))))))
