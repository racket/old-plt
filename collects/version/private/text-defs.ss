(module text-defs mzscheme
  (require (lib "unitsig.ss"))

  (require "checksigs.ss")

  (provide text-defs@) 

  (define text-defs@
    (unit/sig defs^
      (import)

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
