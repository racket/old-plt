(module gui-defs mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))

  (require "checksigs.ss")

  (provide gui-defs@) 

  (define gui-defs@
    (unit/sig defs^
      (import)

      (define (run-thunk th)
	(parameterize
	 ([current-eventspace (make-eventspace)])
	 (queue-callback th)))

      (define (show-ok title caption details)
	(message-box title 
		     (if details			
			 (format "~a~nDetails:~n~a"
				 caption details)
			 caption)
		     #f
		     '(ok)))
   
     (define (show-error-ok title caption)
       (show-ok title (format "Error: ~a" caption) #f))

     (define (make-wait-dialog parent title caption close-fun)
       (let ([dialog 
	      (instantiate 
	       dialog% () 
	       (label title)
	       (parent parent)
	       (width 100)
	       (height 50)
	       (stretchable-width #t)
	       (stretchable-height #t))])
	 (instantiate 
	  message% ()
	  (label caption)
	  (parent dialog))
	 (instantiate 
	  button% () 
	  (label "Cancel") 
	  (parent dialog)
	  (callback (lambda (button ce)
		      (close-fun)
		      (send dialog show #f))))
	 dialog))

     (define (show-wait-dialog dialog)
       (send dialog center)
       (thread
	(lambda () 
	  (send dialog show #t)))
       (send dialog focus))

     (define (hide-wait-dialog dialog)
       (send dialog show #f)))))
