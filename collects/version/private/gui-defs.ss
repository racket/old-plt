(module gui-defs mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "mred.ss" "mred"))

  (require "checksigs.ss")

  (provide gui-defs@) 

  (define gui-defs@
    (unit/sig defs^
      (import)

      (define (get-yes-no s1 s2)	      
	(message-box s1 s2
	 #f
	 '(yes-no)))

     (define (show-ok s1 s2)
       (message-box s1 s2 #f '(ok)))
   
     (define (show-error-ok s1 s2)
       (show-ok s1 (format "Error: ~a" s2))))))
