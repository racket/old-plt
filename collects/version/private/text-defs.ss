(module text-defs mzscheme
  (require (lib "unitsig.ss"))

  (require "checksigs.ss")

  (provide text-defs@) 

  (define text-defs@
    (unit/sig defs^
      (import)

      (define (run-thunk th)
	(th))

      (define (show-ok title caption details)
	(printf "~a~n" caption)
	(when details
	      (printf "Details:~n")
	      (printf "~a~n" details)))
      
      (define (show-error-ok parent title caption)
	(show-ok title (format "Error: ~a" caption parent)))

      (define (make-wait-dialog parent title caption close-fun)
	(list title caption))

      (define (show-wait-dialog dialog)
        ; dialog is the pair returned by make-wait-dialog 
	(printf "~a~n" (cadr dialog)))
      
      (define (hide-wait-dialog dialog)
	(void)))))


