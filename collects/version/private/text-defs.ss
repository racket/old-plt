(module text-defs mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "string-constant.ss" "string-constants"))

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
	      (printf (string-constant vc-details-text))
	      (printf "~a~n" details)))
      
      (define (show-error-ok title caption)
	(show-ok #f
		 (format (string-constant vc-error-format) caption)
		 #f))

      (define (make-wait-dialog parent title caption close-fun)
	(list title caption))

      (define (show-wait-dialog dialog)
        ; dialog is the pair returned by make-wait-dialog 
	(printf "~a~n" (cadr dialog)))
      
      (define (hide-wait-dialog dialog)
	(void)))))


