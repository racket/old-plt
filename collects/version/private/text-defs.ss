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

      ; string (list string (listof string)) (union (listof string) #f) -> void
      (define (show-ok title captions details)
	(printf "~a~n" (car captions))
	(for-each
	 (lambda (c)
	   (printf " ~a~n" c))
	 (cdr captions))
	(when (and details (not (null? details)))
	      (printf (string-constant vc-details-text))
              (for-each
		(lambda (d)		
		  (printf " ~a~n" d))
		details)))
      
      (define (show-error-ok title caption)
	(show-ok title
		 (list (format (string-constant vc-error-format) caption))
		 #f))

      (define (make-wait-dialog parent title caption close-fun)
	(list title caption))

      (define (show-wait-dialog dialog)
        ; dialog is the pair returned by make-wait-dialog 
	(printf "~a~n" (cadr dialog)))
      
      (define (hide-wait-dialog dialog)
	(void)))))


