
;; Glue mzc to the real MrSpidey. In contrast, spnoop.ss provides
;;  dummy version of these functions that don't use the real MrSpidey

(unit/sig compiler:mrspidey^
  (import (mrspidey : mrspidey:sba^)
	  compiler:library^)
  
  (define copy-annotations!
    (lambda (new old)
      (mrspidey:set-parsed-ftype! new (mrspidey:parsed-ftype old))
      (mrspidey:set-parsed-check! new (mrspidey:parsed-check old))
      (mrspidey:set-parsed-atprim! new (mrspidey:parsed-atprim old))
      (mrspidey:set-app-tvar-args! new (mrspidey:app-tvar-args old))
      (mrspidey:set-binding-refs! new (mrspidey:binding-refs old))
      (mrspidey:set-binding-mutated! new (mrspidey:binding-mutated old))
      new))

  (define binding-mutated mrspidey:binding-mutated)

  (define analyze-program-sexps mrspidey:analyze-program-sexps)

  (define (constant-value ast)
    (let ([fail (lambda () (values #f #f))])
      (let loop ([ftype (mrspidey:parsed-ftype ast)])
	; (printf "ftype ~a~n" ftype)
	(if ftype
	    (cond
	     [(mrspidey:fo-FlowType? ftype)
	      (let ([d (mrspidey:fo-FlowType-def ftype)])
		; (printf "d ~a~n" d)
		(cond
		 [(mrspidey:atconst? d) (values #t (mrspidey:atconst-c d))]
		 [(and (mrspidey:atvalues? d) (= 1 (length (mrspidey:atvalues-values d))))
		  ; (printf "go again~n")
		  (loop (car (mrspidey:atvalues-values d)))]
		 [else (fail)]))]
	     [(mrspidey:Tvar? ftype)
	      (let ([objs (mrspidey:Tvar-objs ftype)])
		(if (= 1 (length objs))
		    (let ([misc (mrspidey:AV-misc (car objs))])
		      ; (printf "obj-misc: ~a~n" misc)
		      (if (or (symbol? misc) (number? misc) (boolean? misc) (char? misc))
			  ;; Looks simple enough; let's give it a try
			  (let ([sdl (SDL-type ast)])
			    (if (and sdl (pair? sdl) (eq? (car sdl) 'quote))
				(values #t (cadr sdl))
				(fail)))
			  (fail)))
		    (fail)))]
	     [else (fail)])
	    (fail)))))

#|
  (define (constant-value ast)
    (printf "calculate~n")
    (let ([sdl (SDL-type ast)])
      (printf "~a~n" sdl)
      (if (and (pair? sdl) (eq? (car sdl) 'quote))
	  (values #t (cadr sdl))
	  (values #f #f))))
|#

  (mrspidey:st:constants #t)
  (mrspidey:st:const-merge-size 2)
  (mrspidey:st:unit-read-za #f)
  (mrspidey:st:unit-write-za #f)

  (define (SDL-type ast)
    (let* ([ftype (mrspidey:parsed-ftype ast)])
      (and ftype (mrspidey:FlowType->SDL ftype))))

  (define parsed-ftype mrspidey:parsed-ftype)

  (define Tvar-objs mrspidey:Tvar-objs)
  (define Tvar? mrspidey:Tvar?)

  (define fo-FlowType? mrspidey:fo-FlowType?)

  (define FlowType->Tvar mrspidey:FlowType->Tvar)

  (define (prim-av? av) 
    (mrspidey:atprim? 
     (mrspidey:AV-misc av)))

  (define (fo-ftype->AVs fo-ftype)
    (letrec ([AVs-from-ftypes
	      (lambda (ftypes)
		(if (null? ftypes) 
		    '()
		    (let ([the-ftype (car ftypes)])
		      (if (mrspidey:Tvar? the-ftype)
			  (append (mrspidey:Tvar-objs the-ftype)
				  (AVs-from-ftypes (cdr ftypes)))
			  (AVs-from-ftypes (cdr ftypes))))))])
      (let ([atype 
	     (mrspidey:fo-FlowType-def fo-ftype)])
	(if (mrspidey:atvalues? atype)
	    (AVs-from-ftypes 
	     (mrspidey:atvalues-values atype))
	    '()))))

  (define (ast->AVs ast)
    (let ([fo-ftype (mrspidey:parsed-ftype ast)])
      (if fo-ftype
	  (fo-ftype->AVs fo-ftype)
	  '())))

  (define (AV->AVs av)
    (let* ([av-vec (mrspidey:AV-fields+ av)])

      ; in case of multiple-values, disregard any AV's
      ; other than in 0th position

       (if (>= (vector-length av-vec) 1)

	 (list->set (mrspidey:Tvar-objs (vector-ref av-vec 0)))

	 empty-set)))
)
