
;; Glue mzc to the real MrSpidey. Not yet ported to v200.

;; spnoop.ss provides dummy version of these functions that don't use
;; the real MrSpidey.

#|

(unit/sig mrspidey:file-read^
  (import) 
  (define (make-file-thunk-thunk filename)   	
    (lambda () 
      (let ([file-port (open-input-file filename)])
	(lambda ()
	  (let ([c (read-char file-port)])
	    (when (eof-object? c)
	      (close-input-port file-port))
	    c))))))

(unit/sig mrspidey:interaction^
  (import (compiler:option : compiler:option^)
	  (zodiac : zodiac:system^)
	  compiler:zlayer^
	  compiler:driver^)

  (define (xloc o)
    (if (or (not o)
	    (zodiac:zodiac? o)
	    (zodiac:location? o))
	o
	(begin
	  (compiler:warning 
	   #f 
	   (format "internal error: not a location: ~a" o))
	  #f)))
  
  (define mrspidey:error
    (case-lambda
     [(message object)
      ; (printf "Error~n")
      (analysis-error (xloc object) message)]
     [(message)
      (mrspidey:error message #f)]))

  (define mrspidey:internal-error
    (case-lambda
     [(message object)
      (apply analysis-internal-error 
	     `(,(xloc object) ,object))]
     [(message)
      (mrspidey:internal-error message #f)]
     [message
      (mrspidey:internal-error message #f)]))

  (define mrspidey:warning
    (case-lambda
     [(str loc word-no)
      ; (printf "Warning~n")
      (compiler:warning (xloc loc) str)]
     [(str) 
      (mrspidey:warning str #f #f)]))

  (define mrspidey:add-summary
    (case-lambda
     [(str loc word-no) 
      ; (printf "Summary~n")
      (when (compiler:option:verbose)
	(when (xloc loc)
	  (zodiac:print-start! loc))
	(printf "~a~n" str))]
     [(str) 
      (mrspidey:add-summary str #f #f)]))
  
  (define mrspidey:add-summary-handler
    (make-parameter
     (lambda args
       (apply mrspidey:add-summary args))))

  (define mrspidey:progress
    (lambda (str . rest)
      (when (and #f (compiler:option:verbose))
	(printf "   ~a~n" str))))

  (define mrspidey:progress-handler
    (make-parameter
     (lambda args
       (apply mrspidey:progress args))))

  (define record-analyzed-file
    (lambda args
      (apply (record-analyzed-file-hook) args)))

  (define record-analyzed-file-hook
    (make-parameter void)))

(unit/sig compiler:mrspidey^
  (import (mrspidey : mrspidey:sba^)
	  compiler:library^)
  
  (define get-annotations
    (lambda (old)
      (list
       (mrspidey:parsed-ftype old)
       (mrspidey:parsed-check old)
       (mrspidey:parsed-atprim old)
       (mrspidey:app-tvar-args old)
       (mrspidey:binding-refs old)
       (mrspidey:binding-mutated old))))

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

  (mrspidey:st:constants #f)
  (mrspidey:st:unit-read-za #f)
  (mrspidey:st:unit-write-za #f)
  (mrspidey:st:if-split #f)
  (mrspidey:st:flow-sensitive #f)

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
      (if (and fo-ftype (not (void? fo-ftype)))
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

|#
