;; Moy
;; reworked by Robby

;; aries adds only begin and let to the transformed source

(plt:require-library "ariess.ss")

(define plt:aries@
  (unit/sig plt:aries^
    (import [zodiac : zodiac:system^]
	    [zodiac:interface : zodiac:interface^])

    (define error-box
      (box #f))

    (define improper-map
      (lambda (f list)
	(cond
	 ((null? list) list)
	 ((pair? list) (cons (f (car list)) (improper-map f (cdr list))))
	 (else (f list)))))


    (define unparse-read
      (lambda (read)
	(cond
	  [(zodiac:improper-list? read)
	    (let loop ([l (zodiac:read-object read)])
	      (cond
		[(null? (cdr l)) (unparse-read (car l))]
		[else (cons (unparse-read (car l)) (loop (cdr l)))]))]
	  [(zodiac:vector? read)
	    (apply vector (map unparse-read (zodiac:read-object read)))]
	  [(zodiac:list? read) (map unparse-read (zodiac:read-object read))]
	  [else (zodiac:read-object read)])))


    (define wrap
      (lambda (zodiac x)
	(let ([start (zodiac:zodiac-start zodiac)]
	       [finish (zodiac:zodiac-finish zodiac)])
	  `(#%begin (,set-box! ,error-box
		      ,(zodiac:make-zodiac #f start finish))
	     ,x))))

    (define check-for-keyword
      (lambda (id)
	(when (keyword-name? (zodiac:read-object id))
	  (zodiac:static-error id "Invalid use of keyword"))))

    (define annotate
      (lambda (expr)
	(cond
	  [(zodiac:lexical-varref? expr)
	    (zodiac:id-var expr)]

	  [(zodiac:top-level-varref? expr)
	    (wrap expr (zodiac:id-var expr))]
	 
	  [(zodiac:app? expr)
	    (let* ([aries:app-arg (gensym 'aries:app-arg)]
		    [aries:app-break (gensym 'aries:app-break)]
		    [last-arg (gensym 'last-arg)]
		    [fun-sym (gensym "fun")]
		    [args (map (lambda (x) `(,(gensym "arg")
					      ,(annotate x)))
			    (zodiac:app-args expr))])
	      `(#%let ([,fun-sym ,(annotate (zodiac:app-fun expr))]
			,@args)
		 ,(wrap expr `(,fun-sym ,@(map car args)))))]

	  [(zodiac:delay-form? expr)
	    `(#%delay ,(annotate (zodiac:delay-form-expr expr)))]
	 
	  [(zodiac:if-form? expr)
	    `(#%if ,(annotate (zodiac:if-form-test expr))
	       ,(annotate (zodiac:if-form-then expr))
	       ,(annotate (zodiac:if-form-else expr)))]
	 
	  [(zodiac:lambda-form? expr)
	    (improper-map check-for-keyword
	      (zodiac:lambda-form-args expr))
	    (let ([args (improper-map zodiac:bound-var
			  (zodiac:lambda-form-args expr))])
	      `(#%lambda ,args
		 ,(annotate (zodiac:lambda-form-body expr))))]
	 
	  [(zodiac:set!-form? expr)
	    (let ([g (gensym "set!")])
	      `(#%let ([,g ,(annotate (zodiac:set!-form-val expr))])
		 ,(wrap expr 
		    `(#%set! ,(zodiac:id-var (zodiac:set!-form-var expr))
		       ,g))))]
	 
	  ; [(zodiac:time-form? expr) 
	  ; `(time ,(annotate (zodiac:time-form-expr expr)))]

	  [(zodiac:quote-form? expr)
	    `(#%quote ,(unparse-read (zodiac:quote-form-expr expr)))]

	  [(zodiac:begin-form? expr)
	    `(#%begin ,(annotate (zodiac:begin-form-first expr))
	       ,(annotate (zodiac:begin-form-rest expr)))]
	 
	  [(zodiac:letrec-form? expr)
	    (let ([bindings
		    (map (lambda (var val)
			   `(,(zodiac:bound-var var) ,(annotate val)))
		      (zodiac:letrec-form-vars expr)
		      (zodiac:letrec-form-vals expr))])
	      `(#%letrec ,bindings
		 ,(annotate (zodiac:letrec-form-body expr))))]
	 
	  [(zodiac:define-form? expr)
	    `(#%define ,(zodiac:id-var (zodiac:define-form-var expr))
	       ,(annotate (zodiac:define-form-val expr)))]

	  [(zodiac:define-struct-form? expr)
	    `(#%define-struct
	       ,(if (zodiac:define-struct-form-super expr)
		  `(,(zodiac:symbol-orig-name
		       (zodiac:define-struct-form-type expr))
		     ,(annotate (zodiac:define-struct-form-super expr)))
		  (zodiac:symbol-orig-name
		    (zodiac:define-struct-form-type expr)))
	       ,(map zodiac:symbol-orig-name
		  (zodiac:define-struct-form-fields expr)))]

	  [else
	    (print-struct #t)
	    (zodiac:interface:internal-error
	      expr
	      (format "unknown object to annotate, ~a~n" expr))])))

    (define transform
      (lambda (port offset file)
	(let ([reader (zodiac:read port (zodiac:make-location 1 1 offset file))])
	  (let read-loop ([exprs null])
	    (let ([expr (reader)])
	      '(printf "expr: ~s~n" expr)
	      (if (zodiac:eof? expr)
		  (apply values (reverse exprs))
		  (let* ([expanded (zodiac:expand expr)]
			 [_ '(printf "expanded: ~s~n" expanded)]
			 [annotated (annotate expanded)])
		    '(begin ((global-defined-value 'pretty-print) annotated)
			   (newline))
		    (read-loop (cons annotated exprs)))))))))))
