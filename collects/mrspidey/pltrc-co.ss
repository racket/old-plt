;; pltrc-co.ss
;; Stuff that released code needs
;; ----------------------------------------------------------------------

(define-macro defmacro
  (lambda (name args . body)
    `(define-macro ,name (lambda ,args ,@body))))

(begin-elaboration-time
 (define (struct-expander-fn def-str struct:)
  (#%let ([make-exn make-exn:syntax]
	  [debug current-continuation-marks])
  (#%lambda body
     (#%let ([syntax-error
	      (#%lambda (s)
		(#%raise
		 (make-exn
		  (#%format "~s: ~a" (cons def-str body) s)
		  (debug)
		  (#%cons 'define-struct body))))]
	     [build-struct-names
	      (#%lambda (name fields)
		(#%let ([name (#%symbol->string name)]
			[fields (#%map #%symbol->string fields)]
			[+ #%string-append])
		  (#%map #%string->symbol
			 (#%append
			  (#%list 
			   (+ "struct:" name)
			   (+ "make-" name)
			   (+ name "?"))
			  (#%apply
			   #%append
			   (#%map
			    (#%lambda (f) 
			       (#%list 
				(+ name "-" f)
				(+ "set-" name "-" f "!")))
			   fields))))))])
	    (#%or (#%pair? body)
		  (syntax-error "empty declaration"))
	    (#%or (#%= 2 (#%length body))
		  (syntax-error "wrong number of parts"))
	    (#%or (#%symbol? (#%car body))
		  (#%and (#%pair? (#%car body))
			 (#%symbol? (#%caar body))
			 (#%pair? (#%cdar body))
			 (#%null? (#%cddar body)))
		  (syntax-error "first part must be an identifier or identifier-expression pair"))
	    (#%or (#%list? (#%cadr body))
		  (syntax-error "improper field list"))
	    (#%let* ([name (#%if (#%symbol? (#%car body))
			      (#%car body)
			      (#%caar body))]
		  [fields (#%cadr body)]
                  [fields
                   (map (lambda (arg)
                          (match arg
                            [((or ': '!) field type) field]
                            [(? symbol? field) field]
                            [x (syntax-error (format "field name not a identifier at ~s" x))]))
                        fields)])
	      `(#%define-values ,(build-struct-names name fields)
                                (,struct: ,(car body) ,fields))))))))

(#%define-macro define-const-typed-structure
  (struct-expander-fn ' define-const-typed-structure '#%struct))
(#%define-macro define-typed-structure
  (struct-expander-fn 'define-typed-structure  '#%struct))

;; ----------------------------------------------------------------------

(define-macro let*-vals 
  (let ([cout 0]
	[wh-cout (box '())])
   (lambda args
    (match args
      [(([varss exps] ...) . body)
       (set! cout (add1 cout))
       (printf "let*-vals ~s~n" cout)
       (let* ([varss (map (lambda (vars) 
			    (map 
                             (lambda (x) (if (eq? x '_) (gensym) x))
                             (if (symbol? vars) (list vars) vars)))
			  varss)]
              [binds (map list varss exps)])
	 `(begin 
	    (set-box! (global-defined-value 'wh-cout)
	    (cons ,cout (unbox (global-defined-value 'wh-cout))))
	    (let*-values ,binds 
             (begin            
               (set-box! (global-defined-value 'wh-cout)
                 (cdr (unbox (global-defined-value 'wh-cout))))
               . ,body))))]))))

(define-macro let*-vals 
  (lambda args
  (match args
    [(([varss exps] ...) . body)
      (let* ([varss (map (lambda (vars) 
                           (map 
                             (lambda (x) (if (eq? x '_) (gensym) x))
                             (if (symbol? vars) (list vars) vars)))
                      varss)]
              [binds (map list varss exps)])
        `(let*-values ,binds . ,body))])))

(define-macro for 
  (lambda args
  (match args
    [(var base limit . body)
     (let ([loop (gensym)][l (gensym)])
       `(let ([,l ,limit])
          (recur ,loop ([,var ,base])
                 (when (< ,var ,l)
                   ,@body
                   (,loop (add1 ,var))))))])))

(begin-elaboration-time
 (define assert-on (make-parameter #f (lambda (x) x))))

(define-macro assert 
 (lambda args
  (match args     
    [(exp . rest)
     (if (assert-on)
         `(unless ,exp 
            ,@(apply append
                     (map (lambda (r) `((display ,r) (newline))) rest))
            (error 'assert "Assertion failed: ~s" ',exp))
         `(void))])))

;; ----------------------------------------------------------------------

'(unless (defined? '__keep-mrspidey-annotations)

   (defmacro begin-test-case exps '(void))
   ;;(defmacro define-type exps '(void))

   (defmacro define-typed-structure args
     (match args
       [(name.parent fields)
         `(define-struct 
            ,name.parent
            ,(map (match-lambda
                    [((or ': '!) (? symbol? s) type) s]
                    [(? symbol? s) s]
                    [field
                      (error 'define-typed-structure "Bad field ~s" field)])
               fields))]
       [_ (error 'define-typed-structure 
            "Bad syntax ~s" `(define-typed-structure ,@args))]))

   (defmacro define-const-typed-structure args
     `(define-typed-structure ,@args))

   (defmacro : args
     (match args     
       [(exp type) exp]))

   (defmacro cache-exp args
     (match args
       [(exp zafile) exp]))

   (defmacro cache-inv args
     (match args
       [(exp zafile) exp]))

   ;; (load "~cormac/scheme/remove-mrspidey-annotations.ss"))

   )


; Like parameterize, but works on non-parameter procedures
; that act like parameters
(#%define-macro parameterize*
  (#%let ([make-exn make-exn:syntax]
	  [debug current-continuation-marks])
    (#%lambda (params . body)
     (#%let ([fail
	      (#%lambda (msg)
	       (#%raise (make-exn msg (debug)
				  (#%list* 'parameterize* params body))))])
      (#%if (#%null? body) (fail "parameterize*: bad syntax (empty body)"))
      (#%if (#%null? params)
        `(#%begin ,@body)
	(#%if (#%or (#%not (#%pair? params))
		    (#%not (#%pair? (#%car params)))
		    (#%not (#%pair? (#%cdar params)))
		    (#%not (#%null? (#%cddar params))))
	      (fail "parameterize*: bad syntax")
	      (#%let ([param (#%caar params)]
		      [orig (#%gensym)]
		      [pz (#%gensym)])
		 `(#%let* ([param ,param]
                           [,pz ,param]
			   [,orig (,pz)])
			  (#%dynamic-wind
			   (#%lambda () (,pz ,(#%cadar params)))
			   (#%lambda () (parameterize* ,(cdr params) ,@body))
			   (#%lambda () (,pz ,orig)))))))))))


;;----------------------------------------------------------------------
