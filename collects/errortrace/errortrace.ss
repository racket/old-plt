
;; Poor man's stack-trace-on-exceptions/profiler

;; see doc.txt for information

(module errortrace mzscheme

  (define key (gensym 'key))
  
  (define instrumenting-enabled (make-parameter #t))
  (define error-context-display-depth (make-parameter 10000 (lambda (x) (and (integer? x) x))))
  
  (define profile-thread #f)
  (define profile-key (gensym))
  
  (define profiling-enabled (make-parameter #f))
  (define profile-paths-enabled (make-parameter #f))
  
  (define profile-info (make-hash-table))
  
  (define (register-profile-start key)
    (let ([v (hash-table-get profile-info key)])
      (let ([b (car v)]
	    [v (cdr v)])
	(set-car! v (add1 (car v)))
	(when (profile-paths-enabled)
	  (let ([v (cdddr v)])
	    (set-car! v (cons (current-continuation-marks profile-key) (car v)))))
	(if (unbox b)
	    #f
	    (begin
	      (set-box! b #t)
	      (current-process-milliseconds))))))
  
  (define (register-profile-done key start)
    (when start
      (let ([v (hash-table-get profile-info key)])
	(let ([b (car v)]
	      [v (cddr v)])
	  (set-box! b #f)
	  (let ([v (cdr (hash-table-get profile-info key))])
	    (set-car! v (+ (- (current-process-milliseconds) start) (car v))))))))
  
  (define (get-profile-results)
    (hash-table-map profile-info (lambda (key val)
                                   (let ([count (cadr val)]
                                         [time (caddr val)]
                                         [name (cadddr val)]
                                         [file (cadddr (cdr val))]
                                         [cmss (cadddr (cddr val))])
                                     (list count time name file
                                           (map
                                            (lambda (cms)
                                              (map (lambda (k)
                                                     (let ([v (cdr (hash-table-get profile-info k))])
                                                       (list (caddr v) (cadddr v))))
                                                   cms))
                                            cmss))))))
  
  ;; with-mark : stx stx -> stx
  (define (with-mark mark expr)
    (with-syntax ([expr expr]
		  [mark mark]
		  [current-file (datum->syntax current-file #f #f)])
      (syntax
       `(with-continuation-mark
	 key
	 (quote-syntax (current-file . mark))
	 expr))))
  
  
  (define (profile-point body name expr env)
    (let ([body (map (lambda (e) (annotate e env)) (stx->list body))])
      (if (profiling-enabled)
          (let ([key (gensym)])
            (hash-table-put! profile-info key (list (box #f) 0 0 (or name expr) current-file null))
	    (with-syntax ([key (datum->syntax key #f #f)]
			  [start (datum->syntax (gensym) #f #f)])
	      (with-syntax ([rest 
			     (insert-at-tail*
			      (syntax (register-profile-done 'key start))
			      body)])
		(syntax
		 ((let ([start (register-profile-start 'key)])
		    (with-continuation-mark
		     profile-key
		     'key
		     (begin . rest))))))))
          body)))
  
  (define (insert-at-tail* e exprs)
    (if (stx-null? (cdr exprs))
        (list (insert-at-tail e (stx-car exprs)))
        (cons (stx-car exprs) (insert-at-tail* e (stx-cdr exprs)))))
  
  (define (insert-at-tail se sexpr)
    (with-syntax ([expr sexpr]
		  [e se])
      (syntax-case sexpr (quote #%datum #%unbound
				lambda case-lambda
				let-values letrec-values
				begin begin0
				with-continuation-mark
				if #%app)
	(cond
	 ;; negligible time to eval
	 [id
	  (identifer? sexpr)
	  (syntax (begin e sexpr))]
	 [(quote _) (syntax (begin e expr))]
	 [(#%datum . d) (syntax (begin e expr))]
	 [(#%unbound . d) (syntax (begin e expr))]

	 ;; No tail effect, and we want to account for the time
	 [(lambda . _) (syntax (begin0 expr e))]
	 [(case-lambda . _) (syntax (begin0 expr e))]
	 [(set! . _) (syntax (begin0 expr e))]
	 [(struct . _) (syntax (begin0 expr e))]

	 [(let-values bindings . body)
	  (with-syntax ([rest (insert-at-tail* se (syntax body))])
	    (syntax (let-values bindings . rest)))]
	 [(letrec-values bindings . body)
	  (with-syntax ([rest (insert-at-tail* se (syntax body))])
	    (syntax (letrec-values bindings . rest)))]

	 [(begin . _)
	  (insert-at-tail* se sexpr)]
	 [(with-continuation-mark . _)
	  (insert-at-tail* se sexpr)]

	 [(begin0 body ...)
	  (syntax (begin0 body ... e))]

	 [(if test then)
	  (with-syntax ([then2 (insert-at-tail se (syntax then))])
	    (if test then2))]
	 [(if test then else)
	  ;; WARNING: e inserted twice!
	  (with-syntax ([then2 (insert-at-tail se (syntax then))]
			[else2 (insert-at-tail se (syntax else))])
	    (if test then2 else2))]

	 [(#%app . _)
	  ;; application; exploit guaranteed left-to-right evaluation
	  (insert-at-tail* se sexpr)]))))
  
  (define (make-annotate top? name)
    (lambda (expr env)
      (syntax-case expr (quote #%datum #%unbound
			       lambda case-lambda
			       let-values letrec-values
			       begin begin0
			       with-continuation-mark
			       if #%app)
	[_
	 (identifier? expr)
	 (if (stx-memq expr env)
	     ;; lexical variable - no error possile
	     expr
	     ;; might be undefined/uninitialized
	     (with-mark expr expr))]

	;; Can't put annotation on the outside
	[(define-values names rhs)
	 top?
	 (with-syntax ([marked (with-mark expr
					  (annotate-named
					   (syntax-case names ()
					     [(id)
					      (syntax id)]
					     [_else #f])
					   (syntax rhs)
					   env))])
	   (syntax (define-values names marked)))]
	[(begin . exprs)
	 top?
	 (with-syntax ([marked (with-mark expr
					  (annotate
					   (syntax exprs)
					   env))])
	   (syntax (begin . marked)))]
	
	[

	[(and top?
	     (stx-pair? expr)
	     (memq (syntax-e (stx-car expr))
		   '(#%define-values #%define-macro
                      #%define-id-macro
                      #%define-expansion-time
                      #%begin)))
         
         (if (eq? (car expr) '#%begin)
             `(#%begin ,@(map (lambda (e) (annotate-top e env)) (cdr expr)))
             `(,(car expr) ,(cadr expr) 
               ,(with-mark expr (annotate-named
                                 (and (eq? (car expr) '#%define-values)
                                      (= 1 (length (cadr expr)))
                                      (caadr expr))
                                 (caddr expr)
                                 env))))]
        [(and (pair? expr) (eq? (car expr) '#%quote))
         expr]
        [(pair? expr)
         (with-mark
          expr
          (case (car expr)
            [(#%quote) expr]
            [(#%lambda)
	     (let ([env (let loop ([v (cadr expr)])
			  (cond
			   [(null? v) env]
			   [(symbol? v) (cons v env)]
			   [else (cons (car v) (loop (cdr v)))]))])
	       `(#%lambda ,(cadr expr)
	            ,@(profile-point 
		       (cddr expr)
		       name expr env)))]
            [(#%case-lambda)
             `(#%case-lambda 
               ,@(map (lambda (clause)
			(let ([env (let loop ([v (car clause)])
				     (cond
				      [(null? v) env]
				      [(symbol? v) (cons v env)]
				      [else (cons (car v) (loop (cdr v)))]))])
			  `(,(car clause) 
			    ,@(profile-point
			       (cdr clause)
			       name expr env))))
			(cdr expr)))]
            [(#%let-values #%letrec-values)
             (let* ([vars (apply append (map car (cadr expr)))]
                    [body-env (append vars env)]
                    [rhs-env (if (eq? (car expr) '#%letrec-values)
                                 body-env
                                 env)])
               `(,(car expr)
                 ,(map (lambda (clause)
                         `(,(car clause) ,(annotate-named
                                           (and (= (length (car clause)) 1)
                                                (caar clause))
                                           (cadr clause)
                                           rhs-env)))
                       (cadr expr))
                 ,@(map (lambda (e) (annotate e body-env)) (cddr expr))))]
            [(#%set!)
             `(#%set! ,(cadr expr) ,(annotate-named (cadr expr) (caddr expr) env))]
            [(#%begin #%begin0 #%if #%with-continuation-mark)
             `(,(car expr) ,@(map (lambda (e) (annotate e env)) (cdr expr)))]
            [(#%cond) expr]
            [(#%struct)
             (if (pair? (cadr expr))
                 `(#%struct (,(caadr expr) ,(annotate (cadadr expr) env)) ,@(cddr expr))
                 expr)]
            [(#%unit)
             (let* ([vars (let loop ([body (cdddr expr)])
                            (cond
                              [(null? body) null]
                              [(and (pair? (car body))
                                    (eq? '#%define-values (caar body)))
                               (append (cadar body) (loop (cdr body)))]
                              [else (loop (cdr body))]))]
                    [body-env (append vars env)])
               `(#%unit ,(cadr expr) ,(caddr expr)
                 ,@(map (lambda (e) (annotate-top e body-env)) (cdddr expr))))]
            [(#%compound-unit)
             `(#%compound-unit
               ,(cadr expr)
               (link ,@(map (lambda (clause)
                              `(,(car clause) (,(annotate (caadr clause) env) ,@(cdadr clause))))
                            (cdaddr expr)))
               ,(cadddr expr))]
            [(#%invoke-unit)
             `(,(car expr) ,(annotate (cadr expr) env) ,@(cddr expr))]
            [(#%class*/names)
             (let* ([car* (lambda (v)
                            (if (pair? v)
                                (car v)
                                v))]
                    [init-vars (let loop ([l (list-ref expr 4)])
                                 (cond
                                   [(pair? l) (cons (car* (car l))
                                                    (loop (cdr l)))]
                                   [(symbol? l) (list l)]
                                   [else null]))]
                    [ivars (apply
                            append
                            (map (lambda (clause)
                                   (case (car clause)
                                     [(sequence) null]
                                     [(inherit rename) (map car* (cdr clause))]
                                     [(public override private)
                                      (map car* (map car* (cdr clause)))]))
                                 (list-tail expr 5)))]
                    [body-env (append init-vars ivars)])
               `(#%class*/names ,(cadr expr) 
                 ,(annotate (caddr expr) env) ,(map (lambda (e) (annotate e env)) (cadddr expr))
                 ,(let loop ([l (list-ref expr 4)])
                    (if (pair? l)
                        (cons (let ([v (car l)])
                                (if (pair? v)
                                    `(,(car v) ,(annotate (cadr v) body-env))
                                    v))
                              (loop (cdr l)))
                        l))
                 ,@(map (lambda (clause)
                          (case (car clause)
                            [(sequence) `(sequence ,@(map (lambda (e) (annotate e body-env)) (cdr clause)))]
                            [(inherit rename) clause]
                            [(public override private)
                             `(,(car clause) ,@(map (lambda (binding)
                                                      (if (or (symbol? binding)
                                                              (null? (cdr binding)))
                                                          binding
                                                          `(,(car binding) 
                                                            ,(annotate-named 
                                                              (let ([name (car binding)])
                                                                (if (symbol? name)
                                                                    name
                                                                    (car name)))
                                                              (cadr binding)
                                                              body-env))))
                                                    (cdr clause)))]))
                        (list-tail expr 5))))]
            [(#%interface)
             `(#%interface ,(map (lambda (e) (annotate e env)) (cadr expr)) ,@(cddr expr))]
            [else 
             (map (lambda (e) (annotate e env)) expr)]))]
        [else expr])))
  
  (define annotate (make-annotate #f #f))
  (define annotate-top (make-annotate #t #f))
  (define annotate-named (lambda (name expr env) ((make-annotate #t name) expr env)))
  
  (current-eval
   (let* ([orig (current-eval)]
          [errortrace-eval-handler
           (lambda (e)
             (if (and (instrumenting-enabled)
                      (with-handlers ([void (lambda (x) #f)])
                        (global-defined-value '#%with-continuation-mark)))
                 (let ([a (annotate-top (expand-defmacro e) null)])
                   ; (printf "~s~n" a)
                   (orig a))
                 ;; The empty namespace, maybe? Don't annotate.
                 (orig e)))])
     errortrace-eval-handler))
  
  ;; port exn -> void
  ;; effect: prints out the context surrounding the exception
  (define (print-error-trace p x)
    (let loop ([n (error-context-display-depth)]
               [l (continuation-mark-set->list (exn-continuation-marks x) key)])
      (cond
        [(or (zero? n) (null? l)) (void)]
        [(pair? l)
         (let ([m (car l)])
           (fprintf p "  ~e in ~a~n" 
                    (cdr m)
                    (let ([file (car m)])
                      (if file
                          file
                          "UNKNOWN"))))
         (loop (- n 1)
               (cdr l))]
        [else (void)])))
  
  (current-exception-handler
   (let* ([orig (current-exception-handler)]
          [errortrace-exception-handler
           (lambda (x)
             (if (exn? x)
                 (let ([p (open-output-string)])
                   (display (exn-message x) p)
                   (newline p)
                   (print-error-trace p x)
		   ((error-display-handler) (get-output-string p))
		   ((error-escape-handler)))
                 (orig x)))])
     errortrace-exception-handler))
  
  (define current-file #f)
  
  (current-load
   (let* ([load (current-load)]
          [errortrace-load-handler
           (lambda (f)
             (let ([cf current-file])
               (dynamic-wind
                (lambda () (set! current-file f))
                (lambda () (load f))
                (lambda () (set! current-file cf)))))])
     errortrace-load-handler))
  
  (export print-error-trace 
		error-context-display-depth 
		instrumenting-enabled 
		profiling-enabled
		profile-paths-enabled 
		get-profile-results)
 
