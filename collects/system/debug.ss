(begin-elaboration-time
 (define mred:debug:param (current-parameterization))
 (define-values (mred:debug:printf
		 mred:debug:if
		 mred:debug:when
		 mred:debug:unless
		 mred:debug:turn-on
		 mred:debug:turn-off)
   (let* ([DEFAULT-ON (list 'load 'startup 'html 'dv 'load 'invoke 'splash)]
	  [when (box
		 (let ([debug-env (getenv "MREDDEBUG")])
		   (if debug-env
		       (append
			DEFAULT-ON
			(let ([r (with-handlers ([void exn-message])
						(eval (read (open-input-string debug-env))))])
			  (cond
			   [(symbol? r) (list r)]
			   [(list? r) r]
			   [else (printf "WARNING: environment variable MREDDEBUG expected to evaluate to a list or a symbol got: ~a~n" r)
				 null])))
		       null)))]
	  [release? #f]
	  [when/unless
	   (lambda (when/unless)
	     (lambda (symbol . bodies)
	       (let ([g (gensym "mred:debug:when")])
		 `(let ([,g ,symbol])
		    (cond
		     [(symbol? ,g)
		      (,when/unless (member ,symbol (unbox ,when))
				    ,@bodies)]
		     [(list? ,g)
		      (,when/unless (ormap (lambda (x) (member x (unbox ,when)))
					   ,g)
				    ,@bodies)]
		     [else (error 'mred:debug:when "expected first arg to evalute to a symbol or a list, got: ~a~n"
				  ,g)])))))])
     (values
      (lambda (symbol string . rest)
	`(when (member ,symbol (unbox ,when))
	   ;(printf "DEBUG: ~a~n" ,symbol)
	   (printf (string-append ,string "~n") ,@rest)))
      (lambda (symbol then else)
	(let ([g (gensym "mred:debug:if")])
	  `(if (let ([,g ,symbol])
		 (cond
		  [(symbol? ,g) (member ,symbol (unbox ,when))]
		  [(list? ,g) (ormap (lambda (x) (member x (unbox ,when))) ,g)]
		  [else (error 'mred:debug:if
			       "expected a list or symbol in the test position, got: ~a~n"
			       ,g)]))
	       ,then
	       ,else)))
      (when/unless 'when)
      (when/unless 'unless)
      (lambda (s)
	(let ([g (gensym)])
	  `(let ([,g ,s])
	     (unless (member ,g (unbox ,when))
	       (set-box! ,when (cons ,g (unbox ,when)))))))
      (lambda (s)
	(letrec* ([remove (lambda (l s)
			    (cond
			     [(null? l) null]
			     [(eq? (car l) s) (remove l (cdr s))]
			     [else (cons (car l) (remove l (cdr s)))]))])
	  `(set-box! ,when (,remove (unbox ,when) ,s))))))))
	       
(define-macro mred:debug:printf mred:debug:printf)
(define-macro mred:debug:if mred:debug:if)
(define-macro mred:debug:when mred:debug:when)
(define-macro mred:debug:unless mred:debug:unless)
(define-macro mred:debug:turn-on mred:debug:turn-on)
(define-macro mred:debug:turn-off mred:debug:turn-off)

(mred:debug:when (list 'load)
  (letrec* ([old-handler (current-load)]
	    [offset-string "  "]
	    [indent-string ""])
    (current-load (lambda (f)
		    (let* ([file (if (relative-path? f)
				     (build-path (current-directory) f)
				     f)])
		      (mred:debug:printf 'load "~aLoading ~a..." indent-string file)
		      (let* ([indent
			      (lambda ()
				(set! indent-string (string-append offset-string indent-string)))]
			     [outdent
			      (lambda ()
				(set! indent-string
				      (substring indent-string
						 0
						 (max (- (string-length indent-string)
							 (string-length offset-string))
						      0))))]
			     [answer
			      (dynamic-wind
			       indent
			       (lambda () (call-with-values
					   (lambda () (old-handler f))
					   list))
			       outdent)])
			(mred:debug:printf 'load "~aLoaded ~a." indent-string file)
			(apply values answer)))))))
