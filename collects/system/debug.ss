(#%define mred:debug:debug-param (current-parameterization))
(#%define mred:debug:debug-on
  (#%let ([DEFAULT-ON (list (#%quote load) 
			    (#%quote startup)
			    (#%quote html)
			    (#%quote dv)
			    (#%quote load)
			    (#%quote invoke)
			    (#%quote splash))])
    (box
     (#%let ([debug-env (#%or "(list 'console-threading)"
			      (getenv "MREDDEBUG"))
	     ])
       (#%if debug-env
	   (append
	    DEFAULT-ON
	    (#%let ([r (#%with-handlers ([void exn-message])
		         (eval (read (open-input-string debug-env))))])
	      (#%cond
	       [(symbol? r) (list r)]
	       [(list? r) r]
	       [else (printf "WARNING: environment variable MREDDEBUG expected to evaluate to a list or a symbol got: ~a~n" r)
		     null])))
	   null)))))

(#%define-values (mred:debug:printf
		  mred:debug:if
		  mred:debug:when
		  mred:debug:unless
		  mred:debug:turn-on
		  mred:debug:turn-off)
 (#%let* ([release? #f]
	  [when/unless
	   (#%lambda (when/unless)
	    (#%lambda (symbol . bodies)
	     (#%let ([g (gensym "mred:debug:when")])
	      (#%quasiquote
	       (#%let ([,g ,symbol])
		(#%cond
		 [(symbol? ,g)
		  (,when/unless (member ,symbol
					(unbox
					 (global-defined-value (#%quote mred:debug:debug-on))))
		   ,@bodies)]
		 [(list? ,g)
		  (,when/unless (ormap (lambda (x)
					 (member
					  x
					  (unbox
					   (global-defined-value (#%quote mred:debug:debug-on)))))
				       ,g)
		   ,@bodies)]
		 [else (error (#%quote mred:debug:when) "expected first arg to evalute to a symbol or a list, got: ~a~n"
			      ,g)]))))))])
     (values
      (#%lambda (symbol string . rest)
	(#%quasiquote
	 (#%when (member ,symbol (unbox (global-defined-value (#%quote mred:debug:debug-on))))
	  (with-parameterization (global-defined-value (#%quote mred:debug:debug-param))
	    (#%lambda ()
	     (printf (string-append ,string "~n") ,@rest))))))
      (#%lambda (symbol then else)
	(#%let ([g (gensym "mred:debug:if")])
	  (#%quasiquote
	   (#%if (let ([,g ,symbol])
		   (#%cond
		    [(symbol? ,g) (member ,symbol
					  (unbox (global-defined-value (#%quote mred:debug:debug-on))))]
		    [(list? ,g) (ormap (#%lambda (x) (member
						      x
						      (unbox (global-defined-value (#%quote mred:debug:debug-on)))))
				       ,g)]
		    [else (error (#%quote mred:debug:if)
				 "expected a list or symbol in the test position, got: ~a~n"
				 ,g)]))
	    ,then
	    ,else))))
      (when/unless (#%quote when))
      (when/unless (#%quote unless))
      (#%lambda (s)
	(#%let ([g (gensym)])
	  (#%quasiquote
	   (#%let ([,g ,s])
	    (#%unless (member ,g (unbox (global-defined-value (#%quote mred:debug:debug-on))))
	     (set-box! (global-defined-value (#%quote mred:debug:debug-on))
		       (cons ,g (unbox (global-defined-value (#%quote mred:debug:debug-on))))))))))
      (#%lambda (s)
       (#%letrec ([remove (#%lambda (l s)
			   (cond
			     [(null? l) null]
			     [(eq? (car l) s) (remove l (cdr s))]
			     [else (cons (car l) (remove l (cdr s)))]))])
	(#%quasiquote 
	 (set-box! (#%global-defined-value (#%quote mred:debug:debug-on))
		   (,remove (unbox (global-defined-value (#%quote mred:debug:debug-on))) ,s))))))))
	       
(#%define-macro mred:debug:printf mred:debug:printf)
(#%define-macro mred:debug:if mred:debug:if)
(#%define-macro mred:debug:when mred:debug:when)
(#%define-macro mred:debug:unless mred:debug:unless)
(#%define-macro mred:debug:turn-on mred:debug:turn-on)
(#%define-macro mred:debug:turn-off mred:debug:turn-off)
