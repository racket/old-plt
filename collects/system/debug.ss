(#%define-values (mred:debug:printf/proc
		  mred:debug:when/proc
		  mred:debug:turn-on/proc
		  mred:debug:turn-off/proc)
 (#%let* ([mred:debug:debug-on
	   (#%let ([DEFAULT-ON (list (#%quote load) 
				     (#%quote startup)
				     (#%quote html)
				     (#%quote dv)
				     (#%quote load)
				     ;(#%quote invoke)
				     (#%quote splash))])
		  (#%let ([debug-env (#%or ;"(list 'console-threading)"
				      (getenv "MREDDEBUG"))
				     ])
			 (#%if (equal? #f debug-env)
			       #f
			       (box
				(append
				 DEFAULT-ON
				 (#%let ([r (#%with-handlers ([void exn-message])
							     (eval (read (open-input-string debug-env))))])
					(#%cond
					 [(symbol? r) (list r)]
					 [(list? r) r]
					 [else (printf "WARNING: environment variable MREDDEBUG expected to evaluate to a list or a symbol got: ~a~n" r)
					       null])))))))]
	  [when/unless
	   (#%lambda (when/unless)
	    (#%lambda stuff
             (#%if (#%null? stuff) (#%error when/unless "expected a tag, got no arguments") (#%void))
	     (#%let ([symbol (#%car stuff)]
		     [bodies (#%cdr stuff)]
		     [g (#%gensym "mred:debug:when")])
	      (#%quasiquote
	       (#%let ([,g ,symbol])
		(#%cond
		 [(#%symbol? ,g)
		  (,when/unless (member ,symbol (unbox ,mred:debug:debug-on))
		   ,@bodies)]
		 [(#%list? ,g)
		  (,when/unless (not (equal? #f (ormap (lambda (x)
							 (member
							  x
							  (unbox ,mred:debug:debug-on)))
						       ,g)))
		   ,@bodies)]
		 [else (#%error (#%quote mred:debug:when) "expected first arg to evalute to a symbol or a list, got: ~a~n"
			      ,g)]))))))]
	  [mt (#%lambda x `(void))])
	 (#%if (equal? mred:debug:debug-on #f)
	       (values mt mt mt mt)
	       (values
		(#%lambda stuff
		  (#%let ([symbol (car stuff)]
			  [string (cadr stuff)]
			  [rest (cddr stuff)])
		    (#%quasiquote
		     (#%if (equal? #f (member ,symbol (unbox ,mred:debug:debug-on)))
			   (void)
			   (printf (string-append (symbol->string ,symbol) ">> " ,string "~n") ,@rest)))))
		(when/unless (#%quote when))
		(#%lambda (s)
			  (#%let ([g (gensym)])
				 (#%quasiquote
				  (#%let ([,g ,s])
					 (#%when (equal? #f (member ,g (unbox ,mred:debug:debug-on)))
						 (set-box! ,mred:debug:debug-on
							   (cons ,g (unbox ,mred:debug:debug-on))))))))
		(#%lambda (s)
			  (#%letrec ([remove (#%lambda (l s)
						       (cond
							[(null? l) null]
							[(eq? (car l) s) (remove l (cdr s))]
							[else (cons (car l) (remove l (cdr s)))]))])
				    (#%quasiquote 
				     (set-box! ,mred:debug:debug-on
					       (,remove (unbox ,mred:debug:debug-on) ,s)))))))))

(#%define-macro mred:debug:printf mred:debug:printf/proc)
(#%define-macro mred:debug:when mred:debug:when/proc)
(#%define-macro mred:debug:turn-on mred:debug:turn-on/proc)
(#%define-macro mred:debug:turn-off mred:debug:turn-off/proc)
