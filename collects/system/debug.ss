(begin-elaboration-time
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
	  [param (current-parameterization)]
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
	  (with-parameterization ,param
				 (lambda ()
				   (printf
				    (string-append ,string "~n")
				    ,@rest)))))
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

(mred:debug:when (list 'compile 'compile-and-exit 'load)
  (letrec* ([old-handler (current-load)]
	    [offset-string "  "]
	    [indent-string ""])
    (current-load (lambda (f)
		    (let* ([file (if (relative-path? f)
				     (build-path (current-directory) f)
				     f)]
			   [link?
			    (lambda (x)
			      (let* ([len (string-length x)]
				     [ans (or (and (> len 4)
						   (string=? (substring x (- len 4) len) "link"))
					      (and (> len 6)
						   (string=? (substring x (- len 6) len) "macros")))])
				(when ans
				  (mred:debug:printf 'load "~afilname; skip: ~a" indent-string x))
				ans))]
			   [mzlib?
			    (lambda (x)
			      (let*-values ([(base _1 _2) (split-path x)]
					    [(ans) (and base
							(let-values ([(_1 name _2) (split-path base)])
							  (equal? name "mzlib")))])
					   '(printf "name: ~a~n" name)
				(when ans
				  (mred:debug:printf 'load "~amzlib; skip: ~a" indent-string x))
				ans))])
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
			       outdent)]
			     [len (string-length file)]
			     [basename (substring file 0 (- len 3))]
			     [suffix (substring file (- len 3) len)]
			     [zo (string-append basename ".zo")]
			     [error-handler
			      (lambda (e) 
				(delete-file zo)
				((error-display-handler)
				 (string-append indent-string
						(exn-message e)))
				#f)])
			(mred:debug:when
			 (list 'compile 'compile-and-exit)
			 (when (and (not (link? basename))
				    (string=? ".ss" suffix)
				    (or (not (file-exists? zo))
					(<= (file-modify-seconds zo)
					    (file-modify-seconds file)))
				    (not (mzlib? file)))
			   (mred:debug:printf 'load "~aCompiling ~a..." indent-string file)
			   (indent)
			   (with-handlers ((void error-handler))
					  (compile-file file zo '(preserve-elaborations))
					  #t)
			   (outdent)
			   (mred:debug:printf 'load "~aCompiled ~a." indent-string file)))
			(mred:debug:printf 'load "~aLoaded ~a." indent-string file)
			(apply values answer)))))))
