(load-relative "sig.ss")
(unit/sig split^
  (import jvm^ queue^ gjc^ scanner^ mzlib:function^ mzlib:file^)
  
  (define fin-class (jfind-class "java/io/FileInputStream"))
  (define glist-class (gjc-class "util/List"))
  (define hscan-class (jfind-class "gjc/vhack/parser/Scanner"))
  
  (define glist-1 (jfind-method glist-class "<init>" "(Ljava/lang/Object;)V"))
  (define glist-0 (jfind-method glist-class "<init>" "()V"))
  (define glist-cons (jfind-method glist-class "<init>"
				   (string-append "(Ljava/lang/Object;L"
						  gjc-version
						  "util/List;)V")))
  
  (define fin-init (jfind-method fin-class "<init>" "(Ljava/lang/String;)V"))
  
  ;; Tokens = (make-tokens jobject(String) Queue)
  (define-struct tokens (name q))
  
  ;; compile : String -> Nat
  (define (compile . in-files)
    (let ([tokens-list (map split-java in-files)])
      (compile-classes (apply append tokens-list))))
  
  ;; split-java : String -> (listof Tokens)
  (define (split-java in-file)
    (let* ([fin (jnew fin-class fin-init in-file)]
	   [scan (new-scanner fin)])
      (scan-classes scan)))
  
  ;; scan-classes : jobject(Scanner) -> (listof Tokens)
  (define (scan-classes scan)
    (let* ([q (process-package scan)]
	   [package (names->path q)]
	   [imports (process-imports scan q)])
      (unless (directory-exists? package)
	(make-directory* package))
      (let loop ([acc null])
	(if (token= scan jEOF)
	    acc
	    (loop (cons (split-class scan imports package) acc))))))
  
  ;; process-package : jobject(Scanner) -> Queue
  (define (process-package scan)
    (let ([q (mtq)])
      (when (token= scan jPACKAGE)
	(enq-token! scan q)
	(skip-semi scan q))
      q))
  
  ;; names->path : Queue(Scanned) -> String
  (define (names->path q)
    (let ([q (quick-dupq q)])
      (apply build-path
	     gjc-output-dir
	     (let loop ()
	       (cond
		 [(mtq? q) null]
		 [else
		  (let ([first (deq! q)])
		    (if (= (scanned-token first) jIDENTIFIER)
			(list* (scanned->string first) (loop))
			(loop)))])))))
  
  ;; process-imports : jobject(Scanner) -> Queue
  (define (process-imports scan q)
    (let loop ()
      (cond
	[(token= scan jIMPORT)
	 (enq-token! scan q)
	 (skip-semi scan q)
	 (loop)]
	[else q])))
  
  ;; split-class : jobject(Scanner) Queue String -> Tokens
  (define (split-class scan imports package-name)
    (let ([q (dupq imports)])
      (let loop ()
	(let ([tok (enq-token! scan q)])
	  (next-token scan)
	  (if (= tok jIDENTIFIER)
	      (make-tokens (build-path package-name (string-append (scanned->string (last-q q)) ".java"))
			   (match-brackets scan
					   (lambda (x) (= x jLBRACE))
					   (lambda (x) (= x jRBRACE))
					   void
					   q))
	      (loop))))))
  
  ;; match-brackets : jobject(Scanner) (Int -> Boolean) (Int->Boolean) (Int Int->Boolean) Queue -> Queue
  (define (match-brackets scan open? close? match? q)
    (let loop ()
      (let ([tok (current-token scan)])
	(when (= jEOF tok)
	  (error 'match-brackets "EOF reached before any brackets."))
	(unless (open? (enq-token! scan q))
	  (next-token scan)
	  (loop))))
    (let loop ([stack (list (current-token scan))])
      (unless (null? stack)
	(next-token scan)
	(let ([tok (enq-token! scan q)])
	  (cond
	    [(open? tok) (loop (cons tok stack))]
	    [(close? tok)
	     (if (match? tok (car stack))
		 (loop (cdr stack))
		 (error 'match-brackets "brackets don't match: ~a ~a" (car stack) tok))]
	    [(= jEOF tok) (error 'match-brakcets "EOF reached, but brackets don't match")]
	    [else (loop stack)]))))
    (next-token scan)
    q)
  
  ;; skip-semi : jobejct(Scanner) Queue -> Void
  (define (skip-semi scan q)
    (next-token scan)
    (let loop ()
      (unless (= (enq-token! scan q) jSEMI)
	(next-token scan)
	(loop)))
    (next-token scan))
  
  ;; scanned->string : Scanned -> String
  (define (scanned->string scanned)
    (jstring->string (name->string (scanned-name scanned))))
  
  ; token= : jobject(Scanner) Int
  (define (token= scan tok)
    (= (current-token scan) tok))
  
  
  ;; debugging
  ;; format-pos : jobject(Scanner) jfield(I)
  (define (format-pos scan field)
    (let ([pos (jget-field scan field)])
      (format "~a:~a" (arithmetic-shift pos -10) (bitwise-and 1023 pos))))
  
  ; compile-classes : (listof Tokens) [jobject(Env)] -> Nat
  (define (compile-classes tokens . env)
    (set-token-lst! tokens)
    (apply compile-gjlist
	   (foldr (lambda (cls acc)
		    (jnew glist-class glist-cons (tokens-name cls) acc))
		  (jnew glist-class glist-0)
		  tokens)
	   env))
  
  ;; compile-class : Tokens [jobject(Env)] -> Nat
  (define (compile-class tokens . env)
    (add-tokens! tokens)
    (apply compile-gjlist (jnew glist-class glist-1 (tokens-name tokens)) env))
  
  ;; java native methods & helpers
  
  (define-values (set-token-lst! add-tokens!)
    (let* ([token-lst null]
	   [name-equals? (jfind-method hscan-class "nameEquals" "(Ljava/lang/String;)Z")]
	   [setup (jfind-method hscan-class "setup" "(Ledu/rice/cs/drj/SchemeFunction;Ledu/rice/cs/drj/SchemeFunction;)V")]
	   [install (jfind-static-method hscan-class "install" "(Ledu/rice/cs/drj/SchemeFunction;)V")]
           ;; redundant - reunitize to eliminate - more here
           [scan-class (jfind-class "gjc/vptg/parser/Scanner")]
	   [token (jfind-field scan-class "token" "I")]
	   [pos (jfind-field scan-class "pos" "I")]
	   [lastpos (jfind-field scan-class "lastPos" "I")]
	   [name (jfind-field scan-class "name"
			      (string-append "L"
					     gjc-version
					     "util/Name;"))]
	   [radix (jfind-field scan-class "radix" "I")]
	   [path-field (jfind-field hscan-class "path" "Ljava/lang/String;")])
      (jcall hscan-class install
	     (lambda (jscan)
	       (let* ([tokens (or (ormap (lambda (tokens)
					   (if (jcall jscan name-equals? (tokens-name tokens))
					       tokens
					       #f))
					 token-lst)
				  (error 'scanner "source file ~a not found." (jstring->string (jget-field jscan path-field))))]
		      [q (quick-dupq (tokens-q tokens))]
		      [str #f])
		 (jcall jscan setup
			(lambda ()
			  (if (mtq? q)
			      (jset-field! jscan token jEOF)
			      (let ([s (deq! q)])
				(jset-field! jscan token (scanned-token s))
				(jset-field! jscan pos (scanned-pos s))
				(jset-field! jscan lastpos (scanned-lastpos s))
				(jset-field! jscan name (scanned-name s))
				(jset-field! jscan radix (scanned-radix s))
				(set! str (scanned-str s)))))
			(lambda () str)))))
      (values
       (lambda (x) (set! token-lst x))
       (lambda (x) (set! token-lst (cons x token-lst)))))))
