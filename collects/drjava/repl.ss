(load-relative "sig.ss")

(unit/sig repl^
  (import jvm^ scanner^ queue^ gjc^ split^ mzlib:function^)
  
  ;; squiglies-match? : Queue(Scanned) -> Boolean
  (define (squiglies-match? q)
    (let* ([q (quick-dupq q)]
	   [matching `((,jLPAREN ,jRPAREN) (,jLBRACKET ,jRBRACKET) (,jLBRACE ,jRBRACE))]
	   [closed-list (map cadr matching)]
	   [open? (lambda (x) (assq x matching))]
	   [closed? (lambda (x) (memq x closed-list))]
	   [matches?
	    (lambda (o c)
	      (let ([m (assq o matching)])
		(and m (eq? (cadr m) c))))])
      (let loop ([open null])
	(cond
	  [(mtq? q) (null? open)]
	  [else
	   (let ([tok (scanned-token (deq! q))])
	     (cond
	       [(open? tok) (loop (cons tok open))]
	       [(closed? tok)
		(if (and (pair? open) (matches? (car open) tok))
		    (loop (cdr open))
		    #f)]
	       [else (loop open)]))]))))
  
  ;; tokenize : String -> Queue(Scanned)
  (define (tokenize str)
    (let ([scan (string->scanner str)]
	  [q (mtq)])
      (let loop ()
	(unless (= (enq-token! scan q) jEOF)
	  (next-token scan)
	  (loop)))
      q))
  
  ;; def? : Queue(Scanned) -> Boolean
  (define (def? q)
    (let ([next (gen-next q)])
      (and (memq (next) base-types)
	   (let loop ()
	     (let ([tok (next)])
	       (cond
		 [(= tok jLBRACKET)
		  (and (= (next) jRBRACKET) (loop))]
		 [(= tok jDOT)
		  (and (= (next) jIDENTIFIER) (loop))]
		 [(= tok jIDENTIFIER) #t]
		 [else #f]))))))
  
  ;; statement? : Queue(Scanned) -> Boolean
  (define (statement? q)
    (let ([next (gen-next q)])
      (let loop ([prev (next)])
	(cond
	  [(= prev jEOF) #f]
	  [else
	   (let ([current (next)])
	     (or (and (or (= prev jSEMI) (= prev jRBRACE))
		      (= current jEOF))
		 (loop current)))]))))
  
  ;; gen-next : Queue(Scanned) -> (-> Token)
  (define (gen-next q)
    (let ([q (quick-dupq q)])
      (lambda () (scanned-token (deq! q)))))
  
  (define prim-types (list jBOOLEAN jBYTE jCHAR jSHORT jINT jLONG jFLOAT jDOUBLE))
  (define base-types (cons jIDENTIFIER prim-types))
  
  ;;begin data definitions
  
  ;; Repl = (make-repl (listof Lhs) Env)
  (define-struct repl (defs env))
  
  ;; Lhs = (make-lhs Type Id Scanned)
  (define-struct lhs (type id scanned))
  
  ;; Type = Queue(Scanned)
  ;; Id = jobject(Name)
  ;; EnvPos = Fixnum
  
  ;; Env = jobject(Env)
  
  ;; Rawdef = (make-rawdef Queue(Scanned) Scanned Queue(Scanned))
  (define-struct rawdef (type id rhs))
  
  ;;end data definitions
  
  (define funny-chars "$$$")
  
  (define list-name (string-append "myListVariable" funny-chars))
  (define env-name (string-append "replEnvVariable" funny-chars))
  (define env-class-name (string-append "edu/rice/cs/drj/Env" funny-chars))
  (define env-dot-class-name (string-append "edu.rice.cs.drj.Env" funny-chars))
  
  (define Env-class (jfind-class env-class-name))
  (define Env-tag (jfind-field Env-class "tag" "I"))
  (define Env-boolean (jfind-field Env-class "z" "Z"))
  (define Env-byte (jfind-field Env-class "b" "B"))
  (define Env-char (jfind-field Env-class "c" "C"))
  (define Env-short (jfind-field Env-class "s" "S"))
  (define Env-int (jfind-field Env-class "i" "I"))
  (define Env-long (jfind-field Env-class "j" "J"))
  (define Env-float (jfind-field Env-class "f" "F"))
  (define Env-double (jfind-field Env-class "d" "D"))
  (define Env-object (jfind-field Env-class "a" "Ljava/lang/Object;"))
  
  (define load-class (jfind-method Env-class "loadClass" "(Ljava/lang/String;)Ljava/lang/Class;"))
  (define new-instance (jfind-method (jfind-class "java/lang/Class") "newInstance" "()Ljava/lang/Object;"))
  
  ;; selectors : Vector(jfield)
  ;; The index values must match the static final fields of EnvTags.
  (define selectors
    (vector Env-boolean Env-byte Env-char Env-short Env-int Env-long Env-float Env-double Env-object))
  
  ;; new-repl : -> Repl
  (define new-repl
    (let* ([env-init (jfind-method Env-class "<init>" "(Ljava/lang/String;)V")])
      (lambda ()
	(make-repl null (jnew Env-class env-init gjc-output-dir)))))
  
  ;; eval-str : Repl String -> (U Void jobject)
  (define (eval-str repl str)
    (let* ([q (tokenize str)])
      (if (squiglies-match? q)
	  (cond
            [(main? q) (eval-main repl q)]
	    [(def? q) (eval-def repl q)]
	    [(statement? q) (eval-statement repl q)]
	    [else (eval-expression repl q)])
	  (raise 'bad-squiglies))))
  
  ;; main? : Queue(Scanned) -> Boolean
  (define (main? q)
    (cond
      [(mtq? q) #f]
      [else
       (let ([first (first-q q)])
         (and (eq? (scanned-token first) jIDENTIFIER)
              (eq? (scanned->symbol first) 'java)
              (let ([dup (quick-dupq q)])
                (deq! dup)
                (eq? (scanned-token (deq! dup)) jIDENTIFIER))))]))
  
  ;;  eval-main: Repl Queue(Scanned) -> Void
  (define (eval-main repl q)
    (deq! q)
    (cond
      [(mtq? q) (void)]
      [else
       (eval-statement repl
                       (tokenize (format "~a.main(null);" (scanned->string  (deq! q)))))]))
  
  ;;  eval-def : Repl Queue(Scanned) -> Void
  (define (eval-def repl q)
    (let* ([rdef (rawdef<-q q)]
	   [type (rawdef-type rdef)]
	   [id (rawdef-id rdef)]
	   [rhs (rawdef-rhs rdef)]
	   [id-symbol (scanned->symbol id)]
	   [def (make-lhs type id-symbol id)]
	   [bogus-def (make-lhs 'dud 'dud-id (first-q funny-name-q))]
	   [old-defs (repl-defs repl)])
      (let-values ([(index old-def-pair) (def-index id-symbol old-defs)])
	(let* ([old-def (and index (car old-def-pair))]
	       [gen-wrap-def
		;; gen-wrap-def : (->Void) (->Void) Queue(Scanned) (listof Lhs) -> (listof Lhs) String Queue(Scanned) -> Queue(Scanned)
		(lambda (pre guts post defs)
		  (lambda (__ class-name _)
		    (begin0 (q-append (general-header class-name old-defs)
				      type funny-name-q rhs
				      guts
				      (begin
					(pre)
					(general-trailer defs)))
			    (post))))])
	  (when (eval-common repl rhs
                             (if index
                                 (gen-wrap-def (lambda () (set-car! old-def-pair bogus-def))
                                               (mtq)
                                               (lambda () (set-car! old-def-pair old-def))
                                               old-defs)
                                 (gen-wrap-def void set-funny-q void (cons bogus-def old-defs))))
            (if index
                (set-car! old-def-pair def)
                (set-repl-defs! repl (cons def old-defs))))))))
  
  ;;   rawdef<-q : Queue(Scanned) -> Rawdef
  ;;   pre: (def? q) must be true
  (define (rawdef<-q q)
    (let* ([q (quick-dupq q)]
	   [base (deq! q)]
	   [type (mtq)])
      (enq! base type)
      (collect-dots q type)
      (collect-brackets q type)
      (let* ([id (deq! q)])
	(collect-brackets q type)
	(enq! (last-q q) type) ; add eof
	(make-rawdef type id q))))
  
  ;;    collect-brackets : Queue(Scanned) Queue(Scanned) -> Void
  (define (collect-brackets from to)
    (let loop ()
      (unless (mtq? from)
	(when (eq? jLBRACKET (scanned-token (first-q from)))
	  (enq! (deq! from) to)
	  ; more-here : check for jRBRACKET
	  (enq! (deq! from) to)
	  (loop)))))
  
  ;; collect-dots : Queue(Scanned) Queue(Scanned) -> Void
  (define (collect-dots from to)
    (let loop ()
      (let ([maybe-dot (scanned-token (first-q from))])
	(when (= maybe-dot jDOT)
	  (enq! (deq! from) to)
	  (enq! (deq! from) to)
	  (loop)))))
  
  ;;  eval-statement : Repl Queue(Scanned) -> Boolean
  ;;  abstract w. eval-expression
  (define (eval-statement repl q)
    (eval-common repl q wrap-statement))
  
  ;;   wrap-statement : (listof Lhs) String Queue(Scanned) -> Queue(Scanned)
  (define (wrap-statement defs name q)
    (q-append (general-header name defs)
	      q
	      (general-trailer defs)))
  
  ;;  eval-expression : Repl Queue(Scanned) -> Value
  (define (eval-expression repl q)
    (if (eval-common repl q wrap-expression)
        (let ([env (repl-env repl)])
          (jget-field env (vector-ref selectors (jget-field env Env-tag))))
        (void)))
  
  ;;   eval-common : Repl Queue(Scanned) -> Boolean
  (define (eval-common repl q wrap)
    (let ([name (next-class-name)])
      (and (zero? (compile-repl-class name
                                      (wrap (repl-defs repl) name q)
                                      (repl-env repl)))
           (let* ([class-object (jcall (repl-env repl) load-class name)])
             (jcall class-object new-instance)
             (void)))))
  
  (define nl (list->string (list #\newline)))
  (define new-list-iterator (tokenize (string-append "    " list-name " = " env-name ".defs.listIterator(0);" nl)))
  
  ;; general-header : String -> Queue(Scanned)
  (define (general-header name defs)
    (set-scan-q-pos!
     (apply q-append 
	    (tokenize
	     (string-append
              "public class " name " {" nl
              "  public " name "() throws java.lang.Throwable {" nl
              "    " env-dot-class-name " " env-name " = ("env-dot-class-name")getClass().getClassLoader();"
              "    java.util.ListIterator "))
	    new-list-iterator
	    (map lhs->code defs))
     start-pos))
  
  ;; general-trailer : (listof Lhs) -> Queue(Scanned)
  (define general-trailer
    (let ([trailer (tokenize (string-append "  }" nl
					    "}"))]
	  [def-header (tokenize (string-append "    " list-name ".next();" nl
					       "    " list-name ".set(" env-name ".wrap(" ))]
	  [def-trailer (tokenize (string-append "));" nl))])
      (lambda (defs)
	(apply q-append
	       (cons new-list-iterator
		     (foldr (lambda (lhs acc)
			      (let ([new-q (mtq)])
				(enq! (lhs-scanned lhs) new-q)
				(cons (q-append def-header new-q def-trailer)
				      acc)))
			    (list trailer)
			    defs))))))
  
  ;;   wrap-expression : (listof Lhs) String Queue(Scanned) -> Queue(Scanned)
  ;;   This needs a lot more here.
  (define (wrap-expression defs name q)
    (q-append (general-header name defs)
	      (tokenize (string-append
			 env-name ".setVal("))
	      q
	      (tokenize ");")
	      (general-trailer defs)))
  
  ;;   next-class-name : -> String
  (define next-class-name
    (let ([class-number 0])
      (lambda ()
	(set! class-number (add1 class-number))
	(format "Repl_cla~a_~a" funny-chars class-number))))
  
  ;;   compile-repl-class : String Queue(Scanned) jobject(Env) -> Nat
  ;;   mutation: the tokenized class is added to the list of tokenized class queues
  (define (compile-repl-class name q env)
    (let ([tokens (make-tokens (string-append name ".java") q)])
      (compile-class tokens env)))
  
  ;; lhs->code : lhs -> Queue(Scanned)
  (define (lhs->code lhs)
    (let* ([type (lhs-type lhs)]
	   [name (mtq)]
	   [cast-dot-list (cast-dot type)]
	   [cast (car cast-dot-list)]
	   [dot (cadr cast-dot-list)])
      (enq! (lhs-scanned lhs) name)
      (q-append type
		name
		(tokenize "= ((")
		cast
		(tokenize
		 (string-append ")" list-name ".next())"))
		dot
		(tokenize ";"))))
  
  ;; cast-dot : Queue(Scanned) -> (cons Queue(Scanned) Queue(Scanned))
  (define cast-dot
    (let ([assoc-list
	   (list (list jBOOLEAN (tokenize "Boolean") (tokenize ".booleanValue()"))
		 (list jBYTE (tokenize "Byte") (tokenize ".byteValue()"))
		 (list jCHAR (tokenize "Character") (tokenize ".charValue()"))
		 (list jSHORT (tokenize "Short") (tokenize ".shortValue()"))
		 (list jINT (tokenize "Integer") (tokenize ".intValue()"))
		 (list jLONG (tokenize "Long") (tokenize ".longValue()"))
		 (list jFLOAT (tokenize "Float") (tokenize ".floatValue()"))
		 (list jDOUBLE (tokenize "Double") (tokenize ".doubleValue()")))])
      (lambda (q)
	(let ([default (list q (mtq))])
	  (if (= 2 (q-length q))
	      (let ([type (assq (scanned-token (first-q q)) assoc-list)])
		(if type
		    (cdr type)
		    default))
	      default)))))
  
  ;; def-index : Symbol (listof lhs) -> (U (#f x #f) (Nat x (cons lhs (listof lhs))))
  (define (def-index id defs)
    (let loop ([acc 0] [defs defs])
      (cond
	[(null? defs) (values #f #f)]
	[(eq? id (lhs-id (car defs)))
	 (values acc defs)]
	[else (loop (add1 acc) (cdr defs))])))
  
  ;; scanned->string : Scanned -> String
  (define (scanned->string id)
    (jstring->string (name->string (scanned-name id))))
  
  ;; scanned->symbol : Scanned -> Symbol
  (define (scanned->symbol id)
    (string->symbol (scanned->string id)))
  
  ;; q-append : Queue(Scanned)* -> Queue(Scanned)
  ;; Note: jEOF's and anything after that in a queue are skipped
  ;; This is not in the q unit since it's specific to Scanned tokens.
  (define q-append
    (lambda queues
      (let ([prefix (mtq)])
	(let loop ([queues queues])
	  (cond
	    [(null? queues) prefix]
	    [else
	     (let ([q1 (quick-dupq (car queues))])
	       (let loop ()
		 (unless (mtq? q1)
		   (let ([el (deq! q1)])
		     (unless (= jEOF (scanned-token el))
		       (enq! el prefix)
		       (loop))))))
	     (loop (cdr queues))])))))
  
  ;; set-scan-q-pos! : Queue(Scanned) Num -> Queue(Scanned)
  (define (set-scan-q-pos! q pos)
    (unless (mtq? q)
      (for-each (lambda (x)
		  (set-scanned-pos! x pos)
		  (set-scanned-lastpos! x pos))
		(car q)))
    q)
  
  (define funny-name-q (tokenize (string-append "new_def_var" funny-chars)))
  (define set-funny-q
    (q-append (tokenize (string-append
			 ;env-name ".defs.addFirst("env-name ".wrap("))
			 env-name ".defs.add(0, "env-name ".wrap("))
	      funny-name-q
	      (tokenize "));")))
  
  ;; line 1 col 1 = 1<<10 | 1 = 1025 from GJ
  (define start-pos 1025)
  
  
  ;; debugging:
  ;; println : jobject -> Void
  (define (println str)
    (let* ([sys-class (jfind-class "java/lang/System")]
	   [out-field (jfind-static-field sys-class "out" "Ljava/io/PrintStream;")]
	   [out (jget-field sys-class out-field)]
	   [pclass (jfind-class "java/io/PrintStream")]
	   [ps (jfind-method pclass "println" "(Ljava/lang/Object;)V")])
      (jcall out ps str)
      str))
  
  ;; print-q : Queue(Scanned) -> Void
  (define (print-q q)
    (let ([q (quick-dupq q)])
      (let loop ()
	(unless (mtq? q)
	  (let ([x (deq! q)])
	    (printf "<~a ~a ~a ~n"
		    (scanned-token x)
		    (scanned-pos x)
		    (scanned-lastpos x))
	    (if (= jIDENTIFIER (scanned-token x))
		(println (scanned-name x))
		(printf "#f~n"))
	    (printf "  ~a ~a>~n"
		    (scanned-radix x)
		    (if (scanned-str x)
			(jstring->string (scanned-str x))
			#f))
	    (loop))))))
  
  ;; tee : (a -> b) a -> a
  (define (tee f  x)
    (f x)
    x))
