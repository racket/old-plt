(unit/sig gjc^
  (import jvm^ error^)
  
  ;(define gjc-version "gjc/v6/")
  (define gjc-version "gjc/vptg/")
  
  ;; gjclass : String -> jclass
  (define (gjc-class x) (jfind-class (string-append gjc-version x)))
  
  (define log-class (jfind-class "gjc/vhack/util/Log"))
  (define log-init (jfind-method log-class "<init>" "(Ledu/rice/cs/drj/SchemeFunction;Ledu/rice/cs/drj/SchemeFunction;)V"))
  (define log-nerrors (jfind-field (jfind-class "gjc/vptg/util/Log") "nerrors" "I"))
  
  (define gj-class (gjc-class "JavaCompiler"))
  (define gj-compile
    (jfind-method gj-class "compile"
		  (string-append "(L"gjc-version"util/List;)L"gjc-version"util/List;")))
  
  (define (gen-make-gj arg)
    (jfind-static-method gj-class "make"
			 (string-append "(L"gjc-version"util/Log;Ljava/lang/String;"arg")L"gjc-version"JavaCompiler;")))
  
  (define make-gj (gen-make-gj ""))
  (define make-gj2 (gen-make-gj "Ledu/rice/cs/drj/Env$$$;"))
  
  ; get-token : String -> Int
  (define get-token
    (let ([token-class (gjc-class "parser/Tokens")])
      (lambda (name)
	(jget-field token-class (jfind-static-field token-class name "I")))))
  
  (define jEOF (get-token "EOF"))
  (define jIDENTIFIER (get-token "IDENTIFIER"))
  (define jIMPORT (get-token "IMPORT"))
  (define jPACKAGE (get-token "PACKAGE"))
  (define jSEMI (get-token "SEMI"))
  (define jLBRACE (get-token "LBRACE"))
  (define jRBRACE (get-token "RBRACE"))
  (define jLBRACKET (get-token "LBRACKET"))
  (define jRBRACKET (get-token "RBRACKET"))
  (define jLPAREN (get-token "LPAREN"))
  (define jRPAREN (get-token "RPAREN"))
  (define jINTLITERAL (get-token "INTLITERAL"))
  (define jLONGLITERAL (get-token "LONGLITERAL"))
  (define jFLOATLITERAL (get-token "FLOATLITERAL"))
  (define jDOUBLELITERAL (get-token "DOUBLELITERAL"))
  (define jCHARLITERAL (get-token "CHARLITERAL"))
  (define jSTRINGLITERAL (get-token "STRINGLITERAL"))
  (define jBOOLEAN (get-token "BOOLEAN"))
  (define jBYTE (get-token "BYTE"))
  (define jCHAR (get-token "CHAR"))
  (define jSHORT (get-token "SHORT"))
  (define jINT (get-token "INT"))
  (define jLONG (get-token "LONG"))
  (define jFLOAT (get-token "FLOAT"))
  (define jDOUBLE (get-token "DOUBLE"))
  (define jDOT (get-token "DOT"))
  
  ;; name->str : jobject(Name) -> String
  (define name->string
    (let* ([name-class (gjc-class "util/Name")]
	   [toString (jfind-method name-class "toString" "()Ljava/lang/String;")])
      (lambda (name)
	(jcall name toString))))
  
  ;; compile-gjlist : jobject [jobject(Env$$$)] -> Nat
  (define (compile-gjlist gjlist . env)
    (let ([log (jnew log-class log-init report-error report-warning)])
      (jcall (if (null? env)
		 (jcall gj-class make-gj log gjc-path)
		 (jcall gj-class make-gj2 log gjc-path (car env)))
             gj-compile gjlist)
      (jget-field log log-nerrors)))
  
  ;; build-gjc-path : String -> String
  (define (build-gjc-path dir)
    (string-append dir ":" (getenv "GJC_PATH")))
  
  (define gjc-output-dir ".")
  (define gjc-path (build-gjc-path gjc-output-dir))
  
  ;; set-gjc-output-dir! : String -> Void
  (define (set-gjc-output-dir! dir)
    (set! gjc-output-dir dir)
    (set! gjc-path (build-gjc-path dir))))
