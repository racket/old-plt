(load-relative "sig.ss")
(unit/sig scanner^
  (import jvm^ gjc^ queue^)
  
  (define scan-class (gjc-class "parser/Scanner"))
  (define log-class (gjc-class "util/Log"))
  (define scanner-init (jfind-method scan-class
				     "<init>"
				     (string-append "(Ljava/io/InputStream;L"
						    gjc-version
						    "util/Log;)V")))
  (define log-init (jfind-method log-class "<init>" "()V"))
  
  (define token (jfind-field scan-class "token" "I"))
  (define pos (jfind-field scan-class "pos" "I"))
  (define lastpos (jfind-field scan-class "lastPos" "I"))
  (define name (jfind-field scan-class "name"
			    (string-append "L"
					   gjc-version
					   "util/Name;")))
  (define radix (jfind-field scan-class "radix" "I"))
  (define next (jfind-method scan-class "nextToken" "()V"))
  (define scan-str (jfind-method scan-class "stringVal" "()Ljava/lang/String;"))
  (define bytearrayinputstream-class (jfind-class "java/io/ByteArrayInputStream"))
  (define bais-init (jfind-method bytearrayinputstream-class "<init>" "([B)V"))
  
  ;; Token = Nat
  ;; Scanned = (make-scanned (Token Nat Nat jobject(Name) Nat jobject(String)))
  (define-struct scanned (token pos lastpos name radix str))
  
  (define literals (list jINTLITERAL jLONGLITERAL jFLOATLITERAL jDOUBLELITERAL jCHARLITERAL jSTRINGLITERAL))
  
  ;; new-scanner : jobject(InputStream) -> jobject(Scanner)
  (define (new-scanner fin)
    (let ([log (jnew log-class log-init)])
      (jnew scan-class scanner-init fin log)))
  
  ;; string->scanner : String -> jobject(Scanner)
  (define (string->scanner str)
    (let* ([bytes (string->jbytes str)]
	   [in (jnew bytearrayinputstream-class bais-init bytes)])
      (new-scanner in)))
  
  ;; enq-token! : jobject Queue -> Int
  (define (enq-token! scan q)
    (let ([t (jget-field scan token)]
	  [p (jget-field scan pos)]
	  [l (jget-field scan lastpos)]
	  [n (jget-field scan name)]
	  [r (jget-field scan radix)])
      (enq! (make-scanned t p l n r (if (memq t literals)
					(jcall scan scan-str)
					#f))
	    q)
      t))
  
  ;; next-token : jobject(Scanner) -> Void
  (define (next-token scan)
    (jcall scan next))
  
  ;; current-token : jobject(Scanner) -> Nat
  (define (current-token scan)
    (jget-field scan token)))