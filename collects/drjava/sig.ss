(define-signature tokens^
  (jEOF jIDENTIFIER jIMPORT jPACKAGE jSEMI jLBRACE jRBRACE jLBRACKET jRBRACKET jLPAREN
        jRPAREN jINTLITERAL jLONGLITERAL jFLOATLITERAL jDOUBLELITERAL jCHARLITERAL
        jSTRINGLITERAL jBOOLEAN jBYTE jCHAR jSHORT jINT jLONG jFLOAT jDOUBLE jDOT))

(define-signature gjc-core^
  (name->string compile-gjlist gjc-version gjc-class gjc-output-dir set-gjc-output-dir!))

(define-signature gjc^ ((open gjc-core^) (open tokens^)))

(define-signature queue^
  (enq! deq! dupq quick-dupq mtq mtq? last-q first-q q-length q-ref))

(define-signature split^
  (compile scan-classes compile-classes compile-class (struct tokens (name q))
           (struct split-error (message pos))))

(define-signature scanner^
  (new-scanner enq-token! next-token current-token string->scanner (struct scanned (token pos lastpos name radix str))))

(define-signature repl^
  (new-repl eval-str (struct repl (env))))

(define-signature error^ (report-error report-warning))
(define-signature gui^ (new-document))
(define-signature gui-text^ (definitions-text-mixin repl-text-mixin add-parts current-goobers (open error^)))
(define-signature goober^ (goober-panel%))
(define-signature input-base^ (frame:basic% text:basic%))

(define-signature channel^ (make-channel channel-get channel-put))