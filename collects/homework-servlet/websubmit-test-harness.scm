(module websubmit-test-harness mzscheme
  (require (lib "unitsig.ss")
           "websubmit-sig.scm"
           "websubmit-machine.scm"
           )
  
  (provide test-machine^
           token-stream^
           token-stream@
           (struct exn-finished ())
           dummy-unit-from-sig
           )
  
  (define-signature token-stream^ (reset-token-stream! get-next-token get-result))
  (define-signature test-machine^ (start-machine reset-token-stream! get-result))
  
  (define-struct (exn-finished exn) ())
  
  (define token-stream@
    (unit/sig token-stream^
      (import)
      
      (define the-stream #f)
      (define the-result '())
      
      (define reset-token-stream!
        (lambda (l)
          (set! the-stream l)
          (set! the-result '())))
      
      (define get-next-token
        (lambda ignore
          (when (null? the-stream) (raise (make-exn-finished "finished" (current-continuation-marks))))
          (begin0
            (car the-stream)
            (set! the-result (append the-result (list (car the-stream))))
            (set! the-stream (cdr the-stream)))))
      
      (define get-result
        (lambda () the-result))))
  
   (define-syntax (dummy-unit-from-sig stx)
    (syntax-case stx ()
      [(_ sig)
       (let* [(sig-stuff (syntax-local-value #'sig))
              (sig-quoted-vector ((syntax-local-value
                                   #'signature->symbols)
                                  #'(_ sig)))
              (names (syntax-case sig-quoted-vector (quote)
                       [(quote #(n ...))
                        #'(n ...)]))]
         (with-syntax ([(name ...)
                        (syntax-object->datum names)]
                       [(struct-base-str ...)
                        (map
                         (lambda (a-name)
                           (substring (symbol->string a-name) 4))
                         (syntax-object->datum names))]
                       [(pred? ...)
                        (datum->syntax-object
                         stx
                         (map
                          (lambda (a-name)
                            (string->symbol
                             (string-append (substring (symbol->string a-name) 4) "?")))
                          (syntax-object->datum names)))])
           
           #'(unit/sig sig
               (import token-stream^)
               (define name
                 (lambda who-cares
                   (let ([tok (get-next-token)])
                     (if (pred? tok)
                         tok
                         (error (format "Inconsistent state. Expected ~s, given: " struct-base-str) tok)))))
               ...)))]))
  
  )