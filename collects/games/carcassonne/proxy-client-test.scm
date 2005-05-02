#cs
(module proxy-client-test mzscheme
  
  ;; testing the client and its proxy-admin class
  
  (require "if.scm"
           "proxy-server.scm"
           "proxy-client.scm"
           "proxy-auxs.scm"
           (lib "class.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 0))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (define tc1 
    (make-test-case 
     "start a client, connect"
     (assert equal? 
             (let ()
               (define o (open-output-string))
               (parameterize ([current-output-port o])
                 (define listen (tcp-listen PORT 30 #t #f))
                 (define th (thread (get-registration listen)))
                 (define cl (client "m"))
                 (thread-wait th)
                 (kill-thread th)
                 (kill-thread cl))
               (get-output-string o))
             (format "registration ~a: ~a~n" "m" RED))))
  
  (define (get-registration listen)
    (lambda ()
      (define-values (in out) (tcp-accept listen))
      (let/ec done 
        ((make-listen in void void void)
         (lambda (m args)
           (if (eq? 'register m)
               (done (void))
               (error 'get-registration "~e" `(,m . ,args))))))
      ((make-return out) RED)))
  
  (test/text-ui (make-test-suite "client" tc1)))