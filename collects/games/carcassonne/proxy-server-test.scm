#cs
(module proxy-server-test mzscheme 
  
  (require "if.scm"
           "proxy-player.scm"
           "proxy-auxs.scm"
           "tiles.scm"
           "aux.scm"
           (lib "class.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 0))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (require (planet "util.ss" ("schematics" "schemeunit.plt" 1 0)))
  
  (require/expose 
   "proxy-auxs.scm" 
   (create-call parse-call create-return parse-return read-message))
  
  (require/expose "proxy-server.scm" (create-proxy-player))
  
  (define tc1
    (make-test-case
     "create proxy server"
     (assert equal? 
             (let ()
               (define-values (in cp) (make-pipe))
               ((make-call cp) 'register "m")
               (create-proxy-player in (open-output-string)))
             "m")))
  
  (test/text-ui (make-test-suite "create proxy player" tc1))
  
  (define tc2
    (make-test-case 
     "server"
     (assert (set=? string=?)
             (let ()
               (define ad (new fake-admin%))
               (define cu (make-custodian))
               (define p1 (thread (register "p1")))
               (define p2 (thread (register "p2")))
               (define p3 (thread (register "p3")))
               (define sv (parameterize ([current-custodian cu])
                            (server ad)))
               (thread-wait p1)
               (thread-wait p2)
               (thread-wait p3)
               (for-each kill-thread (list p1 p2 p3))
               (custodian-shutdown-all cu)
               (send ad get-players))
             '("p1" "p2" "p3"))))
  
  (define fake-admin%
    (class object% 
      (super-new)
      (define players '())
      (define colors '("red" "green" "blue"))
      (define/public (register m) 
        (when (pair? colors)
          (set! players (cons (get-field name m) players))
          (begin0 (car colors)
                  (set! colors (cdr colors)))))
      (define/public (get-players) players)))
  
  ;; String -> (-> Void)
  (define (register x)
    (lambda ()
      (define-values (in out) 
        (let L ()
          (with-handlers ([exn:fail? (lambda (y) (L))])
            (tcp-connect "localhost" PORT))))
      ((make-call out) 'register x)
      ((make-listen in void void void))))
  
  (test/text-ui (make-test-suite "server" tc2))
  
  )
