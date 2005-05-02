#cs
(module run-proxy-bad-players mzscheme
  
  (require "if.scm" "proxy-auxs.scm" (lib "class.ss"))
  
  (provide
   qos-attack bad-register bad-register2 good-register-no-return ;; -> Void 
   ;; send a badly formatted registration message
   
   make-bad ;; (Symbol u String) ( [Symbol Any ... -> Void] [ -> Any] OPort -> Void ) -> Thread 
   ;; a player that registers and sets up caller/listener and then defers to the 
   ;; given procedure 
   )

  (define (qos-attack)
    (for-each bad-register '(0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9)))

  (define (bad-register . i)
    (define-values (in out) (let L () 
                              (with-handlers ([exn? (lambda (u) (L))])
                                (tcp-connect (GAME-MACHINE) PORT))))
    (fprintf out "<call name=\"register\" />"))
  
  (define (bad-register2)
    (define-values (in out) (let L () 
                              (with-handlers ([exn? (lambda (u) (L))])
                                (tcp-connect (GAME-MACHINE) PORT))))
    (fprintf out "<call name=\"register\""))
  
  (define (good-register-no-return)
    (define-values (in out) (let L () 
                              (with-handlers ([exn? (lambda (u) (L))])
                                (tcp-connect (GAME-MACHINE) PORT))))
    (define call (make-call out))
    (call 'register "good-register-bad-return")
    (close-input-port in)
    (close-output-port out))
  
  (define (make-bad tag p)
    (lambda ()
      (define name (format "bad-~a" tag))
      (define-values (in out) (let L () 
                                (with-handlers ([exn? (lambda (u) (L))])
                                  (tcp-connect (GAME-MACHINE) PORT))))
      (define (prot . x) (printf "!!! ~s~n" x))
      (define listen (make-listen in prot prot prot))
      (define call (make-call out))
      (call 'register name)
      (printf "registered: ~a as ~a~n" name (listen))
      (thread 
       (lambda ()
         (listen (lambda (m args)
                   (if (eq? m 'take-turn)
                       (p call listen out)
                       (printf ">>> [~a] ~s ~s~n" name m args))))))))
  )
