#cs
(module proxy-client mzscheme
  
  ;; a function for creating a CLIENT thread 
  
  (require "if.scm"
           "player.scm"
	   "proxy-admin.scm"
           "proxy-player.scm"
           (lib "etc.ss")
           (lib "class.ss"))
  
  (provide
   client ;; String [player<%>-class] [with-view] -> (union Void Thread)
   ;; effects: 
   ;; 1. connect to the server 
   ;; 2. create a proxy-admin 
   ;; 3. create and register the player (name) with the proxy
   ;; 4. run the game (if possible)
   )
  
  (define client 
    (opt-lambda (n [player% player%][with-view (lambda (x) x)])
      (define-values (input output)
        (let loop ()
          (with-handlers
              ([exn:fail?  ; i/o:tcp
                (lambda (x) 
                  (fprintf (current-error-port) "tcp couldn't connect: ~e~n" n)
                  (loop))])            
            (tcp-connect (GAME-MACHINE) PORT))))
      (define padmin (new proxy-admin% [input input][output output]))
      (define player (new (with-view player%) [name n]))
      (define signup (send padmin register player))
      (printf "registration ~a: ~a~n" n signup)
      (if (boolean? signup)
          (client n player% with-view)
          (send padmin run-game)))))
