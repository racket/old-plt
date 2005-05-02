#cs
(module run-proxy-aux mzscheme
  
  (require "if.scm"
           "proxy-server.scm"
           "proxy-client.scm"
           "admin.scm"
           "player.scm"
           "view.scm"
           (lib "class.ss")
           (lib "list.ss")
           (lib "pretty.ss")
           (lib "mred.ss" "mred"))
  
  (provide 
   go
   ;; Listof[(union String (-> Thread) (list String Class[player<%>]))] -> (-> Void)
   ;; return: a thunk that can stop everything
   ;; effect: create these players as clients, start the server
   
   run-clients
   ;; Listof[(union String (-> Thread) (list String Class[player<%>]))] -> Custodian
   ;; start the given clients, return custodian that controls them all 
   
   ;; ASSUMPTION: the players are sequential until they have registered, 
   ;; then they spawn a thread 
   
   go-distributed ;; -> Void
   ;; fire up a server that plays as many round-1 tournaments as 
   ;; necessary to get all players through
   
   run-distributed
   ;; Listof[(union String (-> Thread) (list String Class[player<%>]))] {Any} -> Custodian
   ;; start the given clients, return custodian that controls them all 
   ;; if a second argument is given, just keep repeating the loop while exn:i/o is raised
   )
  
  (define (go l view-demanded?)
    (define custo (make-custodian))    
    (parameterize ([current-custodian custo])      
      (parameterize ([current-eventspace (make-eventspace)])
        (define _ (thread (lambda () (run-clients l view-demanded?))))
        (define admin (new admin%))
        (define sthrd (server admin))
        (when view-demanded? (new view% [admin admin]))
        (thread-wait
         (thread
          (lambda ()
            (printf ">>> running admin run-game: max turn time is ~s~n" (max-turn-time))
            (time (printf "--- the end ---~n~a~n" (send admin run-game))))))))
    (printf "shutting down everything!~n")
    (custodian-shutdown-all custo))
  
  (define (run-clients l view-demanded? . repeat)
    (define custo (make-custodian))
    (parameterize ([current-custodian custo])
      (define pv (if view-demanded? player-with-view (lambda (x) x)))
      (with-handlers ([exn? (lambda (x) ; i/o
                                  (if (pair? repeat)
                                      (run-clients l view-demanded? 'yes)
                                      (raise x)))])
        (for-each (lambda (x)   
                    (cond
                      [(string? x) (client x player% pv)]
                      [(procedure? x) (x)]
                      [(and (pair? x) (pair? (cdr x))) (client (car x) (cadr x) pv)]
                      [else (error 'run-clients "bad client ~e" x)]))
                  l))))
  
  
  ;; --- 
  (define players-left '("Anne"    ; 1
                         "Barbara" ;2 
                         "Cornelia" ;3 
                         "Doris" ; 4
                         "Elisabeth" ; 5
                         "Franzesca" ; 6
                         "Gisela" ; 7 
                         "Hilde" ; 8 
                         "Ingrid" ; 9 
                         "Jutta" ; 10 
                         "Karin" ; 11
                         "Lotte" ; 12
                         "Michaela" ;13 
                         "Nicole" ; 14
                         ))
 
  (define play-list2%
    (class play-list% 
      (init-field o)
      (super-new)
      (define/override (legal? p) 
        (and (string? p) (or (member p players-left) (regexp-match "Felleisen" p))))
      (define/override (registered! p)
        (fprintf o ">>> picked up ~s~n" p)
        (set! players-left (remove p players-left)))))
  
  (define (go-distributed)
    (define custo (make-custodian))
    (define co (current-output-port))
    (printf ">>> still in play: ~n")
    (pretty-print players-left)
    (max-turn-time 30)
    (printf "end of tournament~n~a~n" 
            (parameterize ([current-custodian custo])
                (define admin (new admin%))
                (define sthrd (server admin (new play-list2% [o co])))
                (define result (send admin run-game))
                (printf "--- the end ---~n")
                (pretty-print result)
                result))
    (printf "shutting down one round!~n")
    (custodian-shutdown-all custo)
    (if (or (null? players-left) (null? (cdr players-left)))
        (printf "one round of tournament; players left: ~s~n" players-left)
        (go-distributed)))
  
  (define (run-distributed)
    (define (thread- n th)
      (thread
       (lambda ()
         (with-handlers ([void (lambda _ 
                                 (printf ">>> sleeping: ~s~n" n)
                                 (sleep 60)
                                 (printf ">>> waking: ~s~n" n)
                                 (thread- n th))])
           (printf ">>> starting ~s~n" n)
           (th)
           (printf ">>> done ~s~n" n)))))
    (for-each
     (lambda (x)   
       (cond
         [(string? x)
          (thread- x (lambda () (client x)))]
         [(procedure? x) 
          (thread- "bad" x)]
         [(and (pair? x) (pair? (cdr x))) 
          (thread- (car x) (lambda () (client (car x) (cadr x))))]
         [else (error 'run-clients "bad client ~e" x)]))
     players-left))
  
  )
