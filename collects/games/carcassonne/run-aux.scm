(module run-aux mzscheme

  ;; auxliary functions for an ordinary run
  
  (require "admin.scm" "view.scm" "player.scm" (lib "class.ss"))
  
  (provide 
   go ;; Listof[(list Class String)] Boolean -> Thread
   ;; set up, run, time a game ; show me if given true 
   )
  
  (define (go l view-demanded?)
    ;; an administrator
    (define admin (new admin%))

    ;; Class String -> Void
    ;; makes player, registers it with admin, prints confirmation 
    (define (r +view)
      (lambda (c% n)
        (printf ">>> ~a plays as ~a~n" n 
                (send admin register (new (+view c%) [name n])))))

    ;; -> Void
    ;; run game and print statistics 
    (define (go) (time (printf "--- the end ---~n~a~n" (send admin run-game))))

    ;; a view, if demanded 
    (if view-demanded?
        (begin
          (for-each (r player-with-view) (map car l) (map cadr l))
          (new view% [admin admin]))
        (for-each (r (lambda (x) x)) (map car l) (map cadr l)))
    
    ;; go, go, go:
    (thread go))
  )
