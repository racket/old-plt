#cs
(module admin mzscheme 
  
  ;; the game administrator: keeps track of indexes, players, scores
  
  (require "if.scm"
           "graph.scm"
           "test-regions.scm"
           "tiles.scm"
           "tile-info.scm"
           "aux.scm"
           "contract.scm"
           "view.scm"
           (file "Testing/testing.scm")
           (lib "pretty.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss"))
  
  (provide admin%)
  
  (define ROUNDS 75)
  
  ;; (notify <object> <method> <argument> ... handler: <expression> okay: <expression>)
  ;; call <method> on <object> with <argument> ... 
  ;; the object must evaluate to an instance of observer
  ;; if this raises a contract error, evaluate <expression>, inform user
  ;; if it raises an ordinary error, do same
  (define-syntax notify
    (syntax-rules (handler: okay:)
      [(_ o m args ... handler: h)
       (notify o m args ... handler: h okay: (void))]
      [(_ o m args ... handler: h okay: s)
       {(let/ec done
          (begin
            (let ([ov o])
              (with-handlers ([contract? (lambda (x)
                                           (printf "CONTRACT: ! ")
                                           (printf (contract-msg x))  
                                           (newline)
                                           (done (lambda () h)))]
                              [exn:fail? (lambda (x) 
                                      (parameterize 
                                          ([current-output-port (current-error-port)])
                                        (printf "ERROR: ")
                                        (printf "~a~n" (exn-message x))
                                        (newline)
                                        (done (lambda () h))))])
                (send ov m args ...)))
            (lambda () s)))}]))
  
  ;; --- internale player rep --- 
  (define-struct iplayer (player follower score followers) (make-inspector))
  ;; Iplayer = (make-iplayer player<%> Follower Number)
  
  (set! make-iplayer
        (let ([make-iplayer0 make-iplayer])
          (lambda (a b c) (make-iplayer0 a b c MAX-FOLLOWERS))))
  
  ;; Listof[observer<%>] (observer<%> -> Void) -> Void 
  ;; notify each p in obs via note
  (define (notify* obs note) (for-each note obs))
  
  ;; Listof[Iplayer] Follower -> Listof[player<%>] 
  ;; remove player with follower f 
  (define (not-f ps f)
    (filter (lambda (p) (not (follower=? (iplayer-follower p) f))) ps))
  
  ;; Iplayer -> Boolean 
  ;; does the iplayer still contain a player? 
  (define (good? ip) (is-a? (iplayer-player ip) player<%>))
  
  ;; Follower Listof[Iplayer] -> (union false Iplayer)
  ;; look for player with matching follower f in players 
  (define (lookup-player f players)            
    (let L ([ps players])
      (cond [(null? ps) #f]
            [(follower=? (iplayer-follower (car ps)) f) (car ps)]
            [else (L (cdr ps))])))
  
  ;; --- the administrator --- 
  (define admin%
    (class* object% (admin<%>)
      (super-new)
      
      (init-field [show #f])
      
      (field [players   '()]             ;; Listof[Iplayer]
             [obs*      '()]             ;; Listof[observer<%>]
             [followers (all-followers)] ;; Listof[Follower]
             [graph     (new graph%)])             
      
      (define/public (get-graph) graph)
      
      ;; --- register --- 
      (define/public (register-observer o) (set! obs* (cons o obs*)))
      
      (define/public (register s) (if (pair? followers) (register! s) #f))
      
      ;; player<%> -> Follower
      ;; ASSUME: there is at least one follower 
      (define/private (register! s)
        (define f (begin0
                    (car followers)
                    (set! followers (cdr followers))))
        (set! players (cons (make-iplayer s f 0) players))
        f)
      
      (define/public (test-register)
        (define f followers)
        (printf "testing register ...~n")
        (test== (send this register "1") (car f))
        (test== (length followers) (- (length f) 1))
        (test== players (list (make-iplayer "1" (car f) 0)))
        (set! followers '())
        (test== (send this register "2") #f)
        (printf "done.~n"))
      
      ;; --- run-game --- 
      (define/public (run-game) 
        (unless (pair? players)
          (sequence/violation 
           "run-game should only be invoked after at least one player registered"))
        (set! followers '())
        (run/p (make-random-set-of-tiles ROUNDS) players))
      
#|
      ;; Listof[-> Void]
      ;; the list of observer actions to run after a turn completes w/o error
      (define todo '())
      (define (todo-reset) (set! todo '()))
      (define (todo-add a) (set! todo (cons a todo)))
      (define (todo-run) (for-each (lambda (a) (a)) todo))
|#      
      ;; Listof[Index] Listof[Iplayer] -> String
      ;; run the entire game, starting with the given indexes and players 
      (define/private (run/p indexes players)
        (define (ht ip)
          (printf "there goes player ~a~n" (iplayer-follower ip))
          (set-iplayer-player! ip #f)
          (set! players (filter good? players)))
        (notify* players
                 (lambda (pl)
                   (define p (iplayer-player pl))
                   (notify p inform (iplayer-follower pl) handler: (ht pl))))
        (let run/p ([r 0][indexes indexes][players players])
          (define (play-one-turn x)
            (define rest (caddr x))
            (define next (car players))
            (define foll (iplayer-follower next))
            (define (handler)
              (printf "there goes player ~a ~n" foll)
              (run/p r indexes (filter good? (cdr players))))
            (define turn (make-object turn-state% (car x) next (cadr x) players))
            (notify turn hand-yourself-to-player
                    handler: (handler)
                    okay:
                    (run/p (+ r 1) rest (filter good? (rotate players)))))
          (printf ">>> round ~s~n" r)
          (cond
            [(null? indexes) (stop r players)]
            [(null? players) "game over, all players failed"]
            [(pick-next-playable-tile indexes) => play-one-turn]
            [else (stop r players)])))
      
      ;; Number Listof[Iplayer] -> String
      ;; create final message 
      (define/private (stop rounds players)
        (string-append 
         (format length-fmt rounds)
         (final-message players)))
      
      (define FMT:CONGRATS
        "Congrats to rank ~a (out of ~a).\nGood game, as they say here.\n")
      
      ;; Listof[Iplayer] -> String 
      ;; turn the final ranking into a string 
      ;; effect: amend message and send to each player 
      ;; ASSUME: (pair? players)
      (define/private (final-message players)
        (define (score< ip1 ip2) (> (iplayer-score ip1) (iplayer-score ip2)))
        (define ranks (quicksort (final-board-eval players) score<))
        (define msg   (ranks->string ranks))
        (define le    (length players))
        (for-each (lambda (ip)
                    (define rk (iplayer-score ip))
                    (define xp (iplayer-player ip))
                    (define (h) 
                      (printf "~a crashed during final-message~n"
                              (iplayer-follower ip)))
                    (define mg (format FMT:CONGRATS rk le))
                    (notify xp inform mg handler: (h)
                            okay: 
                            (notify xp inform msg handler: (h))))
                  ranks)
        msg)
      
      ;; Listof[Iplayer] -> Listof[Iplayer]
      ;; compute the final score for all active players
      ;; effect: modify the players so that they have their follower back 
      ;; and reflect the current score [should I return the follower]
      (define/private (final-board-eval players)
        (define a (send graph abbey-regions 'remove-them))
        (for-each (lambda (sc)
                    (define f (car sc))
                    (define s (cadr sc))
                    (define ip (lookup-player f players))
                    (when (iplayer? ip)
                      (set-iplayer-score! ip (+ (iplayer-score ip) s))
                      #;(set-iplayer-followers! ip  (+ (iplayer-followers ip) 1))))
                  a)
        players)
      
      (define/public (test-final-eval)
        (define dummy (new test-player%))
        (define players1 (list (make-iplayer dummy RED 0)
                               (make-iplayer dummy GREEN 0)
                               (make-iplayer dummy BLACK 0)
                               (make-iplayer dummy BLUE 0)))
        (define (set-graph)
          (let* ([comp road0]
                 [comp (place-follower comp -1 0 INNER RED)]
                 [comp (place-follower comp +2 0 INNER GREEN)]
                 [comp (place-follower comp 0 +1 INNER BLUE)])
            (let-values ([(g _) (comp)])
              (set! graph g))))
        (printf "testing final eval ...~n")
        (set-graph)
        (test== (final-board-eval '())  '() "final  eval, empty list of players")
        (set-graph)
        (test== (final-board-eval players1) 
                (list (make-iplayer dummy RED 2)
                      (make-iplayer dummy GREEN 3)
                      (make-iplayer dummy BLACK 0)
                      (make-iplayer dummy BLUE 4)))
        (printf "done.~n"))
      
      ;; Listof[Iplayer] -> Listof[(list Iplayer Number String)]
      ;; create ranking message
      ;; effect: set score field of each player to final ranking 
      ;; ASSUME: (pair? players)
      (define/private (ranks->string ranks)
        (define fst (car ranks))
        (define scr (iplayer-score fst))
        (set-iplayer-score! fst 1)
        (string-append
         (format rank-format "1" (iplayer-follower fst) scr)
         (let L ([ranks (cdr ranks)][r 1][c 2][s scr])
           (cond
             [(null? ranks) ""]
             [else 
              (let* ([fst (car ranks)]
                     [col (iplayer-follower fst)]
                     [scr (iplayer-score fst)])
                (if (= scr s)
                    (begin
                      (set-iplayer-score! fst r)
                      (string-append
                       (format rank-format (number->string r) col s) 
                       (L (cdr ranks) r (+ c 1) s)))
                    (begin 
                      (set-iplayer-score! fst c)
                      (string-append 
                       (format rank-format (number->string c) col scr)
                       (L (cdr ranks) c (+ c 1) scr)))))]))))
      
      (field [rank-format "~a: ~a scored ~a\n"]  ;; rank, color score
             [length-fmt  "The game went for ~s rounds.~n"]) ;; number of rounds 
      
      (define/public (test-ranking)
        (define (mk l)
          (define tps (map (lambda _ (new test-player%)) l))
          (apply values (append tps (map make-iplayer tps (map car l) (map cadr l)))))
        (define (test-msg tp exp-message n m lst)
          (test== (final-message lst) exp-message)
          (test== (car (send tp retrieve)) (list exp-message (format FMT:CONGRATS n m))))
        
        (printf "testing ranking ...~n")
        (let ()
          (define-values (tp1 ip1) (mk `((,RED 10))))
          (test== (iplayer-score ip1) 10)
          (test-msg tp1 (format rank-format 1 RED 10) 1 1 (list ip1)))
        
        (let ()
          (define-values (tp1 tp2 ip1 ip2) (mk `((,RED 10) (,GREEN 10))))
          (define msg (string-append (format rank-format 1 RED 10) 
                                     (format rank-format 1 GREEN 10)))
          (test-msg tp1 msg 1 2 (list ip1 ip2)))
        
        (let ()
          (define-values (tp1 tp2 tp3 ip1 ip2 ip3)
            (mk `((,RED 10) (,GREEN 10) (,BLACK 8))))
          (define msg (string-append (format rank-format 1 RED 10)
                                     (format rank-format 1 GREEN 10)
                                     (format rank-format 3 BLACK 8)))
          (test-msg tp2 msg 1 3 (list ip1 ip2 ip3)))
        
        (let ()
          (define-values (tp1 tp2 tp3 ip1 ip2 ip3)
            (mk `((,RED 10) (,GREEN 9) (,BLACK 8))))
          (define msg (string-append (format rank-format 1 RED 10)
                                     (format rank-format 2 GREEN 9)
                                     (format rank-format 3 BLACK 8)))
          (test-msg tp3 msg 3 3 (list ip1 ip2 ip3)))
        
        (printf "done.~n"))
      
      
      ;; Listof[Index] -> (union #f (list Index Listof[tile<%>] Listof[Index]))
      ;; can this admin place any of the remaining tiles on the graph
      (define/private (pick-next-playable-tile indexes)
        (let until-placable ([indexes indexes][seen '()])
          (cond
            [(null? indexes) #f]
            [else 
             (let* ([first  (car indexes)]
                    [places (send graph potential-locations-for-tile first)])
               (if (pair? places)
                   (list first places (append (cdr indexes) seen))
                   (until-placable (cdr indexes) (cons first seen))))])))
      
      ;; TESTing pick
      (define/public (test-pnt)
        (define-values (g t) (castle1))
        (printf "testing pick next playable tile ... ~n")
        (set! graph g)
        (test== (pick-next-playable-tile '()) #f "empty indexes")
        (set! graph g)
        (test== (pick-next-playable-tile '("3")) #f "non-playable index")
        (set! graph g)
        (test== (pick-next-playable-tile '("3" "3")) #f "two non-playeable indexes")
        (set! graph g)
        (test-p (pick-next-playable-tile '("5"))
                (lambda (x)
                  (and (string=? (car x) "5")
                       (null? (caddr x))
                       (list-tile=
                        (cadr x)
                        (list (make-tile "5" -1 -1 270)
                              (make-tile "5" +0 +1 180))))))
        (set! graph g)
        (test-p (pick-next-playable-tile '("3" "5" "3"))
                (lambda (x)
                  (and (string=? (car x) "5")
                       (equal? (caddr x) '("3" "3"))
                       (list-tile=
                        (cadr x)
                        (list (make-tile "5" -1 -1 270)
                              (make-tile "5" +0 +1 180))))))
        (set! graph (new graph%))
        (printf "done~n"))
      
      ;;--- new turn 
      (define turn-state% 
        (class* object% (turn<%>)
          (super-new)
          (init-field index iplayer places-for-i players)

          (define f (iplayer-follower iplayer))
          
          (define turn-base% 
            (class* object% (turn<%>)
              (super-new)
              ;; --- get-index ---
              (define/public (get-index) index)
              ;; --- potential-locations-for-tile ---
              (define/public (potential-locations-for-tile) places-for-i)
              ;; --- place-tile --- 
              (define/public (place-tile t)
                (define (inform-others)
                  (notify* obs* 
                           (lambda (o)
                             (notify o placed-tile t handler: (ho o))))
                  (notify* (not-f players f)
                           (lambda (pl)
                             (define p (iplayer-player pl))
                             (notify p placed-tile t handler: (ht pl)))))
                (set! graph (send graph insert-tile t))
                (for-each (lambda (cr) (process-completed cr)) 
                          (send graph complete-regions))
                (todo-add inform-others))
              ;; --- potential-locations-for-followers ---
              (define/public (potential-locations-for-followers)
                (send graph potential-locations-for-followers))
              ;; --- place-follower ---
              (define/public (place-follower t p)
                (define (inform-others)
                  (notify* obs* 
                           (lambda (o)
                             (notify o placed-follower t f p handler: (ho o))))
                  (notify* (not-f players f)
                           (lambda (ip)
                             (define tp (iplayer-player ip))
                             (notify tp placed-follower t f p handler: (ht ip)))))
                (unless (> (iplayer-followers iplayer) 0)
                  (contract/violation
                   (format "player ~a tried to place a follower it didn't have" f)))
                ;; this should be a todo?? 
                (set-iplayer-followers! iplayer (- (iplayer-followers iplayer) 1))
                (set! graph (send graph place-follower t p f))
                (todo-add inform-others))))
          
          ;; initial turn: disable place-follower, everything else is allowed 
          (define turn0% 
            (class* turn-base% (turn<%>)
              (super-new)
              ; (rename [super-place-tile place-tile])
              (define/override (place-tile t)
                (super place-tile t)
                (set! turn-state (new turn-placed-tile%)))
              (define/override (place-follower t p)
                (sequence/violation
                 (format "player ~a tried to place a follower before placing a tile" f)))))
          
          ;; a tile has been placed but it is still possible to place a follower 
          (define turn-placed-tile%
            (class* turn-base% (turn<%>)
              (super-new)
              (define/override (place-tile t)
                (sequence/violation "player tried to place a tile twice during a turn"))
              ;(rename [super-place-follower place-follower])
              (define/override (place-follower t p)
                (super place-follower t p)
                (set! turn-state (new turn-placed-follower%)))))
          
          ;; a tile and a follower have been placed 
          (define turn-placed-follower% 
            (class* turn-base% (turn<%>)
              (super-new)
              (define/override (place-tile t)
                (sequence/violation "player tried to place a tile twice during a turn"))
              (define/override (place-follower t p)
                (sequence/violation
                 (format "player ~a tried to place a second follower" f)))))
          
          (field [turn-state (new turn0%)]
                 [todo '()])
          
          ;; (-> Void) -> Void
          ;; effect: add t to todo
          (define/private (todo-add t) (set! todo (cons t todo)))
          
          ;; -> Void
          ;; effect: execute all thunks on todo, reset 
          (define/private (todo-run) 
            (for-each (lambda (th) (th)) (reverse todo))
            (set! todo '()))
          
          ;; -> Void 
          ;; remove player locally and globally 
          (define/private (ht ip)
            (printf "there goes player ~a~n" (iplayer-follower ip))
            (set-iplayer-player! ip #f)
            (set! players (filter good? players)))
          
          ;; -> Void 
          ;; remove observer from registry
          (define/private (ho o)
            (printf "there goes an observer ~a~n" o)
            (set! obs* (remq o obs*)))
          
          ;; Listof[completed<%>] -> Void
          (define/private (process-completed cr)
            ;; evaluate the region:
            (for-each
             (lambda (fn) 
               (define ip (lookup-player (car fn) players))
               (define n  (cadr fn))
               (define scr (caddr fn))
               (when (iplayer? ip) ;; it could have been thrown out already 
                 (let ([f (iplayer-follower ip)])
                   (define (inform-others)
                     (notify* (not-f players (iplayer-follower ip))
                              (lambda (ip)
                                (define tp (iplayer-player ip))                              
                                (notify tp other-score-and-token f scr n
                                        handler: (ht ip))))
                     (notify* obs* 
                              (lambda (o)
                                (notify o other-score-and-token f scr n
                                        handler: (ho o)))))
                   (set-iplayer-score! ip (+ (iplayer-score ip) scr))
                   (set-iplayer-followers! ip (+ (iplayer-followers ip) n))
                   (notify (iplayer-player ip) score-and-token scr n
                           handler: (ht ip))
                   (todo-add inform-others))))
             (assign-points (send cr followers) (send cr score)))
            ;; then: 
            (todo-add (lambda () (send cr remove-followers))))
          
          ;; Listof[(list Follower Number)] -> Listof[(list Follower Number Number)]
          (define/private (assign-points l scr)
            (if (null? l)
                '()
                (let ([mf (apply max (map cadr l))])
                  (map (lambda (x) (append x (list (if (= (cadr x) mf) scr 0))))
                       l))))
          
          (test== (assign-points '((a 1) (b 2) (c 1) (d 2) (e 0)) 8)
                  '((a 1 0) (b 2 8) (c 1 0) (d 2 8) (e 0 0)))
          
          ;; -> Void
          (define/public (hand-yourself-to-player)
            (unless (is-a? turn-state turn0%)
              (sequence/violation
               (format "can't call hand-yourself-to-player anymore")))
            (send (iplayer-player iplayer) take-turn this)
            (unless (or (is-a? turn-state turn-placed-follower%)
                        (is-a? turn-state turn-placed-tile%))
              (sequence/violation
               (format "player ~a didn't even place a tile~n" f)))
            (todo-run))
          
          (define/public (get-index) index)
          
          (define/public (potential-locations-for-tile) 
            (send turn-state potential-locations-for-tile))
          
          (define/public (place-tile t) (send turn-state place-tile t))
          
          (define/public (potential-locations-for-followers)
            (send turn-state potential-locations-for-followers))
          
          (define/public (place-follower t p) (send turn-state place-follower t p))))
      
      ;; --- tests --- 
      
      (define/public (test-run)
        (printf "testing run ...~n")
        
        (let () ;; no player available 
          (test== (run/p '("1" "2" "3") '()) "game over, all players failed"))
        
        (let-values ([(g x) (castle1)]) ;; no index placable 
          (set! graph g)
          (test== (run/p '("3") (list (make-iplayer (new test-player%) RED 0)))
                  ;; "3" can't be placed with castle1 as given situation 
                  (string-append (format length-fmt 0)
                                 (format rank-format 1 RED 0))
                  "run/p 1"))
        
        (let-values ([(g x) (castle1)]) ;; play 1 round
          (define tp (new test-player%))
          (set! graph g)
          (test== (run/p '("4") (list (make-iplayer tp RED 0)))
                  (string-append (format length-fmt 1)
                                 (format rank-format 1 RED 0))
                  "run/p 2"))
        
        (let () ;; play two rounds, check notification system
          (define tp 
            (new (test-2-18-player% ;; one 
                  (lambda (t decr) 
                    (define pf (send t potential-locations-for-followers))
                    (send t place-tile (make-tile (send t get-index) 1 0 90))
                    (decr)
                    (send t place-follower (caar pf) (cadar pf))))))
          (define ip (make-iplayer tp RED 0))
          (define tp2 
            (new (test-2-18-player%  ;; two
                  (lambda (t decr) 
                    (define tile (make-tile (send t get-index) 0 -1 180))
                    (send t place-tile tile)))))
          (define ip2 (make-iplayer tp2 GREEN 0))   
          (define fmg (string-append (format rank-format 1 RED 4) 
                                     (format rank-format 2 GREEN 0)))
          (set! graph (new graph%))
          (test== (run/p '("2" "18") (list ip ip2)) 
                  (string-append (format length-fmt 2) fmg)
                  "run/p 3")
          (test== (iplayer-followers ip) MAX-FOLLOWERS "2-18: followers")
          (let* ([r (send tp2 retrieve)]
                 [msg (car r)]
                 [tls (cadr r)]
                 [fls (car (caddr r))]
                 [scr (cadddr r)])
            (test== msg (list (string-append fmg)
                              (format FMT:CONGRATS 2 2)
                              "green") "notification msg 3")
            ;; followers:
            (test-eq (car fls) (make-tile "00" 0 0 0) tile= "notification followers tile")
            (test== (cadr fls)  RED "notification followers follower")
            (test== (caddr fls) NORTH "notification followers position")
            (test== scr 0)
            ;; tiles
            (test-eq tls (list (make-tile "2" 1 0 90)) list-tile=))
          (show-me graph))
        
        (let () ;; play two rounds, with one cheater; check elimination system 
          (define tp 
            (new (test-2-18-player% ;; one 
                  (lambda (t decr) 
                    (define pf (send t potential-locations-for-followers))
                    (define first-pf (car pf))
                    (define second-pf (cadr pf))
                    (send t place-tile (make-tile (send t get-index) 1 0 90))
                    (decr)
                    (send t place-follower (car first-pf) (cadr first-pf))
                    (send t place-follower (car second-pf) (cadr second-pf))))))
          (define ip (make-iplayer tp RED 0))
          (define tp2 
            (new (test-2-18-player%  ;; two
                  (lambda (t decr) 
                    (define tiles (send t potential-locations-for-tile))
                    (send t place-tile (car tiles))))))
          (define ip2 (make-iplayer tp2 GREEN 0))   
          (define fmg (string-append (format rank-format 1 GREEN 0)))
          ;"1: player: red score: 4\n2: player: green score: 0\n"
          (define o (open-output-string))
          (set! graph (new graph%))
          (parameterize ([current-output-port o])
            (test== (run/p '("2" "18") (list ip ip2)) 
                    fmg 
                    "run/p 3 cheater, retract scoring"))
          ;; now check properties 
          (test-eq (get-output-string o) 'stupid 
                   (lambda (x y) (regexp-match "CONTRACT" x))
                   (format "contract not violated: ~s" (get-output-string o)))
          (test== (iplayer-followers ip2) MAX-FOLLOWERS "2-18: followers")
          (let* ([r (send tp2 retrieve)]
                 [msg (car r)]
                 [tls (cadr r)]
                 [fls (caddr r)]
                 [scr (cadddr r)])
            (test== msg (list fmg (format FMT:CONGRATS 1 1) "green") "notification msg 4")
            ;; tiles
            (test== tls '() "notification: tile placement 4")
            ;; followers:
            (test== fls '() "notification: follower placement 4")
            (test== scr 0))
          (show-me graph))
        
        (let () ;; chris's problem 
          (define __ (printf "testing Chris's bug~n"))
          (define (pp t) (send t to-string))
          (define tp
            (new (test-2-18-player%  ;; one
                  (lambda (t decr) 
                    (define plt (send t potential-locations-for-tile))
                    (define _1 (pretty-print (map pp plt)))
                    (define i (send t get-index))
                    (if (string=? i "6")
                        (let ()
                          (define _2 (send t place-tile (car (cdddr plt))))
                          (define plf (send t potential-locations-for-followers))
                          (define ff  (caddr plf))
                          (pretty-print (map (lambda (t) (list (pp (car t)) (cadr t))) plf))
                          (send t place-follower (car ff) (cadr ff)))
                        (let ()
                          (define _2 (send t place-tile (car plt)))
                          (define plf (send t potential-locations-for-followers))
                          (pretty-print (map (lambda (t) (list (pp (car t)) (cadr t))) plf))))))))
          (define ip (make-iplayer tp WHITE 0))
          (define tp2
            (new (test-2-18-player% 
                  (lambda (t decr) 
                    (define pf (send t potential-locations-for-tile))
                    (void "now die")))))
          (define ip2 (make-iplayer tp2 RED 0))
          (define fmg (string-append (format rank-format 2 GREEN 0)))
          (set! graph (new graph%))
          (run/p '("6" "21") (list ip ip2))
          (show-me graph))
        (printf "done~n"))
      ))
  
  ;; --- test players ---
  (define test-player%
    (class* object% (player<%>)
      (super-new)
      (field [m '()][ts '()][fs '()][score 0])
      (define/public (inform msg) (set! m (cons msg m)))
      (define/public (retrieve) (list m ts fs score))
      
      (define/public (take-turn t) 
        (define tiles (send t potential-locations-for-tile))
        (define choic (list-ref tiles (random (length tiles))))
        (send t place-tile choic))
      
      (define/public (score-and-token tokens ponts) (raise 'score-and-token))
      
      (define/public (placed-tile t) (set! ts (cons t ts)))
      
      (define/public (placed-follower t f p) (set! fs (cons (list t f p) fs)))
      (define/public (other-score-and-token f s n) (void))
      ))
  
  ;; --- test players for a specific case ---
  (define (test-2-18-player% tt)
    (class* test-player% ()
      (super-new)
      (field [available-followers 7]) (inherit-field score)
      (define/override (score-and-token s t) 
        (set! available-followers (+ available-followers t))
        (set! score (+ score s)))
      (define/override (take-turn t) 
        (tt t (lambda () (set! available-followers (- available-followers 1)))))))
  
  (new admin%)
  
  )
