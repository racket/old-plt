#cs
(module server mzscheme
  (require "io.ss"
           "draw.ss"
           "data.ss"
           (lib "cmdline.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "etc.ss"))
  
  ;; ---------- Configuration -----------
  
  (define server-port 4004)

  (define num-players 2)
  
  (define board-file (build-path (this-expression-source-directory) "map"))
  (define package-file (build-path (this-expression-source-directory) "packs"))
  ; maps & packages available at the contest web site
  
  (define robot-capacity 100)
  (define start-money 500)
  
  (command-line
   "plt-robot-server"
   (current-command-line-arguments)
   [once-each
    [("-p") portno "serve at TCP port number <portno>; default is 4004"
     (let ([n (string->number portno)])
       (if (and n (exact? n) (integer? n) (<= 1 n 65535))
           (set! server-port n)
           (error 'command-line "given <portno> is not a an exact integer in [1,65535]: ~e" portno)))]
    [("-n") num "host <num> players; default is 2"
     (let ([n (string->number num)])
       (if (and n (exact? n) (integer? n) (positive? n))
           (set! num-players n)
           (error 'command-line "given <num> is not a positive exact integer: ~e" num)))]
    [("-m") map-file "sets the map file; default is \"map\" in collection"
     (set! board-file map-file)]
    [("-k") pack-file "sets the package file; default is \"packs\" in collection"
     (set! package-file pack-file)]
    [("-f") fuel "sets the fuel for each player, between 1 and 1000000000"
     (let ([n (string->number fuel)])
       (if (and n (exact? n) (integer? n) (<= 1 n 1000000000))
           (set! start-money n)
           (error 'command-line "given <fuel> is not an exact integer in [1,1000000000]: ~e" fuel)))]
    [("-c") capacity "sets the fuel for each player, bnetween 1 and 1000000000"
     (let ([n (string->number capacity)])
       (if (and n (exact? n) (integer? n) (<= 1 n 1000000000))
           (set! robot-capacity n)
           (error 'command-line "given <capacity> is not an exact integer in [1,1000000000]: ~e" capacity)))]])
    
     
     
     
  
  ;; -----------------------------------
  
  (define board
    (let-values ([(width height)
                  (with-input-from-file board-file
                    (lambda ()
                      (values (string-length (read-line))
                              (let loop ([n 1])
                                (if (eof-object? (read-line))
                                    n
                                    (loop (add1 n)))))))])
      (call-with-input-file board-file (lambda (p) (read-board width height p reverse)))))

  (define board-width (vector-length (vector-ref board 0)))
  (define board-height (vector-length board))
  
  (define packages
    (let ([str (with-input-from-file package-file (lambda () (read-string (file-size package-file))))])
      (let ([p (open-input-string (regexp-replace* ",|Mk|(dest=)|(uid=)|(weight=)" str " "))])
        (let ([raw-packs (read p)])
          (apply
           append
           (map (lambda (one-place)
                  (let ([x (caar one-place)]
                        [y (cadar one-place)])
                    (map (lambda (one)
                           (let ([id (car one)]
                                 [xy (cadr one)]
                                 [wt (caddr one)])
                             (make-pkg id
                                       x
                                       y
                                       (car xy)
                                       (cadr xy)
                                       wt)))
                         (cadr one-place))))
                raw-packs))))))
  
  (define robots (let loop ([id 1])
                   (if (> id num-players)
                       null
                       (cons (list id 1 1 start-money robot-capacity null)
                             (loop (add1 id))))))
  
  (define activity null)
  
  (define-struct state (robots packages actions))
  
  (define past-states null)

  (define f (make-object (class frame% 
                           (define/override (on-close) (exit))
                           (super-instantiate ("Robot")))))
  (define drawn (instantiate board-panel% (f board-width board-height board)))
  (send drawn install-robots&packages robots packages)

  (define bottom (instantiate horizontal-panel% (f) [alignment '(center top)]))

  (define replay-panel (instantiate vertical-panel% (bottom)))
  (define replay-top (instantiate horizontal-panel% (replay-panel)))
  (define backward-button (make-object button% "<<" replay-top (lambda (b e)
                                                                  (set! state-index (sub1 state-index))
                                                                  (refresh-state #f))))
  (make-object vertical-pane% replay-top)
  (define forward-button (make-object button% ">>" replay-top (lambda (b e)
                                                                  (set! state-index (add1 state-index))
                                                                  (refresh-state #t))))
  (define replay-slider #f)
  (define (set-slider! v)
    (when replay-slider
      (send replay-slider show #t)
      (send (send replay-slider get-parent) delete-child replay-slider))
    (set! replay-slider
          (make-object slider% #f 0 v replay-panel
            (lambda (s e)
              (set! state-index (send s get-value))
              (refresh-state #f))
            v)))
  (set-slider! 0)
  
  (define running-old? #f)
  (define pause/play-panel (instantiate vertical-panel% (bottom) [stretchable-width #f]))
  (define pause-button (make-object button% "Pause" pause/play-panel
                         (lambda (b e)
                           (if running-old?
                               (set! running-old? #f)
                               (set! running? #f))
                           (send pause-button enable #f))))
  (define play-button (make-object button% "Play" pause/play-panel
                        (lambda (b e)
                          (let ([len (length past-states)])
                            (set! running-old? #t)
                            (unless (= state-index len)
                              (send play-button enable #f)
                              (send replay-panel enable #f)
                              (send pause-button enable #t)
                              (let loop ()
                                (unless (or (not running-old?)
                                            (= state-index len))
                                  (set! state-index (add1 state-index))
                                  (refresh-state #t)
                                  (loop)))
                              (unless running-old?
                                (send pause-button enable #f)
                                (send replay-panel enable #t)
                                (send play-button enable #t)))
                            (when running-old?
                              (set! running-old? #f)
                              (unless game-over?
                                (set! running? #t)
                                (set! current-internal-state #f)
                                (send replay-panel enable #f)
                                (send play-button enable #f)
                                (send pause-button enable #t)
                                (let ([sema resume-sema])
                                  (set! resume-sema #f)
                                  (semaphore-post sema))))))))

  (send play-button enable #f)
  (send replay-panel enable #f)
  
  (define running? #t)
  (define resume-sema #f)
  
  (define game-over? #f)

  (define state-index 0)

  (define current-internal-state #f)
  
  (define (setup-replay-panel)
    (send pause-button enable #f) ;; already done, unless this is for end-of-game
    (set! state-index (length past-states))
    (set! current-internal-state (send drawn get-internal-state))
    (set-slider! state-index)
    (send backward-button enable #t)
    (send play-button enable #t)
    (send replay-panel enable #t))
  
  (define (refresh-state forward?)    
    (send replay-slider set-value state-index)
    (let* ([last? (= state-index (length past-states))]
           [s (if last?
                  current-internal-state
                  (list-ref past-states (- (length past-states) state-index 1)))])
      (when forward?
        (let ([old? running-old?])
          (unless old?
            (send replay-panel enable #f)
            (send play-button enable #f))
        (send drawn apply-queued-actions)
          (unless old?
            (send play-button enable #t)
            (send replay-panel enable #t))))
      (send drawn set-internal-state s)
      (send backward-button enable (> state-index 0))
      (send forward-button enable (not last?))))
  
  (send f show #t)
  
  (define-struct command (bid type args))
  
  (define player-commands (make-vector num-players #f))
  
  (define (get-command id in)
    (parameterize ([read-case-sensitive #t])
      (let* ((bid (read in))
             (command (read in)))
        (cond
          ((eq? '|Move| command)
           (let ((dir (read in)))
             (vector-set! player-commands (sub1 id)
                          (make-command bid
                                        'move
                                        (case dir
                                          [(E) 'e]
                                          [(W) 'w]
                                          [(N) 'n]
                                          [(S) 's]
                                          [else (error 'robot "bad direction: ~e" dir)])))))
          (else 
           (let ((args (read-line in)))
             (vector-set! player-commands (sub1 id)
                          (make-command bid 
                                        (case command
                                          [(Pick) 'pick]
                                          [(Drop) 'drop]
                                          [else (error 'robot "bad command: ~e" command)])
                                        (read (open-input-string
                                               (string-append "(" args ")")))))))))
      #t))
    
  (define (semaphore-Pn s n)
    (when (> n 0)
      (yield s)
      (semaphore-Pn s (sub1 n))))
  
  (define (start-server)
    (let* ([d (make-object (class dialog%
                             (define/override (on-close) (exit))
                             (super-instantiate ()))
                "Waiting" f)]
           [mk-label (lambda (num-players)
                       (format "Waiting for ~a clients..." num-players))]
           [m (make-object message% (mk-label num-players) d)])
      (make-object button% "Exit" d (lambda (b e) (exit)))
      (thread
       (lambda ()
         (let ((listener (tcp-listen server-port 5 #t))
               (server-sema (make-semaphore)))
           (let ([client-semas
                  (let server-loop ((id 1))
                    (if (<= id num-players)
                        (let-values (((input output) (tcp-accept listener))
                                     ((client-sema) (make-semaphore)))
                          (send m set-label (mk-label (- num-players id)))                                
                          (thread (lambda () 
                                    (with-handlers ([void
                                                     (lambda (exn)
                                                       (unless (eq? exn 'dead)
                                                         (fprintf (current-error-port)
                                                                  "bot ~a exn: ~e~n"
                                                                  id (if (exn? exn)
                                                                         (exn-message exn)
                                                                         exn)))
                                                       (close-input-port input)
                                                       (close-output-port output)
                                                       ;; For now, we'll just go into a
                                                       ;; do-nothing loop.
                                                       (let loop ()
                                                         (semaphore-post server-sema)
                                                         (semaphore-wait client-sema)
                                                         (loop)))])
                                      (client-handler input output client-sema server-sema id))))
                          (cons client-sema (server-loop (add1 id))))
                        null))])
             (thread 
              (lambda ()
                (server client-semas server-sema)))))
         (send d show #f)))
      (send d show #t)))
  
  (define (randomize l)
    (let loop ([l l])
      (if (null? l)
          l
          (let ([n (random (length l))])
          (cons (list-ref l n)
                (loop (remq (list-ref l n) l)))))))
  
  (define (update-state!?)
    (let ([commands (let loop ([i 0])
                     (if (= i num-players)
                         null
                         (let ([cmd (vector-ref player-commands i)])
                           (vector-set! player-commands i #f)
                           (if cmd
                               (cons
                                (list (add1 i)
                                      (command-bid cmd)
                                      (case (command-type cmd)
                                        [(move) (command-args cmd)]
                                        [(pick drop) (cons (command-type cmd) (command-args cmd))]))
                                (loop (add1 i)))
                               ;; otherwise, bot is apparently dead:
                               (loop (add1 i))))))])
      (let ([actions (quicksort (randomize commands) (lambda (a b)
                                                       (< (cadr a) (cadr b))))])
        (send drawn queue-robot-actions actions)
        (set! past-states (cons (send drawn get-internal-state) past-states)))
      (send drawn apply-queued-actions)
      (set! activity (send drawn get-most-recent-activity))
      (set!-values (robots packages) (send drawn get-robots&packages))
      ;; Continue if non-empty action list:
      (pair? commands)))
  
  (define (server client-semas server-sema)
    (semaphore-Pn server-sema num-players)
    (let ([continue? #f]
          [sema (make-semaphore)])
      (queue-callback (lambda ()
                        (set! continue? (update-state!?))
                        (unless continue?
                          (set! game-over? #t)
                          (set! running? #f))
                        (if running?
                            (semaphore-post sema)
                            (begin
                              (setup-replay-panel)
                              (set! resume-sema sema)))))
      (semaphore-wait sema)
      (when continue?
        (map semaphore-post client-semas)
        (server client-semas server-sema))))
    
  (define (client-handler input output client-sema server-sema id)
    (when (regexp-match "^Player" input)
      ;; Send board
      (fprintf output "~a ~a~n" board-width board-height)
      (let ([s (make-string board-width)])
        (let loop ([j 0])
          (unless (= j board-height)
            (let loop ([i 0])
              (unless (= i board-width)
                (string-set! s i
                             (case (vector-ref (vector-ref board j) i)
                               [(wall) #\#]
                               [(plain) #\.]
                               [(base) #\@]
                               [(water) #\~]))
                (loop (add1 i))))
            (fprintf output "~a~n" s)
            (loop (add1 j)))))
      (let ([r (assoc id robots)])
        (fprintf output "~a ~a ~a~n" id (list-ref r 3) (list-ref r 4)))
      ;; Initial robot positions
      (for-each (lambda (r)
                  (fprintf output "#~a X ~a Y ~a " (car r) (cadr r) (caddr r)))
                robots)
      (newline output)
      (let loop ([orig-robots robots])
        ;; Print packages at my position:
        (let* ([r (let ([r (assoc id robots)])
                    (or r
                        ;; Otherwise, this robot is dead.
                        (raise 'dead)))]
               [x (list-ref r 1)]
               [y (list-ref r 2)])
          (for-each (lambda (pack)
                      (when (and (= x (cadr pack))
                                 (= y (caddr pack))
                                 (not (ormap (lambda (r)
                                               (member (car pack)
                                                       (list-ref r 5)))
                                             robots)))
                        (fprintf output "~a ~a ~a ~a "
                                (car pack) ; id
                                (list-ref pack 3)  ; dest-x
                                (list-ref pack 4)  ; dest-x
                                (list-ref pack 5)))) ; weight
                    packages))
        (newline output)
        (when (get-command id input)
          (semaphore-post server-sema)
          ;; Wait until all turns taken...
          (semaphore-wait client-sema)
          ;; Tell the client what happened:
          (for-each (lambda (act)
                      (fprintf output "#~a " (car act))
                      (when orig-robots
                        (let* ([r (assoc (car act) orig-robots)])
                          (fprintf output "X ~a Y ~a " (cadr r) (caddr r))))
                      (for-each (lambda (i)
                                  (if (symbol? i)
                                      (fprintf output "~a " i)
                                      (fprintf output "~a ~a " (car i) (cadr i))))
                                (cdr act)))
                    activity)
          (newline output)
          ;; Continue
          (loop #f)))))

  (start-server))
