#cs
(module server mzscheme
  (require "io.ss"
           "draw.ss"
           "data.ss"
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  ;; ---------- Configuration -----------
  
  (define server-port 4004)

  (define num-players 2)
  (define board-file "~/tmp/map3")  ; maps available at the contest web site
  (define pack-file "~/tmp/packs3") ; pkg configuartions available there, too
  
  (define robot-capacity 100)
  (define start-money 1000)
  
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
    (let ([str (with-input-from-file pack-file (lambda () (read-string (file-size pack-file))))])
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

  (define replay-panel (instantiate horizontal-panel% (bottom) [alignment '(center top)]))
  (define replay-left (instantiate vertical-panel% (replay-panel) [alignment '(right top)]))
  (define replay-right (instantiate vertical-panel% (replay-panel) [stretchable-width #f]))
  (define backward-button (make-object button% "<<" replay-left (lambda (b e)
                                                                  (set! state-index (sub1 state-index))
                                                                  (refresh-state #f))))
  (define replay-slider #f)
  (define (set-slider! v)
    (when replay-slider
      (send replay-slider show #t)
      (send (send replay-slider get-parent) delete-child replay-slider))
    (set! replay-slider
          (make-object slider% #f 0 v replay-left 
            (lambda (s e)
              (set! state-index (send s get-value))
              (refresh-state #f))
            v)))
  (set-slider! 10)
  (define forward-button (make-object button% ">>" replay-right (lambda (b e)
                                                                  (if (< state-index (length past-states))
                                                                      (begin
                                                                        (set! state-index (add1 state-index))
                                                                        (refresh-state #t))
                                                                      (begin
                                                                        (set! running? #t)
                                                                        (set! current-internal-state #f)
                                                                        (send replay-panel enable #f)
                                                                        (send pause enable #t)
                                                                        (let ([sema resume-sema])
                                                                          (set! resume-sema #f)
                                                                          (semaphore-post sema)))))))
  
  (define pause (make-object button% "Pause" bottom (lambda (b e)
                                                      (set! running? #f)
                                                      (send pause enable #f))))
  (send replay-panel enable #f)
  
  (define running? #t)
  (define resume-sema #f)

  (define state-index 0)

  (define current-internal-state #f)
  
  (define (setup-replay-panel)
    (set! state-index (length past-states))
    (set! current-internal-state (send drawn get-internal-state))
    (set-slider! state-index)
    (send backward-button enable #t)
    (send replay-panel enable #t))
  
  (define (refresh-state forward?)
    (send backward-button enable (> state-index 1))
    (send replay-slider set-value state-index)
    (let ([s (if (= state-index (length past-states))
                 current-internal-state
                 (list-ref past-states (- (length past-states) state-index 1)))])
      (when forward?
        (send drawn apply-queued-actions))
      (send drawn set-internal-state s)))
  
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
    (let ((listener (tcp-listen server-port 5 #t))
          (server-sema (make-semaphore)))
      (let ([client-semas
             (let server-loop ((id 1))
               (if (<= id num-players)
                   (let-values (((input output) (tcp-accept listener))
                                ((client-sema) (make-semaphore)))
                     (thread (lambda () 
                               (with-handlers ([void
                                                (lambda (exn)
                                                  (fprintf (current-error-port)
                                                           "bot ~a exn: ~e~n"
                                                           id (if (exn? exn)
                                                                  (exn-message exn)
                                                                  exn))
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
           (server client-semas server-sema))))))
  
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
      (for-each (lambda (d)
                  (fprintf (current-error-port)
                           "Robot ~a final score: ~a~n"
                           (car d) (cadr d)))
                (send drawn get-dead-robot-scores))
      ;; Continue if non-empty action list:
      (pair? commands)))
  
  (define (server client-semas server-sema)
    (semaphore-Pn server-sema num-players)
    (let ([continue? #f]
          [sema (make-semaphore)])
      (queue-callback (lambda ()
                        (set! continue? (update-state!?))
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
