#cs
(module server mzscheme
  (require "io.ss"
           "draw.ss"
           (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  ;; ---------- Configuration -----------
  
  (define server-port 4004)

  (define num-players 1)
  (define board-file "~/tmp/map2")  ; maps available at the contest web site
  (define pack-file "~/tmp/packs2") ; pkg configuartions available there, too
  
  (define robot-capacity 100)
  (define start-money 100)
  
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
                             (list id
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

  (define f (instantiate frame% ("Robot")))
  (define drawn (instantiate board-panel% (f board-width board-height board)))
  (send drawn install-robots&packages robots packages)
  
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
  
  (define (semaphore-Vn s n)
    (when (> n 0)
      (semaphore-post s)
      (semaphore-Vn s (sub1 n))))
  
  (define (start-server)
    (let ((listener (tcp-listen server-port 5 #t))
          (client-sema (make-semaphore))
          (server-sema (make-semaphore)))
      (let server-loop ((id 1))
        (when (<= id num-players)
          (let-values (((input output) (tcp-accept listener)))
            (thread (lambda () (client-handler input output client-sema server-sema id)))
            (server-loop (add1 id)))))
      (server client-sema server-sema)))
  
  (define (update-state!)
    (let ([commands (let loop ([i 0])
                     (if (= i num-players)
                         null
                         (let ([cmd (vector-ref player-commands i)])
                           (cons
                            (list (add1 i)
                                  (command-bid cmd)
                                  (case (command-type cmd)
                                    [(move) (command-args cmd)]
                                    [(pick drop) (cons (command-type cmd) (command-args cmd))]))
                            (loop (add1 i))))))])
      (send drawn queue-robot-actions commands)
      (send drawn apply-queued-actions)
      (set! activity (send drawn get-most-recent-activity))
      (set!-values (robots packages) (send drawn get-robots&packages))))
  
  (define (server client-sema server-sema)
    (semaphore-Pn server-sema num-players)
    (update-state!)
    (semaphore-Vn client-sema num-players)
    (server client-sema server-sema))
    
  (define (client-handler input output client-sema server-sema id)
    (when (regexp-match "^Player" input)
      (printf "player!~n")
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
        (let* ([r (assoc id robots)]
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
                          (fprintf output "#~a X ~a Y ~a " (car r) (cadr r) (caddr r))))
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
