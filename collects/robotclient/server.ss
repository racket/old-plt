(module server mzscheme
  
  (require "array2d.ss")
  
  (define server-port 8002)
  (define board-width 10)
  (define board-height 5)
  (define robot-capacity 10)
  (define start-money 100)
  
  ;; The packages that exist in the game
  (define-struct package (dest-x dest-y weight))
  (define packages (vector (make-package 4 4 10)))
  
  ;; pack-list should contain a list of integers to index into packages.
  (define-struct board-spot (kind pack-list))
  (define (make-board)
    (let ((b (make-array2d board-width board-height (make-board-spot #\. null))))
      b))
  
  (define board (make-board))

  ;; pack-list should contain a list of integers to index into packages.  
  (define-struct player-state (id x y money pack-list))
  (define player-state-vector (vector (make-player-state 0 1 1 start-money null)))
  (define num-players (vector-length player-state-vector))
  
  (define (send-board out)
    (display board-width out)
    (display #\space out)
    (display board-height out)
    (newline out)
    (let loop ((i 0)
               (j 0))
      (cond
        ((>= i board-height) (void))
        ((>= j board-width)
         (newline out)
         (loop (add1 i) 0))
        (else
         (display (board-spot-kind (array2d-ref board i j)))
         (loop i (add1 j))))))
        
  (define (send-player-config out id)
    (display id out)
    (display robot-capacity out)
    (display start-money out)
    (newline))
    
  (define (send-initial-response out)
    (let loop ((id 0))
      (cond 
        ((< id num-players)
         (display #\# out)
         (display id out)
         (display #\space out)
         (display #\X out)
         (display #\space out)
         (display (player-state-x (vector-ref player-state-vector)) out)
         (display #\space out)
         (display #\Y out)
         (display #\space out)
         (display (player-state-y (vector-ref player-state-vector)) out)
         (loop (add1 id)))
        (else (newline out)))))
    
  (define (send-package-list x y out)
    (for-each
     (lambda (pack)
       (display pack out)
       (display #\space out)
       (let ((p (vector-ref packages pack)))
         (display (package-dest-x p) out)
         (display #\space out)
         (display (package-dest-y p) out)
         (display #\space out)
         (display (package-weight p) out)
         (display #\space out)))
     (board-spot-pack-list (array2d-ref board x y)))
    (newline out))
           
  (define-struct command (id bid type args))
  
  (define player-commands (make-vector num-players #f))
  
  (define (get-commands id in)
    (let* ((bid (read in))
           (command (read in)))
      (cond
        ((eq? '|Move| command)
         (let ((dir (read in)))
           (vector-set! player-commands id (make-command bid command dir))))
        (else 
         (let ((args (read-line in)))
           (vector-set! player-commands id 
                        (make-command id 
                                      bid 
                                      command
                                      (read (open-input-string
                                             (string-append "(" args ")"))))))))))
  
  (define (semaphore-Pn s n)
    ((> n 0)
     (semaphore-wait s)
     (semaphore-Pn s (sub1 n))))
  
  (define (semaphore-Vn s n)
    ((> n 0)
     (semaphore-post s)
     (semaphore-Vn s (sub1 n))))
  
  (define (start-server)
    (let ((listener (tcp-listen server-port))
          (client-sema (make-semaphore))
          (server-sema (make-semaphore)))
      (thread (server client-sema server-sema))
      (let server-loop ((id 0))
        (cond
          ((< id num-players)
           (let-values (((input output) (tcp-accept listener)))
             (thread (client-handler input output client-sema server-sema id))
             (server-loop (add1 id))))))))
  
  (define (server client-sema server-sema)
    (semaphore-Pn server-sema num-players)
    (update-state!)
    (semaphore-Vn client-sema)
    (server client-sema server-sema))
  
  (define (get-robot x y)
    (let loop ((i 0))
      (cond
        ((< i num-players)
         (let ((r (vector-ref player-state-vector i)))
           (cond
             ((and (= x (player-state-x r))
                   (= y (player-state-y r)))
              r)
             (else (loop (add1 i))))))
        (else #f))))
           
  
  (define (get-spaces-until-no-robot x y dx dy)
    (cond
      ((or (>= x board-width) (>= y board-width))
       (list (make-board-spot #\# null)))
      (else
       (let ((r (get-robot x y)))
         (cond
           ((not r) (list (array2d-ref board x y)))
           (else
            (cons r
                  (get-spaces-until-no-robot (+ x dx) (+ y dy) dx dy))))))))
  
  (define (do-move r dx dy pushed)
    (let ((robots (get-spaces-until-no-robot (player-state-x r)
                                             (player-state-y r)
                                             dx
                                             dy)))
      (for-each
       (lambda (r)
         (cond
           ((player-state? r) (set! pushed (player-state-id r) #t))))
       (cdr robots))
      (
     
  
  (define (update-state!)
    (let ((pushed (make-vector num-players #f))
          (commands
           (mergesort (vector->list player-commands)
                      (lambda (a b)
                        (< (command-bid a) (command-bid b))))))
      ;; need to randomize equal commands
      (for-each
       (lambda (command)
         (let ((r (vector-ref player-state-vector (command-id command))))
           (case (command-type command)
             ((|Move|)
              (case (command-args command)
                ((|N|)
                 (do-move r 0 1 pushed))
                ((|E|)
                 (do-move r 1 0 pushed))
                ((|S|)
                 (do-move r 0 -1 pushed))
                ((|W|)
                 (do-move r -1 0 pushed))))))
         commands))))
    
  (define (client-handler input output client-sema server-sema id)
    (lambda ()
      (read-string 6 input) ;; look for string Player
      (send-board output)
      (send-player-config output)
      (send-initial-response output)
      (let loop ()
        (let ((ps (vector-ref player-state-vector id)))
          (send-package-list (player-state-x ps)
                             (player-state-y ps)
                             output))
        (get-commands id input)
        (semaphore-post server-sema)
        (semaphore-wait client-sema)
        ;; response
        (loop))))
)