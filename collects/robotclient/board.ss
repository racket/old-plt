(module board mzscheme
  
  (require "array2d.ss"
           (lib "list.ss"))

  (provide get-type get-robot get-valid set-valid set-invalid get-weight set-weight
           get-spot set-spot get-player-x get-player-y
           board-height board-width board
           (struct command (bid command arg))
           (struct package (id weight x y))
           player-id player-money player-capacity packages-held
           (struct robot (id x y))
           read-board! read-response!
           fix-home!)
           
  ;; empty = 0
  ;; water = 1
  ;; wall = 2
  ;; home = 3
  
  (define-syntax get-type
    (syntax-rules ()
      ((_ n)
       (bitwise-and #b11 (arithmetic-shift n -26)))))
  
  (define-syntax set-empty
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b10011111111111111111111111111))))
  
  (define-syntax set-water
    (syntax-rules ()
      ((_ n)
       (bitwise-ior
        (bitwise-and n #b10011111111111111111111111111)
        #b00100000000000000000000000000))))
  
  (define-syntax set-wall
    (syntax-rules ()
      ((_ n)
       (bitwise-ior
        (bitwise-and n #b10011111111111111111111111111)
        #b01000000000000000000000000000))))
  
  (define-syntax set-home
    (syntax-rules ()
      ((_ n)
       (bitwise-ior n #b01100000000000000000000000000))))
  
  (define-syntax get-robot
    (syntax-rules ()
      ((_ n)
       (arithmetic-shift n -28))))
  

  (define-syntax set-robot
    (syntax-rules ()
      ((_ n)
       (bitwise-ior n #b10000000000000000000000000000))))

  (define-syntax set-robot-empty
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b01111111111111111111111111111))))
      
  (define-syntax get-valid
    (syntax-rules ()
      ((_ n)
       (bitwise-and #b1 (arithmetic-shift n -25)))))
  
  (define-syntax set-valid
    (syntax-rules ()
      ((_ n)
       (bitwise-ior n #b00010000000000000000000000000))))
  
  (define-syntax set-invalid
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b11101111111111111111111111111))))
  
  (define-syntax get-weight
    (syntax-rules ()
      ((_ n)
       (bitwise-and n #b00001111111111111111111111111))))
  
  (define-syntax set-weight
    (syntax-rules ()
      ((_ n w)
       (bitwise-ior (bitwise-and n #b11110000000000000000000000000)
                    w))))

  (define-syntax get-spot
    (syntax-rules ()
      ((_ board x y)
       (let ((x1 x)
             (y1 y))
         (cond
           ((and (> x1 0) (> y1 0) (<= x1 (board-width)) (<= y1 (board-height)))
            (array2d-ref board (sub1 y1) (sub1 x1)))
           (else
            #b01000000000000000000000000000))))))
          
  (define-syntax set-spot
    (syntax-rules ()
      ((_ board x y new-val)
       (let ((x1 x)
             (y1 y))
         (cond
           ((and (> x1 0) (> y1 0) (<= x1 (board-width)) (<= y1 (board-height)))
            (array2d-set! board (sub1 y1) (sub1 x1) new-val)))))))
  
  (define-syntax get-player-x
    (syntax-rules ()
      ((_) (robot-x (hash-table-get (robot-table) (player-id))))))

  (define-syntax get-player-y
    (syntax-rules ()
      ((_) (robot-y (hash-table-get (robot-table) (player-id))))))
  
  
  (define board-height (make-parameter 0))
  (define board-width (make-parameter 0))
  (define board (make-parameter (vector)))
  
  ;; (make-command num `(s n e w p d) list-of-package)
  (define-struct command (bid command arg))
  
  (define-struct package (id weight x y))
  
  (define player-id (make-parameter 0))
  (define player-money (make-parameter 0))
  (define player-capacity (make-parameter 0))
  (define packages-held (make-parameter null))
  
  (define-struct robot (id x y))
  (define robot-table (make-parameter (make-hash-table)))
  (define robot-indexes (make-parameter null))
  
  (define (read-good-char in)
    (let ((c (read-char in)))
      (case c
        ((#\. #\~ #\# #\@) c)
        (else (read-good-char in)))))

  (define-struct response (id name arg))
  
  (define (read-response id s)
    (let* ((command (read s)))
      (case command
        ((N S E W) (make-response id command #f))
        ((P D) (make-response id command (read s)))
        ((X) 
         (let ((x (read s)))
           (read s)
           (make-response id 'X (cons x (read s))))))))
  
  (define (read-initial-response! in init-gui)
    (let ((s (open-input-string (regexp-replace "#" (read-line in) " ^ "))))
      (let* ((responses
              (filter (lambda (x)
                        (printf "~a~n" x)
                        (eq? 'X (response-name x)))
                      (let loop ((in (read s)))
                        (cond
                          ((eof-object? in) null)
                          (else
                           (cons (read-response in s)
                                 (loop (read s))))))))
             (robots
              (let loop ((responses responses))
                (cond
                  ((null? responses) null)
                  (else
                   (cons (make-robot (response-id (car responses))
                                     (car (response-arg (car responses)))
                                     (cdr (response-arg (car responses))))
                         (loop (cdr responses))))))))
        (for-each (lambda (robot)
                    (let ((x (robot-x robot))
                          (y (robot-y robot)))
                      (set-spot (board) x y (set-robot (get-spot (board) x y))))
                    (hash-table-put! (robot-table) (robot-id robot) robot))
                  robots)
        (init-gui (board-width) (board-height) (board)
                  robots)
        (robot-indexes (map robot-id robots)))))
    
  (define (find-dead alive all)
    (cond
      ((null? alive) all)
      ((= (car alive) (car all))
       (find-dead (cdr alive) (cdr all)))
      (else
       (cons (car all)
             (find-dead alive (cdr all))))))
  
  (define (read-response! packages in gui-update)
    (let ((s (open-input-string (regexp-replace "#" (read-line in) " ^ "))))
      (let* ((responses (read-response s))
             (alive-robots (map response-id responses))
             (dead-robots
              (find-dead (mergesort alive-robots <)
                         (mergesort (robot-indexes) <)))
             (package-table (make-hash-table)))
        (gui-update (map (lambda (r)
                           (list (response-id r)
                                 0
                                 (case (response-command r)
                                   ((n s w e) (response-command r))
                                   ((p) (list 'pick (response-arg
        (for-each
         (lambda (p)
           (hash-table-put! package-table
                            (package-id p)
                            p))
         packages)
        (for-each
         (lambda (id)
           (let* ((r (hash-table-get (robot-table) id))
                  (x (robot-x r))
                  (y (robot-y r)))
             (set-spot (board) x y (set-robot-empty (get-spot (board) x y)))
             (hash-table-remove! (robot-table) id)))
         dead-robots)
        (begin0
          (map
           (lambda (r)
             (let* ((old-robot (hash-table-get (robot-table) (response-id r)))
                    (new-robot
                     (make-robot
                      (robot-id old-robot)
                      (robot-x old-robot)
                      (robot-y old-robot))))
               (case (response-name r)
                 ((P)
                  (cond
                    ((= (response-id r) (player-id))
                     (packages-held
                      (append
                       (map
                        (lambda (id)
                          (hash-table-get package-table id))
                        (response-arg r))
                       (packages-held))))))
                 ((D)
                  (cond
                    ((= (response-id r) (player-id))
                     (let ((drops (make-hash-table)))
                       (for-each
                        (lambda (d)
                          (hash-table-put! drops d #t)))
                       (packages-held
                        (filter (lambda (p)
                                  (not (hash-table-get drops (package-id p)
                                                       (lambda () #f))))
                                (packages-held)))))))
                 ((N) (set-robot-y! new-robot (add1 (robot-y new-robot))))
                 ((S) (set-robot-y! new-robot (sub1 (robot-y new-robot))))
                 ((E) (set-robot-x! new-robot (add1 (robot-x new-robot))))
                 ((W) (set-robot-x! new-robot (sub1 (robot-x new-robot))))
                 ((X)
                  (set-robot-x! new-robot (car (response-arg r)))
                  (set-robot-y! new-robot (cdr (response-arg r)))))
               (set-spot (board)
                         (robot-x old-robot)
                         (robot-y old-robot)
                         (set-robot-empty (get-spot (board)
                                                    (robot-x old-robot)
                                                    (robot-y old-robot))))
               (set-spot (board)
                         (robot-x new-robot)
                         (robot-y new-robot)
                         (set-robot (get-spot (board)
                                              (robot-x new-robot)
                                              (robot-y new-robot))))
               (hash-table-put! (robot-table) (robot-id new-robot) new-robot)
               old-robot))
           responses)
          (robot-indexes alive-robots)))))
        
  (define (read-board! input init-gui)
    (board-width (read input))
    (board-height (read input))
    (board (make-array2d (board-height) (board-width) 0))
    (let loop ((i 1)
               (j 1))
      (cond
        ((> i (board-height)) (void))
        ((> j (board-width)) (loop (add1 i) 1))
        (else
         (let ((c (read-good-char input)))
           (set-spot (board) j i
                     (case c
                       ((#\~) (set-water 0))
                       ((#\#) (set-wall 0))
                       ((#\@) (set-home 0))
                       (else 0))))
         (loop i (add1 j)))))
    (player-id (read input))
    (player-capacity (read input))
    (player-money (read input))
    (read-line input)
    (read-initial-response! input init-gui))
    
  (define (fix-home!)
    (let ((spot (get-spot (board) (get-player-x) (get-player-y))))
      (cond
        ((= 3 (get-type spot))
         (set-spot (board) (get-player-x) (get-player-y) (set-empty spot))))))
  )