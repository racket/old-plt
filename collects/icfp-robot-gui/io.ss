
(module io mzscheme
  (require (lib "list.ss")
           "data.ss")
  
  (provide connect move read-board)
  
  (define-struct session (r w))
  
  (define (check what got)
    (unless (equal? what got)
      (error 'check "expected ~e, got ~e" what got)))
  
  (define (skip-space r)
    (when (eq? #\space (peek-char r))
      (read-char r)
      (skip-space r)))
  
  (define (read-board width height r reorder-lines)
    (list->vector
     (reorder-lines
      (let loop ([j 0])
        (if (= j height)
            null
            (cons
             (list->vector
              (let ([s (read-string width r)])
                (check #\newline (read-char r))
                (map (lambda (c)
                       (case c
                         [(#\.) 'plain]
                         [(#\#) 'wall]
                         [(#\@) 'base]
                         [(#\~) 'water]
                         [else (error 'read-board "bad board")]))
                     (string->list s))))
             (loop (add1 j))))))))
  
  (define (connect server port)
    (let-values ([(r w) (tcp-connect server port)])
      (fprintf w "Player~n")
      (let ([width (read r)]
            [height (read r)])
        (skip-space r)
        (check #\newline (read-char r))
        (let ([board (read-board width height r values)])
          (let ([me (read r)]
                [money (read r)]
                [max-lift (read r)])
            (skip-space r)
            (check #\newline (read-char r))
            (let-values ([(robots packages)
                          (read-state r me (list (list me '? '? money max-lift null)) null)])
              (values (make-session r w)
                      board
                      me
                      robots
                      packages)))))))

  (define (read-state r me old-robots old-packages)
    (let ([state (read-line r)]
          [drops null])
      ;; (printf "~s~n" state)
      (let ([robots (let ([p (open-input-string state)])
                      (let loop ([id -1][accum null])
                        (cond
                          [(regexp-match-peek "^ *#([0-9]*) *" p)
                           (let ([m (regexp-match "^ *#([0-9]*) *" p)])
                             (let ([id (string->number (cadr m))])
                               (loop id
                                     (cons (let ([m (assoc id old-robots)])
                                             (if m
                                                 (apply list m)
                                                 (list id
                                                       '?
                                                       '?
                                                       0
                                                       0
                                                       null)))
                                           accum))))]
                          [else (let ([cmd (read p)])
                                  (if (eof-object? cmd)
                                      (reverse accum)
                                      (let ([robot (assoc id accum)])
                                        (case cmd
                                          [(x) (list-set! robot 1 (read p))]
                                          [(y) (list-set! robot 2 (read p))]
                                          [(d) (let ([pid (read p)])
                                                 ;; We know about this package, now...
                                                 (set! drops (cons (list pid (bot-x robot) (bot-y robot)) 
                                                                   drops))
                                                 (drop! robot pid))]
                                          [(p) (pickup! robot (read p))]
                                          [(e) (move-it! robot 1 0)]
                                          [(w) (move-it! robot -1 0)]
                                          [(n) (move-it! robot 0 1)]
                                          [(s) (move-it! robot 0 -1)])
                                        (loop id accum))))])))])
        (let ([me-x (cadr (assoc me robots))]
              [me-y (caddr (assoc me robots))])
          (values
           robots
           (add-dropped-packages
            drops
            old-packages
            (add-owned-packages
             robots
             old-packages
             (let ([packs-here (read-line r)])
               (let ([p (open-input-string packs-here)])
                 (let loop ()
                   (let ([id (read p)]
                         [dest-x (read p)]
                         [dest-y (read p)]
                         [weight (read p)])
                     (if (eof-object? id)
                         null
                         (cons
                          (list id me-x me-y
                                dest-x dest-y
                                weight)
                          (loop))))))))))))))
  
  (define (move bid session m me old-robots old-packages)
    (let ([mstr
           (case m
             [(n) "Move N"]
             [(e) "Move E"]
             [(w) "Move W"]
             [(s) "Move S"]
             [else (format "~a~a"
                           (if (eq? (car m) 'drop)
                               "Drop"
                               "Pick")
                           (let loop ([l (cdr m)])
                             (if (null? l)
                                 ""
                                 (format " ~a~a" (car l) (loop (cdr l))))))])])
      (fprintf (session-w session) "~a ~a~n" bid mstr)
      (read-state (session-r session) me old-robots old-packages)))
  
  (define (list-set! l pos v)
    (if (= pos 0)
        (set-car! l v)
        (list-set! (cdr l) (sub1 pos) v)))
  
  (define (move-it! l dx dy)
    (let ([x (list-ref l 1)]
          [y (list-ref l 2)])
      (list-set! l 1 (+ x dx))
      (list-set! l 2 (+ y dy))))

  (define (pickup! l pid)
    (list-set! l 5 (let ([l (list-ref l 5)])
                     (if (member pid l)
                         l
                         (cons pid l)))))

  (define (drop! l pid)
    (list-set! l 5 (remove pid (list-ref l 5))))

  (define (mk-pack pid x y old-l)
    ;; Used when we know about a package only by drop/pick action
    (let ([old-pack (assoc pid old-l)])
      (list pid
            x
            y
            (if old-pack
                (list-ref old-pack 3)
                #f)
            (if old-pack
                (list-ref old-pack 4)
                #f)
            (if old-pack
                (list-ref old-pack 5)
                #f))))
  
  (define (add-owned-packages r old-l l)
    (let loop ([r r][accum l])
      (cond
        [(null? r) accum]
        [else (loop
               (cdr r)
               (append
                (let loop ([pl (list-ref (car r) 5)])
                  (cond
                    [(null? pl) null]
                    [(assoc (car pl) accum) (loop (cdr pl))]
                    [else (cons (mk-pack (car pl)
                                         (list-ref (car r) 1)
                                         (list-ref (car r) 2)
                                         old-l)
                                (loop (cdr pl)))]))
                accum))])))
  
  (define (add-dropped-packages dropped old-l l)
    ;; A drop is (list id x y)
    (let loop ([dropped dropped][l l])
      (cond
        [(null? dropped) l]
        [(assoc (caar dropped) l)
         ;; dropped pkg already known...
         (loop (cdr dropped) l)]
        [else
         (loop (cdr dropped) (cons (mk-pack (caar dropped)
                                            (cadar dropped)
                                            (caddar dropped)
                                            old-l)
                                   l))])))
  
  (define (try-one server port)
    (let-values ([(session board me robots packages) (connect server port)])
      (printf "~a~n" robots)
      (move 1 session 's me robots)))
  )
