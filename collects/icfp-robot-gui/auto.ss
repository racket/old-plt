
(module auto mzscheme
  (require "io.ss"
           "data.ss"
           (lib "list.ss"))

  (provide run-robot run-robots)
  
  (define (match-up steps possible-moves possible-dests)
    (if (null? steps)
        'n ;; arbitrary
        (ormap (lambda (m d)
                 (and (equal? d (car steps)) m))
               possible-moves
               possible-dests)))

  (define (run-robots n server port)
    (if (<= n 1)
        (run-robot server port)
        (begin
          (thread (lambda () (run-robot server port)))
          (run-robots (sub1 n) server port))))
  
 (define (run-robot server port)
   (let-values ([(session board me robots0 packages0) (connect server port)])
     (let ([width (vector-length (vector-ref board 0))]
           [height (vector-length board)])
       
       (define bases
         (let iloop ([i 0])
           (if (= i width)
               null
               (let loop ([j 0])
                 (if (= j height)
                     (iloop (add1 i))
                     (if (eq? 'base (get-cell board i j))
                         (cons (cons i j) (loop (add1 j)))
                         (loop (add1 j))))))))
       (define possible-bases null)
       
       (define (find-steps-toward-targets starts targets)
         ;; Copy the board:
         (let ([b2 (list->vector
                    (map (lambda (b)
                           (list->vector
                            (map (lambda (c)
                                   (case c
                                     [(wall water) #f]
                                     [(base plain) #t]))
                                 (vector->list b))))
                         (vector->list board)))])
           (let tloop ([targets targets])
             ;; Any of the targets == any of the starts?
             (let ([found (let loop ([new targets])
                            (cond
                              [(null? new) null]
                              [(member (car new) starts) (cons (car new) (loop (cdr new)))]
                              [else (loop (cdr new))]))])
               (if (null? found)
                   ;; Nothing found
                   (begin
                     ;; Mark targets as non-starts:
                     (for-each (lambda (t)
                                 (set-cell! b2 (car t) (cdr t) #f))
                               targets)
                     ;; Generate new targets:
                     (let ([new (let nloop ([targets targets])
                                  (if (null? targets)
                                      null
                                      (let ([x (caar targets)]
                                            [y (cdar targets)])
                                        (let ploop ([possibles (list (cons (sub1 x) y)
                                                                     (cons (add1 x) y)
                                                                     (cons x (add1 y))
                                                                     (cons x (sub1 y)))])
                                          (if (null? possibles)
                                              (nloop (cdr targets))
                                              (let ([x (caar possibles)]
                                                    [y (cdar possibles)])
                                                (cond
                                                  [(and (< -1 x width) (< -1 y height)
                                                        (get-cell b2 x y))
                                                   (set-cell! b2 x y #f)
                                                   (cons (car possibles) (ploop (cdr possibles)))]
                                                  [else (ploop (cdr possibles))])))))))])
                       (if (null? new)
                           ;; Give up
                           null
                           ;; Try again:
                           (tloop new))))
                   ;; Found possibilities:
                   found)))))
       
       (define (get-possible-moves rx ry)
         (let loop ([try '((e 1 . 0) (w -1 . 0) (s 0 . -1) (n 0 . 1))]
                    [m null][d null])
           (if (null? try)
               (values m d)
               (let ([x (+ rx (cadar try))]
                     [y (+ ry (cddar try))])
                 (if (and (< -1 x width) (< -1 y height)
                          (memq (get-cell board x y) '(plain base)))
                     (loop (cdr try)
                           (cons (caar try) m)
                           (cons (cons x y) d))
                     (loop (cdr try) m d))))))
                         
       (let run-loop ([robots robots0][packages packages0])
         (let* ([r (assoc me robots)]
                [rx (sub1 (bot-x r))]
                [ry (sub1 (bot-y r))]
                [packages-here (filter (lambda (p)
                                         (and (= rx (sub1 (pkg-x p))) (= ry (sub1 (pkg-y p)))
                                              (not (member (pkg-id p) (bot-packages r)))))
                                       packages)]
                [packages-can-lift (if (null? packages-here)
                                       null
                                       (let loop ([current-weight
                                                   (apply + (map (lambda (p) (pkg-weight (assoc p packages)))
                                                                 (bot-packages r)))]
                                                  [packages packages-here])
                                         (if (null? packages)
                                             null
                                             (let ([new-weight (+ current-weight (pkg-weight (car packages)))])
                                               (if (< new-weight (bot-max-lift r))
                                                   (cons (car packages) (loop new-weight (cdr packages)))
                                                   (loop current-weight (cdr packages)))))))]
                [packages-should-drop (filter (lambda (pid)
                                                (let ([p (assoc pid packages)])
                                                  (and (= rx (sub1 (pkg-dest-x p)))
                                                       (= ry (sub1 (pkg-dest-y p))))))
                                              (bot-packages r))])
           
           ;; ----------------------------------------
           ;; Base management
           ;; Forget bases that have no packages:
           (when (and (null? packages-here)
                      (member (cons rx ry) bases))
             (set! bases (remove (cons rx ry) bases)))
           ;; If we know about an unowned package, add its
           ;;  location to the list of possible "bases"
           (let ([owned-packages (apply append
                                        (map bot-packages robots))]
                 [bases (append bases possible-bases)])
             (for-each (lambda (p)
                         (when (not (member (pkg-id p) owned-packages))
                           (let ([px (sub1 (pkg-x p))]
                                 [py (sub1 (pkg-y p))])
                             (when (not (member (cons px py) bases))
                               (set! possible-bases (cons (cons px py) possible-bases))))))
                       packages))
           ;; No bases? Copy over possible bases:
           (when (null? bases)
             (set! bases possible-bases)
             (set! possible-bases null))
               
           ;; ----------------------------------------
           ;; Move decision
           (let ([m (cond
                      [(pair? packages-should-drop)
                       ;; Drop delivered packages:
                       `(drop ,@packages-should-drop)]
                      [(pair? packages-can-lift)
                       ;; Pick up as many packages as possible:
                       `(pick ,@(map pkg-id packages-can-lift))]
                      [(pair? (bot-packages r))
                       ;; Move towards a package dest, if any
                       (let-values ([(possible-moves possible-dests) (get-possible-moves rx ry)])
                         (let ([steps (find-steps-toward-targets possible-dests 
                                                                 (map (lambda (pid)
                                                                        (let ([p (assoc pid packages)])
                                                                          (cons (sub1 (pkg-dest-x p))
                                                                                (sub1 (pkg-dest-y p)))))
                                                                      (bot-packages r)))])
                           (match-up steps possible-moves possible-dests)))]
                      [else
                       ;; Move toward a base?
                       (let-values ([(possible-moves possible-dests) (get-possible-moves rx ry)])
                         ;; Walk towards bases
                         (let ([steps (find-steps-toward-targets possible-dests bases)])
                           ;; Map first step back to move
                           (match-up steps possible-moves possible-dests)))])])
             (let-values ([(new-robots new-packages) (move 1 session m me robots packages)])
               (run-loop new-robots new-packages)))))))))
