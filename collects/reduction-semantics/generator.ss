(module generator mzscheme
  (require "private/matcher.ss")

  (provide lang->generator-table
           for-each-generated
           for-each-generated/size
           generate-all/size)
  
  (define cache-limit 10)
  
  (define (force-cache! min-size max-size cache gen)
    (when (<= min-size max-size)
      (if (vector-ref cache min-size)
          (force-cache! (add1 min-size) max-size cache gen)
          (begin
            ;; Init cache slots between min-size and max-size
            (let iloop ([i min-size])
              (vector-set! cache i null)
              (unless (= i max-size) 
                (iloop (add1 i))))
            ;; Generate all from min-size to max-size:
            (let loop ([gen gen])
              (gen min-size
                   max-size
                   (lambda (v size leftover-max gen-next)
                     (vector-set! cache size 
                                  (cons v (vector-ref cache size)))
                     (loop gen-next))
                   void))))))
  
  (define (xcache-small gen)
    (let ([cache (make-vector (add1 cache-limit) #f)])
      (lambda (min-size max-size next-k done-k)
        (let loop ([i min-size][next-k next-k])
          (cond
            [(i . > . max-size) (done-k)]
            [(i . <= . cache-limit)
             (force-cache! i (min cache-limit max-size) cache gen)
             (let iloop ([l (vector-ref cache i)][next-k next-k])
               (if (null? l)
                   (loop (add1 i) next-k)
                   (next-k (car l)
                           i
                           (- max-size i)
                           (lambda (s xs next-k done-again-k)
                             (iloop (cdr l) next-k)))))]
            [else (gen i max-size next-k done-k)])))))
    
  (define (cache-small gen) gen)
    
  (define (lang->generator-table lang skip-kws)
    (let ([nts (compiled-lang-lang lang)]
          [nt-map (make-hash-table)])
      (for-each (lambda (nt) (hash-table-put! nt-map (nt-name nt) 
                                              (cons (lambda () 0)
                                                    (lambda () +inf.0))))
                nts)
      (let ([gens (make-hash-table)]
            [atomic-alts (lambda (l)
                           (values
                            (lambda (min-size max-size next-k done-k)
                              (if (<= min-size 1 max-size)
                                  (let loop ([l l][next-k next-k])
                                    (if (null? l)
                                        (done-k)
                                        (next-k (car l)
                                                1
                                                (sub1 max-size)
                                                (lambda (s xs next-k done-again-k)
                                                  (loop (cdr l) next-k)))))
                                  (done-k)))
                            (lambda () 1)
                            (lambda () 1)))]
            [to-do nts])
        (letrec ([make-gen/get-size
                  (lambda (p)
                    (cond
                      [(hash-table-get nt-map p (lambda () #f))
                       => (lambda (get-sizes)
                            (values
                             (lambda (min-size max-size next-k done-k)
                               ((hash-table-get gens p) min-size max-size next-k done-k))
                             (car get-sizes)
                             (cdr get-sizes)))]
                      [(eq? 'number p) (atomic-alts '(0 1))]
                      [(eq? 'string p) (atomic-alts '("s1" "s2"))]
                      [(eq? 'any p) (atomic-alts '(0 "s1" '(1 2)))]
                      [(or (eq? 'variable p)
                           (and (pair? p)
                                (eq? (car p) 'variable-except)))
                       (atomic-alts '(x y))]
                      [(symbol? p) (if (memq p skip-kws)
                                       (values
                                        (lambda (min-size max-size next-k done-k)
                                          (done-k))
                                        (lambda () +inf.0)
                                        (lambda () -1))
                                       (atomic-alts (list p)))]
                      [(null? p) (atomic-alts (list null))]
                      [(and (pair? p)
                            (or (not (pair? (cdr p)))
                                (not (eq? '... (cadr p)))))
                       (make-pair-gen/get-size p cons)]
                      [(and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)))
                       (make-pair-gen/get-size (cons (star (car p)) (cddr p)) append)]
                      [else
                       (error 'make-gen "unrecognized pattern: ~e" p)]))]
                 [make-pair-gen/get-size
                  (lambda (p combiner)
                    (let*-values ([(first first-min-size first-max-size) 
                                   (make-gen/get-size (car p))]
                                  [(rest rest-min-size rest-max-size) 
                                   (make-gen/get-size (cdr p))]
                                  [(this-min-size) (lambda ()
                                                     (+ 1
                                                        (first-min-size)
                                                        (rest-min-size)))]
                                  [(this-max-size) (lambda ()
                                                     (+ 1
                                                        (first-max-size)
                                                        (rest-max-size)))])
                      (values
                       (cache-small
                        (lambda (min-size max-size next-k done-k)
                          (if (or (max-size . < . (this-min-size))
                                  (min-size . > . (this-max-size)))
                              (done-k)
                              (let floop ([first first][next-k next-k])
                                (first 
                                 0
                                 (sub1 max-size)
                                 (lambda (elem result-size rest-max-size next-first)
                                   (let rloop ([rest rest][next-k next-k])
                                    (rest (max 0 (- min-size result-size 1))
                                          rest-max-size
                                          (lambda (rest rest-size r-max-size next-rest)
                                            (next-k 
                                             (combiner elem rest)
                                             (+ result-size rest-size 1)
                                             r-max-size
                                             (lambda (ns xs next-k done-again-k)
                                               (rloop next-rest next-k))))
                                          (lambda ()
                                            (floop next-first next-k)))))
                                 done-k)))))
                       this-min-size
                       this-max-size)))]
                 [star (lambda (p)
                         (let ([n (gensym)])
                           (hash-table-put! nt-map n (cons (lambda () 0)
                                                           (lambda () +inf.0)))
                           (set! to-do (cons (make-nt 
                                              n 
                                              (list (make-rhs '())
                                                    (make-rhs (cons p n))))
                                             to-do))
                           n))])
          (let to-do-loop ([nts (reverse to-do)])
            (set! to-do null)
            (for-each (lambda (nt)
                        (hash-table-put!
                         gens
                         (nt-name nt)
                         (let* ([gens+sizes
                                 (map (lambda (rhs)
                                        (let-values ([(gen get-min-size get-max-size)
                                                      (make-gen/get-size 
                                                       (rhs-pattern rhs))])
                                          (cons gen (cons get-min-size get-max-size))))
                                      (nt-rhs nt))]
                                [get-min-size
                                 (let ([get-min-sizes (map cadr gens+sizes)])
                                   (let ([v #f])
                                     (lambda ()
                                       (unless v
                                         (set! v (apply min (map (lambda (gs) (gs))
                                                                 get-min-sizes))))
                                       v)))]
                                [get-max-size
                                 (let ([get-max-sizes (map cddr gens+sizes)])
                                   (let ([v #f])
                                     (lambda ()
                                       (unless v
                                         (set! v (apply max (map (lambda (gs) (gs))
                                                                 get-max-sizes))))
                                       v)))])
                           (hash-table-put! nt-map (nt-name nt)
                                            (cons get-min-size get-max-size))
                           (cache-small
                            (lambda (min-size max-size next-k done-k)
                              (if (or (max-size . < . (get-min-size))
                                      (min-size . > . (get-max-size)))
                                  (done-k)
                                  (let loop ([l (map car gens+sizes)][next-k next-k])
                                    (if (null? l)
                                        (done-k)
                                        (let iloop ([alt-next (car l)]
                                                    [next-k next-k])
                                          (alt-next
                                           min-size
                                           max-size
                                           (lambda (alt a-size l-max-size alt-next)
                                             (next-k
                                              alt
                                              a-size
                                              l-max-size
                                              (lambda (ns xs next-k done-again-k)
                                                (iloop alt-next next-k))))
                                           (lambda ()
                                             (loop (cdr l) next-k)))))))))))))
                      nts)
            (unless (null? to-do)
              (to-do-loop to-do))))
        gens)))
  
  (define (for-each-generated/size proc gens min-size max-size nonterm)
    (let ([gen (hash-table-get gens nonterm)])
      (let loop ([gen gen])
        (gen
         min-size
         max-size
         (lambda (val z1 z2 gen-next)
           (proc val)
           (loop gen-next))
         void))))

  (define (generate-all/size gens size nonterm)
    (let ([l null])
      (for-each-generated/size 
       (lambda (x) (set! l (cons x l)))
       gens size nonterm)
      l))
  
  (define (for-each-generated proc gens nonterm)
    (let loop ([i 0])
      (for-each-generated/size proc gens i nonterm)
      (loop (add1 i)))))
