(unit/sig stepper:marks^
  (import [z : zodiac:system^]
          [e : zodiac:interface^]
          [cp : stepper:client-procs^]
          mzlib:function^)
  
  ; debug-key: this key will be used as a key for the continuation marks.
  (define-struct debug-key ())
  (define debug-key (make-debug-key))
  
  (define (extract-mark-list mark-set)
    (continuation-mark-set->list mark-set debug-key))

  (define (make-full-mark location label bindings)
    `(#%lambda () (#%list ,location (#%quote ,label) ,@(apply append bindings))))
  
  (define (make-cheap-mark location)
    location)
  
  (define (cheap-mark? mark)
    (z:zodiac? mark))
  
  (define (cheap-mark-source mark)
    mark)
  
  (define (mark-source mark)
    (if (cheap-mark? mark)
        (cheap-mark-source mark)
        (car (mark))))
  
  ;; extract-zodiac-locations : mark-set -> (listof zodiac)
  (define (extract-zodiac-locations mark-set)
    (map mark-source (extract-mark-list mark-set)))
    
  (define (mark-bindings mark)
    (letrec ([pair-off
              (lambda (lst)
                (cond [(null? lst) null]
                      [(null? (cdr lst)) (error 'mark-bindings 
                                                           "uneven number of vars and bindings")]
                      [else (cons (list (car lst) (cadr lst)) (pair-off (cddr lst)))]))])
      (pair-off (cddr (mark)))))
  
  (define (mark-label mark)
    (cadr (mark)))
  
  (define (mark-binding-value mark-binding)
    (car mark-binding))
  
  (define (mark-binding-binding mark-binding)
    (cadr mark-binding))

  (define (expose-mark mark)
    (let ([source (mark-source mark)]
          [label (mark-label mark)]
          [bindings (mark-bindings mark)])
      (list source
            label
            (map (lambda (binding)
                   (list (let ([lhs (mark-binding-binding binding)])
                           (if (z:binding? lhs) 
                               (z:binding-orig-name lhs)
                               lhs))
                         (mark-binding-value binding)))
                 bindings))))
  
  (define (display-mark mark)
    (let ([exposed (expose-mark mark)])
      (printf "source: ~a~n" (let ([read (cp:read-getter (car exposed))])
                               (and read
                                    (z:sexp->raw read))))
      (printf "label: ~a~n" (cadr exposed))
      (printf "bindings:~n")
      (for-each (lambda (binding-pair)
                  (printf " ~a : ~a~n" (car binding-pair) (cadr binding-pair)))
                (caddr exposed))))
  
  (define (lookup-binding mark-list binding)
    (if (null? mark-list)
        (error 'lookup-binding "variable not found in environment: ~a" binding)
        (let* ([bindings (mark-bindings (car mark-list))]
               [matches (filter (lambda (b)
                                  (eq? binding (mark-binding-binding b)))
                                bindings)])
          (cond [(null? matches)
                 (lookup-binding (cdr mark-list) binding)]
                [(= (length matches) 1)
                 (car matches)]
                [else 
                 (error 'lookup-binding "multiple bindings found for ~a" binding)]))))
  
  ; I'm not really sure this belongs here, but it's a convenient spot.
  (define ankle-wrap-enabled 
    (make-parameter #t (lambda (x) x))))
