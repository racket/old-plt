(module marks mzscheme
  (require (lib "unitsig.ss")
	   (lib "zodiac-sig.ss" "syntax")
	   "sig.ss")

  (provide marks@)

  (define marks@
(unit/sig stepper:marks^
  (import [z : zodiac^]
          [cp : stepper:client-procs^])
  
  ; debug-key: this key will be used as a key for the continuation marks.
  (define-struct debug-key ())
  (define debug-key (make-debug-key))
  
  (define (extract-mark-list mark-set)
    (continuation-mark-set->list mark-set debug-key))
  
  (define label-list null)
  (define (get-label-num sym)
    (let loop ([l-list label-list] [count 0])
      (if (null? l-list)
          (begin
            (set! label-list (append label-list (list sym)))
            count)
          (if (eq? sym (car l-list))
              count
              (loop (cdr l-list) (+ count 1))))))

  (equal?
   (list (get-label-num 'foo)
         (get-label-num 'bar)
         (get-label-num 'baz)
         (get-label-num 'bar)
         (get-label-num 'foo)
         (get-label-num 'baz)
         (get-label-num 'quux))
   '(0 1 2 1 0 2 3))
  
  (define (make-full-mark location label bindings)
    `(#%lambda () (#%list ,location ,(get-label-num label) ,@(apply append bindings))))
  
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
    (list-ref label-list (cadr (mark))))
  
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
  
  (define (binding-matches mark binding)
    (let ([matches
           (filter (lambda (b)
                     (eq? binding (mark-binding-binding b)))
                   (mark-bindings mark))])
      (if (> (length matches) 1)
          (error 'lookup-binding "multiple bindings found for ~a" binding)
          matches)))
  
  (define (lookup-binding mark-list binding)
    (if (null? mark-list)
        (error 'lookup-binding "variable not found in environment: ~a" binding)
        (let ([matches (binding-matches (car mark-list) binding)])
          (cond [(null? matches)
                 (lookup-binding (cdr mark-list) binding)]
                [else
                 (car matches)]))))
  
  (define (lookup-binding-list mark-list binding)
    (apply append (map (lambda (x) (binding-matches x binding)) mark-list)))
  
  ; I'm not really sure this belongs here, but it's a convenient spot.
  (define ankle-wrap-enabled 
    (make-parameter #t (lambda (x) x))))
