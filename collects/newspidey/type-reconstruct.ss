;; non-recursive rec bindings at the beginning of list
(define rec-bindings 'not-initialized)

(define *rec-bindings-rec-table* (make-hash-table))

(define *set-var-to-type-table* (make-hash-table))

(define (associate-set-var-and-type set-var type)
  (hash-table-put! *set-var-to-type-table* (Set-var-name set-var) type))

(define (lookup-type-from-set-var set-var)
  (hash-table-get *set-var-to-type-table* (Set-var-name set-var) (lambda () #f)))

(define (init-set-var-to-type)
  (for-each
   (lambda (set-var)
     (associate-set-var-and-type set-var (make-type-aux set-var)))
   (get-all-set-vars)))

(define (mk-type set-var)
  (let ([omega-set-var (lookup-type-from-set-var set-var)]
        [all-set-vars (get-all-set-vars)])
    (make-Type-Rec
     ;;(map list all-set-vars (map lookup-type-from-set-var all-set-vars))
     rec-bindings ;; already reduced
     omega-set-var))) ;; inline alpha

(define (set-var-upper-bounds set-var accum)
  (let* ([ubs (lookup-lo-and-filter Set-var? set-var)]
         [new-ubs (filter (lambda (ub)
                            (not (memq ub accum)))
                          ubs)]
         [new-accum (cons set-var (append new-ubs accum))])
    (remove-duplicates-eq
     (cons
      set-var
      (apply append
             (map (lambda (ub)
                    (set-var-upper-bounds ub new-accum))
                  new-ubs))))))

(define (remove-duplicates-eq l)
  ;;(printf "list: ~a~n" l)
  (if (null? l)
      '()
      (let ([l-car (car l)]
            [l-cdr (cdr l)])
        (if (memq l-car l-cdr)
            (remove-duplicates-eq l-cdr)
            (cons l-car (remove-duplicates-eq l-cdr))))))

(define (make-type-aux alpha)
  (let* ([lookup-filtered-set-exp
          (lambda (pred alpha)
            (types->type
             (lookup-hi-and-filter pred alpha)))]
         [omega-c
          (lookup-filtered-set-exp ;;(lambda (set-exp) (or (Const? set-exp)
                                   ;;                      (Listexp? set-exp)))
           Const?
           alpha)]
         [omega-cons
          (if (null? (lookup-hi-and-filter Token? alpha))
              *empty-type*
              (let ([omega-car (lookup-filtered-set-exp Set-var? (make-Car alpha))]
                    [omega-cdr (lookup-filtered-set-exp Set-var? (make-Cdr alpha))])
                (make-Type-Cons omega-car omega-cdr)))]
         [omega-arrow
          (let* ([labels (lookup-hi-and-filter Label? alpha)])
            (if (null? labels)
                *empty-type*
                (make-Type-Union
                 (let* ([arities (apply append (map (lambda (label)
                                                       (lookup-ars-from-label (Label-name label)))
                                                     labels))]
                        [deltas (set-var-upper-bounds alpha '())]
;                        [doms-omega (apply APPLY
;                                     append
;                                     (map
;                                      (lambda (delta)
;                                        (lookup-hi-and-filter
;                                         Set-var?
;                                         (make-Dom-interval (make-Interval 'star 'star) 0 delta)))
;                                      deltas))]
			)
                   ;;(printf "alpha: ~a~nlabels: ~a~ndeltas: ~a~n" alpha labels deltas)
                   (map
                    (lambda (arity)
                      (let* ([int (Arity-req arity)]
                             [n (Interval-lo int)]
                             [m (Interval-hi int)]
                             [max-j (if (eq? m 'omega) (add1 n) n)]
                             [omega-doms (let loop ([j 1])
                                           (if (> j max-j)
                                               '()
                                               (cons
                                                (types->type (remove-duplicates-eq
                                                 (apply
                                                  append
                                                  ;; doms-omega APPLY
                                                  (map
                                                   (lambda (delta)
                                                     (lookup-hi-and-filter
                                                      Set-var?
                                                      (make-Dom-interval int j delta)))
                                                   deltas))))
                                                (loop (add1 j)))))]
                             [omega-rng (lookup-filtered-set-exp Set-var? (make-Rng-arity arity alpha))])
                        ;;(printf "max-j: ~a~nomega-doms: ~a~n" max-j omega-doms)
                        (make-Type-Arrow omega-doms omega-rng)))
                    arities)))))])
    (make-Type-Union (list omega-c omega-cons omega-arrow))))
  
(define (types->type set-vars)
  (let ([len (length set-vars)])
    (cond
      [(= len 0) *empty-type*]
      [(= len 1) (car set-vars)]
      [else (make-Type-Union set-vars)])))

(define (type-reduce-rec-bindings)
  (let* ([bindings (hash-table-map *set-var-to-type-table*
                                   (lambda (name type)
                                     (make-Type-Binding
                                      (make-Set-var name)
                                      type)))])
(set! rec-bindings (let loop ([cur-bindings bindings]
                                  [accum '()])
                         (if (null? cur-bindings)
                             accum
                             (let* ([binding_i (car cur-bindings)]
                                    [set-var_i (Type-Binding-set-var binding_i)]
                                    [type_i (Type-Binding-type binding_i)])
                               (if (is-recursive-rec-binding? binding_i)
                                   (loop (cdr cur-bindings)
                                         (cons (make-Type-Binding
                                                set-var_i
                                                (type-reduce type_i))
                                               accum))
                                   (cons (make-Type-Binding
                                          set-var_i
                                          (type-reduce type_i))
                                         (loop (cdr cur-bindings)
                                               accum)))))))))

(define (is-recursive-rec-binding? binding)
  (let ([name (Set-var-name (Type-Binding-set-var binding))]
        [type (Type-Binding-type binding)])
    (hash-table-get *rec-bindings-rec-table* name
                    (lambda ()
                      (let ([answer (memq name (extract-set-vars type))])
                        (hash-table-put! *rec-bindings-rec-table* name answer)
                        answer)))))

(define (extract-set-vars type)
  ;;(printf "type: ~a~n" type)
  (let ([map-and-flatten (lambda (lst)
                           (apply append
                                  (map extract-set-vars lst)))])
    (cond
      [(Const? type) '()]
      [(Set-var? type) (list (Set-var-name type))]
      [(Type-Rec? type)
       (remove-duplicates-eq
        (apply append
               (map-and-flatten (map Type-Binding-type (Type-Rec-bindings type)))
               (extract-set-vars (Type-Rec-type type))))]
      [(Type-Arrow? type)
       (remove-duplicates-eq
        (append
         (map-and-flatten (Type-Arrow-doms type))
         (extract-set-vars (Type-Arrow-rng type))))]
      [(Type-Cons? type)
       (remove-duplicates-eq
        (append (extract-set-vars (Type-Cons-car type))
                (extract-set-vars (Type-Cons-cdr type))))]
      [(Type-Union? type)
       (remove-duplicates-eq
        (map-and-flatten (Type-Union-types type)))]
      [(Type-Empty? type) '()])))


(define (type-reduce type)
  ;;(printf "type-reduce: ~a~n" type)
  (cond
    [(Set-var? type) type]
    [(Type-Rec? type)
     (cond
       ;; rec already done when creating the rec-type in mk-type
       ;; rec-elim
       [(null? (Type-Rec-bindings type))
        (type-reduce (Type-Rec-type type))]
       ;; unfold
       [else (let* ([first-rec-binding (car (Type-Rec-bindings type))]
                    [rest-rec-bindings (cdr (Type-Rec-bindings type))]
                    [alpha (Set-var-name (Type-Binding-set-var first-rec-binding))]
                    [omega (Type-Binding-type first-rec-binding)]
                    [body-type (Type-Rec-type type)])
               (when (not (is-recursive-rec-binding? first-rec-binding))
                 (if (extract-set-vars body-type) ;; optimisation
                     (type-reduce (make-Type-Rec
                                   (map (lambda (binding)
                                          (make-Type-Binding
                                           (Type-Binding-set-var binding)
                                           (type-subst (Type-Binding-type binding) alpha omega)))
                                        rest-rec-bindings)
                                   (type-subst body-type alpha omega)))
                     (type-reduce body-type))))])]
    [(Type-Union? type)
     (let* ([types (Type-Union-types type)]
            ;; union
            [types-reduced (map type-reduce types)]
            ;; empty-filter
            [types-filtered (filter (lambda (type) (not (eq? type *empty-type*))) types-reduced)]
            ;; union-merger
            [types-flattened (let loop ([cur-types types-filtered])
                               (if (null? cur-types)
                                   '()
                                   (let ([cur-type (car cur-types)])
                                     (if (Type-Union? cur-type)
                                         (append (Type-Union-types cur-type)
                                                 (loop (cdr cur-types)))
                                         (cons cur-type
                                               (loop (cdr cur-types)))))))]
            [types-length (length types-flattened)])
       (cond
         [(= types-length 0) *empty-type*]
         [(= types-length 1) (car types-flattened)]
         [else (make-Type-Union types-flattened)]))]
    [(Type-Cons? type)
     (make-Type-Cons (type-reduce (Type-Cons-car type))
                     (type-reduce (Type-Cons-cdr type)))]
    [(Type-Arrow? type)
     (make-Type-Arrow (map type-reduce (Type-Arrow-doms type))
                      (type-reduce (Type-Arrow-rng type)))]
    [(Const? type) type]
    [(Type-Empty? type) type]))

;; alpha = symbol
(define (type-subst type alpha omega)
  (cond
    [(Type-Union? type)
     (make-Type-Union (map (lambda(type) (type-subst type alpha omega)) (Type-Union-types type)))]
    [(Type-Cons? type)
     (make-Type-Cons (type-subst (Type-Cons-car type) alpha omega)
                     (type-subst (Type-Cons-cdr type) alpha omega))]
    [(Type-Arrow? type)
     (make-Type-Arrow (map (lambda (type) (type-subst type alpha omega)) (Type-Arrow-doms type))
                      (type-subst (Type-Arrow-rng type) alpha omega))]
    [(Type-Rec? type)
     (make-Type-Rec (map (lambda (binding)
                           (let ([set-var (Type-Binding-set-var binding)]
                                 [type (Type-Binding-type binding)])
                             (make-Type-Binding set-var (type-subst type alpha omega))))
                         (Type-Rec-bindings type))
                    (type-subst (Type-Rec-type type) alpha omega))]
    [(Set-var? type)
     (if (symbol=? (Set-var-name type) alpha)
         omega
         type)]
    [(Const? type) type]
    [(Type-Empty? type) type]
    [else (error "type-subst: unknown type: ~a~n" type)]))
