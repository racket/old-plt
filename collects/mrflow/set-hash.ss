; sets implementation, using hash tables.
; - value equality based on eq? by default, uses equal? if given the 'equal flag
; - raises exn:set:value-not-found if value not in set when trying
;   to remove a value.
; - raise exn:set:duplicate-value by default when trying to add a value to a
;   set where it already exists
; - strange things might happen if you use set-union, set-intersection,
;   or set-difference with two sets that don't use the same comparaison
;   function: you might end up with duplicate values in some sets.

(module set-hash mzscheme
  (require
   (lib "etc.ss") ; for opt-lambda
   "set-exn.ss" ; no prefix so we can re-provide
   (prefix argexn: "arg-mismatch-exn.ss")
   )
  
  (provide
   exn:set?             ; value -> boolean
   (struct exn:set:value-not-found (set value)) ; set value
   (struct exn:set:duplicate-value (set value)) ; set value
   make-set             ; (opt 'equal) -> set
   set-reset            ; set -> set
   set?                 ; value -> boolean
   set-set              ; set value (opt boolean) -> set
   set-in?              ; set value -> boolean
   set-remove           ; set value (opt boolean) -> set
   set-cardinality      ; set -> exact-non-negative-integer
   set-empty?           ; set -> boolean
   set-copy             ; set -> set
   set-map              ; set (value -> value) -> (listof value)
   set-fold             ; set (value value -> value) value -> value
   set-for-each         ; set (value -> value) -> set
   set-for-each!        ; set (value -> value) -> set
   set-filter           ; set (value -> boolean) (opt (union 'new 'same)) -> set
   set-union            ; set set (opt (union 'new 'first 'second)) -> set
   set-intersection     ; set set (opt (union 'new 'first 'second)) -> set
   set-difference       ; set set (opt (union 'new 'first 'second)) -> set
   )
  
  ; table = (hashtableof value value)
  (define-struct set (cardinality table))
  
  ; we'll need the real one later, since we set! make-set below
  (define real-make-set make-set)
  
  ; (opt 'equal) -> set
  ; we test the optional argument ourselves to preserve data abstraction even in the
  ; presence of an exception
  (set! make-set
        (case-lambda
          [() (real-make-set 0 (make-hash-table))]
          [(flag) (if (eq? flag 'equal)
                      (real-make-set 0 (make-hash-table 'equal))
                      (argexn:raise-arg-mismatch-exn "make-set" 'equal flag))]))
  
  ; set -> set
  (define (set-reset set)
    (set-set-table! set (make-hash-table))
    (set-set-cardinality! set 0)
    set)
  
  ; value -> boolean
  ; set? comes from the structure definition
  
  ; set value (opt boolean) -> set
  (define set-set
    (let ([dummy (gensym)])
      (opt-lambda (set value (exn? #t))
        (if (set-in? set value)
            (when exn?
              (raise-duplicate-value-exn "set-set" set value))
            (begin
              (set-set-cardinality! set (add1 (set-cardinality set)))
              (hash-table-put! (set-table set) value dummy)))
        set)))
  
  ; set value -> boolean
  (define set-in? 
    (let ([sym (gensym)])
      (lambda (set value)
        (not (eq? sym (hash-table-get (set-table set) value (lambda () sym)))))))
  
  ; set value (opt boolean) -> set
  (define set-remove
    (opt-lambda (set value (exn? #t))
      (if (set-in? set value)
          (begin
            (set-set-cardinality! set (sub1 (set-cardinality set)))
            (hash-table-remove! (set-table set) value))
          (when exn?
            (raise-value-not-found-exn "set-remove" set value)))
      set))
  
  ; set -> exact-non-negative-integer
  ; set-cardinality comes from the structure definition
  
  ; set -> boolean
  (define (set-empty? set)
    (= 0 (set-cardinality set)))
  
  ; set -> set
  (define (set-copy set)
    (let ([new-table (make-hash-table)])
      (hash-table-for-each (set-table set)
                           (lambda (key value)
                             (hash-table-put! new-table key value)))
      (real-make-set (set-cardinality set)
                     new-table)))
  
  ; set (value -> value) -> (listof value)
  (define (set-map set f)
    (let ([binary-f (lambda (value dummy)
                      (f value))])
      (hash-table-map (set-table set) binary-f)))
  
  ; set (value value -> value) value -> value
  (define (set-fold set f acc)
    (let ([acc acc])
      (hash-table-for-each (set-table set)
                           (lambda (value dummy)
                             (set! acc (f value acc))))
      acc))
  
  ; set (value -> value) -> set
  (define (set-for-each set f)
    (let ([binary-f (lambda (value dummy)
                      (f value))])
      (hash-table-for-each (set-table set) binary-f))
    set)
  
  ; set (value -> value) -> set
  ; it's up to the user to make sure f is injective. Otherwise we might end up with
  ; a smaller set and the wrong cardinality.
  (define (set-for-each! set f)
    (let ([new-table (make-hash-table)])
      (hash-table-for-each (set-table set)
                           (lambda (value dummy)
                             (hash-table-put! new-table (f value) dummy)))
      (set-set-table! set new-table))
    set)
  
  ; set (value -> boolean) (opt (union 'new 'same)) -> set
  (define set-filter
    (let (; set (value -> boolean) -> set
          [filter-into-new-set
           (lambda (set tester)
             (let ([table (make-hash-table)]
                   [count 0])
               (hash-table-for-each (set-table set)
                                    (lambda (value dummy)
                                      (when (tester value)
                                        (hash-table-put! table value dummy)
                                        (set! count (add1 count)))))
               (real-make-set count table)))])
      (opt-lambda (set tester (which-set 'new))
        (let ([new-set (filter-into-new-set set tester)])
          (case which-set
            [(new) new-set]
            [(same)
             (set-set-table! set (set-table new-set))
             (set-set-cardinality! set (set-cardinality new-set))
             set]
            [else (argexn:raise-arg-mismatch-exn "set-filter" '(union new same) which-set)])))))
  
  ; set set (opt (union 'new 'first 'second)) -> set
  (define set-union
    (let (; set set -> set
          [union-second-set-into-first
           (lambda (set1 set2)
             (let ([table (set-table set1)]
                   [count (set-cardinality set1)])
               (hash-table-for-each (set-table set2)
                                    (lambda (value dummy)
                                      (unless (set-in? set1 value)
                                        (hash-table-put! table value dummy)
                                        (set! count (add1 count)))))
               (set-set-cardinality! set1 count))
             set1)])
      (opt-lambda (set1 set2 (which-set 'new))
        (case which-set
          [(new)
           ; copying is presumably faster than testing
           (if (< (set-cardinality set1) (set-cardinality set2))
               (union-second-set-into-first (set-copy set2) set1)
               (union-second-set-into-first (set-copy set1) set2))]
          [(first) (union-second-set-into-first set1 set2)]
          [(second) (union-second-set-into-first set2 set1)]
          [else (argexn:raise-arg-mismatch-exn "set-union" '(union new first second) which-set)]))))
  
  ; set set (opt (union 'new 'first 'second)) -> set
  (define set-intersection
    (let (; set set -> set
          [intersect-into-new-set
           (lambda (set1 set2)
             (let ([table (make-hash-table)]
                   [count 0])
               (hash-table-for-each (set-table set1)
                                    (lambda (value dummy)
                                      (when (set-in? set2 value)
                                        (hash-table-put! table value dummy)
                                        (set! count (add1 count)))))
               (real-make-set count table)))])
      (opt-lambda (set1 set2 (which-set 'new))
        (let ([new-set
               (if (< (set-cardinality set1) (set-cardinality set2))
                   (intersect-into-new-set set1 set2)
                   (intersect-into-new-set set2 set1))])
          (case which-set
            [(new) new-set]
            [(first)
             (set-set-table! set1 (set-table new-set))
             (set-set-cardinality! set1 (set-cardinality new-set))
             set1]
            [(second)
             (set-set-table! set2 (set-table new-set))
             (set-set-cardinality! set2 (set-cardinality new-set))
             set2]
            [else (argexn:raise-arg-mismatch-exn "set-intersection" '(union new first second) which-set)])))))
  
  ; set set (opt (union 'new 'first 'second)) -> set
  (define set-difference
    (let (; set set -> set
          [difference-into-new-set
           (lambda (set1 set2)
             (let ([table (make-hash-table)]
                   [count 0])
               (hash-table-for-each (set-table set1)
                                    (lambda (value dummy)
                                      (unless (set-in? set2 value)
                                        (hash-table-put! table value dummy)
                                        (set! count (add1 count)))))
               (real-make-set count table)))])
      (opt-lambda (set1 set2 (which-set 'new))
        (let ([new-set (difference-into-new-set set1 set2)])
          (case which-set
            [(new) new-set]
            [(first) 
             (set-set-table! set1 (set-table new-set))
             (set-set-cardinality! set1 (set-cardinality new-set))
             set1]
            [(second)
             (set-set-table! set2 (set-table new-set))
             (set-set-cardinality! set2 (set-cardinality new-set))
             set2]
            [else (argexn:raise-arg-mismatch-exn "set-difference" '(union new first second) which-set)])))))
  
  )
