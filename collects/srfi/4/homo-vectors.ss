(module homo-vectors mzscheme
  (require (lib "contract.ss")
           (lib "file.ss" "dynext"))
  
  (define (vector-fn-bundle filename)
    (let-values (((prim:vector->homo-vector prim:homo-vector->vector prim:homo-vector-length prim:homo-vector-ref
                                            prim:homo-vector-set! prim:homo-vector? prim:homo-vector+ prim:homo-vector- 
                                            prim:homo-vector* prim:homo-vector-norm prim:homo-vector-type 
                                            prim:make-homo-vector-uninitialized prim:make-homo-vector)
                  (load-extension (build-path (collection-path "srfi")
                                              "4"
                                              "compiled" 
                                              "native" 
                                              (system-library-subpath) 
                                              filename))))
      (values
       (lambda (v) ;; vector->homo-vector
         (let ((ok (real-vec? v)))
           (if ok (prim:vector->homo-vector v) (raise-type-error 'vector->homo-vector "vector of real numbers" v))))
       
       (lambda args ;; vector
         (if (andmap real? args)
             (prim:vector->homo-vector (apply vector args))
             (raise-type-error 'vector "list of real numbers" args)))
       
       (lambda (v)  ;; homo-vector->vector 
         (if (prim:homo-vector? v)
             (prim:homo-vector->vector v)
             (raise-type-error 'homo-vector->vector "homo-vector" v)))
       
       (lambda (v)  ;; homo-vector-length 
         (if (prim:homo-vector? v)
             (prim:homo-vector-length v)
             (raise-type-error 'homo-vector-length "homo-vector" v)))
       
       (lambda (v index)  ;; homo-vector-ref 
         (if (prim:homo-vector? v) 
             (if (and (integer? index) (>= index 0) (exact? index))
                 (if (< index (prim:homo-vector-length v))
                     (prim:homo-vector-ref v index)
                     (raise-mismatch-error 'homo-vector-ref 
                                           (format "index ~a out of range [~a, ~a] for homo-vector: "
                                                   index 0 (sub1 (prim:homo-vector-length v)))
                                           v))
                 (raise-type-error 'homo-vector-ref "non-negative exact integer" 1 v index))
             (raise-type-error 'homo-vector-ref "homo-vector" 0 v index)))
       
       (lambda (v index val)  ;; homo-vector-set! 
         (if (prim:homo-vector? v)
             (if (and (integer? index) (>= index 0) (exact? index))
                 (if (< index (prim:homo-vector-length v))
                     (if (real? val)
                         (prim:homo-vector-set! v index val)
                         (raise-type-error 'homo-vector-set! "real" 2 v index val))
                     (raise-mismatch-error 'homo-vector-set! 
                                           (format "index ~a out of range [~a, ~a] for homo-vector: "
                                                   index 0 (sub1 (prim:homo-vector-length v)))
                                           v))
                 (raise-type-error 'homo-vector-set! "non-negative exact integer" 1 v index val))
             (raise-type-error 'homo-vector-set! "homo-vector" 0 v index val)))
       
       prim:homo-vector?
       
       (lambda (v1 v2)  ;; homo-vector-sum
         (if (prim:homo-vector? v1)
             (if (prim:homo-vector? v2)
                 (if (= (prim:homo-vector-length v1)
                        (prim:homo-vector-length v2))
                     (prim:homo-vector+ v1 v2)
                     (raise-mismatch-error 'homo-vector+
                                           (format "all homo-vectors must have the same length; had lengths ~a and ~a." 
                                                   (prim:homo-vector-length v1)
                                                   (prim:homo-vector-length v2))
                                           ""))
                 (raise-type-error 'homo-vector+ "homo-vector" 0 v1 v2))
             (raise-type-error 'homo-vector+ "homo-vector" 1 v1 v2)))
       
       (lambda (v1 v2)  ;; homo-vector-difference
         (if (prim:homo-vector? v1)
             (if (prim:homo-vector? v2)
                 (if (= (prim:homo-vector-length v1)
                        (prim:homo-vector-length v2))
                     (prim:homo-vector- v1 v2)
                     (raise-mismatch-error 'homo-vector-
                                           (format "all homo-vectors must have the same length; had lengths ~a and ~a." 
                                                   (prim:homo-vector-length v1)
                                                   (prim:homo-vector-length v2))
                                                   ""))
                 (raise-type-error 'homo-vector-difference "homo-vector" 0 v1 v2))
             (raise-type-error 'homo-vector-difference "homo-vector" 1 v1 v2)))
       
       (lambda (d v)  ;; homo-vector-scale
         (if (prim:homo-vector? v)
             (if (real? d)
                 (prim:homo-vector* d v)
                 (raise-type-error 'homo-vector-scale "real" 0 d v))
             (raise-type-error 'homo-vector-scale "real" 1 d v)))
       
       
       (lambda (v)  ;; homo-vector-norm 
         (if (prim:homo-vector? v)
             (prim:homo-vector-norm v)
             (raise-type-error 'homo-vector-length "homo-vector" 0 v)))
       
       prim:homo-vector-type
       
       (case-lambda ((l) ;; make-homo-vector
                     (if (and (integer? l)
                              (>= l 0))
                         (prim:make-homo-vector-uninitialized l)
                         (raise-type-error 'make-homo-vector "non-negative integer" 0 l)))
                    ((l i) 
                     (if (and (integer? l)
                              (>= l 0))
                         (if (real? i)
                             (prim:make-homo-vector l i)
                             (raise-type-error 'make-homo-vector "real" 1 l i))
                         (raise-type-error 'make-homo-vector "non-negative integer" 0 l i))))
       
       (lambda (l) ;; list->homo-vector
         (let* ([new-vec (prim:make-homo-vector-uninitialized (length l))])
           (let loop ([i 0] [l l])
             (unless (null? l) 
               (if (real? (car l))
                   (prim:homo-vector-set! new-vec i (car l))
                   (raise-type-error 'list->homo-vector "(listof real?)" 0 l))
               (loop (+ i 1) (cdr l))))
           new-vec))
       
       (lambda (v) ;; homo-vector->list
         (if (prim:homo-vector? v)
             (let* ([vec-len (prim:homo-vector-length v)])
               (let loop ([i 0]) 
                 (if (= i vec-len)
                     null
                     (cons (prim:homo-vector-ref v i)
                           (loop (+ i 1))))))
             (raise-type-error 'homo-vector->length "homo-vector" 0 v))))))
  
  (define (make-filename type-symbol)
    (append-extension-suffix (format "homo-~a-vector-prims" (symbol->string type-symbol))))
  
  (define (real-vec? v)
    (and (vector? v)
         (andmap real? (vector->list v))))
  
  (define-syntax (make-vector-funs stx)
    (syntax-case stx ()
      ((_ type-symbol pred/length-assoc type-assoc)
       (let* ([type-string (symbol->string (syntax-e (syntax type-symbol)))]
              [name-maker (lambda (str) (string->symbol (format str type-string)))]
              [vec->homo (name-maker "vector->~avector")]
              [maker (name-maker "~avector")]
              [homo->vec (name-maker "~avector->vector")]  
              [length (name-maker "~avector-length")]
              [getter (name-maker "~avector-ref")]
              [setter (name-maker "~avector-set!")]
              [pred (name-maker "~avector?")]
              [sum (name-maker "~avector-sum")]
              [difference (name-maker "~avector-difference")]
              [scale (name-maker "~avector-scale")]
              [norm (name-maker "~avector-norm")]
              [type-idx (name-maker "~avector-type")]
              [functional-maker (name-maker "make-~avector")]
              [list->homo (name-maker "list->~avector")]
              [homo->list (name-maker "~avector->list")]
              ; warning! the order of the list below must match the definitions in the 'values' above. 
              [names (list vec->homo   
                           maker
                           homo->vec
                           length
                           getter
                           setter
                           pred
                           sum
                           difference
                           scale
                           norm
                           type-idx
                           functional-maker
                           list->homo
                           homo->list)])
         (with-syntax ([(name ...) names])
           #`(begin
               (provide name ...)
               
               (define-values (name ...)
                 (let ([filename (make-filename (quote type-symbol))])
                   (vector-fn-bundle filename)))
               (set! pred/length-assoc (cons (list #,pred #,length) pred/length-assoc))
               (set! type-assoc (cons (list #,type-string #,type-idx) type-assoc))))))))
  
  (define-syntax (make-all-vector-funs stx)
    (syntax-case stx ()
      [(_ type ...)
       (let ([pred/length-assoc-stx #`pred/length-assoc]
             [type-assoc-stx #`type-assoc])
         #`(begin 
             (define #,pred/length-assoc-stx null)
             (define #,type-assoc-stx null)
             (begin (make-vector-funs type #,pred/length-assoc-stx #,type-assoc-stx)
                    ...)
             (provide/contract [homo-vector-length (-> any? number?)])
             (define (homo-vector-length v)
               (let ([match (ormap (lambda (assoc-pair)
                                     (if ((car assoc-pair) v)
                                         assoc-pair
                                         #f))
                                   #,pred/length-assoc-stx)])
                 (if match
                     ((cadr match) v)
                     (error 'homo-vector-length "expects input of homo-vector, given: ~v" v))))
             
             (provide/contract [#,type-assoc-stx (listof (list/p string? number?))])))]))
  
  (make-all-vector-funs f64 f32 s32 s16 s8 u32 u16 u8))


