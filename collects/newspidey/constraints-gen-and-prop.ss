(unit/sig newspidey:constraints-gen-and-prop^
  (import [zodiac : zodiac:system^]
          [mzlib : mzlib:core-flat^]
          [setexp : newspidey:datadef-setexp^]
          [cft : newspidey:constraints-from-type^])

;; use our own gensym for now
;; because real gensym generates uninterned symbols
;; that are not eq? to symbols entered in the REPL
;; Use "k" instead of "g" to help debugging (it's never too late...)
(define counter 0)
(define (interned-gensym)
  (set! counter (add1 counter))
  (string->symbol (string-append "k" (number->string counter))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constraints

;; lo = set-exp, hi = set-exp
(define-struct constraint (lo hi))

;; (listof constraint)
(define *the-constraints* '())
;; (hash-table-of symbol (list (listof set-exp) (listof set-exp)))
;; coded set-exp -> lists of lower and upper bounds
(define *constraint-table* (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; common stuff

(define *Nullexp* (setexp:make-Const '()))
(define *Voidexp* (setexp:make-Const (void)))
(define *pair-token* (setexp:make-Token 'pair))

;; -> Set-var
;; generate fresh set variable, add to table
(define (gen-set-var)
  (let ([set-var (setexp:make-Set-var (interned-gensym))])
    (add-set-var set-var)
    set-var))

;; (listof Set-var)
(define *all-set-vars* '())

;; Set-var -> void
(define (add-set-var set-var)
  (set! *all-set-vars* (cons set-var *all-set-vars*)))

;; -> (listof Set-var)
(define (get-all-set-vars)
  *all-set-vars*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arity to intervals and interval to arities tables
;;
;; given an arity, gives all the intervals that match,
;; and vice versa, for both dom and rng
;; note: all the keys need to be encoded, because they are often
;; recreated from scratch, and hash-table-get uses eq?

;; (hash-table-of symbol (listof Dom-Arity))
;; encoded Dom-Arity pos and set-var -> (listof Dom-Arity)
(define *dom-interval->dom-arity-table* (make-hash-table))

;; (hash-table-of symbol (listof Dom-Interval))
;; encoded Dom-Interval pos and set-var -> (listof Dom-Interval)
(define *dom-arity->dom-interval-table* (make-hash-table))

;; (hash-table-of symbol (listof Rng-Arity))
;; encoded Rng-Arity set-var -> (listof Rng-Arity)
(define *rng-interval->rng-arity-table* (make-hash-table))

;; (hash-table-of symbol (listof Rng-Interval))
;; encoded Rng-Interval set-var -> (listof Rng-Interval)
(define *rng-arity->rng-interval-table* (make-hash-table))

;; number Set-var -> symbol
;; Dom encoder
(define (encode-dom n alpha)
  (string->symbol 
   (string-append "dom_" (number->string n) 
		  "(" (symbol->string (setexp:Set-var-name alpha)) ")")))

;; Dom-Interval -> void
;; add dom-interval to dom-interval list for corresponding dom-arity
(define (relate-dom-int-to-ar dom-int)
  (let* ([n (setexp:Dom-interval-pos dom-int)]
         [alpha (setexp:Dom-interval-set-var dom-int)]
         [dom-encoding (encode-dom n alpha)]
         [dom-int-list (lookup-dom-ar dom-encoding)])
    (hash-table-put! *dom-arity->dom-interval-table* dom-encoding (cons dom-int dom-int-list))))

;; sym -> (listof Dom-Interval)
(define (lookup-dom-ar sym)
  (hash-table-get *dom-arity->dom-interval-table* sym (lambda () '())))

;; Dom-Arity -> void
;; add dom-arity to dom-arity list for corresponding dom-interval
(define (relate-dom-ar-to-int dom-ar)
  (let* ([n (setexp:Dom-arity-pos dom-ar)]
         [alpha (setexp:Dom-arity-set-var dom-ar)]
         [dom-encoding (encode-dom n alpha)]
         [dom-ar-list (lookup-dom-int dom-encoding)])
    (hash-table-put! *dom-interval->dom-arity-table* dom-encoding (cons dom-ar dom-ar-list))))

;; sym -> (listof Dom-Arity)
(define (lookup-dom-int sym)
  (hash-table-get *dom-interval->dom-arity-table* sym (lambda () '())))

;; Set-var -> symbol
;; Rng encoder
(define (encode-rng alpha)
  (string->symbol (string-append "rng_(" (symbol->string (setexp:Set-var-name alpha)) ")")))

;; Rng-Interval -> void
;; add rng-interval to rng-interval list for corresponding rng-arity
(define (relate-rng-int-to-ar rng-int)
  (let* ([alpha (setexp:Rng-interval-set-var rng-int)]
         [rng-encoding (encode-rng alpha)]
         [rng-int-list (lookup-rng-ar rng-encoding)])
    (hash-table-put! *rng-arity->rng-interval-table* rng-encoding (cons rng-int rng-int-list))))

;; symbol -> (listof Rng-Interval)
(define (lookup-rng-ar sym)
  (hash-table-get *rng-arity->rng-interval-table* sym (lambda () '())))

;; Rng-Arity -> void
;; add rng-arity to rng-arity list for corresponding rng-interval
(define (relate-rng-ar-to-int rng-ar)
  (let* ([alpha (setexp:Rng-arity-set-var rng-ar)]
         [rng-encoding (encode-rng alpha)]
         [rng-ar-list (lookup-rng-int rng-encoding)])
    (hash-table-put! *rng-interval->rng-arity-table* rng-encoding (cons rng-ar rng-ar-list))))

;; sym -> (listof Rng-Arity)
(define (lookup-rng-int sym)
  (hash-table-get *rng-interval->rng-arity-table* sym (lambda () '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set-var and label to arities tables
;;
;; works the same way as in the previous section

;; (hash-table-of symbol (listof Rng-Arity))
;; encoded set-var -> (listof Rng-Arity)
(define *set-var->rng-arities-table* (make-hash-table))
;; (hash-table-of symbol (listof Dom-Arity))
;; encoded set-var -> (listof Dom-Arity)
(define *set-var->dom-arities-table* (make-hash-table))

;; Rng-Arity -> void
;; add rng-arity to rng-arity list for correspoding set-var
(define (relate-set-var-to-rng-ar rng-ar)
  (let* ([beta (setexp:Rng-arity-set-var rng-ar)]
         [rng-ars (hash-table-get *set-var->rng-arities-table* beta (lambda () '()))])
    (hash-table-put! *set-var->rng-arities-table* beta (cons rng-ar rng-ars))))

;; symbol -> (listof Rng-Arity)
(define (lookup-rng-ars-from-set-var set-var)
  (hash-table-get *set-var->rng-arities-table* set-var (lambda () '())))

;; Dom-Arity -> void
;; add dom-arity to dom-arity list for corresponding set-var
(define (relate-set-var-to-dom-ar dom-ar)
  (let* ([alpha (setexp:Dom-arity-set-var dom-ar)]
         [dom-ars (hash-table-get *set-var->dom-arities-table* alpha (lambda () '()))])
    (hash-table-put! *set-var->dom-arities-table* alpha (cons dom-ar dom-ars))))

;; symbol -> (listof Dom-Arity)
(define (lookup-dom-ars-from-set-var set-var)
  (hash-table-get *set-var->dom-arities-table* set-var (lambda () '())))

;; (hash-table-of symbol (listof Arity))
;; encoded label -> (listof Arity)
(define *label-to-ars-table* (make-hash-table))

;; Label Arity -> void
;; add arity to arity list for label
(define (associate-label-with-ars label ar)
  (let ([ars (hash-table-get *label-to-ars-table* label (lambda () '()))])
    (hash-table-put! *label-to-ars-table* label (cons ar ars))))

;; symbol -> (listof Arity)
(define (lookup-ars-from-label label)
  (hash-table-get *label-to-ars-table* label (lambda () '())))

;; (hash-table-of symbol (listof Set-var))
;; encoded Dom-Interval interval, pos, and alpha -> (listof Set-var)
(define *dom-to-set-var* (make-hash-table))

;; Dom-Interval Set-var -> void
;; add set-var to set-var list for corresponding dom-interval
(define (relate-set-var-to-dom dom-int set-var)
  (let* ([pos (setexp:Dom-interval-pos dom-int)]
         [alpha (setexp:Dom-interval-set-var dom-int)]
         [int (setexp:Dom-interval-interval dom-int)]
         [code (encode-dom-int int alpha pos)]
         [cur-list (lookup-set-vars-from-dom-int int alpha pos)])
    (hash-table-put! *dom-to-set-var* code (cons set-var cur-list))))

;; Interval Set-var number -> (listof Set-var)
(define (lookup-set-vars-from-dom-int int alpha pos)
  (let ([code (encode-dom-int int alpha pos)])
    (hash-table-get *dom-to-set-var* code (lambda () '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set-var and label to term tables
;;
;; Note: setvars are represented as symbols, because they are sometimes reconstructed from scratch
;; (i.e. eq? would not work on the structures, and hash tables use eq ?)
;; (hash-table-of symbol zodiac:parsed) ;; XXX this should be a list of zodiac:parsed, because of top level redefinitions
(define *set-var-to-term-table* (make-hash-table))
;; (hash-table-of zodiac:parsed symbol) note: not used in practice
(define *term-to-set-var-table* (make-hash-table))
;; (hash-table-of Label zodiac:parsed))
(define *label-to-lambda-table* (make-hash-table))
;; (hash-table-of zodiac:parsed Label) note: not used in practice
(define *lambda-to-label-table* (make-hash-table))
;; (hash-table-of Label symbol)
(define *label-to-set-var-table* (make-hash-table))
;; (hash-table-of symbol Label)
(define *set-var-to-label-table* (make-hash-table))

;; (listof (list number symbol))
;; need this for DrScheme, to find the set-var that
;; corresponds to the terms starting at the given location/offset
;; ("terms" plural because of macros...) Could use a hash table...
(define *location-list* '())

;; symbol number -> void
;; add (list number symbol) to the list of locations, in sorted order
(define (add-to-location-list sym offset)
;  (letrec (;; symbol number (listof (list number symbol)) -> (listof (list number symbol))
;           [sort-loc (lambda (sym offset l)
;                       (if (null? l)
;                           (list (list offset sym))
;                           ;; in case of equality, add after the existing
;                           ;; one => top term appears first for a given
;                           ;; offset ?
;                           (if (< offset (caar l))
;                               (cons (list offset sym)
;                                     l)
;                               (cons (car l)
;                                     (sort-loc sym offset (cdr l))))))])
    (set! *location-list* (cons (list offset sym) *location-list*)))

;; symbol zodiac:parsed -> void
(define (associate-set-var-and-term alpha term)
  (hash-table-put! *set-var-to-term-table* alpha term)
  (add-to-location-list alpha (zodiac:location-offset (zodiac:zodiac-start term)))
  (hash-table-put! *term-to-set-var-table* term alpha))

;; symbol -> zodiac:parsed
(define (lookup-term-from-set-var set-var)
  (hash-table-get *set-var-to-term-table* set-var (lambda () #f)))

;; zodiac:parsed -> symbol
;; not used...
(define (lookup-set-var-from-term term)
  (hash-table-get *term-to-set-var-table* term (lambda () #f)))

;; Label zodiac:parsed -> void
(define (associate-label-and-lambda label term)
  (hash-table-put! *label-to-lambda-table* label term)
  (hash-table-put! *lambda-to-label-table* term label))

;; Label -> zodiac:parsed
(define (lookup-lambda-from-label label)
  (hash-table-get *label-to-lambda-table* label (lambda () #f)))

;; zodiac:parsed -> Label
;; not used...
(define (lookup-label-from-lambda ar)
  (hash-table-get *lambda-to-label-table* ar (lambda () #f)))

;; Label symbol -> void
(define (associate-label-and-set-var label setvar)
  ;;(printf "LABEL: ~a~nSETVAR: ~a~n" label setvar)
  (hash-table-put! *label-to-set-var-table* label setvar)
  (hash-table-put! *set-var-to-label-table* setvar label))
  
;; Label -> symbol
(define (lookup-set-var-from-label label)
  (hash-table-get *label-to-set-var-table* label (lambda () #f)))

;; symbol -> Label
;; not used...
(define (lookup-label-from-set-var setvar)
  (hash-table-get *set-var-to-label-table* setvar (lambda () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc encoders for hash table keys

;; Interval Set-var number -> symbol
(define (encode-dom-int int alpha pos)
  (string->symbol (string-append (number->string (setexp:Interval-lo int))
                                 ":"
                                 (let ([hi (setexp:Interval-hi int)])
                                   (if (number? hi)
                                       (number->string hi)
                                       (symbol->string hi)))
                                 ":"
                                 (symbol->string (setexp:Set-var-name alpha))
                                 ":"
                                 (number->string pos))))

;; (union bool num string char '() sym) -> string
(define (scalar->string v)
  (cond 
   [(boolean? v)
    (string-append "bool:" (if v "#t" "#f"))]
   [(number? v)
    (string-append "num:" (number->string v))]
   [(string? v)
    (string-append "str:" v)]
   [(char? v)
    (string-append "char:" (string v))]
   [(symbol? v)
    (string-append "sym:" (symbol->string v))]
   [(null? v) "empty-list"]
   [(void? v) "void"]
   [else (error 'scalar->string "unknown value: ~a" v)]))

;; (nonempty-listof string) -> string
(define (make-hash-string . strings)
  (mzlib:foldr (lambda (s1 s2)
                 (if (string? s2)
                     (string-append s1 "$$" s2)
                     s1))
               'dummy strings))
  
;; Interval -> string
(define (encode-interval interval)
  (string-append "interval:["
                 (let ([lo (setexp:Interval-lo interval)])
                   (if (number? lo)
                       (number->string lo)
                       (symbol->string lo)))
                 ","
                 (let ([hi (setexp:Interval-hi interval)])
                   (if (number? hi)
                       (number->string hi)
                       (symbol->string hi)))
                 "]"))

;; Arity -> string
(define (encode-arity arity)
  (apply string-append
         "arity:"
         (encode-interval (setexp:Arity-req arity))
         ":"
         (map encode-interval (setexp:Arity-proh arity))))

; Set-exp -> string
(define (set-exp->string val)
  ;;(printf "VAL: ~a~n" val)
  (let ([build-hash-from-sym
         (lambda (s sym)
           (make-hash-string s (symbol->string sym)))])
    (cond
      [(setexp:Const? val)
       (make-hash-string 
        "const"
        (scalar->string (setexp:Const-val val)))]
      [(setexp:Set-var? val)
       (build-hash-from-sym "var" (setexp:Set-var-name val))]
      [(setexp:Token? val)
       (build-hash-from-sym "token" (setexp:Token-name val))]
      [(setexp:Label? val)
       (build-hash-from-sym "label" (setexp:Label-name val))]
      [(setexp:Dom-arity? val)
       (make-hash-string
        "dom-arity" (encode-arity (setexp:Dom-arity-arity val)) (number->string (setexp:Dom-arity-pos val))
        (symbol->string (setexp:Set-var-name (setexp:Dom-arity-set-var val))))]
      [(setexp:Dom-interval? val)
       (make-hash-string
        "dom-interval" (encode-interval (setexp:Dom-interval-interval val)) (number->string (setexp:Dom-interval-pos val))
        (number->string (setexp:Dom-interval-n val))
        (symbol->string (setexp:Set-var-name (setexp:Dom-interval-set-var val))))]
      [(setexp:Rng-arity? val)
       (build-hash-from-sym (string-append "rng-arity" (encode-arity (setexp:Rng-arity-arity val)))
                            (setexp:Set-var-name (setexp:Rng-arity-set-var val)))]
      [(setexp:Rng-interval? val)
       (build-hash-from-sym (string-append "rng-interval" (encode-interval (setexp:Rng-interval-interval val))
                                           (number->string (setexp:Rng-interval-n val)))
                            (setexp:Set-var-name (setexp:Rng-interval-set-var val)))]
      [(setexp:Car? val)
       (build-hash-from-sym "car" (setexp:Set-var-name (setexp:Car-set-var val)))]
      [(setexp:Cdr? val)
       (build-hash-from-sym "cdr" (setexp:Set-var-name (setexp:Cdr-set-var val)))]
      [(setexp:Struct? val)
       (apply make-hash-string
        "struct:" (symbol->string (setexp:Struct-name val))
        (map setexp:Set-var-name (setexp:Struct-fields val)))]
      [else (error 'set-exp->string "unknown set expression: ~a" val)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constraint hash table manipulation

;; Set-exp -> (list (listof Set-exp) (listof Set-exp))
;; encodes set-exp and returns list of lower and upper bounds for given set-expr
(define (lookup-in-constraint-table set-exp)
    (lookup-coded-in-constraint-table (string->symbol (set-exp->string set-exp))))

;; ((list (listof Set-exp) (listof Set-exp)) -> (listof Set-exp)) -> 
;;   Set-exp -> (listof Set-exp)
;; returns list of lower bounds or list of upper bounds, depending
;; on accessor
(define (make-lookup-in-constraint-table accessor)
  (lambda (set-exp)
    (accessor (lookup-in-constraint-table set-exp))))

;; Set-exp -> (listof Set-exp)
;; returns list of lower bounds (i.e. look up for set-exp
;; in constrains where it appears as an upper bound)
(define lookup-in-hi-constraint-table
  (make-lookup-in-constraint-table car))

(define *empty-pairs-thunk* (lambda () '(() ()))) 

;; sym -> (list (listof Set-exp) (listof Set-exp))
;; returns list of lower and upper bounds for given encoded set-expr
(define (lookup-coded-in-constraint-table coded-set-exp)
    (hash-table-get *constraint-table* coded-set-exp
                    *empty-pairs-thunk*))

;; sym (listof Set-exp) (listof Set-exp) -> void
;; update lists of lower and upper bounds for given encoded set-expr
(define (set-bounds! coded-set-exp los his)
  (hash-table-put! *constraint-table*
                   coded-set-exp
                   (list los his)))

;; Set-exp ((listof Set-exp) -> (listof Set-exp)) ((listof Set-exp) -> (listof Set-exp)) -> void
;; add new bound for set-exp. The new bound to add is wrapped inside lo-fun or hi-fun
;; (one is the identity, the other one just adds the bound to the list of bounds)
(define (add-new-bound! set-exp lo-fun hi-fun)
  ;;(printf "set exp: ~a~n" set-exp)
  (let* ([coded-set-exp (string->symbol (set-exp->string set-exp))]
         [all-bounds 
          (lookup-coded-in-constraint-table coded-set-exp)]
         [old-los (car all-bounds)]
         [old-his (cadr all-bounds)])
    (set-bounds! coded-set-exp 
                 (lo-fun old-los) (hi-fun old-his)))) 

;; Set-exp Set-exp -> void
(define (add-new-upper-bound! set-exp hi)
  (add-new-bound! set-exp
                  (lambda (lo) lo)
                  (lambda (old-his)
                    (cons hi old-his))))

;; Set-exp Set-exp -> void
(define (add-new-lower-bound! set-exp lo)
  (add-new-bound! set-exp
                  (lambda (old-los)
                    (cons lo old-los))
                  (lambda (hi) hi)))

;; Set-exp Set-exp -> (union #f (listof Set-exp))
;; could just as well have used lo-constraint-table
(define (exists-constraint? lo hi)
 (member lo (lookup-in-hi-constraint-table hi)))

;; Constraint -> boolean
(define (add-constraint constraint)
  (let ([lo (constraint-lo constraint)]
        [hi (constraint-hi constraint)])
    (if (exists-constraint? lo hi)
        #f
        (begin
          (add-new-lower-bound! hi lo)
          (add-new-upper-bound! lo hi)
          (set! *the-constraints*
                (cons constraint
                      *the-constraints*))
          #t))))

;; Set-exp Set-exp -> boolean
;; add constraints and update all the hash tables 
(define (add-constraint-and-update-tables lo hi)
  ;;(printf "lo: ~a~nhi: ~a~n" lo hi)
  (let ([result (add-constraint (make-constraint lo hi))])
    (when result
      (cond
        [(setexp:Dom-interval? lo)
         (relate-dom-int-to-ar lo)]
        [(setexp:Dom-arity? lo)
         (relate-dom-ar-to-int lo)
         (relate-set-var-to-dom-ar lo)]
        [(setexp:Rng-interval? lo)
         (relate-rng-int-to-ar lo)]
        [(setexp:Rng-arity? lo)
         (relate-rng-ar-to-int lo)
         (relate-set-var-to-rng-ar lo)]
        [else (void)])
      (cond
        [(setexp:Dom-interval? hi)
         (relate-dom-int-to-ar hi)
         (when (setexp:Set-var? lo)
           (relate-set-var-to-dom hi lo))]
        [(setexp:Dom-arity? hi)
         (relate-dom-ar-to-int hi)
         (relate-set-var-to-dom-ar hi)]
        [(setexp:Rng-interval? hi)
         (relate-rng-int-to-ar hi)]
        [(setexp:Rng-arity? hi)
         (relate-rng-ar-to-int hi)
         (relate-set-var-to-rng-ar hi)]
        [else (void)]))
    result))
  
;; Set-var Set-var boolean -> boolean
;; flag determines which param is the lower bound...
(define (add-constraint-with-bounds lo hi flag)
  (if flag
      (add-constraint-and-update-tables lo hi)
      (add-constraint-and-update-tables hi lo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; environment

;; (listof (symbol Set-var)) symbol -> (union Set-var #f)
(define (lookup-in-env Gamma x)
  (let ([binding (assq x Gamma)])
    (if binding
        (cadr binding)
        #f)))

;; -> Label
(define (create-label)
  (setexp:make-Label (interned-gensym)))

;; (listof (symbol Set-var)) (listof symbol) (listof Set-var) -> (listof (symbol Set-var))
(define (spidey-extend-env Gamma xs alphas)
  ;;(printf "xs: ~a~nGamma: ~a~nalphs: ~a~n" xs Gamma alphas)
  (append (map list xs alphas)
          Gamma))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; top level environment
  
;; symbol -> Set-var
;; keeps track of name<->set-var association for top-level names
(define get-top-level-var 
  (let ([top-level-vars (make-hash-table)])
    (lambda (var)
      (hash-table-get top-level-vars var
                      (lambda ()
                        (let ([set-var (gen-set-var)])
                          (hash-table-put! top-level-vars var set-var)
                          set-var))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constraint generation

(define *whole-interval* (setexp:make-Interval 0 'omega))

;(define (add-constraints-for-apply alpha) APPLY
;  (let ([alpha1 (gen-set-var)]
;        [alpha2 (gen-set-var)]
;        [alpha3 (gen-set-var)])
;    (add-constraint-with-bounds (setexp:make-Dom-arity (setexp:make-Arity (setexp:make-Interval 2 2) '()) 1 alpha)
;                                alpha1
;                                #t)
;    (add-constraint-with-bounds (setexp:make-Dom-arity (setexp:make-Arity (setexp:make-Interval 2 2) '()) 2 alpha)
;                                alpha2
;                                #t)
;    (add-constraint-with-bounds alpha2
;                                (setexp:make-Dom-interval (setexp:make-Interval 'star 'star) 0 alpha1)
;                                #t)
;    (add-constraint-with-bounds (setexp:make-Rng-interval (setexp:make-Interval 'star 'star) alpha1)
;                                alpha3
;                                #t)
;    (add-constraint-with-bounds alpha3
;                                (setexp:make-Rng-arity (setexp:make-Arity (setexp:make-Interval 2 2) '()) alpha)
;                                #t)))

;; (listof (symbol Set-var)) zodiac:parsed -> Set-var
;; walk zodiac parse tree and generate constraints
(define (derive-top-term-constraints Gamma term)
  (let ([alpha (gen-set-var)])
    (associate-set-var-and-term (setexp:Set-var-name alpha) term)
    (cond
      [(zodiac:quote-form? term)
       (add-constraint-with-bounds (setexp:make-Const (zodiac:read-object (zodiac:quote-form-expr term))) alpha #t)]
      ;; note: a lambda-varref is also a lexical-varref, and a lambda-binding is also a lexical-binding.
      ;; so what works for let-bound variables here also works for lambda-bound variables.
      [(zodiac:lexical-varref? term)
       (let* ([name (zodiac:binding-orig-name (zodiac:bound-varref-binding term))]
              [set-var (lookup-in-env Gamma name)])
         ;;(printf "set-var: ~a~nGamma: ~a~nterm: ~a~n" set-var Gamma (zodiac:binding-orig-name (zodiac:bound-varref-binding term)))
         (add-constraint-with-bounds set-var alpha #t))]
      [(zodiac:define-values-form? term)
       (if (zodiac:struct-form? (zodiac:define-values-form-val term))
           (let* ([struct (zodiac:define-values-form-val term)]
                  [struct-name (zodiac:symbol-orig-name (zodiac:struct-form-type struct))]
                  [struct-name-string (symbol->string struct-name)]
                  [field-names (map zodiac:symbol-orig-name (zodiac:struct-form-fields struct))]
                  [n (length field-names)]
                  [alphas-fields (mzlib:build-list n (lambda (n) (gen-set-var)))]
                  [constructor-name (string->symbol (string-append "make-" struct-name-string))]
                  [alpha-constructor (get-top-level-var constructor-name)]
                  [constructor-label (create-label)]
                  [predicate-name (string->symbol (string-append struct-name-string "?"))]
                  [alpha-predicate (get-top-level-var predicate-name)]
                  [predicate-label (create-label)]
                  [selector-names (map (lambda (sym)
                                         (string->symbol (string-append struct-name-string "-" (symbol->string sym))))
                                       field-names)]
                  [alphas-selectors (map get-top-level-var selector-names)]
                  [selectors-labels (mzlib:build-list n (lambda (n) (create-label)))]
                  [mutator-names (map (lambda (sym)
                                        (string->symbol (string-append "set-" struct-name-string "-"
                                                                       (symbol->string sym) "!")))
                                      field-names)]
                  [alphas-mutators (map get-top-level-var mutator-names)]
                  [mutators-labels (mzlib:build-list n (lambda (n) (create-label)))]
                  [beta-constructor (gen-set-var)]
                  [beta-predicate (gen-set-var)]
                  [betas-mutators (mzlib:build-list n (lambda (n) (gen-set-var)))]
                  [arity-n (setexp:make-Arity (setexp:make-Interval n n) '())]
                  [arity-1 (setexp:make-Arity (setexp:make-Interval 1 1) '())]
                  [arity-2 (setexp:make-Arity (setexp:make-Interval 2 2) '())])
             ;; constructor
             (add-constraint-with-bounds constructor-label alpha-constructor #t)
             (let loop ([i 1]
                        [alphas-fields alphas-fields])
               (when (<= i n)
                 (add-constraint-with-bounds (setexp:make-Dom-arity arity-n i alpha-constructor) (car alphas-fields) #t)
                 (loop (add1 i) (cdr alphas-fields))))
             (add-constraint-with-bounds (setexp:make-Struct struct-name alphas-fields) beta-constructor #t)
             (add-constraint-with-bounds beta-constructor (setexp:make-Rng-arity arity-n alpha-constructor) #t)
             (associate-set-var-and-term (setexp:Set-var-name alpha-constructor) term)
             (associate-label-and-lambda constructor-label term)
             (associate-label-and-set-var constructor-label (setexp:Set-var-name alpha-constructor))
             (associate-label-with-ars (setexp:Label-name constructor-label) arity-n)
             ;; predicate
             ;; no constraint on domain, since it accepts top
             (add-constraint-with-bounds predicate-label alpha-predicate #t)
             (add-constraint-with-bounds (setexp:make-Const #t) beta-predicate #t)
             (add-constraint-with-bounds (setexp:make-Const #f) beta-predicate #t)
             (add-constraint-with-bounds beta-predicate (setexp:make-Rng-arity arity-1 alpha-predicate) #t)
             (associate-set-var-and-term (setexp:Set-var-name alpha-predicate) term)
             (associate-label-and-lambda predicate-label term)
             (associate-label-and-set-var predicate-label (setexp:Set-var-name alpha-predicate))
             (associate-label-with-ars (setexp:Label-name predicate-label) arity-1)
             ;; selectors
             ;; no constraint on domains, but final debug phase should check that argument is actually a
             ;; structure of the right type. XXX
             (for-each (lambda (alpha-field alpha-selector selector-label)
                         (add-constraint-with-bounds selector-label alpha-selector #t)
                         (add-constraint-with-bounds alpha-field (setexp:make-Rng-arity arity-1 alpha-selector) #t)
                         (associate-set-var-and-term (setexp:Set-var-name alpha-selector) term)
                         (associate-label-and-lambda selector-label term)
                         (associate-label-and-set-var selector-label (setexp:Set-var-name alpha-selector))
                         (associate-label-with-ars (setexp:Label-name selector-label) arity-1))
                       alphas-fields alphas-selectors selectors-labels)
             ;; mutators
             ;; no constraint on first argument, but final debug phase should check that argument is actually a
             ;; structure of the right type. XXX
             (for-each (lambda (alpha-mutator alpha-field beta-mutator mutator-label)
                         (add-constraint-with-bounds mutator-label alpha-mutator #t)
                         (add-constraint-with-bounds (setexp:make-Dom-arity arity-2 2 alpha-mutator) alpha-field #t)
                         (add-constraint-with-bounds *Voidexp* beta-mutator #t)
                         (add-constraint-with-bounds beta-mutator (setexp:make-Rng-arity arity-2 alpha-mutator) #t)
                         (associate-set-var-and-term (setexp:Set-var-name alpha-mutator) term)
                         (associate-label-and-lambda mutator-label term)
                         (associate-label-and-set-var mutator-label (setexp:Set-var-name alpha-mutator))
                         (associate-label-with-ars (setexp:Label-name mutator-label) arity-2))
                       alphas-mutators alphas-fields betas-mutators mutators-labels))
           (if (= 1 (length (zodiac:define-values-form-vars term)))
               (let* ([var (car (zodiac:define-values-form-vars term))]
                      [top-level-var (get-top-level-var (zodiac:varref-var var))]
                      [rhs-var (derive-top-term-constraints Gamma (zodiac:define-values-form-val term))])
                 (associate-set-var-and-term (setexp:Set-var-name top-level-var) var)
                 (add-constraint-with-bounds rhs-var top-level-var #t))
               (error 'derive-top-term-constraints "define-values forms may only contain a single var")))
       (add-constraint-with-bounds *Voidexp* alpha #t)]
      [(zodiac:top-level-varref/bind/unit? term)
       ;; XXX we should have a function is-primitive? and do all the primitive stuff inside add-constraints-from-type type
       (let* ([name (zodiac:varref-var term)]
              [type (cft:lookup-prim-type name)]
              [label (cft:lookup-prim-label name)]) ;; TOPLEVEL
         (if type ;;(or type (mzlib:symbol=? name 'apply)) APPLY
             ;; primitive
             (let (;;[prim-setvar (get-top-level-var name)]
                   [label (create-label)]
                   )
               ;; XXX
               ;;(set! alpha prim-setvar)
               (associate-set-var-and-term (setexp:Set-var-name alpha) term)
               (associate-label-and-lambda label term)
               (associate-label-and-set-var label (setexp:Set-var-name alpha))
               (add-constraint-with-bounds label alpha #t)
;               (if (mzlib:symbol=? name 'apply) APPLY
;                   (add-constraints-for-apply alpha)
               ;; XXX This will miserably fail as soon as we come accross a primitive that doesn't have an
               ;; arrow type (or an arrow type inside a scheme type). Does such a thing exist ? The current
               ;; Mrspidey considers nil and true/false as primitives...
               (associate-label-with-ars (setexp:Label-name label) (cft:get-arity type))
               (cft:add-constraints-from-type type alpha #t (make-hash-table))) ;;) APPLY
               
;               ;; TOPLEVEL
;               ;; primitives should always be associated with the same set-var, so the type for the primitive
;               ;; is computed correctly
;               (set! alpha (get-top-level-var name))
;               ;; The primitive's setvar is associated to the primitive's label, and the generation of constraints
;               ;; for the primitive is all done in the init-prim function.
;               ;; XXX label->term is overwritten each time we see the primitive, but term->label is ok
;               ;; XXX this creates a bug if a primitive has an arity error, because it doesn't have any
;               ;; term actually associated with it. This problem should disappear when the "doesn't underline
;               ;; the right thing" problem disappears... Maybe...
;               (associate-label-and-lambda (cft:lookup-prim-label name) term)
;               (when (mzlib:symbol=? name 'apply) APPLY
;                 (add-constraints-for-apply alpha)
;                 ) APPLY
;               ) TOPLEVEL
             
             ;; variable, XXX should test somewhere at the end whether it's bound
             (add-constraint-with-bounds (get-top-level-var name) alpha #t)
             ))]
      [(zodiac:case-lambda-form? term)
       (let* (;;[xs-l (map (lambda (l) (map zodiac:binding-orig-name (zodiac:arglist-vars l))) (zodiac:case-lambda-form-args term))]
              [xs-l (zodiac:case-lambda-form-args term)]
              [xs-l-sym (map (lambda (l) (map zodiac:binding-orig-name (zodiac:arglist-vars l))) xs-l)]
              [body-l (zodiac:case-lambda-form-bodies term)]
              [label (create-label)]
              [indices (map length xs-l-sym)]
              ;;[bar (printf "indices: ~a~n" indices)]
              [intervals (map (lambda (arg-list len)
                                (cond
                                 [(zodiac:sym-arglist? arg-list) *whole-interval*]
                                 [(zodiac:list-arglist? arg-list) (setexp:make-Interval len len)]
                                 [(zodiac:ilist-arglist? arg-list) (setexp:make-Interval (sub1 len) 'omega)]))
                              xs-l indices)]
              [arities (let loop ([loc-int intervals]
                                  [previous '()])
                         (if (null? loc-int)
                             '()
                             (let ([cur-int (car loc-int)])
                               (cons (setexp:make-Arity cur-int previous)
                                     (loop (cdr loc-int)
                                           (cons cur-int previous))))))]
              ;;[_ (printf "arities: ~a~n" arities)]
              [alphas-l (map (lambda (index) (mzlib:build-list index (lambda (_) (gen-set-var)))) indices)]
              ;;[foo (printf "indices: ~a~n" indices)]
              [betas (map (lambda (xs body alphas)
;                            (map (lambda (alpha term) (associate-set-var-and-term (setexp:Set-var-name alpha) term))
;                                 alphas xs)
                            (derive-top-term-constraints (spidey-extend-env Gamma xs alphas) body))
                          xs-l-sym body-l alphas-l)])
         (for-each (lambda (xs alphas)
                     (map (lambda (alpha term) (associate-set-var-and-term (setexp:Set-var-name alpha) term))
                          alphas (zodiac:arglist-vars xs)))
                   xs-l alphas-l)
         ;;(printf "betas: ~a~n" betas)
         (associate-label-and-lambda label term)
         (associate-label-and-set-var label (setexp:Set-var-name alpha))
         (add-constraint-with-bounds label alpha #t)
         (for-each (lambda (alphas arity index beta)
                     (let loop ([j 1]
                                [loc-alphas alphas])
                       (when (<= j index)
                         (add-constraint-with-bounds (setexp:make-Dom-arity arity j alpha) (car loc-alphas) #t)
                         (loop (add1 j) (cdr loc-alphas))))
                     (add-constraint-with-bounds beta (setexp:make-Rng-arity arity alpha) #t)
                     (associate-label-with-ars (setexp:Label-name label) arity))
                   alphas-l arities indices betas)
         ;;(printf "ARS: ~a~n" (lookup-ars-from-label (setexp:Label-name label)))
         )]
      [(zodiac:app? term)
       (let* ([fun (zodiac:app-fun term)]
              [args (zodiac:app-args term)]
              [len (length args)]
              [beta0 (derive-top-term-constraints Gamma fun)]
              [betas (map (lambda (arg) (derive-top-term-constraints Gamma arg)) args)]
              [alphas (mzlib:build-list (add1 len) (lambda (n) (gen-set-var)))]
              ;;[rhos (mzlib:build-list len (lambda (n) (gen-set-var)))] APPLY
	      )
         (add-constraint-with-bounds (setexp:make-Rng-interval (setexp:make-Interval len len) len beta0) alpha #t)
         (let loop ([i 0])
           (when (<= i len)
             (add-constraint-with-bounds (setexp:make-Rng-interval (setexp:make-Interval i 'omega) len beta0) alpha #t)
             (loop (add1 i))))

;         (add-constraint-with-bounds (setexp:make-Dom-arity (setexp:make-Arity (setexp:make-Interval 'star 'star) '()) APPLY
;                                                     0
;                                                     alpha)
;                                     (car rhos)
;                                     #t)
;         (let loop ([i 1]
;                    [loc-alphas alphas]
;                    [loc-rhos rhos])
;           (if (< i len)
;             (let ([alpha_i (car loc-alphas)]
;                   [rho_i (car loc-rhos)])
;               (add-constraint-with-bounds (setexp:make-Car rho_i)
;                                           alpha_i
;                                           #t)
;               (add-constraint-with-bounds (setexp:make-Cdr rho_i)
;                                           (cadr loc-rhos)
;                                           #t)
;               (loop (add1 i) (cdr loc-alphas) (cdr loc-rhos)))
;             (let ([alpha_n (car loc-alphas)]
;                   [alpha_n+1 (cadr loc-alphas)]
;                   [rho_n (car loc-rhos)])
;               (add-constraint-with-bounds (setexp:make-Car rho_n)
;                                           alpha_n
;                                           #t)
;               (add-constraint-with-bounds (setexp:make-Cdr rho_n)
;                                           alpha_n+1
;                                           #t))))
         (let loop ([i 1]
                    [loc-betas betas]
                    [loc-alphas alphas])
           (if (<= i len)
               (let ([beta_i (car loc-betas)]
                     [alpha_i (car loc-alphas)])
                 (add-constraint-with-bounds beta_i
                                             (setexp:make-Dom-interval (setexp:make-Interval len len) i len beta0)
                                             #t)
                 (add-constraint-with-bounds beta_i
                                             (setexp:make-Car alpha_i)
                                             #t)
                 (add-constraint-with-bounds (cadr loc-alphas)
                                             (setexp:make-Cdr alpha_i)
                                             #t)
                 (add-constraint-with-bounds alpha_i
                                             (setexp:make-Dom-interval (setexp:make-Interval (sub1 i) 'omega) i len beta0)
                                             #t)
                 (add-constraint-with-bounds *pair-token*
                                             alpha_i
                                             #t)
                 (loop (add1 i) (cdr loc-betas) (cdr loc-alphas)))
               (let ([alpha_n+1 (car loc-alphas)])
                 (add-constraint-with-bounds *Nullexp*
                                             alpha_n+1
                                             #t)
                 (add-constraint-with-bounds alpha_n+1
                                             (setexp:make-Dom-interval (setexp:make-Interval len 'omega) (add1 len) len beta0)
                                             #t))))
         (let loop-i ([i 0])
           (when (<= i len)
             (let loop-j ([j 1]
                          [loc-betas betas])
               (when (<= j i)
                 (add-constraint-with-bounds (car loc-betas)
                                             (setexp:make-Dom-interval (setexp:make-Interval i 'omega) j len beta0)
                                             #t)
                 (loop-j (add1 j) (cdr loc-betas))))
             (loop-i (add1 i)))))]
      [(zodiac:if-form? term)
       (let ([test (zodiac:if-form-test term)]
             [then (zodiac:if-form-then term)]
             [else (zodiac:if-form-else term)])
         (if #f ;; (and (zodiac:app? test)
;                  ;; it's an app
;                  (let ([fun (zodiac:app-fun test)]
;                        [args (zodiac:app-args test)])
;                    (and (zodiac:top-level-varref/bind/unit? fun)
;                         ;; and the app's function is a top level variable
;                         (let* ([name (zodiac:varref-var term)]
;                                [type (cft:lookup-prim-type name)])
;                           (and type ;; it's a primitive
;                                (predicate? name) ;; It's a predicate
;             
;
;                                XXX what happens if user redefines a predicate ?
;                                
;               (predicate? (zodiac:app-fun test)
;                   
;                         (zodiac:top-level-varref/bind/unit? term)
;       (let* ([name (zodiac:varref-var term)]
;              [type (cft:lookup-prim-type name)]
;              [label (cft:lookup-prim-label name)])
;         (associate-label-and-lambda label term)
;         (associate-label-and-set-var label ?)
;         (if type ;;(or type (mzlib:symbol=? name 'apply)) APPLY
;             ;; primitive  
;                           
;                           
;        
             (void)
             (let ([test-alpha (derive-top-term-constraints Gamma test)]
                   [then-alpha (derive-top-term-constraints Gamma then)]
                   [else-alpha (derive-top-term-constraints Gamma else)])
               (add-constraint-with-bounds then-alpha alpha #t)
               (add-constraint-with-bounds else-alpha alpha #t))))]
      [(zodiac:let-values-form? term)
       (let* ([vars (map (lambda (binding-list)
                           (if (not (= (length binding-list) 1))
                               (error 'derive-top-term-constraints "let-values not supported")
                               (car binding-list)))
                         (zodiac:let-values-form-vars term))]
              [vars-names (map zodiac:binding-orig-name vars)]
              [alphas-vars (map (lambda (_) (gen-set-var)) vars-names)]
              [vals (zodiac:let-values-form-vals term)]
              [alphas-vals (map (lambda (t) (derive-top-term-constraints Gamma t)) vals)]
              [body (zodiac:let-values-form-body term)]
              [alpha-body (derive-top-term-constraints (spidey-extend-env Gamma vars-names alphas-vars) body)])
         (for-each (lambda (alpha-var alpha-val var)
                     (add-constraint-with-bounds alpha-val alpha-var #t)
                     (associate-set-var-and-term (setexp:Set-var-name alpha-var) var))
                   alphas-vars alphas-vals vars)
         (add-constraint-with-bounds alpha-body alpha #t))]
      [else (error 'derive-top-term-constraints "unknown term ~a~n" term)]
      )
    alpha))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc

;; Set-exp -> boolean
(define (constant-set-exp? set-exp)
  (or (setexp:Const? set-exp) (setexp:Label? set-exp) (setexp:Token? set-exp) (setexp:Struct? set-exp)))

;; Set-exp -> boolean
(define (selector? set-exp)
  (or (setexp:Car? set-exp) (setexp:Cdr? set-exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; satisfaction

;; Interval Interval -> boolean
;; is first interval in second ?
(define (satisfies-int int1 int2)
  (let ([n (setexp:Interval-lo int1)]
        [m (setexp:Interval-hi int1)]
        [p (setexp:Interval-lo int2)]
        [q (setexp:Interval-hi int2)])
    ;;(printf "n: ~a m: ~a p: ~a q: ~a~n" n m p q)
    (or ;;(and (symbol? n) (mzlib:symbol=? n 'star)) APPLY
	(and (andmap number? (list n m p q))
             (= n m p q))
        (and (= n p)
             (andmap symbol? (list m q))
             (andmap (lambda (sym) (mzlib:symbol=? sym 'omega)) (list m q))))))

;; number Interval -> boolean
;; is n in interval ?
(define (in-interval n int)
  (and (>= n (setexp:Interval-lo int))
       (let ([upper (setexp:Interval-hi int)])
         (or (eq? upper 'omega)
             (<= n upper)))))

;; Interval number Arity -> boolean
;; does interval satisfy the arity ?
;; - interval should satisfy requisite interval
;; - interval should not satisfy prohibited intervals
;; - supplied number of args should be in requisite interval
(define (satisfies int num-of-args ar)
  (let ([ar-req (setexp:Arity-req ar)]
        [ar-proh (setexp:Arity-proh ar)])
    (and (satisfies-int int ar-req)
         (in-interval num-of-args ar-req)
         (not (ormap (lambda (ar-proh-int) (in-interval num-of-args ar-proh-int)) ar-proh)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constraint solving

;; (Set-exp -> boolean) Set-esp -> (listof Set-exp)
;; looks for all the constraints that contain the given set-exp
;; as an upper bound, and returns all the lower bounds that
;; satisfy "pred"
(define (lookup-lo-and-filter pred set-exp)
  ;;(printf "SETEXP: ~a~n" set-exp)
  (mzlib:filter pred (cadr (lookup-in-constraint-table set-exp))))

;; (Set-exp -> boolean) Set-exp -> (listof Set-exp)
;; looks for all the constraints that contain the given set-exp
;; as a lower bound, and returns all the upper bounds that
;; satisfy "pred"
(define (lookup-hi-and-filter pred set-exp)
  (mzlib:filter pred (car (lookup-in-constraint-table set-exp))))

;; Set-exp (listof Set-exp) (Set-exp -> Set-exp) -> void
;; creates one new constrain for each Set-exp in "his", using
;; "lo" as lower bound and a new upper bound created on the
;; fly using make-selector. See the rng-prop rule and others...
(define (fire-covariant-selector-rule lo his make-selector)
  ;;(printf "covariant: ~a ~a~n" lo his)
  (for-each
   (lambda (hi)
     (add-one-constraint-and-propagate lo (make-selector hi)))
   his))

;; (listof Set-exp) Set-exp (Set-exp -> Set-exp) -> void
;; creates one new constrain for each Set-exp in "los", using
;; "hi" as upper bound and a new lower bound created on the
;; fly using make-selector. See the dom-prop rule.
(define (fire-contravariant-selector-rule los hi make-selector)
  (for-each
   (lambda (lo)
     (add-one-constraint-and-propagate (make-selector lo) hi))
   los))

;; Set-exp Set-exp -> void
;; add one constraint and re-tries to fire propagation rules
(define (add-one-constraint-and-propagate lo hi)
  (if (add-constraint-with-bounds lo hi #t)
      (begin
        ;;(printf "add-constraint~n")
        (propagate-one-constraint (make-constraint lo hi)))
      ;;(printf "not add-constraint~n")
      ))

;; -> void
;; propagates each constraint in turn
(define (propagate-constraints)
  (for-each propagate-one-constraint *the-constraints*))

;; Constraint -> void
;; tries to match a constraint (either one that was originally
;; generated, or one that has just been created due to propagation)
;; against each propagation rule, generating new constraints if
;; necessary, until no propagation is possible anymore.
;; First tries to match the constraint against a rule where it appears
;; on the left, then, in the second part of the function, against a
;; rule where it appears on the right.
(define (propagate-one-constraint constraint)
  (let ([lo (constraint-lo constraint)]
        [hi (constraint-hi constraint)])
    ;;(pp-constraints (list constraint))
    ;; trans-cons
    (when (and (constant-set-exp? lo)
               (setexp:Set-var? hi))
      (let ([his-other (lookup-lo-and-filter setexp:Set-var? hi)])
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;; trans-sel
    (when (and (selector? hi)
               (setexp:Set-var? lo))
      (let ([his-other (lookup-lo-and-filter setexp:Set-var? hi)])
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;;trans-dom
    (when (and (setexp:Dom-interval? hi)
               (setexp:Set-var? lo))
      (let* ([interval (setexp:Dom-interval-interval hi)]
             [pos (setexp:Dom-interval-pos hi)]
             [n (setexp:Dom-interval-n hi)]
             [dom-arities (lookup-dom-int (encode-dom pos (setexp:Dom-interval-set-var hi)))]
             [good-dom-ars (mzlib:filter (lambda (dom-ar)
                                           (satisfies interval n (setexp:Dom-arity-arity dom-ar)))
                                         dom-arities)]
             [his-other
              (apply append (map (lambda (dom-ar) (lookup-lo-and-filter setexp:Set-var? dom-ar)) good-dom-ars))])
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;;trans-rng
    (when (and (setexp:Rng-arity? hi)
               (setexp:Set-var? lo))
      ;;(printf "trans-rng 1~n")
      (let* ([arity (setexp:Rng-arity-arity hi)]
             [rng-intervals (lookup-rng-ar (encode-rng (setexp:Rng-arity-set-var hi)))]
             [good-rng-ints
              (mzlib:filter (lambda (rng-int)
                              (satisfies (setexp:Rng-interval-interval rng-int)
                                         (setexp:Rng-interval-n rng-int) arity)) rng-intervals)]
             [his-other
              (apply append (map (lambda (rng-int) (lookup-lo-and-filter setexp:Set-var? rng-int)) good-rng-ints))])
        ;;(printf "arity: ~a~nrng-intervals: ~a~ngood-rng-ints: ~a~nhis-other: ~a~n" arity rng-intervals good-rng-ints his-other)
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;; rng-prop
    (when (and (setexp:Rng-arity? hi)
               (setexp:Set-var? lo))
      (let* ([arity (setexp:Rng-arity-arity hi)]
             [beta (setexp:Rng-arity-set-var hi)]
             [his-other (lookup-lo-and-filter setexp:Set-var? beta)])
        (fire-covariant-selector-rule lo his-other (lambda (gamma) (setexp:make-Rng-arity arity gamma)))))
    ;; dom-prop
    (when (and (setexp:Dom-arity? lo)
               (setexp:Set-var? hi))
      (let* ([arity (setexp:Dom-arity-arity lo)]
             [n (setexp:Dom-arity-pos lo)]
             [alpha (setexp:Dom-arity-set-var lo)]
             [his-other (lookup-lo-and-filter setexp:Set-var? alpha)])
        (fire-contravariant-selector-rule his-other hi (lambda (gamma) (setexp:make-Dom-arity arity n gamma)))))
    ;; car-prop
    (when (and (setexp:Car? hi)
               (setexp:Set-var? lo))
      (let* ([beta (setexp:Car-set-var hi)]
             [his-other (lookup-lo-and-filter setexp:Set-var? beta)])
        (fire-covariant-selector-rule lo his-other (lambda (gamma) (setexp:make-Car gamma)))))
    ;; cdr-prop
    (when (and (setexp:Cdr? hi)
               (setexp:Set-var? lo))
      (let* ([beta (setexp:Cdr-set-var hi)]
             [his-other (lookup-lo-and-filter setexp:Set-var? beta)])
        (fire-covariant-selector-rule lo his-other (lambda (gamma) (setexp:make-Cdr gamma)))))

    
    ;; Reverse rules
    (when (and (setexp:Set-var? lo)
               (setexp:Set-var? hi))
      ;; trans-const
      (let* ([los-other (lookup-hi-and-filter constant-set-exp? lo)])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other))
      ;; rng-prop
      (let* ([rng-arities (lookup-rng-ars-from-set-var lo)])
        (for-each
         (lambda (rng-ar)
           (let* ([arity (setexp:Rng-arity-arity rng-ar)]
                  [rng-arity-gamma (setexp:make-Rng-arity arity hi)]
                  [los-other (lookup-hi-and-filter setexp:Set-var? rng-ar)])
             (for-each
              (lambda (lo-other)
                (add-one-constraint-and-propagate lo-other rng-arity-gamma))
              los-other)))
         rng-arities))
      ;; dom-prop
      (let* ([dom-arities (lookup-dom-ars-from-set-var lo)])
        (for-each
         (lambda (dom-ar)
           (let* ([arity (setexp:Dom-arity-arity dom-ar)]
                  [n (setexp:Dom-arity-pos dom-ar)]
                  [dom-arity-gamma (setexp:make-Dom-arity arity n hi)]
                  [his-other (lookup-lo-and-filter setexp:Set-var? dom-ar)])
             (for-each
              (lambda (hi-other)
                (add-one-constraint-and-propagate dom-arity-gamma hi-other))
              his-other)))
         dom-arities))
      ;; car-prop
      (let* ([car-gamma (setexp:make-Car hi)]
             [los-other (lookup-hi-and-filter setexp:Set-var? (setexp:make-Car lo))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other car-gamma))
         los-other))
      ;; cdr-prop
      (let* ([cdr-gamma (setexp:make-Cdr hi)]
             [los-other (lookup-hi-and-filter setexp:Set-var? (setexp:make-Cdr lo))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other cdr-gamma))
         los-other)))
    ;; trans-sel
    (when (and (selector? lo)
               (setexp:Set-var? hi))
      (let* ([los-other (lookup-hi-and-filter setexp:Set-var? lo)])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other)))
    ;; trans-dom
    (when (and (setexp:Dom-arity? lo)
               (setexp:Set-var? hi))
      (let* ([arity (setexp:Dom-arity-arity lo)]
             [pos (setexp:Dom-arity-pos lo)]
             [dom-intervals (lookup-dom-ar (encode-dom pos (setexp:Dom-arity-set-var lo)))]
             [good-dom-ints (mzlib:filter (lambda (dom-int)
                                            (satisfies (setexp:Dom-interval-interval dom-int)
                                                       (setexp:Dom-interval-n dom-int) arity))
                                          dom-intervals)]
             [los-other
              (apply append (map (lambda (dom-int) (lookup-hi-and-filter setexp:Set-var? dom-int)) good-dom-ints))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other)))
    ;; trans-rng
    (when (and (setexp:Rng-interval? lo)
               (setexp:Set-var? hi))
      ;;(printf "trans-rng 2~n")
      (let* ([interval (setexp:Rng-interval-interval lo)]
             [n (setexp:Rng-interval-n lo)]
             [rng-arities (lookup-rng-int (encode-rng (setexp:Rng-interval-set-var lo)))]
             [good-rng-ars
              (mzlib:filter (lambda (rng-ar) (satisfies interval n (setexp:Rng-arity-arity rng-ar))) rng-arities)]
             [los-other
              (apply append (map (lambda (rng-ar) (lookup-lo-and-filter setexp:Set-var? rng-ar)) good-rng-ars))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constraint pretty printing

;; Interval -> void
;; Interval pretty printer
(define (pp-interval int)
  (printf "[~a,~a]" (setexp:Interval-lo int) (setexp:Interval-hi int)))
  
;; Arity -> void
;; Arity pretty print
(define (pp-arity ar)
  (pp-interval (setexp:Arity-req ar))
  (printf "(")
  (for-each pp-interval (setexp:Arity-proh ar))
  (printf ")"))
  
;; constraint -> void
;; setexp pretty print
(define (pp-setexp setexp)
  (cond
    [(setexp:Const? setexp)
     (printf "~a" (setexp:Const-val setexp))]
    [(setexp:Token? setexp)
     (printf "~a" (setexp:Token-name setexp))]
    [(setexp:Set-var? setexp)
     (printf "~a" (setexp:Set-var-name setexp))]
    [(setexp:Dom-arity? setexp)
     (printf "DomA^(")
     (pp-arity (setexp:Dom-arity-arity setexp))
     (printf ")_~a(~a)" (setexp:Dom-arity-pos setexp) (setexp:Set-var-name (setexp:Dom-arity-set-var setexp)))]
    [(setexp:Dom-interval? setexp)
     (printf "DomI^")
     (pp-interval (setexp:Dom-interval-interval setexp))
     (printf "_~a,~a(~a)"
             (setexp:Dom-interval-pos setexp)
             (setexp:Dom-interval-n setexp) 
             (setexp:Set-var-name (setexp:Dom-interval-set-var setexp)))]
    [(setexp:Rng-arity? setexp)
     (printf "RngA^(")
     (pp-arity (setexp:Rng-arity-arity setexp))
     (printf ")(~a)" (setexp:Set-var-name (setexp:Rng-arity-set-var setexp)))]
    [(setexp:Rng-interval? setexp)
     (printf "RngI^")
     (pp-interval (setexp:Rng-interval-interval setexp))
     (printf "_~a(~a)" (setexp:Rng-interval-n setexp) (setexp:Set-var-name (setexp:Rng-interval-set-var setexp)))]
    [(setexp:Label? setexp)
     (printf "Label(~a)" (setexp:Label-name setexp))]
    [(setexp:Car? setexp)
     (printf "Car(~a)" (setexp:Set-var-name (setexp:Car-set-var setexp)))]
    [(setexp:Cdr? setexp)
     (printf "Cdr(~a)" (setexp:Set-var-name (setexp:Cdr-set-var setexp)))]
    [(setexp:Struct? setexp)
     (let ([fields (setexp:Struct-fields setexp)])
       (apply printf
              (apply string-append "Struct:~a("
                     (if (null? fields)
                         (list ")")
                         (letrec ([pp-list (lambda (l)
                                             (if (null? (cdr l))
                                                 (list "~a)")
                                                 (cons "~a " (pp-list (cdr l)))))])
                           (pp-list fields))))
              (setexp:Struct-name setexp)
              (map setexp:Set-var-name fields)))]
    [else (printf "unknown set expression: ~a" setexp)]))
    
;; (listof constraint) -> void
;; constraints pretty printer
(define (pp-constraints constraints)
  (for-each
   (lambda (c)
     (pp-setexp (constraint-lo c))
     (printf " <= ")
     (pp-setexp (constraint-hi c))
     (printf "~n"))
   constraints))
  
  ) ;; unit/sig
