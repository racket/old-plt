#|

Note: the patterns described in the doc.txt file are
slightly different than the patterns processed here.
The difference is in the form of the side-condition
expressions. Here they are procedures that accept
binding structures, instead of expressions. The
reduction (And other) macros do this transformation
before the pattern compiler is invoked.

abstract out the `hole and `(hole name) patterns.

|#
(module matcher mzscheme
  (require (lib "list.ss")
           (lib "match.ss")
           (lib "etc.ss")
           (lib "contract.ss"))
  
  (provide (struct nt (name rhs))
           (struct rhs (pattern))
           (struct compiled-lang (lang ht across-ht))
           
           lookup-binding
           compile-pattern
           compile-language
           match-pattern
           
           compiled-pattern
           
           make-bindings bindings-table bindings?
           (struct rib (name exp)))
  
  ;; lang = (listof nt)
  ;; nt = (make-nt sym (listof rhs))
  ;; rhs = (make-rhs single-pattern)
  ;; single-pattern = sexp
  (define-struct nt (name rhs) (make-inspector))
  (define-struct rhs (pattern) (make-inspector))
  
  ;; var = (make-var sym sexp)
  ;; patterns are sexps with `var's embedded
  ;; in them. It means to match the
  ;; embedded sexp and return that binding
  
  ;; bindings = (make-bindings (listof rib))
  ;; rib = (make-rib sym (union hole-binding sexp))
  (define-values (make-bindings bindings-table bindings?)
    (let () 
      (define-struct bindings (table) (make-inspector)) ;; for testing, add inspector
      (values (lambda (table)
                (unless (and (list? table)
                             (andmap rib? table))
                  (error 'make-bindings "expected <(listof rib)>, got ~e" table))
                (make-bindings table))
              bindings-table
              bindings?)))
  (define-struct rib (name exp) (make-inspector)) ;; for testing, add inspector

  ;; hole-binding = (make-hole sexp (listof (union 'car 'cdr)) sym)
  ;; the path should be reversed -- as each layer of the sexp
  ;; is traversed, the 'car and 'cdr's are stuck at the front of
  ;; the path. The result is reversed when put into a hole-path
  (define-struct hole-binding (exp path id) (make-inspector))
  
  ;; repeat = (make-repeat compiled-pattern (listof rib))
  (define-struct repeat (pat empty-bindings) (make-inspector)) ;; inspector for tests below
  
  ;; compiled-pattern : exp (listof hole-path) -> (union #f (listof bindings))
  ;; compiled-lang : (make-compiled-lang (listof nt) 
  ;;                                     hash-table[sym -o> compiled-pattern]
  ;;                                     hash-table[sym -o> compiled-pattern])
  ;; hole-path = (make-hole-path (union #f symbol) symbol (listof (union 'car 'cdr)))
  (define-struct hole-path (name id path) (make-inspector))
  
  (define compiled-pattern (any? (listof hole-path?) . -> . (union false? (listof bindings?))))
  
  (define-struct compiled-lang (lang ht across-ht))
  
  ;; lookup-binding : bindings sym [(-> any)] -> any
  (define lookup-binding 
    (opt-lambda (bindings
		 sym
		 [fail (lambda () (error 'lookup-binding "didn't find ~e in ~e" sym bindings))])
      (let loop ([ribs (bindings-table bindings)])
        (cond
          [(null? ribs) (fail)]
          [else
           (let ([rib (car ribs)])
             (if (eq? (rib-name rib) sym)
                 (rib-exp rib)
                 (loop (cdr ribs))))]))))
  
  ;; compile-language : lang -> compiled-lang
  (define (compile-language lang)
    (let* ([clang-ht (make-hash-table)]
           [across-ht (make-hash-table)]
           [clang (make-compiled-lang lang clang-ht across-ht)]
           [do-compilation
            (lambda (ht lang prefix-cross?)
              (for-each
               (lambda (nt)
                 (for-each
                  (lambda (rhs)
                    (hash-table-put!
                     ht
                     (nt-name nt)
                     (cons (compile-pattern/cross? clang (rhs-pattern rhs) prefix-cross?)
                           (hash-table-get ht (nt-name nt)))))
                  (nt-rhs nt)))
               lang))])
      
      (for-each (lambda (nt)
                  (hash-table-put! clang-ht (nt-name nt) null))
                lang)
      (hash-table-for-each
       clang-ht
       (lambda (nt rhs)
         (when (has-underscore? nt)
           (error 'compile-language "cannot use underscore in nonterminal name, ~s" nt))))
      
      (let ([compatible-context-language
             (build-compatible-context-language clang-ht lang)])
        (for-each (lambda (nt)
                    (hash-table-put! across-ht (nt-name nt) null))
                  compatible-context-language)
        (do-compilation clang-ht lang #t)
        (do-compilation across-ht compatible-context-language #f)
        clang)))
  
  ;; build-compatible-context-language : lang -> lang
  (define (build-compatible-context-language clang-ht lang)
    (apply
     append
     (map 
      (lambda (nt1)
        (map
         (lambda (nt2)
           (let ([compat-nt (build-compatible-contexts/nt clang-ht (nt-name nt1) nt2)])
             (if (eq? (nt-name nt1) (nt-name nt2))
                 (make-nt (nt-name compat-nt)
                          (cons
                           (make-rhs 'hole)
                           (nt-rhs compat-nt)))
                 compat-nt)))
         lang))
      lang)))
    
  ;; build-compatible-contexts : clang-ht prefix nt -> nt
  ;; constructs the compatible closure evaluation context from nt.
  (define (build-compatible-contexts/nt clang-ht prefix nt)
    (make-nt
     (symbol-append prefix '- (nt-name nt))
     (apply append
            (map
             (lambda (rhs)
               (let-values ([(maker count) (build-compatible-context-maker clang-ht
                                                                           (rhs-pattern rhs)
                                                                           prefix)])
                 (let loop ([i count])
                   (cond
                     [(zero? i) null]
                     [else (let ([nts (build-across-nts (nt-name nt) count (- i 1))])
                             (cons (make-rhs (maker (box nts)))
                                   (loop (- i 1))))]))))
             (nt-rhs nt)))))
    
  (define (symbol-append . args)
    (string->symbol (apply string-append (map symbol->string args))))
  
  ;; build-across-nts : symbol number number -> (listof pattern)
  (define (build-across-nts nt count i)
    (let loop ([j count])
      (cond
        [(zero? j) null]
        [else
         (cons (= i (- j 1)) 
               (loop (- j 1)))])))
    
  ;; build-compatible-context-maker : symbol pattern -> (values ((box (listof pattern)) -> pattern) number)
  ;; when the result function is applied, it takes each element
  ;; of the of the boxed list and plugs them into the places where
  ;; the nt corresponding from this rhs appeared in the original pattern. 
  ;; The number result is the number of times that the nt appeared in the pattern.
  (define (build-compatible-context-maker clang-ht pattern prefix)
    (let ([count 0])
      (values
       (let loop ([pattern pattern])
         (match pattern
           [`any (lambda (l) 'any)]
           [`number (lambda (l) 'number)]
           [`string (lambda (l) 'string)]
           [`variable (lambda (l) 'variable)] 
           [`(variable-except ,@(vars ...)) (lambda (l) pattern)]
           [`hole  (lambda (l) 'hole)]
           [`(hole ,(? symbol? hole-name)) (lambda (l) `(hole ,hole-name))]
           [(? string?) (lambda (l) pattern)]
           [(? symbol?) 
            (cond
              [(hash-table-get clang-ht pattern (lambda () #f))
               (set! count (+ count 1))
               (lambda (l)
                 (let ([fst (car (unbox l))])
                   (set-box! l (cdr (unbox l)))
                   (if fst
                       `(cross ,(symbol-append prefix '- pattern))
                       pattern)))]
              [else
               (lambda (l) pattern)])]
           [`(name ,name ,pat)
            (let ([patf (loop pat)])
              (lambda (l)
                `(name ,name ,(patf l))))]
           [`(in-hole ,context ,contractum)
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (lambda (l)
                `(in-hole ,(match-context l)
                          ,(match-contractum l))))]
           [`(in-hole* ,hole-name ,context ,contractum)
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (lambda (l)
                `(in-hole* ,hole-name
                           ,(match-context l)
                           ,(match-contractum l))))]
           [`(in-hole+ ,context ,contractum)
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (lambda (l)
                `(in-hole+ ,(match-context l)
                           ,(match-contractum l))))]
           [`(in-named-hole ,hole-id ,hole-name ,context ,contractum)
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (lambda (l)
                `(in-named-hole ,hole-id
                                ,hole-name
                                ,(match-context l)
                                ,(match-contractum l))))]
           [`(in-named-hole+ ,hole-id ,hole-name ,context ,contractum)
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (lambda (l)
                `(in-named-hole+ ,hole-id
                                 ,hole-name
                                 ,(match-context l)
                                 ,(match-contractum l))))]
           [`(side-condition ,pat ,condition)
            (let ([patf (loop pat)])
              (lambda (l)
                `(side-condition ,(patf l) ,condition)))]
           [(? list?)
            (let ([fs (map loop pattern)])
              (lambda (l)
                (map (lambda (f) (f l)) fs)))]
           [else 
            (lambda (l) pattern)]))
       count)))
  
  ;; match-pattern : compiled-pattern exp -> (union #f (listof bindings))
  (define (match-pattern compiled-pattern exp)
    (let ([results (compiled-pattern exp empty)])
      (and results
           (let ([filtered (filter-multiples results)])
             (and (not (null? filtered))
                  filtered)))))
  
  ;; filter-multiples : (listof bindings) -> (listof bindings)
  (define (filter-multiples bindingss)
    (let loop ([bindingss bindingss]
               [acc null])
      (cond
        [(null? bindingss) acc]
        [else
         (let ([merged (merge-multiples/remove (car bindingss))])
           (if merged
               (loop (cdr bindingss) (cons merged acc))
               (loop (cdr bindingss) acc)))])))
  
  ;; merge-multiples/remove : bindings -> (union #f bindings)
  ;; returns #f if all duplicate bindings don't bind the same thing
  ;; returns a new bindings 
  (define (merge-multiples/remove bindings)
    (let/ec fail
      (let ([ht (make-hash-table)]
            [hole-ribs null]
            [ribs (bindings-table bindings)])
        (for-each
         (lambda (rib)
           (let/ec new
             (let ([name (rib-name rib)]
                   [exp (rib-exp rib)])
               (if (hole-binding? exp)
                   (set! hole-ribs (cons rib hole-ribs))
                   (let ([previous-exp
                          (hash-table-get 
                           ht 
                           name
                           (lambda ()
                             (hash-table-put! ht name exp)
                             (new (void))))])
                     (unless (equal? exp previous-exp)
                       (fail #f)))))))
         ribs)
        (make-bindings (append hole-ribs
                               (hash-table-map ht make-rib))))))
  
  
  (define underscore-allowed '(any number string variable))

  (define (compile-pattern clang pattern) (compile-pattern/cross? clang pattern #t))
  
  ;; compile-pattern : compiled-lang pattern boolean -> compiled-pattern
  ;; matches pattern agains exp in lang. may return multiple bindings
  ;; for a single identifier. Don't extract any values from the compiled-lang
  ;; (except to test if the ht maps things) on the first application --
  ;; it may not be completely filled in yet.
  (define (compile-pattern/cross? clang pattern prefix-cross?)
    (let ([clang-ht (compiled-lang-ht clang)]
          [across-ht (compiled-lang-across-ht clang)])
      (let loop ([pattern pattern])
        (memoize2
         (match pattern
           [`any
            (lambda (exp hole-paths)
              (list (make-bindings null)))]
           [`number 
            (lambda (exp hole-paths)
              (and (number? exp) (list (make-bindings null))))]
           [`string
            (lambda (exp hole-paths)
              (and (string? exp) (list (make-bindings null))))]
           [`variable 
            (lambda (exp hole-paths)
              (and (symbol? exp) (list (make-bindings null))))]
           [`(variable-except ,@(vars ...))
            (lambda (exp hole-paths)
              (and (symbol? exp)
                   (not (memq exp vars))
                   (list (make-bindings null))))]
           [`hole (match-hole #f)]
           [`(hole ,hole-id) (match-hole hole-id)]
           [(? string?) 
            (lambda (exp hole-paths)
              (and (string? exp)
                   (string=? exp pattern)
                   (list (make-bindings null))))]
           [(? symbol?) 
            (cond
              [(hash-table-get clang-ht pattern (lambda () #f))
               (lambda (exp hole-paths)
                 (match-nt clang-ht pattern exp hole-paths))]
              [(has-underscore? pattern)
               (let ([before (split-underscore pattern)])
                 (unless (or (hash-table-get clang-ht before (lambda () #f))
                             (memq before underscore-allowed))
                   (error 'compile-pattern "stuff before underscore is illegal in ~s" pattern))
                 (loop `(name ,pattern ,before)))]
              [else
               (lambda (exp hole-paths)
                 (and (eq? exp pattern)
                      (list (make-bindings null))))])]
           [`(cross ,(and pre-id (? symbol?)))
            (let ([id (if prefix-cross?
                          (symbol-append pre-id '- pre-id)
                          pre-id)])
              (cond
                [(hash-table-get across-ht id (lambda () #f))
                 (lambda (exp hole-paths)
                   (match-nt across-ht id exp hole-paths))]
                [else
                 (error 'compile-pattern "unknown cross reference ~a" id)]))]
           [`(name ,name ,pat)
            (let ([match-pat (loop pat)])
              (lambda (exp hole-paths)
                (let ([matches (match-pat exp hole-paths)])
                  (and matches 
                       (map (lambda (bindings)
                              (make-bindings (cons (make-rib name exp)
                                                   (bindings-table bindings))))
                            matches)))))]
           [`(in-hole ,context ,contractum) (loop `(in-hole* hole ,context ,contractum))]
           [`(in-hole* ,this-hole-name ,context ,contractum) 
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (match-in-hole context contractum exp match-context match-contractum this-hole-name #t #f))]
           [`(in-hole+ ,context ,contractum) 
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (match-in-hole context contractum exp match-context match-contractum 'hole+ #f #f))]
           [`(in-named-hole ,hole-id ,this-hole-name ,context ,contractum) 
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (match-in-hole context contractum exp match-context match-contractum this-hole-name #t hole-id))]
           [`(in-named-hole+ ,hole-id ,context ,contractum) 
            (let ([match-context (loop context)]
                  [match-contractum (loop contractum)])
              (match-in-hole context contractum exp match-context match-contractum 'hole+ #f hole-id))]
           [`(side-condition ,pat ,condition)
            (let ([match-pat (loop pat)])
              (lambda (exp hole-paths)
                (let ([matches (match-pat exp hole-paths)])
                  (and matches
                       (let ([filtered (filter condition matches)])
                         (if (null? filtered)
                             #f
                             filtered))))))]
           [(? list?)
            (let ([rewritten (rewrite-ellipses pattern loop)])
              (lambda (exp hole-paths)
                (match-list rewritten exp hole-paths)))]
           [(? procedure?)
            pattern]
           [else 
            (lambda (exp hole-paths)
              (and (eq? pattern exp)
                   (list (make-bindings null))))])))))

  ;; split-underscore : symbol -> symbol
  ;; returns the text before the underscore in a symbol (as a symbol)
  ;; raise an error if there is more than one underscore in the input
  (define (split-underscore sym)
    (string->symbol
     (list->string
      (let loop ([chars (string->list (symbol->string sym))])
        (cond
          [(null? chars) (error 'split-underscore "bad")]
          [else
           (let ([c (car chars)])
             (cond
               [(char=? c #\_)
                (when (memq #\_ (cdr chars))
                  (error 'compile-pattern "found a symbol with multiple underscores: ~s" sym))
                null]
               [else (cons c (loop (cdr chars)))]))])))))
  
  ;; has-underscore? : symbol -> boolean
  (define (has-underscore? sym)
    (memq #\_ (string->list (symbol->string sym))))
  
  ;; memoize2 : (x y -> w) -> x y -> w
  ;; memoizes a function of two arguments
  ;; limits cache size to fixed size
  (define (memoize2 f)
    (let ([ht (make-hash-table 'equal)]
	  [entries 0])
      (lambda (x y)
        (let* ([key (cons x y)]
               [compute/cache
                (lambda ()
		  (set! entries (+ entries 1))
                  (let ([res (f x y)])
                    (hash-table-put! ht key res)
                    res))])
	  (unless (< entries 10000)
            (set! entries 0)
	    (set! ht (make-hash-table 'equal)))
          (hash-table-get ht key compute/cache)))))

  ;; match-hole : (union #f symbol) -> compiled-pattern
  (define (match-hole hole-id)
    (lambda (exp hole-paths)
      (let ([relevant-hole-paths
             (filter (lambda (x) (eq? (hole-path-id x) hole-id))
                     hole-paths)])
        (if (null? relevant-hole-paths)
            #f
            (begin
              (unless (null? (cdr relevant-hole-paths))
                (error 'match "too many hole paths, ~e" relevant-hole-paths))
              (let ([the-hole-path (car relevant-hole-paths)])
                (list 
                 (make-bindings 
                  (list 
                   (make-rib 
                    (hole-path-name the-hole-path)
                    (make-hole-binding
                     exp
                     (reverse (hole-path-path the-hole-path))
                     hole-id)))))))))))

  ;; match-in-hole : sexp sexp sexp compiled-pattern compiled-pattern symbol boolean (union symbol #f) -> compiled-pattern
  (define (match-in-hole context contractum exp match-context match-contractum this-hole-name bind-hole? hole-id)
    (lambda (exp hole-paths)
      (when (this-hole-id-used? hole-id hole-paths)
        (if hole-id
            (error 'match "multiple hole interference for hole-id ~s" hole-id)
            (error 'match "multiple hole interference")))
      (let ([bindingss (match-context exp (cons (make-hole-path this-hole-name hole-id '()) hole-paths))])
        (and bindingss
             (let loop ([bindingss bindingss]
                        [acc null])
               (cond
                 [(null? bindingss) acc]
                 [else 
                  (let ([pre-bindings (car bindingss)])
                    (let* ([same-hole?
                            (lambda (x) 
                              (and (hole-binding? (rib-exp x))
                                   (eq? this-hole-name (rib-name x))
                                   (eq? hole-id (hole-binding-id (rib-exp x)))))]
                           [bindings
                            (if bind-hole?
                                pre-bindings
                                (make-bindings
                                 (filter (lambda (x) (not (same-hole? x)))
                                         (bindings-table pre-bindings))))]
                           [holes (filter same-hole? (bindings-table pre-bindings))])
                      (when (null? holes)
                        (error 'match-pattern "no hole~a in ~e for ~e" 
                               (if hole-id
                                   (format " with id ~s" hole-id)
                                   "")
                               context
                               exp))
                      (unless (null? (cdr holes))
                        (error 'match-pattern "found more than one hole match in ~e ~e, holes: ~e" 
                               context exp holes))
                      (let* ([hole-rib (car holes)]
                             [a-hole (rib-exp hole-rib)]
                             [extended-hole-paths (extend-hole-paths hole-paths (reverse (hole-binding-path a-hole)))]
                             [contractum-bindingss 
                              (match-contractum (hole-binding-exp a-hole) extended-hole-paths)])
                        (if contractum-bindingss
                            (let i-loop ([contractum-bindingss contractum-bindingss]
                                         [acc acc])
                              (cond
                                [(null? contractum-bindingss) (loop (cdr bindingss) acc)]
                                [else (let ([contractum-bindings (car contractum-bindingss)])
                                        (i-loop
                                         (cdr contractum-bindingss)
                                         (cons
                                          (make-bindings
                                           (append (bindings-table contractum-bindings)
                                                   (bindings-table bindings)))
                                          acc)))]))
                            (loop (cdr bindingss) acc)))))]))))))
  
  ;; extend-hole-paths : (listof hole-path) (listof (union 'car 'cdr)) -> (listof hole-path)
  (define (extend-hole-paths hole-paths a-hole-path)
    (map (lambda (hp)
           (make-hole-path (hole-path-name hp)
                           (hole-path-id hp)
                           (append a-hole-path (hole-path-path hp))))
         hole-paths))
  
  ;; this-hole-id-used? : (union #f symbol) (listof hole-path) -> boolean
  (define (this-hole-id-used? hole-id hole-paths)
    (let loop ([hole-paths hole-paths])
      (cond
        [(null? hole-paths) #f]
        [else (or (eq? hole-id (hole-path-id (car hole-paths)))
                  (loop (cdr hole-paths)))])))
      
  ;; match-list : (listof (union repeat compiled-pattern)) sexp (listof hole-path) -> (union #f (listof bindings))
  (define (match-list patterns exp hole-paths)
    (let (;; raw-match : (listof (listof (listof bindings)))
          [raw-match (match-list/raw patterns exp hole-paths)])
      
      ;(printf "ml.1: ~s\n" raw-match)
      
      (and (not (null? raw-match))
           
           (let* (;; combined-matches : (listof (listof bindings))
                  ;; a list of complete possibilities for matches 
                  ;; (analagous to multiple matches of a single non-terminal)
                  [combined-matches (map combine-matches raw-match)]
                  ;[_ (printf "ml.2: ~s\n" combined-matches)]
                  
                  ;; flattened-matches : (union #f (listof bindings))
                  [flattened-matches (if (null? combined-matches)
                                         #f
                                         (apply append combined-matches))]
                  ;[_ (printf "ml.3: ~s\n" flattened-matches)]
                  )
             flattened-matches))))
  
  ;; match-list/raw : (listof (union repeat compiled-pattern)) 
  ;;                  sexp
  ;;                  (listof hole-path)
  ;;               -> (listof (listof (listof bindings)))
  ;; the result is the raw accumulation of the matches for each subpattern, as follows:
  ;;  (listof (listof (listof bindings)))
  ;;  \       \       \----------------/  a match for one position in the list (failures don't show up)
  ;;   \       \----------------------/   one element for each position in the pattern list
  ;;    \----------------------------/    one element for different expansions of the ellipses
  ;; the failures to match are just removed from the outer list before this function finishes
  ;; via the `fail' argument to `loop'.
  (define (match-list/raw patterns exp hole-paths)
    (let/ec k
      (let loop ([patterns patterns]
                 [exp exp]
                 [hole-paths hole-paths]
                 ;; fail : -> alpha
                 ;; causes one possible expansion of ellipses to fail
                 ;; initially there is only one possible expansion, so
                 ;; everything fails.
                 [fail (lambda () (k null))])
        (cond
          [(pair? patterns)
           (let ([fst-pat (car patterns)])
             (cond
               [(repeat? fst-pat)
                (if (or (null? exp) (pair? exp))
                    (let ([r-pat (repeat-pat fst-pat)]
                          [r-mt (make-bindings (repeat-empty-bindings fst-pat))])
                      (apply 
                       append
                       (cons (let/ec k
                               (let ([mt-fail (lambda () (k null))])
                                 (map (lambda (pat-ele) (cons (list r-mt) pat-ele))
                                      (loop (cdr patterns) exp hole-paths mt-fail))))
                             (let r-loop ([exp exp]
                                          [hole-paths hole-paths]
                                          ;; past-matches is in reverse order
                                          ;; it gets reversed before put into final list
                                          [past-matches (list r-mt)])
                               (cond
                                 [(pair? exp)
                                  (let* ([fst (car exp)]
                                         [m (r-pat fst (add-to-hole-paths 'car hole-paths))])
                                    (if m
                                        (let* ([combined-matches (collapse-single-multiples m past-matches)]
                                               [reversed (reverse-multiples combined-matches)])
                                          (cons 
                                           (let/ec fail-k
                                             (let ([multi-fail (lambda () (fail-k null))])
                                               (map (lambda (x) (cons reversed x))
                                                    (loop (cdr patterns) 
                                                          (cdr exp)
                                                          (add-to-hole-paths 'cdr hole-paths)
                                                          multi-fail))))
                                           (r-loop (cdr exp) (add-to-hole-paths 'cdr hole-paths) combined-matches)))
                                        (list null)))]
                                 ;; what about dotted things?
                                 [else (list null)])))))
                    (fail))]
               [else
                (cond
                  [(pair? exp)
                   (let* ([fst-exp (car exp)]
                          [match (fst-pat fst-exp (add-to-hole-paths 'car hole-paths))])
                     (if match
                         (map (lambda (x) (cons match x))
                              (loop (cdr patterns) (cdr exp) (add-to-hole-paths 'cdr hole-paths) fail))
                         (fail)))]
                  [else
                   (fail)])]))]
          [else
           (if (null? exp)
               (list null)
               (fail))]))))
  
  ;; add-to-hole-paths : symbol (listof hole-path) -> (listof hole-path)
  (define (add-to-hole-paths sym hole-paths)
    (map (lambda (hole-path)
           (make-hole-path (hole-path-name hole-path)
                           (hole-path-id hole-path)
                           (cons sym (hole-path-path hole-path))))
         hole-paths))
  
  ;; collapse-single-multiples : (listof bindings) (listof bindings[to-lists]) -> (listof bindings[to-lists])
  (define (collapse-single-multiples bindingss multiple-bindingss)
    (map
     make-bindings
     (apply append 
            (map
             (lambda (multiple-bindings)
               (map
                (lambda (single-bindings)
                  (let ([ht (make-hash-table)])
                    (for-each
                     (lambda (multiple-rib)
                       (hash-table-put! ht (rib-name multiple-rib) (rib-exp multiple-rib)))
                     (bindings-table multiple-bindings))
                    (for-each
                     (lambda (single-rib)
                       (let* ([key (rib-name single-rib)]
                              [rst (hash-table-get ht key (lambda () null))])
                         (hash-table-put! ht key (cons (rib-exp single-rib) rst))))
                     (bindings-table single-bindings))
                    (hash-table-map ht make-rib)))
                bindingss))
             multiple-bindingss))))
  
  ;; reverse-multiples : (listof bindings[to-lists]) -> (listof bindings[to-lists])
  ;; reverses the rhs of each rib in the bindingss.
  (define (reverse-multiples bindingss)
    (map (lambda (bindings)
           (make-bindings
            (map (lambda (rib)
                   (make-rib (rib-name rib)
                             (reverse (rib-exp rib))))
                 (bindings-table bindings))))
         bindingss))
  
  ;; match-nt : hash-table[from compiled-lang] sym exp (listof hole-path) -> (union #f (listof bindings))
  (define (match-nt clang-ht nt term hole-paths)
    (let ([compiled-rhss (hash-table-get clang-ht nt)])
      (let loop ([rhss compiled-rhss]
                 [anss null])
        (cond
          [(null? rhss) (if (null? anss) #f (apply append anss))]
          [else
           (let ([mth ((car rhss) term hole-paths)])
             (if mth
                 (loop (cdr rhss) (cons (remove-non-hole-bindings mth) anss))
                 (loop (cdr rhss) anss)))]))))
  
  ;; remove-non-hole-bindings : (listof bindings) -> (listof bindings)
  (define (remove-non-hole-bindings lob)
    (map (lambda (bindings)
           (make-bindings
            (filter (lambda (rib) (hole-binding? (rib-exp rib))) 
                    (bindings-table bindings))))
         lob))
  
  ;; rewrite-ellipses : (listof pattern) 
  ;;                    (pattern -> compiled-pattern)
  ;;                 -> (listof (union repeat compiled-pattern))
  ;; moves the ellipses out of the list and produces repeat structures
  (define (rewrite-ellipses pattern compile)
    (let loop ([exp-eles pattern]
               [fst #f])
      (cond
        [(null? exp-eles)
         (if fst
             (list (compile fst))
             empty)]
        [else
         (let ([exp-ele (car exp-eles)])
           (cond
             [(eq? '... exp-ele)
              (unless fst
                (error 'match-pattern "bad ellipses placement: ~s" pattern))
              (cons (make-repeat (compile fst) 
                                 (extract-empty-bindings fst))
                    (loop (cdr exp-eles) #f))]
             [fst
              (cons (compile fst) (loop (cdr exp-eles) exp-ele))]
             [(not fst)
              (loop (cdr exp-eles) exp-ele)]))])))
  
  ;; extract-empty-bindings : pattern -> (listof rib)
  (define (extract-empty-bindings pattern)
    (let loop ([pattern pattern]
               [ribs null])
      (match pattern
        [`any ribs]
        [`number ribs] 
        [`variable ribs] 
        [`(variable-except ,@(vars ...)) ribs]

        [`hole (error 'match-pattern "cannot have a hole inside an ellipses")]
        [(? symbol?) 
         (cond
           [(has-underscore? pattern)
            (let ([before (split-underscore pattern)])
              (loop `(name ,pattern ,before) ribs))]
           [else ribs])]
        [`(name ,name ,pat) (loop pat (cons (make-rib name '()) ribs))]
        [`(in-hole ,context ,contractum) (loop context (loop contractum ribs))]
        [`(in-hole* ,hole-name ,context ,contractum) (loop context (loop contractum ribs))]
        [`(in-hole+ ,context ,contractum) (loop context (loop contractum ribs))]
        [`(side-condition ,pat ,test) (loop pat ribs)]
        [(? list?)
         (let ([rewritten (rewrite-ellipses pattern (lambda (x) x))])
           (let i-loop ([r-exps rewritten]
                        [ribs ribs])
             (cond
               [(null? r-exps) ribs]
               [else (let ([r-exp (car r-exps)])
                       (cond
                         [(repeat? r-exp) 
                          (i-loop
                           (cdr r-exps)
                           (append (repeat-empty-bindings r-exp) ribs))]
                         [else
                          (i-loop 
                           (cdr r-exps)
                           (loop (car r-exps) ribs))]))])))]
        [else ribs])))
  
  ;; combine-matches : (listof (listof bindings)) -> (listof bindings)
  ;; input is the list of bindings corresonding to a piecewise match
  ;; of a list. produces all of the combinations of complete matches
  (define (combine-matches bindingss)
    (let loop ([bindingss bindingss])
      (cond
        [(null? bindingss) (list (make-bindings null))]
        [(null? (cdr bindingss)) (car bindingss)]
        [else
         (let ([fst (car bindingss)]
               [snd (cadr bindingss)])
           (loop (cons (combine-pair fst snd) (cddr bindingss))))])))
  
  ;; combine-pair : (listof bindings) (listof bindings) -> (listof bindings)
  (define (combine-pair fst snd)
    (let ([bindings null])
      (for-each 
       (lambda (bindings1)
         (for-each
          (lambda (bindings2)
            (set! bindings (cons (make-bindings (append (bindings-table bindings1)
                                                        (bindings-table bindings2)))
                                 bindings)))
          snd))
       fst)
      bindings))
  
  (provide/contract (replace (any? hole-binding? any? . -> . any)))
  ;; replaces `inner' inside `outer' (using eq?) with `new'
  (define (replace outer hb new)
    (let loop ([sexp outer]
               [path (hole-binding-path hb)])
      (cond
        [(null? path) new]
        [else
         (let ([fst (car path)])
           (unless (pair? sexp)
             (error 'replace "path didn't match exp: ~s ~s" 
                    (hole-binding-path hb)
                    outer))
           (case fst
             [(car) 
              (cons (loop (car sexp) (cdr path))
                    (cdr sexp))]
             [(cdr) 
              (cons (car sexp)
                    (loop (cdr sexp) (cdr path)))]))])))

  (define (test)
    (test-empty 'any 1 (list (make-bindings null)))
    (test-empty 'any 'true (list (make-bindings null)))
    (test-empty 'any "a" (list (make-bindings null)))
    (test-empty 'any '(a b) (list (make-bindings null)))
    (test-empty 1 1 (list (make-bindings null)))
    (test-empty 'x 'x (list (make-bindings null)))
    (test-empty 1 2 #f)
    (test-empty "a" "b" #f)
    (test-empty "a" "a" (list (make-bindings null)))
    (test-empty 'number 1 (list (make-bindings null)))
    (test-empty 'number 'x #f)
    (test-empty 'string "a" (list (make-bindings null)))
    (test-empty 'string 1 #f)
    (test-empty 'variable 'x (list (make-bindings null)))
    (test-empty 'variable 1 #f)
    (test-empty '(variable-except x) 1 #f)
    (test-empty '(variable-except x) 'x #f)
    (test-empty '(variable-except x) 'y (list (make-bindings null)))
    (test-empty 'hole 1 #f)
    (test-empty '(hole hole-name) 1 #f)
    (test-empty '(name x number) 1 (list (make-bindings (list (make-rib 'x 1)))))
    (test-empty 'number_x 1 (list (make-bindings (list (make-rib 'number_x 1)))))
    (test-empty 'string_y "b" (list (make-bindings (list (make-rib 'string_y "b")))))
    (test-empty 'any_z '(a b) (list (make-bindings (list (make-rib 'any_z '(a b))))))
    
    (test-ellipses '(a) '(a))
    (test-ellipses '(a ...) `(,(make-repeat 'a '())))
    (test-ellipses '((a ...) ...) `(,(make-repeat '(a ...) '())))
    (test-ellipses '(a ... b c ...) `(,(make-repeat 'a '()) b ,(make-repeat 'c '())))
    (test-ellipses '((name x a) ...) `(,(make-repeat '(name x a) (list (make-rib 'x '()))))) 
    (test-ellipses '((name x (a ...)) ...)
                   `(,(make-repeat '(name x (a ...)) (list (make-rib 'x '())))))
    (test-ellipses '(((name x a) ...) ...)
                   `(,(make-repeat '((name x a) ...) (list (make-rib 'x '())))))
    (test-ellipses '((1 (name x a)) ...)
                   `(,(make-repeat '(1 (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '((any (name x a)) ...)
                   `(,(make-repeat '(any (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '((number (name x a)) ...)
                   `(,(make-repeat '(number (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '((variable (name x a)) ...)
                   `(,(make-repeat '(variable (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '(((name x a) (name y b)) ...)
                   `(,(make-repeat '((name x a) (name y b)) (list (make-rib 'y '()) (make-rib 'x '())))))
    (test-ellipses '((name x (name y b)) ...)
                   `(,(make-repeat '(name x (name y b)) (list (make-rib 'y '()) (make-rib 'x '())))))
    (test-ellipses '((in-hole (name x a) (name y b)) ...)
                   `(,(make-repeat '(in-hole (name x a) (name y b)) 
                                   (list (make-rib 'x '()) (make-rib 'y '())))))
    
    (test-empty '() '() (list (make-bindings null)))
    (test-empty '(a) '(a) (list (make-bindings null)))
    (test-empty '(a) '(b) #f)
    (test-empty '(a b) '(a b) (list (make-bindings null)))
    (test-empty '(a b) '(a c) #f)
    (test-empty '() 1 #f)
    (test-empty '(in-hole (z hole) a) '(z a) 
                (list (make-bindings (list (make-rib 'hole (make-hole-binding 'a '(cdr car) #f))))))
    (test-empty '(in-hole+ (z hole) a) '(z a) 
                (list (make-bindings (list))))
    (test-empty '(in-hole+ (z hole) (in-hole+ (z hole) a)) '(z (z a))
                (list (make-bindings (list))))
    (test-empty '(in-hole* eloh (x hole) a) '(x a)
                (list (make-bindings (list (make-rib 'eloh (make-hole-binding 'a '(cdr car) #f))))))
    (test-empty '(in-hole (z hole) (in-hole* hole2 (x hole) a)) '(z (x a)) 
                (list (make-bindings (list (make-rib 'hole2 (make-hole-binding 'a '(cdr car) #f))
                                           (make-rib 'hole (make-hole-binding '(x a) '(cdr car) #f))))))
    
    (test-empty '(in-named-hole h1 eloh (z (hole h1)) a) 
                '(z a)
                (list (make-bindings (list (make-rib 'eloh (make-hole-binding 'a '(cdr car) 'h1))))))
    
    (test-empty '(in-named-hole+ h1 (z (hole h1)) a) '(z a) (list (make-bindings (list))))
    (test-empty '(in-named-hole+ e ((hole e) (hole c)) x) '(x y) #f)
    (test-empty '(in-named-hole+ e (in-hole+ c ((hole e) (hole c)) y) x) '(x y) #f)
    (test-empty '(in-named-hole+ c (in-hole+ e ((hole e) (hole c)) x) y) '(x y) #f)
    (test-empty '(in-named-hole c eloh (any (hole c)) y)
                '(x y)
                (list (make-bindings (list (make-rib 'eloh (make-hole-binding 'y '(cdr car) 'c))))))
    (test-empty '(in-named-hole+ c (in-named-hole+ e ((hole e) (hole c)) x) y)
                '(x y)
                (list (make-bindings (list))))
    
    (test-empty '((name x number) (name x number)) '(1 1) (list (make-bindings (list (make-rib 'x 1)))))
    (test-empty '((name x number) (name x number)) '(1 2) #f)
    
    (test-empty '(a ...) '() (list (make-bindings empty)))
    (test-empty '(a ...) '(a) (list (make-bindings empty)))
    (test-empty '(a ...) '(a a) (list (make-bindings empty)))
    (test-empty '((name x a) ...) '() (list (make-bindings (list (make-rib 'x '())))))
    (test-empty '((name x a) ...) '(a) (list (make-bindings (list (make-rib 'x '(a))))))
    (test-empty '((name x a) ...) '(a a) (list (make-bindings (list (make-rib 'x '(a a))))))
    
    (test-empty '(b ... a ...) '() (list (make-bindings empty)))
    (test-empty '(b ... a ...) '(a) (list (make-bindings empty)))
    (test-empty '(b ... a ...) '(b) (list (make-bindings empty)))
    (test-empty '(b ... a ...) '(b a) (list (make-bindings empty)))
    (test-empty '(b ... a ...) '(b b a a) (list (make-bindings empty)))
    (test-empty '(b ... a ...) '(a a) (list (make-bindings empty)))
    (test-empty '(b ... a ...) '(b b) (list (make-bindings empty)))
    
    (test-empty '((name y b) ... (name x a) ...) '() 
                (list (make-bindings (list (make-rib 'x '())
                                           (make-rib 'y '())))))
    (test-empty '((name y b) ... (name x a) ...) '(a)
                (list (make-bindings (list (make-rib 'x '(a))
                                           (make-rib 'y '())))))
    (test-empty '((name y b) ... (name x a) ...) '(b) 
                (list (make-bindings (list (make-rib 'x '())
                                           (make-rib 'y '(b))))))
    (test-empty '((name y b) ... (name x a) ...) '(b b a a) 
                (list (make-bindings (list (make-rib 'x '(a a))
                                           (make-rib 'y '(b b))))))
    (test-empty '((name y a) ... (name x a) ...) '(a) 
                (list (make-bindings (list (make-rib 'x '())
                                           (make-rib 'y '(a))))
                      (make-bindings (list (make-rib 'x '(a))
                                           (make-rib 'y '())))))
    (test-empty '((name y a) ... (name x a) ...) '(a a) 
                (list (make-bindings (list (make-rib 'x '())
                                           (make-rib 'y '(a a))))
                      (make-bindings (list (make-rib 'x '(a))
                                           (make-rib 'y '(a))))
                      (make-bindings (list (make-rib 'x '(a a))
                                           (make-rib 'y '())))))

    (test-ab '(bb_y ... aa_x ...) '() 
             (list (make-bindings (list (make-rib 'aa_x '())
                                        (make-rib 'bb_y '())))))
    (test-ab '(bb_y ... aa_x ...) '(a)
             (list (make-bindings (list (make-rib 'aa_x '(a))
                                        (make-rib 'bb_y '())))))
    (test-ab '(bb_y ... aa_x ...) '(b) 
             (list (make-bindings (list (make-rib 'aa_x '())
                                        (make-rib 'bb_y '(b))))))
    (test-ab '(bb_y ... aa_x ...) '(b b a a) 
             (list (make-bindings (list (make-rib 'aa_x '(a a))
                                        (make-rib 'bb_y '(b b))))))
    (test-ab '(aa_y ... aa_x ...) '(a) 
             (list (make-bindings (list (make-rib 'aa_x '())
                                        (make-rib 'aa_y '(a))))
                   (make-bindings (list (make-rib 'aa_x '(a))
                                        (make-rib 'aa_y '())))))
    (test-ab '(aa_y ... aa_x ...) '(a a) 
             (list (make-bindings (list (make-rib 'aa_x '())
                                        (make-rib 'aa_y '(a a))))
                   (make-bindings (list (make-rib 'aa_x '(a))
                                        (make-rib 'aa_y '(a))))
                   (make-bindings (list (make-rib 'aa_x '(a a))
                                        (make-rib 'aa_y '())))))

    (test-empty '((name x number) ...) '(1 2) (list (make-bindings (list (make-rib 'x '(1 2))))))
    
    (test-empty '(a ...) '(b) #f)
    (test-empty '(a ... b ...) '(c) #f)
    (test-empty '(a ... b) '(b c) #f)
    (test-empty '(a ... b) '(a b c) #f)
    
    (test-xab 'exp 1 (list (make-bindings null)))
    (test-xab 'exp '(+ 1 2) (list (make-bindings null)))
    (test-xab '(in-hole ctxt any)
              '1
              (list (make-bindings (list (make-rib 'hole (make-hole-binding 1 '() #f))))))
    (test-xab '(in-hole ctxt any) 
              '(+ 1 2)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding 1 '(cdr car) #f))))
                    (make-bindings (list (make-rib 'hole (make-hole-binding 2 '(cdr cdr car) #f))))
                    (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '() #f))))))
    (test-xab '(in-hole ctxt (+ number number))
              '(+ (+ 1 2) (+ 3 4))
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '(cdr car) #f))))
                    (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 3 4) '(cdr cdr car) #f))))))
    
    (test-empty '(in-hole ((z hole)) any)
                '((z a))
                (list (make-bindings (list (make-rib 'hole (make-hole-binding 'a '(car cdr car) #f))))))
    (test-empty '(in-hole (z ... hole z ...) any)
                '(z z)
                (list 
                 (make-bindings (list (make-rib 'hole (make-hole-binding 'z '(car) #f))))
                 (make-bindings (list (make-rib 'hole (make-hole-binding 'z '(cdr car) #f))))))
    (test-empty '(in-hole (z ... hole z ...) any)
                '(z z z)
                (list 
                 (make-bindings (list (make-rib 'hole (make-hole-binding 'z '(car) #f))))
                 (make-bindings (list (make-rib 'hole (make-hole-binding 'z '(cdr car) #f))))
                 (make-bindings (list (make-rib 'hole (make-hole-binding 'z '(cdr cdr car) #f))))))
    
    (test-empty '(z (in-hole (z hole) a))
                '(z (z a))
                (list 
                 (make-bindings (list (make-rib 'hole (make-hole-binding 'a '(cdr car) #f))))))
    
    (test-empty `(+ 1 (side-condition any ,(lambda (bindings) #t)))
                '(+ 1 b)
                (list (make-bindings '())))
    (test-empty `(+ 1 (side-condition any ,(lambda (bindings) #f)))
                '(+ 1 b)
                #f)
    
    (test-empty `(+ 1 (side-condition b ,(lambda (bindings) #t)))
                '(+ 1 b)
                (list (make-bindings '())))
    (test-empty `(+ 1 (side-condition a ,(lambda (bindings) #t)))
                '(+ 1 b)
                #f)

    (test-empty `(side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)))
                'a
                (list 
                 (make-bindings (list (make-rib 'x 'a)))))

    (test-empty `(+ 1 (side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a))))
                '(+ 1 a)
                (list 
                 (make-bindings (list (make-rib 'x 'a)))))

    (test-empty `(side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)))
                'b
                #f)
    
    (test-empty `(+ 1 (side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a))))
                '(+ 1 b)
                #f)

    (test-xab 'exp_1
              '(+ 1 2)
              (list (make-bindings (list (make-rib 'exp_1 '(+ 1 2))))))
    (test-xab '(exp_1 exp_2)
              '((+ 1 2) (+ 3 4))
              (list (make-bindings (list (make-rib 'exp_1 '(+ 1 2)) (make-rib 'exp_2 '(+ 3 4))))))
    (test-xab '(exp_1 exp_1)
              '((+ 1 2) (+ 3 4))
              #f)
    (test-xab 'nesting-names
              'b
              (list (make-bindings (list))))
    (test-xab 'nesting-names
              '(a b)
              (list (make-bindings (list))))
    (test-xab 'nesting-names
              '(a (a b))
              (list (make-bindings (list))))
    (test-xab '((name x a) nesting-names)
              '(a (a (a b)))
              (list (make-bindings (list (make-rib 'x 'a)))))
    (test-xab 'nesting-names
              '(a (a (a (a b))))
              (list (make-bindings (list))))
    
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ 1 2)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '() #f))))))
    
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ 1 (+ 5 6))
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 5 6) '(cdr cdr car) #f))))))
    
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ (+ (+ 1 2) 3) 4)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '(cdr car cdr car) #f))))))
    
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ (+ 3 (+ 1 2)) 4)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '(cdr car cdr cdr car) #f))))))
    
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ (+ (+ 1 2) (+ 3 4)) (+ 5 6))
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 5 6) '(cdr cdr car) #f))))
                    (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '(cdr car cdr car) #f))))
                    (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 3 4) '(cdr car cdr cdr car) #f))))))
    
    (run-test
     'compatible-context-language1
     (build-compatible-context-language
      (mk-hasheq '((exp . ()) (ctxt . ())))
      (list (make-nt 'exp
                     (list (make-rhs '(+ exp exp))
                           (make-rhs 'number)))
            (make-nt 'ctxt
                     (list (make-rhs '(+ ctxt exp))
                           (make-rhs '(+ exp ctxt))
                           (make-rhs 'hole)))))
     (list
      (make-nt 'exp-exp 
               (list (make-rhs 'hole) 
                     (make-rhs `(+ (cross exp-exp) exp)) 
                     (make-rhs `(+ exp (cross exp-exp)))))
      (make-nt 'exp-ctxt
               (list (make-rhs `(+ (cross exp-ctxt) exp))
                     (make-rhs `(+ ctxt (cross exp-exp)))
                     (make-rhs `(+ (cross exp-exp) ctxt))
                     (make-rhs `(+ exp (cross exp-ctxt)))))
      (make-nt 'ctxt-exp
               (list (make-rhs `(+ (cross ctxt-exp) exp))
                     (make-rhs `(+ exp (cross ctxt-exp)))))
      (make-nt 'ctxt-ctxt
               (list (make-rhs 'hole)
                     (make-rhs `(+ (cross ctxt-ctxt) exp))
                     (make-rhs `(+ ctxt (cross ctxt-exp)))
                     (make-rhs `(+ (cross ctxt-exp) ctxt))
                     (make-rhs `(+ exp (cross ctxt-ctxt)))))))
    
    (run-test
     'compatible-context-language2
     (build-compatible-context-language
      (mk-hasheq '((m . ()) (v . ())))
      (list (make-nt 'm (list (make-rhs '(m m)) (make-rhs '(+ m m)) (make-rhs 'v)))
            (make-nt 'v (list (make-rhs 'number) (make-rhs '(lambda (x) m))))))
     (list
      (make-nt 'm-m
               (list
                (make-rhs 'hole)
                (make-rhs (list (list 'cross 'm-m) 'm))
                (make-rhs (list 'm (list 'cross 'm-m)))
                (make-rhs (list '+ (list 'cross 'm-m) 'm))
                (make-rhs (list '+ 'm (list 'cross 'm-m)))
                (make-rhs (list 'cross 'm-v))))
      (make-nt 'm-v (list (make-rhs (list 'lambda (list 'x) (list 'cross 'm-m)))))
      (make-nt 'v-m
               (list
                (make-rhs (list (list 'cross 'v-m) 'm))
                (make-rhs (list 'm (list 'cross 'v-m)))
                (make-rhs (list '+ (list 'cross 'v-m) 'm))
                (make-rhs (list '+ 'm (list 'cross 'v-m)))
                (make-rhs (list 'cross 'v-v))))
      (make-nt 'v-v (list (make-rhs 'hole) (make-rhs (list 'lambda (list 'x) (list 'cross 'v-m)))))))
    
    (run-test
     'compatible-context-language3
     (build-compatible-context-language
      (mk-hasheq '((M . ()) (seven . ())))
      (list (make-nt 'M (list (make-rhs '(M seven M)) (make-rhs 'number)))
            (make-nt 'seven (list (make-rhs 7)))))
     `(,(make-nt
         'm-m
         `(,(make-rhs 'hole) ,(make-rhs `((cross m-m) seven m)) ,(make-rhs `(m (cross m-seven) m)) ,(make-rhs `(m seven (cross m-m)))))
       ,(make-nt 'm-seven `())
       ,(make-nt
         'seven-m
         `(,(make-rhs `((cross seven-m) seven m)) ,(make-rhs `(m (cross seven-seven) m)) ,(make-rhs `(m seven (cross seven-m)))))
       ,(make-nt 'seven-seven `(,(make-rhs 'hole)))))
    
    
    (test-xab '(in-hole (cross exp) (+ number number))
              '(+ (+ 1 2) 3)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding (list '+ 1 2) (list 'cdr 'car) #f))))))
    
    (unless failure?
      (fprintf (current-error-port) "All ~a tests passed.\n" test-count)))

  ;; mk-hasheq : (listof (cons sym any)) -> hash-table
  ;; builds a hash table that has the bindings in assoc-list
  (define (mk-hasheq assoc-list)
    (let ([ht (make-hash-table)])
      (for-each
       (lambda (a)
         (hash-table-put! ht (car a) (cdr a)))
       assoc-list)
      ht))
  
  ;; test-empty : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with the empty language produces ans.
  (define (test-empty pat exp ans)
    (run-test
     `(match-pattern (compile-pattern (make-compiled-lang '() (make-hash-table) (make-hash-table)) ',pat) ',exp)
     (match-pattern (compile-pattern (make-compiled-lang '() (make-hash-table) (make-hash-table)) pat) exp)
     ans))
  
  (define xab-lang #f)
  ;; test-xab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  (define (test-xab pat exp ans)
    (unless xab-lang
      (set! xab-lang
            (compile-language (list (make-nt 'exp
                                             (list (make-rhs '(+ exp exp))
                                                   (make-rhs 'number)))
                                    (make-nt 'ctxt
                                             (list (make-rhs '(+ ctxt exp))
                                                   (make-rhs '(+ exp ctxt))
                                                   (make-rhs 'hole)))
                                    
                                    (make-nt 'ec-multi
                                             (list (make-rhs 'hole)
                                                   (make-rhs '(in-named-hole+ xx ec-one ec-multi))))
                                    (make-nt 'ec-one
                                             (list (make-rhs '(+ (hole xx) exp))
                                                   (make-rhs '(+ exp (hole xx)))))
                                    
                                    (make-nt 'nesting-names
                                             (list (make-rhs '(a (name x nesting-names)))
                                                   (make-rhs 'b)))))))
    (run-test
     `(match-pattern (compile-pattern xab-lang ',pat) ',exp)
     (match-pattern (compile-pattern xab-lang pat) exp)
     ans))
  
  
  (define ab-lang #f)
  ;; test-xab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  (define (test-ab pat exp ans)
    (unless ab-lang
      (set! ab-lang
            (compile-language (list (make-nt 'aa
                                             (list (make-rhs 'a)))
                                    (make-nt 'bb
                                             (list (make-rhs 'b)))))))
    (run-test
     `(match-pattern (compile-pattern ab-lang ',pat) ',exp)
     (match-pattern (compile-pattern ab-lang pat) exp)
     ans))
  
  ;; test-ellipses : sexp sexp -> void
  (define (test-ellipses pat ans)
    (run-test
     `(rewrite-ellipses ',pat (lambda (x) x))
     (rewrite-ellipses pat (lambda (x) x))
     ans))
  
  ;; run-test : sexp any any
  ;; compares ans with expected. If failure,
  ;; prints info about the test and sets `failure?' to #t.
  (define failure? #f)
  (define test-count 0)
  (define (run-test symbolic ans expected)
    (set! test-count (+ test-count 1))
    (cond
      [(equal/bindings? ans expected)
       '(printf "passed: ~s\n" symbolic)]
      [else 
       (set! failure? #t)
       (fprintf (current-error-port)
                "    test: ~s\nexpected: ~e\n     got: ~e\n"
                symbolic expected ans)]))
  
  ;; equal/bindings? : any any -> boolean
  ;; compares two sexps (with embedded bindings) for equality.
  ;; uses an order-insensitive comparison for the bindings
  (define (equal/bindings? fst snd)
    (let loop ([fst fst]
               [snd snd])
      (cond
        [(pair? fst)
         (and (pair? snd) 
              (loop (car fst) (car snd))
              (loop (cdr fst) (cdr snd)))]
        [(bindings? fst)
         (and (bindings? snd)
              (let ([fst-table (bindings-table fst)]
                    [snd-table (bindings-table snd)])
                (and (= (length fst-table)
                        (length snd-table))
                     (andmap
                      loop
                      (quicksort fst-table rib-lt)
                      (quicksort snd-table rib-lt)))))]
        [else (equal? fst snd)])))
  
  ;; rib-lt : rib rib -> boolean
  (define (rib-lt r1 r2) (string<=? (symbol->string (rib-name r1))
                                    (symbol->string (rib-name r2)))))
