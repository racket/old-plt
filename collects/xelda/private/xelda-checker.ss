(module xelda-checker mzscheme
  (require (lib "class.ss"))
  (require (lib "list.ss"))
  (require (lib "match.ss"))
  
  (require "xl-util.ss")
  (require "formula.ss")
  (require "parser.ss")
  (require "xelda-lib.ss")
  (require "xelda-com.ss")
  
  (provide unit-check)
  
  (define (unit-check gui)
    (let* ([all-formula-texts
            (begin
              (send gui update-status (format "+++SPREADSHEET PREPROCESSING BEGIN+++~n"))
              (send gui update-status "Preprocessing all formula texts.... ")
              (iterate-over-worksheet
               (lambda (cell) (get-cell-formula cell))
               (lambda (formula)
                 (and (not (string=? formula ""))
                      (eq? (string-ref formula 0) #\=)))))]
           [all-names
            (begin
              (send gui next-update-status "Preprocessing all names")
              (iterate-over-worksheet
               (lambda (cell) (get-cell-name cell))
               (lambda (s) (not (string=? s "")))))]
           [symbol-table
            (begin
              (send gui next-update-status "Preprocessing symbol-table")
              (map (lambda (pr) (list (cadr pr) (car pr))) all-names))]
           [parser (make-parser symbol-table)]
           [formulas
            (begin
              (send gui next-update-status "Preprocessing all formulas")
              (map (lambda (f)
                     (let ([cell (car f)]
                           [form (cadr f)])
                       (cond
                         [(tbl-top? form)
                          (list cell
                                (make-tbl-top 
                                 (formula-name form)
                                 (append (formula-dependencies form)
                                         (list (top-of cell)
                                               (get-formula-loc-left cell
                                                                     parser
                                                                     all-formula-texts)))
                                 (tbl-top-input-cell form)))]
                         [(tbl-left? form)
                          (list cell
                                (make-tbl-left 
                                 (formula-name form)
                                 (append (formula-dependencies form)
                                         (list (left-of cell)
                                               (get-formula-loc-up cell
                                                                   parser
                                                                   all-formula-texts)))
                                 (tbl-left-input-cell form)))]
                         [else f])))
                   (map (lambda (frm-text)
                          (list (car frm-text)
                                (call-parser parser (cadr frm-text))))
                        all-formula-texts)))]
           [all-formulas (formula-sort formulas)]
           [circular-non-circular (split-circular-non-circular all-formulas)]
           [all-circular-formulas (first circular-non-circular)]
           [all-non-circular-formulas (second circular-non-circular)]
           [good-and-bad-units
            (split-list
             (begin
               (send gui next-update-status "Preprocessing all units")
               (iterate-over-worksheet
                (lambda (cell)
                  (with-handlers
                      ([void (lambda _ 'bad-unit-format)])
                    (let ([comment-txt (get-cell-comment cell)])
                      (cond
                        [(string=? "" comment-txt) empty]
                        [else (parse-unit comment-txt)]))))
                (lambda (u) (and (not (empty? u)) (pair? u)))))
             (lambda (e) (dim? (second e))))]
           [all-units
            (map (lambda (entry)
                   (list (car entry) (canonicalize-units (cadr entry))))
                 (first good-and-bad-units))]
           [bad-format-units (second good-and-bad-units)]
           [hashed-units (make-hash-table)]
           [hashed-vars (make-hash-table)]
           [hashed-constraints-vars (make-hash-table)])
      (send gui update-status (format "done~n"))
      (send gui update-status (format "+++SPREADSHEET PREPROCESSING END+++~n"))
      
      ;; When bad formatted units are present, don't do unit checking
      (cond
        [(not (empty? bad-format-units)) 
         (send gui set-bad-format-cells
               (map (lambda (u) (car u)) bad-format-units))]
        [else      
         ;; Place all non-formulas units in the computed units(hashed-units)
         (init-hash hashed-units
                    (filter (lambda (u) (not (assq (car u) all-formulas)))
                            all-units))
         
         (send gui update-status (format "+++UNIT CHECKING BEGIN+++~n"))
         
         ;; Compute non-circular formula units
         (send gui update-status "Checking non-circular formulas.... ")
         (for-each (lambda (f)
                     (compute-cell-unit f hashed-units 
                                        all-formulas all-formula-texts parser))
                   all-non-circular-formulas)
         
         ;; Compute circular formula units. Start by assigning unit variables.
         (send gui next-update-status "Assigning dimension variables for circular formulas");
         (assign-variables hashed-vars hashed-units all-circular-formulas)
         
         ;; Generate constraints
         (send gui next-update-status "Creating constraints")
         (let* ([constraints (create-constraints 
                              hashed-units hashed-vars 
                              all-formulas all-circular-formulas all-formula-texts parser)]
                [eq-app-cs (split-list constraints (lambda (c) (eq? (first c) '=)))]
                [equality-constraints (first eq-app-cs)]
                [append-constraints (second eq-app-cs)])
           ;; Create equivalence classes from the equality constraints
           (send gui next-update-status "Creating equivalence classes")
           (prune-constraints equality-constraints hashed-constraints-vars)
           
           ;; Replace variables in append-constraints with their equivalence class
           ;; representative variable or unit (unit only for operands where possible)
           (let ([append-constraints (replace-equiv-in-constraints hashed-constraints-vars
                                                                   append-constraints)])
             ;; Flatten the append constraints
             (send gui next-update-status "Flattening constraints")
             (let ([append-constraints (flatten-constraints append-constraints)])
               ;; Solve the constraints
               (solve-constraints append-constraints
                                  hashed-constraints-vars hashed-vars hashed-units)
               (send gui update-status (format "done~n"))
               (send gui update-status (format "+++UNIT CHECKING END+++~n"))
               
               ;; Report any error back to the gui
               (send gui set-computation-errors 
                     (computation-errors hashed-units all-formula-texts all-formulas))
               (send gui set-mismatch-errors 
                     (mismatch-errors hashed-units all-units all-formula-texts all-formulas))
               )))])))
  
  (define (compute-cell-unit _formula hashed-units all-formulas all-formula-texts parser)
    (let ([cell-name (car _formula)]
          [formula (cadr _formula)])
      (when (not (in-hash? hashed-units cell-name))
        (hash-table-put! 
         hashed-units cell-name
         (let ([unit (compute-formula formula cell-name hashed-units 
                                      all-formulas all-formula-texts parser)])
           (cond ((> (length unit) 1)
                  (filter (lambda (u) (not (eq? (car u) 'empty_unit)))
                          unit))
                 (else unit)))))))
  
  (define (compute-formula formula cell-loc hashed-units all-formulas all-formula-texts parser)
    (match formula
      [($ xl-number name deps val) (empty-unit)]
      [($ cell-ref name cell-name)
       (cond
         [(in-hash? hashed-units name)
          (hash-table-get hashed-units name)]
         [(not (not (assq name all-formulas)))
          (let ([u (compute-formula (cadr (assq name all-formulas))
                                    name
                                    hashed-units
                                    all-formulas
                                    all-formula-texts
                                    parser)])
            (hash-table-put! hashed-units name u)
            u)]
         [(string=? "" (get-cell-text (formula-name formula)))
          (list (list 'error/empty-formula cell-loc))]
         [else (empty-unit)])]
      [($ named-cell-ref name cell-name actual-name)
       (cond
         [(in-hash? hashed-units name)
          (hash-table-get hashed-units name)]
         [(not (not (assq name all-formulas)))
          (let ([u (compute-formula (cadr (assq name all-formulas))
                                    name
                                    hashed-units
                                    all-formulas
                                    all-formula-texts
                                    parser)])
            (hash-table-put! hashed-units name u)
            u)]
         [(string=? "" (get-cell-text (formula-name formula)))
          (list (list 'error/empty-formula cell-loc))]
         [else (empty-unit)])]
      [($ binary-op name deps op arg1 arg2)
       (let ([arg1-unit (compute-formula arg1
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)]
             [arg2-unit (compute-formula arg2
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)])
         (case op
           [(+ -) (check-equal-units (list arg1-unit arg2-unit) cell-loc)]
           [(*) (let ([result (gen-mult-units arg1-unit arg2-unit cell-loc)])
                  (cond ((null? result) (empty-unit))
                        (else result)))]
           [(/) (let ([result (gen-div-units arg1-unit arg2-unit cell-loc)])
                  (cond ((null? result) (empty-unit))
                        (else result)))]
           [(^) (gen-exp-units arg1-unit
                               (cond
                                 [(xl-number? arg2) (xl-number-val arg2)]
                                 [(or (cell-ref? arg2)
                                      (named-cell-ref? arg2))
                                  (get-cell-value (formula-name arg2))]
                                 [else 'bad-exp]) cell-loc)]))]
      [($ boolean-op name deps op arg1 arg2)
       (let ([arg1-unit (compute-formula arg1
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)]
             [arg2-unit (compute-formula arg2 
                                         cell-loc
                                         hashed-units
                                         all-formulas
                                         all-formula-texts
                                         parser)])
         (if (equal? arg1-unit arg2-unit)
             (empty-unit)
             (list (list 'error/bool-non-empty cell-loc))))]
      [($ unary-op name deps op arg)
       (let ([arg-unit (compute-formula arg
                                        cell-loc
                                        hashed-units
                                        all-formulas
                                        all-formula-texts
                                        parser)])
         (case op
           [(+ -) arg-unit]
           [else (error op "Unknown unary op")]))]
      [($ tbl-left name deps input-cell)
       (let ([left-cell (left-of cell-loc)]
             [formula-cell (get-formula-loc-up cell-loc parser all-formula-texts)]
             [left-cell-unit empty]
             [input-cell-unit (compute-formula input-cell
                                               cell-loc
                                               hashed-units
                                               all-formulas
                                               all-formula-texts
                                               parser)])
         (if (in-hash? hashed-units left-cell)
             (set! left-cell-unit (hash-table-get hashed-units left-cell))
             (set! left-cell-unit (compute-cell-unit (assoc left-cell all-formulas)
                                                     hashed-units
                                                     all-formulas all-formula-texts parser)))
         (compute-formula (replace-in-formula
                           (cadr (assq formula-cell all-formulas))
                           (formula-name input-cell) left-cell)
                          cell-loc
                          hashed-units
                          all-formulas
                          all-formula-texts
                          parser))]
      [($ tbl-top name deps input-cell)
       (let ([input-cell-unit (compute-formula input-cell
                                               cell-loc
                                               hashed-units
                                               all-formulas
                                               all-formula-texts
                                               parser)]
             [top-cell (top-of cell-loc)]
             [formula-cell (get-formula-loc-left cell-loc parser all-formula-texts)]
             [top-cell-unit empty])
         (if (in-hash? hashed-units top-cell)
             (set! top-cell-unit (hash-table-get hashed-units top-cell))
             (set! top-cell-unit (compute-cell-unit (assoc top-cell all-formulas)
                                                    hashed-units
                                                    all-formulas all-formula-texts parser)))
         (compute-formula (replace-in-formula
                           (cadr (assq formula-cell all-formulas))
                           (formula-name input-cell) top-cell)
                          cell-loc
                          hashed-units
                          all-formulas
                          all-formula-texts
                          parser))]
      [($ application name deps fun args) 
       (let ([arg-units (map (lambda (a)
                               (compute-formula a 
                                                cell-loc
                                                hashed-units
                                                all-formulas
                                                all-formula-texts
                                                parser)) args)])
         (case fun
           [(average sum min max mina maxa) (check-equal-units arg-units cell-loc)]
           [(not or and)
            (if (andmap empty-unit? arg-units)
                (empty-unit)
                (list (list 'error/bool-non-empty cell-loc)))]
           [(if) (cond
                   ((= 3 (length args))
                    (let ([if-unit (second arg-units)]
                          [else-unit (third arg-units)]
                          [test-unit (first arg-units)])
                      (if (empty-unit? test-unit)
                          (check-equal-units (list if-unit else-unit) cell-loc)
                          (list (list 'error/if-bool-non-empty cell-loc)))))
                   (else (error fun "illegal use")))]
           [(abs) (cond
                    ((= 1 (length args)) (first arg-units))
                    (else (error fun "illegal use")))]
           [(ceiling round)
            (cond
              ((= 2 (length args))
               (let ([num-unit (first arg-units)]
                     [prec-unit (second arg-units)])
                 (cond 
                   ((empty-unit? prec-unit) num-unit)
                   (else (list (list 'error/non-empty-precision cell-loc))))))
              (else (error fun "illegal use")))]
           [(large) (let ([split-units (split-large-args arg-units)])
                      (cond
                        ((= 1 (length (car split-units)))
                         (check-equal-units (cadr split-units) cell-loc))
                        (else (list (list 'error/large-empty-unit cell-loc)))))]
           [(acos acosh asin asinh atan atanh cos cosh sin sinh tan tanh exp)
            (let ([arg-unit (first arg-units)])
              (cond ((= 1 (length args))
                     (if (empty-unit? arg-unit)
                         arg-unit
                         (list (list 'error/non-empty-argument cell-loc))))
                    (else (error fun "illegal use"))))]
           [(sqrt) (let ([arg-unit (first arg-units)])
                     (cond
                       ((= 1 (length args))
                        (cond [(empty-unit? arg-unit) arg-unit]
                              [(andmap (lambda (s_u)
                                         (= (modulo (cadr s_u) 2) 0))
                                       arg-unit)
                               (map (lambda (s_u)
                                      (list (car s_u) (/ (cadr s_u) 2)))
                                    arg-unit)]
                              [else
                               (list (list 'error/invalid-sqrt-dimension cell-loc))]))
                       (else (error fun "illegal use"))))]
           [(fact ln log) (let ([arg-unit (first arg-units)])
                            (cond
                              ((= 1 (length args))
                               (if (empty-unit? arg-unit)
                                   arg-unit
                                   (list (list 'error/non-unitless-argument cell-loc))))
                              (else (error fun "illegal use"))))]
           [(isnumber)
            (cond ((= 1 (length arg-units)) (empty-unit))
                  (else (error fun "illegal use")))]
           [(median stdev) (check-equal-units arg-units cell-loc)]
           [(npv) (cond
                    [(>= (length args) 2)
                     (cond [(empty-unit? (first arg-units))
                            (let ([u (first (rest arg-units))])
                              (foldl (lambda (curr prev) (check-equal-units curr prev))
                                     u
                                     (rest (rest arg-units))))]
                           [else
                            (list (list 'error/npv-rate-non-dimensionless cell-loc))])]
                    [else (error fun "illegal use")])]
           [(intercept)
            (cond
              [(> (length arg-units) 0)
               (let* ([x-unit (first arg-units)]
                      [xy-units (split-list (rest arg-units) (lambda (u) (equal? u x-unit)))]
                      [x-units (cons x-unit (first xy-units))]
                      [y-units (second xy-units)])
                 (cond [(= (length x-units) (length y-units))
                        (check-equal-units y-units cell-loc)]
                       [else (list (list 'error/intercept-invalid-points cell-loc))]))]
              [else (empty-unit)])]
           [(count) (empty-unit)]
           [(frequency)
            (let ([args-unit (check-equal-units arg-units cell-loc)])
              (cond [(is-error-unit? args-unit) args-unit]
                    [else (empty-unit)]))]
           [(mod)
            (cond ((= 2 (length arg-units)) (first arg-units))
                  (else (error fun "illegal use")))]
           [(na pi today now) (cond ((empty? args) (empty-unit))
                                    (else (error fun "illegal use")))]
           [(mmult) (let* ([formula-text (lookup-formula-text cell-loc all-formula-texts)]
                           [Ms (identify-matrices
                                (substring formula-text 7
                                           (sub1 (string-length formula-text))))])
                      (cond
                        [(= 2 (length Ms))
                         (let* ([cell-xy (get-cell-row-col cell-loc)]
                                [M1-row (car cell-xy)]
                                [M2-col (cadr cell-xy)]
                                [M1-row-unit (check-equal-units
                                              (matrix-row-units (car Ms) M1-row hashed-units)
                                              cell-loc)]
                                [M2-col-unit (check-equal-units
                                              (matrix-col-units (cadr Ms) M2-col hashed-units)
                                              cell-loc)])
                           (cond
                             [(is-error-unit? M1-row-unit) M1-row-unit]
                             [(is-error-unit? M2-col-unit) M2-col-unit]
                             [else (gen-mult-units M1-row-unit M2-col-unit cell-loc)]))]
                        [else (error fun "illegal use")]))]
           [else (empty-unit)]))]))  ;; unimplemented functions -> empty unit
  
  (define (replace-in-formula formula orig new)
    (match formula
      [($ xl-number name deps val) formula]
      [($ cell-ref name cell-name)
       (cond
         [(eq? name orig) (make-cell-ref new empty)]
         [else formula])]
      [($ named-cell-ref name cell-name actual-name) formula]
      [($ binary-op name deps op arg1 arg2)
       (make-binary-op name deps op
                       (replace-in-formula arg1 orig new)
                       (replace-in-formula arg2 orig new))]
      [($ boolean-op name deps op arg1 arg2)
       (make-boolean-op name deps op
                        (replace-in-formula arg1 orig new)
                        (replace-in-formula arg2 orig new))]
      [($ unary-op name deps op arg)
       (make-unary-op name deps op (replace-in-formula arg orig new))]
      [($ tbl-left name deps input-cell)
       (cond [(eq? input-cell orig)
              (make-tbl-left name deps new)]
             [else formula])]
      [($ tbl-top name deps input-cell)
       (cond [(eq? input-cell orig)
              (make-tbl-top name deps new)]
             [else formula])]
      [($ application name deps fun args) 
       (let ([replaced-args
              (map (lambda (a)
                     (replace-in-formula a orig new)) args)])
         (make-application name deps fun replaced-args))]
      [else formula]))
    
  (define (remove-duplicates lst)
    (cond
      [(null? lst) null]
      [(member (car lst) (cdr lst)) (remove-duplicates (cdr lst))]
      [else (cons (car lst) (remove-duplicates (cdr lst)))]))
  
  (define (formula-sort fs)
    (quicksort fs
               (lambda (f1 f2)
                 (let* ([name (car f1)]
                        [deps (formula-dependencies (cadr f2))])
                   (memq name deps)))))
  
  (define (get-formula-loc-up loc parser all-formula-texts)
    (let ([formula (call-parser parser (cadr (assoc loc all-formula-texts)))])
      (cond ((not (tbl-left? formula)) loc)
            (else (let* ([xy (cellref->numbers loc)]
                         [x (sub1 (car xy))]
                         [y (sub1 (cadr xy))])
                    (get-formula-loc-up (numbers->cellref (list x y))
                                        parser all-formula-texts))))))
  
  (define (get-formula-loc-left loc parser all-formula-texts)
    (let ([formula (call-parser parser (cadr (assoc loc all-formula-texts)))])
      (cond ((not (tbl-top? formula)) loc)
            (else (let* ([xy (cellref->numbers loc)]
                         [x (- (car xy) 2)]
                         [y (cadr xy)])
                    (get-formula-loc-left (numbers->cellref (list x y))
                                          parser all-formula-texts))))))

  (define (assign-variables hashed-vars hashed-units all-circular-formulas)
    (for-each (lambda (f)
                (let ([a_s (gen-alpha-var)])
                  (hash-table-put! hashed-vars a_s 
                                   (list (car f) (list (list 'error/missing-dimensions (car f)))))
                  (hash-table-put! hashed-units (car f) a_s)))
              all-circular-formulas))
  
  (define (gen-alpha-var)
    (string->symbol (string-append "a_" (symbol->string (gensym)))))
  
  (define (gen-beta-var)
    (string->symbol (string-append "b_" (symbol->string (gensym)))))
  
  (define (alpha-var? a)
    (and (symbol? a)
         (let [(l (string->list (symbol->string a)))]
           (and (> (length l) 3)
                (equal? (first l) #\a)
                (equal? (second l) #\_)
                (equal? (third l) #\g)))))
  
  (define (beta-var? b)
    (and (symbol? b)
         (let [(l (string->list (symbol->string b)))]
           (and (> (length l) 3)
                (equal? (first l) #\b)
                (equal? (second l) #\_)
                (equal? (third l) #\g)))))
  
  (define (dim-var? s) (or (alpha-var? s) (beta-var? s)))
  
  (define (dim? u) (and (list? u)
                        (andmap (lambda (u_)
                                  (and (list? u_)
                                       (= 2 (length u_))
                                       (symbol? (car u_))
                                       (integer? (cadr u_)))) u)))
   
  (define (constraint-var c) (second c))
  (define (constraint-operator c) (first c))
  (define (constraint-left-side c) (third c))
  (define (constraint-right-side c) (fourth c))
 
  (define (create-constraints hashed-units hashed-vars 
                              all-formulas all-circular-formulas all-formula-texts parser)
    (foldl (lambda (f constraints)
             (let ([a_s (hash-table-get hashed-units (car f))])
               (create-constraints-for-formula a_s (cadr f) (car f) constraints
                                               hashed-units hashed-vars 
                                               all-formulas all-formula-texts parser)))
           empty all-circular-formulas))
  
  (define (create-constraints-for-formula sym formula cell-loc formula-constraints
                                          hashed-units  hashed-vars 
                                          all-formulas all-formula-texts parser)
    (match formula
      [($ xl-number name deps val)
       (cons (list '= sym (empty-unit)) formula-constraints)]
      [($ cell-ref name cell-name)
       (cond [(in-hash? hashed-units name)
              (cons
               (list '= sym (hash-table-get hashed-units name))
               formula-constraints)]
             [else (cons (list '= sym (empty-unit)) formula-constraints)])]
      [($ named-cell-ref name cell-name actual-name)
       (cond [(in-hash? hashed-units name)
              (cons
               (list '= sym (hash-table-get hashed-units name))
               formula-constraints)]
             [else (cons (list '= sym (empty-unit)) formula-constraints)])]
      [($ binary-op name deps op arg1 arg2)
       (let* ([left-op (gen-beta-var)]
              [right-op (gen-beta-var)]
              [new-constraints
               (create-constraints-for-formula 
                left-op arg1 cell-loc
                (create-constraints-for-formula
                 right-op arg2 cell-loc formula-constraints hashed-units hashed-vars
                 all-formulas all-formula-texts parser)
                hashed-units hashed-vars all-formulas all-formula-texts parser)])
         (case op
           [(+ -)
            (cons (list '= sym left-op)
                  (cons (list '= sym right-op) new-constraints))]
           [(*)
            (cons (list '@ sym left-op right-op) new-constraints)]
           [(/)
            (cons (list '@/ sym left-op right-op) new-constraints)]
           [(^)
            (cons (list '= right-op (empty-unit)) new-constraints)]))]
      [($ boolean-op name deps op arg1 arg2)
       (let* ([left-op (gen-beta-var)]
              [right-op (gen-beta-var)]
              [new-constraints
               (create-constraints-for-formula 
                left-op arg1 cell-loc
                (create-constraints-for-formula
                 right-op arg2 cell-loc formula-constraints hashed-units hashed-vars
                 all-formulas all-formula-texts parser)
                hashed-units hashed-vars all-formulas all-formula-texts parser)])
         (cons (list '= left-op right-op)
               (cons (list '= sym (empty-unit)) new-constraints)))]
      [($ unary-op name deps op arg)
       (let ([op-sym (gen-beta-var)])
         (cons (list '= sym op-sym)
               (create-constraints-for-formula op-sym arg cell-loc formula-constraints
                                               hashed-units hashed-vars
                                               all-formulas all-formula-texts parser)))]
      [($ tbl-left name deps input-cell)
       (let* ([left-cell (left-of cell-loc)]
              [formula-cell (get-formula-loc-up cell-loc parser all-formula-texts)]
              [formula-sym (gen-beta-var)])
         (cons (list '= sym formula-sym)
               (create-constraints-for-formula 
                formula-sym 
                (replace-in-formula (cadr (assq formula-cell all-formulas))
                                    (formula-name input-cell) left-cell)
                formula-cell formula-constraints hashed-units hashed-vars
                all-formulas all-formula-texts parser)))]
      [($ tbl-top name deps input-cell)
       (let* ([top-cell (top-of cell-loc)]
              [formula-cell (get-formula-loc-left cell-loc parser all-formula-texts)]
              [formula-sym (gen-beta-var)])
         (cons (list '= sym formula-sym)
               (create-constraints-for-formula 
                formula-sym 
                (replace-in-formula (cadr (assq formula-cell all-formulas))
                                    (formula-name input-cell) top-cell)
                formula-cell formula-constraints hashed-units hashed-vars
                all-formulas all-formula-texts parser)))]
      [($ application name deps fun args)
       (let* ([arg-syms (map (lambda (a) (gen-beta-var)))]
              [arg-constraints
               (letrec ([gen-arg-constraints
                         (lambda (as as-syms)
                           (cond 
                             [(empty? as) formula-constraints]
                             [else
                              (create-constraints-for-formula 
                               (first as-syms) (first as) cell-loc
                               (gen-arg-constraints (rest as) (rest as-syms))
                               hashed-units hashed-vars 
                               all-formulas all-formula-texts parser)]))])
                 (gen-arg-constraints args arg-syms))])
         (case fun
           [(average sum min max mina maxa)
            (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                   arg-constraints arg-syms)]
           [(not or and)
            (cons (list '= sym (empty-unit))
                  (foldl (lambda (arg constraints) (cons (list '= sym empty-unit) constraints))
                         arg-constraints arg-syms))]               
           [(if) (cond
                   [(= 3 (length args))
                    (let ([if-sym (second arg-syms)]
                          [else-sym (third arg-syms)]
                          [test-sym (first arg-syms)])
                      (cons (list '= test-sym (empty-unit))
                            (cons (list '= if-sym else-sym)
                                  (cons (list '= sym if-sym) arg-constraints))))]
                   [else (error fun "illegal use")])]
           [(abs) (cond
                    [(= 1 (length args)) (cons (list '= sym (first arg-syms) arg-constraints))]
                    [else (error fun "illegal use")])]
           [(ceiling round)
            (cond
              [(= 2 (length args))
               (let ([num-sym (first arg-syms)]
                     [prec-sym (second arg-syms)])
                 (cons (list '= prec-sym (empty-unit))
                       (cons (list '= sym num-sym) arg-constraints)))]
              [else (error fun "illegal use")])]
           [(large) (let* ([reversed-syms (reverse arg-syms)]
                           [k-sym (first reversed-syms)]
                           [array-syms (rest reversed-syms)])
                      (cons (list '= k-sym (empty-unit))
                            (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                                   arg-constraints array-syms)))]
           [(acos acosh asin asinh atan atanh cos cosh sin sinh tan tanh exp)
            (let ([arg-sym (first arg-syms)])
              (cond [(= 1 (length args))
                     (cons (list '= arg-sym (empty-unit))
                           (cons (list '= sym (empty-unit)) arg-constraints))]
                    [else (error fun "illegal use")]))]
           [(sqrt) (let ([arg-sym (first arg-syms)])
                     (cond
                       [(= 1 (length args))
                        (cons (list '@/ sym arg-sym sym) arg-constraints)]
                       [else (error fun "illegal use")]))]
           [(fact ln log) (let ([arg-sym (first arg-syms)])
                            (cond
                              [(= 1 (length args))
                               (cons (list '= arg-sym (empty-unit))
                                     (cons (list '= sym (empty-unit)) arg-constraints))]
                              [else (error fun "illegal use")]))]
           [(isnumber)
            (cond [(= 1 (length arg-syms)) (cons (list '= sym (empty-unit)) arg-constraints)]
                  [else (error fun "illegal use")])]
           [(median stdev)
            (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                   arg-constraints arg-syms)]
           [(npv)
            (cond [(>= (length args) 2)
                   (cons (cons '= (first arg-syms) (empty-unit))
                         (foldl (lambda (arg constraints) (cons (list '= sym arg) constraints))
                                arg-constraints (rest arg-syms)))]
                  [else (error fun "illegal use")])]
           [(count) (cons (list '= sym (empty-unit)) arg-constraints)]
           [(frequency)
            (when (not (empty? arg-syms))
              (let ([first-sym (first arg-syms)])
                (cons (list '= sym (empty-unit))
                      (foldl (lambda (arg constraints) (cons (list '= first-sym arg) constraints))
                             arg-constraints (rest arg-syms)))))]
           [(mod)
            (cond [(= 2 (length args)) (cons (list '= sym (first arg-syms)) arg-constraints)]
                  [else (error fun "illegal use")])]
           [(na pi today now) 
            (cond [(empty? args) (cons (list '= sym (empty-unit)) arg-constraints)]
                  [else (error fun "illegal use")])]
           [(mmult) (let* ([formula-text (lookup-formula-text cell-loc all-formula-texts)]
                           [Ms (identify-matrices
                                (substring formula-text 7
                                           (sub1 (string-length formula-text))))])
                      (cond
                        [(= 2 (length Ms))
                         (let* ([cell-xy (get-cell-row-col cell-loc)]
                                [M1-row (car cell-xy)]
                                [M2-col (cadr cell-xy)]
                                [M1-row-sym (gen-beta-var)]
                                [M2-col-sym (gen-beta-var)]
                                [row-syms-constraints (matrix-row-syms (car Ms) M1-row
                                                                       hashed-units hashed-vars)]
                                [row-syms (first row-syms-constraints)]
                                [row-constraints (second row-syms-constraints)]
                                [col-syms-constraints (matrix-col-syms (cadr Ms) M2-col
                                                                       hashed-units hashed-vars)]
                                [col-syms (first col-syms-constraints)]
                                [col-constraints (second col-syms-constraints)])
                           (cons (list '@ sym M1-row-sym M2-col-sym)
                                 (foldl (lambda (arg constraints)
                                          (cons (list '= M1-row-sym arg) constraints))
                                        (foldl (lambda (arg constraints)
                                                 (cons (list '= M2-col-sym arg) constraints))
                                               (append arg-constraints row-constraints
                                                       col-constraints)
                                               col-syms)
                                        row-syms)))]
                        [else (error fun "illegal use")]))]         
           [else (cons (list '= sym (list (list 'error/unimplemented-function 1)))
                       arg-constraints)]))]))
  
  (define (prune-constraints equality-constraints hashed-constraints-vars)
    (for-each (lambda (c)
                (let* ([syms (cond [(or (dim? (second c)) (is-error-unit? (second c)))
                                    (list (third c) (second c))]
                                   [else (list (second c) (third c))])]
                       [sym1 (first syms)]
                       [sym2 (second syms)])
                  (if (or (dim? sym2) (is-error-unit? sym2))
                      (resolve-var-dim sym1 sym2 hashed-constraints-vars)
                      (resolve-var-var sym1 sym2 hashed-constraints-vars))))
              equality-constraints))
  
  (define (replace-equiv-in-constraints hashed-constraints-vars append-constraints)
    (let* ([replace-head
            (lambda (var)
              (cond
                [(in-hash? hashed-constraints-vars var)
                 (first (find-rep (first (hash-table-get hashed-constraints-vars var))))]
                [else var]))]
           [replace-operands
            (lambda (operands) 
              (map 
               (lambda (op)
                 (if (dim-var? op)
                     (cond
                       [(in-hash? hashed-constraints-vars op)
                        (let* ([rep (first 
                                     (find-rep 
                                      (first (hash-table-get hashed-constraints-vars op))))]
                               [rep-u (second (hash-table-get hashed-constraints-vars rep))])
                          (if (empty? rep-u)
                              rep
                              rep-u))]
                       [else op])
                     op))
               operands))]
           [map-replace 
            (lambda (c)
              (cons (constraint-operator c)
                    (cons (replace-head (second c))
                          (replace-operands (rest (rest c))))))])
      (remove-duplicates (map map-replace append-constraints))))
  
  (define (flatten-constraints append-constraints)
    (normalize-constraints 
     (map (lambda (c)
            (cons (constraint-var c) (cons '= (flatten-constraint empty c append-constraints))))
          append-constraints)))
  
  (define (flatten-constraint deps constraint constraints)
    (let ([left-side (constraint-left-side constraint)]
          [right-side (constraint-right-side constraint)]
          [operator (constraint-operator constraint)]
          [var (constraint-var constraint)])
      (cond
        [(and (dim-var? left-side) (dim-var? right-side))
         (append (expand-var left-side (cons var deps) constraints)
                 (cons operator 
                       (expand-var right-side (cons var deps) constraints)))]
        [(dim-var? left-side)
         (append (expand-var left-side (cons var deps) constraints)
                 (list operator right-side))]
        [(dim-var? right-side)
         (append (list left-side operator)
                 (expand-var right-side (cons var deps) constraints))]
        [else (list left-side operator right-side)])))
  
  (define (normalize-constraints constraints)
    (letrec ([to-dims 
              (lambda (l)
                (cond [(empty? l) empty]
                      [(eq? '@ (first l))
                       (cond [(dim-var? (second l))
                              (cons (list (list (second l) 1))
                                    (to-dims (rest (rest l))))]
                             [else (cons (second l)
                                         (to-dims (rest (rest l))))])]
                      [(eq? '@/ (first l))
                       (cond [(dim-var? (second l))
                              (cons (list (list (second l) -1))
                                    (to-dims (rest (rest l))))]
                             [else (cons (list (list (first (first (second l)))
                                                     (- 0 (second (first (second l))))))
                                         (to-dims (rest (rest l))))])]
                      [else
                       (cond [(dim-var? (second l))
                              (cons (list (list (second l) 1))
                                    (to-dims (rest (rest l))))]
                             [else (cons (second l) (to-dims (rest (rest l))))])]))]
             [simplify (lambda (l)
                         (cond [(= 1 (length l)) (first l)]
                               [else (gen-mult-units (first l)
                                                     (simplify (rest l)) 'a0)]))])
      (filter 
       (lambda (c)
         (not (and (= 3 (length c))
                   (eq? (first c) (third c)))))
       (map 
        (lambda (c)
          (if (= 3 (length c))
              (if (eq? (first (third c)) 'empty_unit)
                  (cons (first c) (cons (second c) (empty-unit)))
                  c)
              (cons 
               (first c)
               (cons
                (second c)
                (filter
                 (lambda (u)
                   (not (eq? (first u) 'empty_unit))) (rest (rest c)))))))
        (map 
         (lambda (c)
           (cons (first c) (cons (second c) (simplify (rest (rest c))))))
         (map 
          (lambda (c)
            (cons (first c) (cons (second c) (to-dims (cons '@ (rest (rest c)))))))
          constraints))))))
  
  (define (expand-var var deps constraints)
    (if (and (in-constraints? var constraints)
             (not (in-list? var deps)))
        (flatten-constraint deps (get-constraint var constraints) constraints)
        (list var)))
  
  (define (in-constraints? var constraints)
    (ormap (lambda (c) (eq? var (second c))) constraints))
  
  (define (get-constraint var constraints)
    (first (filter (lambda (c) (eq? var (second c))) constraints)))
  
  (define (resolve-var-dim var u hashed-constraints-vars)
    (if (not (in-hash? hashed-constraints-vars var))
        (hash-table-put! hashed-constraints-vars var (list (make-equiv-class var) u))
        (let* ([tbl-entry (hash-table-get hashed-constraints-vars var)]
               [rep (first (find-rep (first tbl-entry)))]
               [rep-tbl-entry (hash-table-get hashed-constraints-vars rep)])
          (let ([var-u
                 (cond [(empty? (second tbl-entry)) u]
                       [(not (or (equal? (second tbl-entry) u) 
                                 (is-error-unit? (second tbl-entry))))
                        (cond [(is-error-unit? u) u]
                              [else (list (list 'error/equality var))])]
                       [else u])])
            (hash-table-put! hashed-constraints-vars  var
                             (list (first tbl-entry) var-u))
            (let ([rep-u
                   (cond [(empty? (second rep-tbl-entry)) var-u]
                         [(not (or (equal? (second rep-tbl-entry) u)
                                   (is-error-unit? (second rep-tbl-entry))))
                          (cond [(is-error-unit? u) u]
                                [else (list (list 'error/equality rep))])]
                         [else var-u])])
              (hash-table-put! hashed-constraints-vars rep
                               (list (first rep-tbl-entry) rep-u)))))))
  
  (define (resolve-var-var var1 var2 hashed-constraints-vars)
    (when (not (in-hash? hashed-constraints-vars var1))
      (hash-table-put! hashed-constraints-vars var1 (list (make-equiv-class var1) empty)))
    (when (not (in-hash? hashed-constraints-vars var2))
      (hash-table-put! hashed-constraints-vars var2 (list (make-equiv-class var2) empty)))
    (let ([var1-tbl-entry (hash-table-get hashed-constraints-vars var1)]
          [var2-tbl-entry (hash-table-get hashed-constraints-vars var2)])
      (union (first (hash-table-get hashed-constraints-vars var1))
             (first (hash-table-get hashed-constraints-vars var2)))
      (let* ([var1-u (second var1-tbl-entry)]
             [var2-u (second var2-tbl-entry)]
             [var-u
              (cond [(empty? var1-u) var2-u]
                    [(empty? var2-u) var1-u]
                    [(not (equal? var1-u var2-u))
                     (list (list 'error/equality var1))]
                    [else var1-u])]
             [rep (first (find-rep (first var1-tbl-entry)))]
             [rep-tbl-entry (hash-table-get hashed-constraints-vars rep)]
             [rep-u (second rep-tbl-entry)]
             [new-u
              (cond [(empty? var-u) rep-u]
                    [(empty? rep-u) var-u]
                    [(not (equal? var-u rep-u))
                     (list (list 'error/equality rep))]
                    [else var-u])])
        (hash-table-put! hashed-constraints-vars var1
                         (list (first var1-tbl-entry) new-u))
        (hash-table-put! hashed-constraints-vars var2
                         (list (first var2-tbl-entry) new-u))
        (hash-table-put! hashed-constraints-vars rep
                         (list (first rep-tbl-entry) new-u)))))
  
  (define (solve-constraints append-constraints
                             hashed-constraints-vars hashed-vars hashed-units)
    (second-pass-solving-constraints 
     (first-pass-solving-constraints append-constraints hashed-constraints-vars)
     hashed-constraints-vars)
    (resolve-constraints-vars hashed-constraints-vars)
    (replace-constraints-vars hashed-constraints-vars hashed-vars hashed-units))
  
  (define (first-pass-solving-constraints cs hashed-constraints-vars)
    (let ([1st-pass-values (make-hash-table)])
      (let ([unsolved-cs
             (filter
              (lambda (c)
                (let ([var (first c)]
                      [right-side (rest (rest c))])
                  (cond [(not (not (assq var right-side)))
                         (let ([var_exp (- (second (assq var right-side)) 1)]
                               [filtered-right-side (filter 
                                                     (lambda (u) (not (eq? (first u) var)))
                                                     right-side)])
                           (cond [(= 0 var_exp)
                                  (let* ([vars-dims (split-list 
                                                     filtered-right-side
                                                     (lambda (u) (dim-var? (first u))))]
                                         [vars (first vars-dims)]
                                         [dims (second vars-dims)])
                                    (cond 
                                      [(= 1 (length vars))
                                       (let ([new-var (first (first vars))]
                                             [exp (- 0 (second (first vars)))])
                                         (hash-table-put! 1st-pass-values 
                                                          new-var 
                                                          'assigned-value)
                                         (if (andmap (lambda (u) 
                                                       (integer? (/ (second u) exp))) dims)
                                             (let ([u (map
                                                       (lambda (u)
                                                         (list (first u) 
                                                               (/ (second u) (- 0 exp))))
                                                       dims)])
                                               (resolve-var-dim var u hashed-constraints-vars))
                                             (resolve-var-dim 
                                              var (list (list 'error/non-integer-exp var))
                                              hashed-constraints-vars)))
                                       #f]
                                      [else #t]))]
                                 [(andmap (lambda (u) (dim-var? (first u))) right-side)
                                  (for-each 
                                   (lambda (u)
                                     (hash-table-put! 1st-pass-values (first u) 'assigned-values)
                                     (resolve-var-dim (first u) (empty-unit)
                                                      hashed-constraints-vars))
                                   right-side)
                                  #f]
                                 [(and (andmap (lambda (u) (not (dim-var? (first u))))
                                               filtered-right-side))
                                  (hash-table-put! 1st-pass-values var 'assigned-value)
                                  (if (andmap (lambda (u) (integer? (/ (second u) var_exp)))
                                              filtered-right-side)
                                      (let ([u (map
                                                (lambda (u)
                                                  (list (first u) (/ (second u) (- 0 var_exp))))
                                                filtered-right-side)])
                                        (resolve-var-dim var u hashed-constraints-vars))
                                      (resolve-var-dim 
                                       var (list (list 'error/non-integer-exp var))
                                       hashed-constraints-vars))
                                  #f]
                                 [else #t]))]
                        [else #t])))
              cs)])
        (replace-1st-pass-values unsolved-cs 1st-pass-values hashed-constraints-vars))))
  
  (define (replace-1st-pass-values cs ht hashed-constraints-vars)
    (map 
     (lambda (c)
       (cons (first c)
             (cons (second c)
                   (reverse
                    (foldl (lambda (operand operands)
                             (cond [(in-hash? ht (first operand))
                                    (let ([op-exp (second operand)]
                                          [op-u (second (hash-table-get 
                                                         hashed-constraints-vars (first operand)))])
                                      (append
                                       (map (lambda (_u)
                                              (list (first _u) (* (second _u) op-exp)))
                                            op-u)
                                       operands))]
                                   [else (cons operand operands)]))
                           empty (rest (rest c)))))))
     cs))
  
  (define (second-pass-solving-constraints cs hashed-constraints-vars)
    (for-each
     (lambda (c)
       (let ([right-side (rest (rest c))])
         (cond [(andmap (lambda (u) (not (dim-var? (first u)))) right-side)
                (resolve-var-dim (first c) (simplify right-side) hashed-constraints-vars)]
               [else
                (let* ([vars-dims (split-list right-side
                                              (lambda (u) (dim-var? (first u))))]
                       [vars (first vars-dims)]
                       [dims (second vars-dims)])
                  (when (and (= 1 (length vars))
                             (in-hash? hashed-constraints-vars (first c)))
                    (let ([left-side-u (second (hash-table-get hashed-constraints-vars
                                                               (first c)))])
                      (when (not (or (empty? left-side-u)
                                     (is-error-unit? left-side-u)))
                        (let ([new-dims 
                               (cond [(empty? dims) left-side-u]
                                     [else (gen-div-units left-side-u dims (first c))])]
                              [new-var (first (first vars))]
                              [new-exp (second (first vars))])
                          (if (andmap (lambda (u) (integer? (/ (second u) new-exp)))
                                      new-dims)
                              (let ([u (map
                                        (lambda (u)
                                          (list (first u) (/ (second u) new-exp)))
                                        new-dims)])
                                (when (not (in-hash? hashed-constraints-vars new-var))
                                  (hash-table-put! 
                                   hashed-constraints-vars new-var (list (list new-var) u)))
                                (resolve-var-dim new-var u hashed-constraints-vars))
                              (resolve-var-dim 
                               new-var
                               (list (list 'error/non-integer-exp new-var))
                               hashed-constraints-vars)))))))])))
     cs))
  
  (define (simplify l)
    (let ([u_l (map (lambda (u) (list u)) l)])
      (letrec ([mu_l
                (lambda (mu l_u)
                  (cond [(empty? l_u) mu]
                        [else (mu_l (gen-mult-units mu (first l_u) 'a0) (rest l_u))]))])
        (mu_l (first u_l) (rest u_l)))))
  
  (define (resolve-constraints-vars hashed-constraints-vars)
    (hash-table-for-each
     hashed-constraints-vars
     (lambda (var vals)
       (let* ([var-u (second vals)]
              [rep (first (find-rep (first vals)))]
              [rep-tbl-entry (hash-table-get hashed-constraints-vars rep)]
              [rep-u (second rep-tbl-entry)])
         (when (not (eq? var rep))
           (let ([new-u
                  (cond [(empty? rep-u) var-u]
                        [(and (not (empty? var-u)) (not (equal? rep-u var-u)))
                         (list (list 'error/equality rep))]
                        [else rep-u])])
             (hash-table-put! hashed-constraints-vars rep
                              (list (first rep-tbl-entry) new-u)))))))
    (hash-table-for-each
     hashed-constraints-vars
     (lambda (var vals) (hash-table-put!
                         hashed-constraints-vars var
                         (list (first vals)
                               (second (hash-table-get 
                                        hashed-constraints-vars
                                        (first (find-rep (first vals))))))))))
  
  (define (replace-constraints-vars hashed-constraints-vars hashed-vars hashed-units)
    (hash-table-for-each
     hashed-constraints-vars
     (lambda (var val)
       (when (alpha-var? var)
         (let ([cell-loc (first (hash-table-get hashed-vars var))]
               [u (second val)])
           (when (not (empty? u))
             (when (is-error-unit? u)
               (set! u (list (list (first (first u)) cell-loc))))
             (hash-table-put! hashed-vars var (list cell-loc u)))))))
    (hash-table-for-each
     hashed-vars
     (lambda (var val)
       (let ([cell-loc (first val)]
             [u (second val)])
         (hash-table-put! hashed-units cell-loc u)))))
    
  (define (lookup-formula-text cell-sym all-formula-texts)
    (let ([entry (assq cell-sym all-formula-texts)])
      (and entry (cadr entry))))
  
  (define (identify-matrices str)
    (map (lambda (range)
           (map (lambda (cell)
                  (string->symbol cell))
                (split-string range #\:)))
         (split-string (to-lower str) #\,)))
  
  (define (matrix-row-units M row hashed-units)
    (let ([row-cells (get-range-cells (first-cell-in-row M row)
                                      (last-cell-in-row M row))])
      (map (lambda (cellref)
             (cond ((in-hash? hashed-units cellref)
                    (hash-table-get hashed-units cellref))
                   (else (empty-unit))))
           row-cells)))
  
  (define (matrix-row-syms M row hashed-units hashed-vars)
    (let ([row-cells (get-range-cells (first-cell-in-row M row)
                                      (last-cell-in-row M row))])
      (foldl (lambda (cellref syms-constraints)
               (let ([sym (gen-beta-var)]
                     [syms (first syms-constraints)]
                     [constraints (second syms-constraints)])
                 (cond [(in-hash? hashed-units cellref)
                        (let ([u (hash-table-get hashed-units cellref)])
                          (cond [(in-hash? hashed-vars u) (list (cons u syms) constraints)]
                                [else (list (cons sym syms)
                                            (cons (list '= sym u) constraints))]))]
                       [else (list (cons sym syms)
                                   (cons (list '= sym (empty-unit)) constraints))])))
           row-cells)))
  
  (define (first-cell-in-row M row)
    (let ([xy (cellref->numbers (car M))])
      (numbers->cellref (list (sub1 (car xy)) (+ (cadr xy) row)))))
  
  (define (last-cell-in-row M row)
    (let ([xy-upper (cellref->numbers (car M))]
          [xy-lower (cellref->numbers (cadr M))])
      (numbers->cellref (list (sub1 (car xy-lower)) (+ (cadr xy-upper) row)))))
  
  (define (matrix-col-units M col hashed-units)
    (let ([col-cells (get-range-cells (first-cell-in-col M col)
                                      (last-cell-in-col M col))])
      (map (lambda (cellref)
             (cond ((in-hash? hashed-units cellref)
                    (hash-table-get hashed-units cellref))
                   (else (empty-unit))))
           col-cells)))
  
  (define (matrix-col-syms M col hashed-units hashed-vars)
    (let ([col-cells (get-range-cells (first-cell-in-col M col)
                                      (last-cell-in-col M col))])
      (foldl (lambda (cellref syms-constraints)
               (let ([sym (gen-beta-var)]
                     [syms (first syms-constraints)]
                     [constraints (second syms-constraints)])
                 (cond [(in-hash? hashed-units cellref)
                        (let ([u (hash-table-get hashed-units cellref)])
                          (cond [(in-hash? hashed-vars u) (list (cons u syms) constraints)]
                                [else (list (cons sym syms)
                                            (cons (list '= sym u) constraints))]))]
                       [else (list (cons sym syms)
                                   (cons (list '= sym (empty-unit)) constraints))])))
           col-cells)))
 
  (define (first-cell-in-col M col)
    (let ([xy (cellref->numbers (car M))])
      (numbers->cellref (list (+ (sub1 (car xy)) col) (cadr xy)))))
  
  (define (last-cell-in-col M col)
    (let ([xy-upper (cellref->numbers (car M))]
          [xy-lower (cellref->numbers (cadr M))])
      (numbers->cellref (list (+ (sub1 (car xy-upper)) col) (cadr xy-lower)))))

  (define (split-large-args l)
    (let ([reversed-l (reverse l)])
      (list (list (first reversed-l)) (reverse (rest reversed-l)))))

  (define (split-circular-non-circular all-formulas)
    (let ([visited (make-hash-table)])
      (letrec ([is-circular?
                (lambda (f fs)
                  (ormap 
                   (lambda (dep)
                     (cond [(in-hash? visited dep)
                            (let ([circularity (hash-table-get visited dep)])
                              (cond [(eq? 'circular circularity) #t]
                                    [else #f]))]
                           [else
                            (let ([circularity
                                   (or (memq dep fs)
                                       (let ([dep_f (assq dep all-formulas)])
                                         (and dep_f
                                              (is-circular? dep_f (cons dep fs)))))])
                              (cond [circularity
                                     (hash-table-put! visited dep 'circular)]
                                    [else
                                     (hash-table-put! visited dep 'non-circular)])
                              circularity)]))
                   (formula-dependencies (cadr f))))])
        (split-list (reverse all-formulas) (lambda (f) (is-circular? f (list (car f))))))))
  
  (define (computation-errors hashed-units all-formula-texts all-formulas)
    (filter 
     (lambda (l) (not (empty? l)))
     (hash-table-map
      hashed-units
      (lambda (cell unit)
       (if (is-error-unit? unit)
           (let ([o (open-output-string)])
             (cond [(equal? 'error/propagated (first (first unit)))
                    (fprintf o "ERROR propagated from cell: ~a"
                             (second (first unit)))]
                   [else
                    (fprintf o "ERROR computed: ~n~a " (car (first unit)))
                    (fprintf o ", cell formula: ~a~n" 
                             (lookup-formula-text cell all-formula-texts))])
             (list cell 
                   unit 
                   (get-output-string o) 
                   (formula-dependencies (cadr (assq cell all-formulas)))))
           empty)))))
  
  (define (mismatch-errors hashed-units all-units all-formula-texts all-formulas)
    (foldl 
     (lambda (annot mismatches)
       (let* ([cell (car annot)]
              [actual (hash-table-get hashed-units cell)])
         (cond [(not (or (is-error-unit? actual)
                         (empty-unit? (cadr annot))
                         (equal? actual (cadr annot))))
                (let ([o (open-output-string)])
                  (fprintf o "MISMATCH computed:~n~a " (unit-pp actual))
                  (fprintf o "cell-formula: ~a~n" (lookup-formula-text cell all-formula-texts))
                  (cons (list cell 
                              actual 
                              (get-output-string o) 
                              (formula-dependencies (cadr (assq cell all-formulas))))
                        mismatches))]
               [else mismatches])))
     empty
     all-units))
   
  (define (unit-pp unit)
    (cond [(empty-unit? unit) "()"]
          [else
           (let* ([unit-top-bottom (split-list unit (lambda (u) (> (second u) 0)))]
                  [unit-top (first unit-top-bottom)]
                  [unit-bottom (map (lambda (u) (list (first u) (- 0 (second u))))
                                    (second unit-top-bottom))]
                  [to-string
                   (lambda (unit)
                     (foldl 
                      (lambda (u visited)
                        (string-append
                         (cond [(> (second u) 1)
                                (string-append "-" (symbol->string (first u)) 
                                               "^" (number->string (second u)))]
                               [else
                                (string-append "-" (symbol->string (first u)))])
                         visited))
                      ""
                      unit))]
                  [top-string (to-string unit-top)]
                  [bottom-string (to-string unit-bottom)]
                  [top-len (string-length top-string)]
                  [bottom-len (string-length bottom-string)])
             (cond [(and (= 0 top-len) (= 0 bottom-len)) ""]
                   [(= 0 top-len) (substring bottom-string 1 bottom-len)]
                   [(= 0 bottom-len) (substring top-string 1 top-len)]
                   [else
                    (string-append (substring top-string 1 top-len) "/("
                                   (substring bottom-string 1 bottom-len) ")")]))]))
  )

