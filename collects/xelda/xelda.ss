(module xelda mzscheme
  (require (lib "list.ss"))
  (require (lib "match.ss"))
  (require (lib "pretty.ss"))
  (require (lib "mysterx.ss" "mysterx"))
  
  (require "private/xl-util.ss")
  (require "private/formula.ss")
  (require "private/parser.ss")
  (require "private/xelda-lib.ss")
  
  (define *excel-progid* "Excel.Application")
  (define *filename* "c:\\Program Files\\plt\\collects\\xelda\\units.xls")
   
  (define (open-xl-workbook xl fname)
    (let ([wbs (com-get-property xl "Workbooks")])
      (with-handlers
          (((lambda (_) #t) (lambda (_) #f)))
        (com-invoke wbs "Open" fname))))
  
  (define (add-xl-workbook xl)
    (let* ([wbs (com-get-property xl "Workbooks")])
      (com-invoke wbs "Add")))
  
  (define (range-from-coords row col)
    (string-append (number->cell-alpha (sub1 col))
                   (number->string row)))
  
  (define xl (cci/progid *excel-progid*))
  
  (com-set-property! xl "Visible" #t)
  
  (define wb (open-xl-workbook xl *filename*))
  
  (define ws (com-get-property wb "ActiveSheet"))
  
  (define (get-cell row col)
    (com-get-property 
     ws 
     `("Range" ,(range-from-coords row col))))
  
  (define (rgb r g b) 
    (+ (* 65536 b) (* 256 g) r))
  
  (define (set-range-color! rng rgb)
    (com-set-property! 
     (com-get-property rng "Interior")
     "Color" rgb))
  
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
  
  ; f extracts something from a cell
  ; pred? tests whether we're interested in it
  (define (iterate-over-worksheet f pred?)
    (let* ([ur (com-get-property ws "UsedRange")]
           [cells (com-get-property ur "Cells")]
           [first-row (com-get-property cells "Row")]
           [first-col (com-get-property cells "Column")]
           [num-rows (com-get-property cells "Rows" "Count")]
           [num-cols (com-get-property cells "Columns" "Count")]
           [last-row (sub1 (+ first-row num-rows))]
           [last-col (sub1 (+ first-col num-cols))])
      (let row-loop ([curr-row first-row]
                     [row-results null])
        (if (> curr-row last-row)
            row-results
            (let col-loop ([curr-col first-col]
                           [col-results null])
              (if (> curr-col last-col)
                  (row-loop (add1 curr-row) 
                            (append col-results 
                                    row-results))
                  (let* ([curr-cell (get-cell curr-row curr-col)]
                         [cell-content (f curr-cell)])
                    (if (pred? (f curr-cell))
                        (col-loop (add1 curr-col)
                                  (cons 
                                   (list (string->symbol
                                          (range-from-coords 
                                           curr-row curr-col))
                                         cell-content)
                                   col-results))
                        (col-loop (add1 curr-col) col-results)))))))))
  
  ; (listof (list cell-ref formula-text))
  (define all-formula-texts
    (begin
      (printf "Computing all formula texts.... ")
      (iterate-over-worksheet
       (lambda (cell) (com-get-property cell "Formula"))
       (lambda (formula)
         (and (not (string=? formula ""))
              (string=? (substring formula 0 1) "="))))))
  
  ;; probably want to extract all names before parsing
  
  (define all-names
    (begin
      (printf "DONE~nComputing all names.... ")
      (iterate-over-worksheet
       (lambda (cell) (with-handlers
                          ([void (lambda _ "")])
                        (com-get-property
                         (com-get-property cell "Name")
                         "Name"))) ; who thought of this one?
       (lambda (s) (not (string=? s ""))))))
  
  (define symbol-table
    (begin
      (printf "DONE~nComputing symbol-table.... ")
      (map (lambda (pr) (list (cadr pr) (car pr))) all-names)))
  
  (define parser (make-parser symbol-table))
  
  (define (get-formula-loc-up loc)
    (let ([formula (call-parser parser (cadr (assoc loc all-formula-texts)))])
      (cond ((not (tbl-left? formula)) loc)
            (else (let* ([xy (cellref->numbers loc)]
                         [x (sub1 (car xy))]
                         [y (sub1 (cadr xy))])
                    (get-formula-loc-up (numbers->cellref (list x y))))))))
  
  (define (get-formula-loc-left loc)
    (let ([formula (call-parser parser (cadr (assoc loc all-formula-texts)))])
      (cond ((not (tbl-top? formula)) loc)
            (else (let* ([xy (cellref->numbers loc)]
                         [x (- (car xy) 2)]
                         [y (cadr xy)])
                    (get-formula-loc-left (numbers->cellref (list x y))))))))
  
  (define all-formulas
    (begin
      (printf "DONE~nComputing all formulas.... ")
      (formula-sort
       (map (lambda (f)
              (let ([cell (car f)]
                    [form (cadr f)])
                (cond
                  ((tbl-top? form)
                   (list cell
                         (make-tbl-top (formula-name form)
                                       (append (formula-dependencies form)
                                               (list (top-of cell)
                                                     (get-formula-loc-left cell)))
                                       (tbl-top-input-cell form))))
                  ((tbl-left? form)
                   (list cell
                         (make-tbl-left (formula-name form)
                                        (append (formula-dependencies form)
                                                (list (left-of cell)
                                                      (get-formula-loc-up cell)))
                                        (tbl-left-input-cell form))))
                  (else f))))
            (map (lambda (frm-text)
                   (list (car frm-text)
                         (call-parser parser (cadr frm-text))))
                 all-formula-texts)))))
  
  ; back to Excel-like representation
  (define (unparse formula)
    (match formula
      [($ xl-number name deps val) (number->string val)]
      [($ cell-ref name cell-names) (symbol->string name)]
      [($ named-cell-ref ref-name cell-names given-name) (symbol->string given-name)] 
      [($ binary-op name deps op arg1 arg2) 
       (string-append "(" (unparse arg1) 
                      " " (symbol->string op)
                      " " (unparse arg2) ")")]
      [($ unary-op name deps op arg) 
       (string-append (symbol->string op) "("
                      (unparse arg) ")")]
      [($ application name deps fun args) 
       (string-append "@" (symbol->string fun) "("
                      (foldr (lambda (s a)
                               (let ([us (unparse s)])
                                 (if a
                                     (string-append us "," a)
                                     us)))
                             #f args)
                      ")")]
      [_ (error formula "Unknown formula structure")]))
  
  (define all-formula-dependencies
    (begin
      (printf "DONE~nComputing all formula dependencies.... ")
      (map (lambda (frm)
             (list (car frm)
                   (formula-dependencies (cadr frm))))
           all-formulas)))
  
  (define all-formula-leaves
    (begin
      (printf "DONE~nComputing all formula leaves.... ")
      (remove-duplicates
       (filter (lambda (cell)
                 (not (assq cell all-formula-dependencies)))
               (apply append (map cadr all-formula-dependencies))))))
  
  ; maybe we want to assume these have (dollar 1)
  ; not currently used
  (define all-dollar-formats
    (begin
      (printf "DONE~nComputing all formats.... ")
      (iterate-over-worksheet
       (lambda (cell) (with-handlers
                          ([void (lambda _ "")])
                        (com-get-property cell "NumberFormat")))
       (lambda (nf)
         (and (not (string=? "" nf))
              (char=? #\$ (string-ref nf 0)))))))
  
  ; sort by unit name
  (define (canonicalize-units us)
    (filter 
     (lambda (u)
       (not (zero? (cadr u))))
     (quicksort us (lambda (u1 u2)
		     (string<=?  
		      (symbol->string (car u1))
		      (symbol->string (car u2)))))))
  
  ; assumes comment containing Scheme pair is a unit annotation
  (define all-units
    (begin
      (printf "DONE~nComputing all units.... ")
      (map (lambda (entry)
             (list (car entry)
                   (canonicalize-units (cadr entry))))
           (iterate-over-worksheet
            (lambda (cell) 
              (read
               (open-input-string 
                (with-handlers
                    ([void (lambda _ "")])
                  (com-invoke (com-get-property cell "Comment") 
                              "Text")))))
            pair?))))
  
  (define (lookup-units cell-sym)
    (let ([entry (assq cell-sym all-units)])
      (and entry (cadr entry))))
  
  (define (lookup-formula-text cell-sym)
    (let ([entry (assq cell-sym all-formula-texts)])
      (and entry (cadr entry))))
  
  (define (create-definition name . body)
    (pretty-print 
     `(define ,name ,@body)))
  
  (define (create-quoted-definition name . body)
    (pretty-print 
     `(define ,name ',@body)))
  
  (define *boilerplate-defns*
    `((require (lib "xelda-lib.ss" "unit-checker"))))
  
  (define (create-boilerplate)
    (for-each pretty-print *boilerplate-defns*))
  
  (define (xl-fun->scheme x)
    (case x
      [(sum) '+]
      [(min) 'min]
      [(max) 'max]
      [(average) 'average]
      [(or) 'or]
      [(and) 'and]
      [(not) 'not]
      [(if) 'if]
      [else (error x "Unknown Excel function")]))
  
  (define (xl-op->scheme x)
    (case x
      [(+) '+]
      [(-) '-]
      [(*) '*]
      [(/) '/]
      [(^) 'expt]))
  
  (define (create-formula-definition formula name)
    (let ([curr-name (or name (formula-name formula))])
      (begin0 
        curr-name
        (match formula
          [($ xl-number name deps val) 
           (create-definition curr-name val)]
          ; cells already have definition
          [($ cell-ref name cell-name) (void)]
          [($ binary-op name deps op arg1 arg2) 
           (let ([arg1-name (create-formula-definition arg1 #f)]
                 [arg2-name (create-formula-definition arg2 #f)])
             (create-definition curr-name
                                `(,(xl-op->scheme op)
                                  ,arg1-name
                                  ,arg2-name)))]
          [($ unary-op name deps op arg) 
           (let ([arg-name (create-formula-definition arg #f)])
             (create-definition curr-name
                                `(,(xl-op->scheme op)
                                  ,arg-name)))]
          [($ application name deps fun args) 
           (let ([arg-names (map (lambda (a)
                                   (create-formula-definition a #f)) args)])
             (create-definition curr-name
                                `(,(xl-fun->scheme fun) ,@arg-names)))]
          [_ (error formula "Unknown formula structure")]))))
  
  (define (symbol-append . syms)
    (string->symbol 
     (apply string-append 
            (map symbol->string syms))))
  
  (define (name->units-name sym)
    (symbol-append sym '-units))
  
  (define (name->derived-units-name sym)
    (symbol-append sym '-derived-units))
  
  (define (all-hashed-units)
    (hash-table-for-each hashed-units
                         (lambda (key val)
                           (printf "Cell:~a -> unit:~a~n" key val))))
  (define (create-unit cell-name unit)
    (list (cell-name unit)))
  
  (define (empty-unit) (list (list 'empty-unit 1)))
  
  (define (empty-unit? u)
    (and (= (length u) 1)
         (eq? (car (first u)) 'empty-unit)
         (= (cadr (first u)) 1)))
  
  (define (in-hash? ht v)
    (not (not (hash-table-get ht v (lambda () #f)))))
  
  (define (init-hash ht l)
    (if (not (empty? l))
        (begin
          (let ([cell-name (car (first l))]
                [u (cadr (first l))])
            (if (not (in-hash? ht cell-name))
                (hash-table-put! ht cell-name u)))
          (init-hash ht (rest l)))))
  
  (define hashed-units (make-hash-table))
  
  (define all-leaves-units
    (begin
      (printf "DONE~nComputing all leaves units.... ")
      (map (lambda (lv)
             (let ([lv-unit (assq lv all-units)])
               (cond ((not lv-unit) (list lv (empty-unit)))
                     (else lv-unit)))) all-formula-leaves)))
  (printf "~n")
  
  (init-hash
   hashed-units
   (filter (lambda (u) (not (assq (car u) all-formulas)))
           all-units))
  
  (define (compute-cell-unit _formula)
    (let ([cell-name (car _formula)]
          [formula (cadr _formula)])
      (begin
        (if (not (in-hash? hashed-units cell-name))
            (hash-table-put! hashed-units cell-name
                             (compute-formula formula cell-name)))
        (hash-table-get hashed-units cell-name))))
  
  (define (compute-formula formula cell-loc)
    (match formula
      [($ xl-number name deps val) (empty-unit)]
      [($ cell-ref name cell-name)
       (cond
         ((in-hash? hashed-units name)
          (hash-table-get hashed-units name))
         (else (empty-unit)))]
      [($ named-cell-ref name cell-name actual-name)
       (cond
         ((in-hash? hashed-units name)
          (hash-table-get hashed-units name))
         (else (empty-unit)))]
      [($ binary-op name deps op arg1 arg2)
       (let ([arg1-unit (compute-formula arg1 cell-loc)]
             [arg2-unit (compute-formula arg2 cell-loc)])
         (case op
           [(+ -) (check-equal-units (list arg1-unit arg2-unit))]
           [(*) (cond
                  ((empty-unit? arg1-unit) arg2-unit)
                  ((empty-unit? arg2-unit) arg1-unit)
                  (else (gen-mult-units arg1-unit arg2-unit)))]
           [(/) (cond
                  ((empty-unit? arg1-unit) (map (lambda(u)
                                                  (let ([name (car u)]
                                                        [exp (cadr u)])
                                                    (list (name (- 0 exp)))))
                                                arg2-unit))
                  ((empty-unit? arg2-unit) arg1-unit)
                  (else (gen-div-units arg1-unit arg2-unit)))]
           [(^) (cond
                  ((empty-unit? arg2-unit)
                   (gen-exp-units arg1-unit (formula-name arg2)))
                  (else ;;exp not a unitless integer
                   '((@error-exponentiation 1))))]))]
      [($ boolean-op name deps op arg1 arg2)
       (let ([arg1-unit (compute-formula arg1 cell-loc)]
             [arg2-unit (compute-formula arg2 cell-loc)])
         (if (and (empty-unit? arg1-unit)
                  (empty-unit? arg2-unit))
             (empty-unit)
             '((@error-bool-non-empty 1))))]
      [($ unary-op name deps op arg)
       (let ([arg-unit (compute-formula arg cell-loc)])
         (case op
           [(+ -) arg-unit]
           [else (error op "Unknown unary op")]))]
      [($ tbl-left name deps input-cell)
       (let* ([input-cell-unit (compute-formula input-cell cell-loc)]
              [left-cell (left-of cell-loc)]
              [formula-cell (get-formula-loc-up cell-loc)]
              [formula-unit (compute-cell-unit (assoc formula-cell all-formulas))]
              [left-cell-unit empty])
         (if (in-hash? hashed-units left-cell)
             (set! left-cell-unit (hash-table-get hashed-units left-cell))
             (set! left-cell-unit (compute-cell-unit (assoc left-cell all-formulas))))
         (if (is-error-unit? (check-equal-units (list left-cell-unit input-cell-unit)))
             '((@error-table-non-equal-inputs 1))
             formula-unit))]
      [($ tbl-top name deps input-cell)
       (let* ([input-cell-unit (compute-formula input-cell cell-loc)]
              [top-cell (top-of cell-loc)]
              [formula-cell (get-formula-loc-left cell-loc)]
              [formula-unit (compute-cell-unit (assoc formula-cell all-formulas))]
              [top-cell-unit empty])
         (if (in-hash? hashed-units top-cell)
             (set! top-cell-unit (hash-table-get hashed-units top-cell))
             (set! top-cell-unit (compute-cell-unit (assoc top-cell all-formulas))))
         (if (is-error-unit? (check-equal-units (list top-cell-unit input-cell-unit)))
             '((@error-table-non-equal-inputs 1))
             formula-unit))]
      [($ application name deps fun args) 
       (let ([arg-units (map (lambda (a)
                               (compute-formula a cell-loc)) args)])
         (case fun
           [(average sum min max) (check-equal-units arg-units)]
           [(not or and)
            (if (andmap empty-unit? arg-units)
                (empty-unit)
                '((@error-bool-non-empty 1)))]
           [(if) (cond
                   ((= 3 (length args))
                    (let ([if-unit (second arg-units)]
                          [else-unit (third arg-units)]
                          [test-unit (first arg-units)])
                      (if (empty-unit? test-unit)
                          (check-equal-units (list if-unit else-unit))
                          '((@error-if-bool-non-empty 1)))))
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
                   (else '((@error-non-empty-precision 1))))))
              (else (error fun "illegal use")))]
           [(large) (let ([split-units (split-list arg-units empty-unit?)])
                      (cond
                        ((= 1 (length (car split-units)))
                         (check-equal-units (cadr split-units)))
                        (else '((@error-large-empty-unit 1)))))]
           [(acos acosh asin asinh atan atanh cos cosh sin sinh tan tanh)
            (let ([arg-unit (first arg-units)])
              (cond ((= 1 (length args))
                     (if (empty-unit? arg-unit)
                         arg-unit
                         '((@error-non-empty-trig-arg 1))))
                    (else (error fun "illegal use"))))]
           [(sqrt) (let ([arg-unit (first arg-units)])
                     (cond
                       ((= 1 (length args))
                        (if (andmap (lambda (s_u)
                                      (= (modulo (cadr s_u) 2) 0))
                                    arg-unit)
                            (map (lambda (s_u)
                                   (list (car s_u) (/ (cadr s_u) 2)))
                                 arg-unit)
                            '((@error-invalid-sqrt-dimension 1))))
                       (else (error fun "illegal use"))))]
           [(fact) (let ([arg-unit (first arg-units)])
                     (cond
                       ((= 1 (length args))
                        (if (empty-unit? arg-unit)
                            arg-unit
                            (let ([exp (com-invoke 
                                        ws "Evaluate"
                                        (string-append
                                         "0+"
                                         (unparse (first args))))])
                              (map (lambda (s_u)
                                     (list (car s_u)
                                           (* (inexact->exact exp)
                                              (cadr s_u))))
                                   arg-unit))))
                       (else (error fun "illegal use"))))]
           [(isnumber)
            (cond ((= 1 (length arg-units)) (empty-unit))
                  (else (error fun "illegal use")))]
           [(median) (check-equal-units arg-units)]
           [(mod)
            (cond ((= 2 (length arg-units)) (first arg-units))
                  (else (error fun "illegal use")))]
           [(na pi) (cond ((empty? args) (empty-unit))
                          (else (error fun "illegal use")))]
           [else (empty-unit)]))]))  ;; unimplemented functions -> empty unit
  
  (define (compute-formulas)
    (for-each (lambda (f)
                (begin
                  (compute-cell-unit f)
                  (printf "Computed unit for cell ~a~n" (car f))))
              all-formulas))
  
  (define (is-error-unit? u)
    (let ([str (symbol->string (car (first u)))])
      (cond
        ((and (> (string-length str) 7)
              (string=? (substring str 0 7) "@error-")) #t)
        (else #f))))
  
  (define error-cells (make-hash-table))
  
  (define (mark-error-cells)
    (hash-table-for-each
     hashed-units
     (lambda (cell unit)
       (if (is-error-unit? unit)
           (let* ([cell-coord (cellref->numbers cell)]
                  [cell-rng (get-cell (cadr cell-coord) (car cell-coord))]
                  [o (open-output-string)])
             (set-range-color! cell-rng (rgb 250 150 0))
             (fprintf o "ERROR unit: ~n~a " (car (first unit)))
             (fprintf o "|| cell: ~a~n" (lookup-formula-text cell))
             (insert-comment cell-rng (get-output-string o))
             (hash-table-put! error-cells cell 'on)
             (printf "Cell ~a error in computed units!~n" cell))))))
  
  (define (compare-actual-annotated)
    (for-each 
     (lambda (annot)         
       (let* ([cell (car annot)]
              [actual (hash-table-get hashed-units cell)])
         (if (not (or (in-hash? error-cells cell)
                      (equal? actual (cadr annot))))
             (let* ([cell-coord (cellref->numbers cell)]
                    [cell-rng (get-cell (cadr cell-coord) (car cell-coord))]
                    [o (open-output-string)])
               (set-range-color! cell-rng (rgb 0 200 190))
               (fprintf o "MISMATCH unit:~n~a " actual)
               (fprintf o "|| cell: ~a~n" (lookup-formula-text cell))
               (insert-comment cell-rng (get-output-string o))
               (hash-table-put! error-cells cell 'on)
               (printf "Cell ~a mismatch between annotated and computed units!~n"
                       cell)))))
     all-units))
  
  (define colored-dependents (make-hash-table))
  (define-struct dependent (count orig-color) (make-inspector))
  
  (define (get-dependent-color loc action cell-rng)
    (if (eq? action 'on)
        (begin
          (if (in-hash? colored-dependents loc)
              (let ([dep (hash-table-get colored-dependents loc)])
                (hash-table-put! colored-dependents loc
                                 (make-dependent (+ (dependent-count dep) 1)
                                                 (dependent-orig-color dep))))
              (hash-table-put! colored-dependents loc
                               (make-dependent 
                                0 
                                (com-get-property 
                                 (com-get-property cell-rng "Interior")
                                 "Color"))))
          (rgb 250 0 0))
        (let ([dep (hash-table-get colored-dependents loc)])
          (if (= 0 (dependent-count dep))
              (begin
                (hash-table-remove! colored-dependents loc)
                (dependent-orig-color dep))
              (begin
                (hash-table-put! colored-dependents loc
                                 (make-dependent (sub1 (dependent-count dep))
                                                 (dependent-orig-color dep)))
                (rgb 250 0 0))))))
  
  (define (color-dependents l action)
    (when (not (empty? l))
      (let* ([xy (cellref->numbers (first l))]
             [cell-rng (get-cell (cadr xy) (car xy))]
             [col (get-dependent-color (first l) action cell-rng)])
        (set-range-color! cell-rng col))
      (color-dependents (rest l) action)))
  
  (com-register-event-handler 
   xl "SheetBeforeRightClick"
   (lambda (ws rng b)
     (let* ([row (com-get-property rng "Row")]
            [col (com-get-property rng "Column")]
            [cell-ref (numbers->cellref (list (sub1 col) row))])
       (when (in-hash? error-cells cell-ref)
         (set-box! b #t)
         (let ([formula (cadr (assq cell-ref all-formulas))])
           (if (eq? (hash-table-get error-cells cell-ref) 'off)
               (begin
                 (hash-table-put! error-cells cell-ref 'on)
                 (color-dependents (formula-dependencies formula) 'off)
                 (com-invoke rng "ShowPrecedents" #t))
               (begin
                 (hash-table-put! error-cells cell-ref 'off)               
                 (color-dependents (formula-dependencies formula) 'on)
                 (com-invoke rng "ShowPrecedents"))))))))
  
  (define (unit-check)
    (compute-formulas)
    (mark-error-cells)
    (compare-actual-annotated))
  
  (define (insert-comment rng scheme-comment)
    (let* ([comment-obj (com-get-property rng "Comment")] 
           [comment-text
            (read
             (open-input-string 
              (with-handlers
                  ([void (lambda _ "(())")])
                (com-invoke comment-obj
                            "Text"))))])
      (with-handlers ([void (lambda _ "")])
        (com-invoke comment-obj "Delete"))
      (com-invoke rng "AddComment"
                  (format "~a~n; ~a~n" 
                          comment-text 
                          scheme-comment)))))
