;;(module xelda mzscheme
(require (lib "list.ss"))
(require (lib "match.ss"))
(require (lib "pretty.ss"))
(require (lib "mysterx.ss" "mysterx"))

(require "private/xl-util.ss")
(require "private/formula.ss")
(require "private/parser.ss")
(require "private/xelda-lib.ss")

;;  (provide
;;   load-xl-file
;;   unit-check-xl-file
;;   unit-recheck-xl-file)

(define *excel-progid* "Excel.Application")
(define *filename* #f)
(define xl (cci/progid *excel-progid*))
(define wb #f)
(define ws #f)
(define all-formula-texts #f)
(define all-names #f)
(define symbol-table #f)
(define parser (make-parser symbol-table))
(define all-formulas #f)
(define all-formula-dependencies #f)
(define all-formula-leaves #f)
(define all-units #f)
(define all-dollar-formats #f)
(define hashed-units #f)
(define all-leaves-units #f)
(define error-cells #f)
(define colored-dependents #f)

(define-struct dependent (count orig-color) (make-inspector))

(com-register-event-handler 
 xl "SheetBeforeRightClick"
 (lambda (ws rng b)
   (let* ([row (com-get-property rng "Row")]
          [col (com-get-property rng "Column")]
          [cell-ref (numbers->cellref (list (sub1 col) row))])
     (when (and (hash-table? error-cells)
                (in-hash? error-cells cell-ref))
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


;; WINDOW API ==== BEGIN =====

(define *file-checked* #f)
(define *file-opened* #f)
(define *status-text-editor* #f)

(define (set-status! str)
  (send *status-text-editor* erase)
  (send *status-text-editor* insert str))

(define (update-status! str)
  (send *status-text-editor* insert str))

(define xelda-frame%
  (class frame%
    (field
     [status-panel #f]
     [status-text #f]
     [status-message #f]
     [assign-panel #f]
     [assign-button #f]
     [range-text #f]
     [units-text #f]
     [buttons-panel #f]
     [load-button #f]
     [quit-button #f]
     [close-button #f]
     [clear-button #f]
     [check-button #f])
    (public*
     [reset-frame!
      (lambda()
        (for-each (lambda (panel)
                    (when panel (send panel show #f)))
                  (list status-panel buttons-panel))
        (set! buttons-panel
              (instantiate horizontal-panel% ()
                (parent this)
                (stretchable-height #f)
                (vert-margin 2)
                (alignment '(center center))))
        (set! status-panel
              (instantiate vertical-panel% ()
                (parent this)
                (border 5)
                (horiz-margin 5)
                (vert-margin 10)
                (stretchable-height #t)
                (style '(border))))
        (set! assign-panel
              (instantiate vertical-panel% ()
                (parent this)
                (stretchable-height #f)
                (style '(border))
                (border 5)
                (spacing 3)
                (horiz-margin 5)
                (vert-margin 5)
                (alignment '(center center))))
        (set! status-message
              (instantiate message% ()
                (label "XeLda Status:")
                (parent status-panel)))
        (set! status-text
              (instantiate text-field% ()
                (label #f)
                (init-value (format "Load Exel file~n"))
                (parent status-panel)
                (style (list 'multiple))
                (stretchable-height #t)
                (enabled #t)
                (callback (lambda (txt-fld cntrl-evnt) ()))))
        (set! *status-text-editor* (send status-text get-editor))
        (set! range-text
              (instantiate text-field% ()
                (label "Cell(s) Range(s):")
                (parent assign-panel)
                (enabled #f)
                (callback (lambda (txg-fld cntrl-evnt) ()))))
        (set! units-text
              (instantiate text-field% ()
                (label "      Cell(s) Units:")
                (parent assign-panel)
                (enabled #f)
                (callback (lambda (txt-fld cntrl-evnt) ()))))
        (set! assign-button
              (instantiate button% ()
                (label "Assign Units")
                (parent assign-panel)
                (enabled #f)
                (callback 
                 (lambda (b env)
                   (let* ([range-editor (send range-text get-editor)]
                          [units-editor (send units-text get-editor)]
                          [range-txt (send range-editor get-text)]
                          [units-txt (send units-editor get-text)]
                          [unit-list (get-ranges-from-txt range-txt)]
                          [unit (get-units-from-txt units-txt)])
                     (cond
                       ((empty? unit)
                        (message-box "Invalid Input"
                                     "Improper unit format"
                                     frame
                                     '(ok)))
                       ((empty? unit-list)
                         (message-box "Invalid Input"
                                     "Improper unit format"
                                     assign-panel
                                     '(ok)))
                      (else
                        (send range-editor erase)
                        (send units-editor erase)
                        (for-each (lambda (us)
                                    (for-each (lambda (c) 
                                                (set-comment c units-txt))
                                              us))
                                  unit-list))))))))
        (set! load-button
              (instantiate button% ()
                (label "Load File")
                (parent buttons-panel)
                (callback (lambda (b ev) 
                            (set! *file-checked* #f)
                            (set! *file-opened* #t)
                            (set! *filename* (get-filename))
                            (update-status! (format "Loaded file: ~a~n" *filename*))
                            (open-frame!)))))
        (set! close-button
              (instantiate button% ()
                (label "Close File")
                (parent buttons-panel)
                (enabled #f)
                (callback (lambda (b ev)
                            (send check-button set-label "Check File")
                            (set! *file-opened* #f)
                            (set-status! (format "Load Exel file~n"))
                            (close-frame!)))))
        (set! check-button
              (instantiate button% ()
                (label "Check File")
                (parent buttons-panel)
                (enabled #f)
                (callback (lambda (b ev)
                            (send quit-button enable #f)
                            (send close-button enable #f)
                            (send clear-button enable #f)
                            (send check-button enable #f)
                            (send assign-button enable #f)
                            (send units-text enable #f)
                            (send range-text enable #f)
                            (thread (lambda()
                                      (cond (*file-checked* (unit-recheck-xl-file))
                                            (else (unit-check-xl-file)))
                                      (set! *file-checked* #t)
                                      (send clear-button enable #t)
                                      (send check-button enable #t)
                                      (send close-button enable #t)
                                      (send assign-button enable #t)
                                      (send units-text enable #t)
                                      (send range-text enable #t)
                                      (send quit-button enable #t)))))))
        (set! clear-button
              (instantiate button% ()
                (label "Clear Precedents")
                (parent buttons-panel)
                (enabled #f)
                (callback (lambda (b ev)
                            (clear-all-precedents)))))
        (set! quit-button
              (instantiate button% ()
                (label "Quit Xelda")
                (parent buttons-panel)
                (callback (lambda (b ev)
                            (when *file-opened* (close-xl-workbook xl))
                            (send frame show #f))))))]
     [open-frame!
      (lambda()
        (send load-button enable #f)
        (send close-button enable #t)
        (send check-button enable #t)
        (send assign-button enable #t)
        (send range-text enable #t)
        (send units-text enable #t)
        (load-xl-file *filename*))]
     [close-frame!
      (lambda()
        (send load-button enable #t)
        (send close-button enable #f)
        (send check-button enable #f)
        (send clear-button enable #f)
        (send assign-button enable #f)
        (send range-text enable #f)
        (send units-text enable #f)
        (close-xl-workbook xl))])
    (super-instantiate())
    (reset-frame!)))

(define (get-filename)
  (get-file ".xls files"
            frame
            (build-path (collection-path "xelda") "games")
            #f
            "xls"
            '()
            '(("Excel Files" "*.xls"))))

(define frame
  (instantiate xelda-frame% ()
    (label "XeLda")
    (width 300)
    (height 400)))

(send frame center)
(send frame show #t)
;; WINDOW API ==== END =====


(define (load-xl-file filename)
  (com-set-property! xl "Visible" #t)
  (set! wb (open-xl-workbook xl filename))
  (set! ws (com-get-property wb "ActiveSheet")))

(define (unit-check-xl-file)
  (preprocess)
  (unit-check))

(define (unit-recheck-xl-file)
  (unmark-errors)
  (unit-check-xl-file))

(define (clear-all-precedents)
  (when (hash-table? error-cells)
    (hash-table-for-each
     error-cells
     (lambda (cell-ref state)
       (when (eq? state 'off)
         (let ([formula (cadr (assq cell-ref all-formulas))])
           (hash-table-put! error-cells cell-ref 'on)
           (color-dependents (formula-dependencies formula) 'off)
           (com-invoke (cellref->rng cell-ref) "ShowPrecedents" #t)))))))

(define (preprocess)
  (set! all-formula-texts
        (begin
          (printf "Computing all formula texts.... ")
          (update-status! (format "=================UNIT CHECKING==============~n"))
          (update-status! "Computing all formula texts.... ")
          (iterate-over-worksheet
           (lambda (cell) (com-get-property cell "Formula"))
           (lambda (formula)
             (and (not (string=? formula ""))
                  (string=? (substring formula 0 1) "="))))))
  (set! all-names
        (begin
          (printf "DONE~nComputing all names.... ")
          (update-status! (format "DONE~nComputing all names.... "))
          (iterate-over-worksheet
           (lambda (cell) (with-handlers
                              ([void (lambda _ "")])
                            (com-get-property
                             (com-get-property cell "Name")
                             "Name"))) ; who thought of this one?
           (lambda (s) (not (string=? s ""))))))
  (set! symbol-table
        (begin
          (printf "DONE~nComputing symbol-table.... ")
          (update-status! (format "DONE~nComputing symbol-table.... "))
          (map (lambda (pr) (list (cadr pr) (car pr))) all-names)))
  (set! parser (make-parser symbol-table))
  (set! all-formulas
        (begin
          (printf "DONE~nComputing all formulas.... ")
          (update-status! (format "DONE~nComputing all formulas.... "))
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
  (set! all-formula-dependencies
        (begin
          (printf "DONE~nComputing all formula dependencies.... ")
          (update-status! (format "DONE~nComputing all formula dependencies.... "))
          (map (lambda (frm)
                 (list (car frm)
                       (formula-dependencies (cadr frm))))
               all-formulas)))
  (set! all-formula-leaves
        (begin
          (printf "DONE~nComputing all formula leaves.... ")
          (update-status! (format "DONE~nComputing all formula leaves.... "))
          (remove-duplicates
           (filter (lambda (cell)
                     (not (assq cell all-formula-dependencies)))
                   (apply append (map cadr all-formula-dependencies))))))
  (set! all-dollar-formats
        (begin
          (printf "DONE~nComputing all formats.... ")
          (update-status! (format "DONE~nComputing all formats.... "))
          (iterate-over-worksheet
           (lambda (cell) (with-handlers
                              ([void (lambda _ "")])
                            (com-get-property cell "NumberFormat")))
           (lambda (nf)
             (and (not (string=? "" nf))
                  (char=? #\$ (string-ref nf 0)))))))
  ; assumes comment containing Scheme pair is a unit annotation
  (set! all-units
        (begin
          (printf "DONE~nComputing all units.... ")
          (update-status! (format "DONE~nComputing all units.... "))
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
  (set! hashed-units (make-hash-table))
  (set! all-leaves-units
        (begin
          (printf "DONE~nComputing all leaves units.... ")
          (update-status! (format "DONE~nComputing all leaves units.... "))
          (map (lambda (lv)
                 (let ([lv-unit (assq lv all-units)])
                   (cond ((not lv-unit) (list lv (empty-unit)))
                         (else lv-unit)))) all-formula-leaves)))
  (printf "DONE~n")
  (update-status! (format "DONE~n"))
  (init-hash
   hashed-units
   (filter (lambda (u) (not (assq (car u) all-formulas)))
           all-units))
  (set! error-cells (make-hash-table))
  (set! colored-dependents (make-hash-table)))

(define (open-xl-workbook xl fname)
  (let ([wbs (com-get-property xl "Workbooks")])
    (with-handlers
        (((lambda (_) #t) (lambda (_) #f)))
      (com-invoke wbs "Open" fname))))

(define (close-xl-workbook xl)
  (let ([wbs (com-get-property xl "Workbooks")])
    (with-handlers
        (((lambda (_) #t) (lambda (_) #f)))
      (com-invoke wbs "Close"))))

(define (add-xl-workbook xl)
  (let* ([wbs (com-get-property xl "Workbooks")])
    (com-invoke wbs "Add")))

(define (range-from-coords row col)
  (string-append (number->cell-alpha (sub1 col))
                 (number->string row)))

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

; sort by unit name
(define (canonicalize-units us)
  (filter 
   (lambda (u)
     (not (zero? (cadr u))))
   (quicksort us (lambda (u1 u2)
                   (string<=?  
                    (symbol->string (car u1))
                    (symbol->string (car u2)))))))

(define (lookup-units cell-sym)
  (let ([entry (assq cell-sym all-units)])
    (and entry (cadr entry))))

(define (lookup-formula-text cell-sym)
  (let ([entry (assq cell-sym all-formula-texts)])
    (and entry (cadr entry))))

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

(define (symbol-append . syms)
  (string->symbol 
   (apply string-append 
          (map symbol->string syms))))

(define (name->units-name sym)
  (symbol-append sym '-units))

(define (name->derived-units-name sym)
  (symbol-append sym '-derived-units))

(define (all-hashed-values ht)
  (hash-table-for-each ht
                       (lambda (key val)
                         (printf "Key:~a -> Value:~a~n" key val))))

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

(define (compute-cell-unit _formula)
  (let ([cell-name (car _formula)]
        [formula (cadr _formula)])
    (begin
      (if (not (in-hash? hashed-units cell-name))
          (hash-table-put! 
           hashed-units cell-name
           (let ([unit (compute-formula formula cell-name)])
             (cond ((> (length unit) 1)
                    (filter (lambda (u) (not (eq? (car u) 'empty-unit)))
                            unit))
                   (else unit)))))
      (hash-table-get hashed-units cell-name))))

(define (compute-formula formula cell-loc)
  (match formula
    [($ xl-number name deps val) (empty-unit)]
    [($ cell-ref name cell-name)
     (cond
       ((in-hash? hashed-units name)
        (hash-table-get hashed-units name))
       ((string=? "" (com-get-property (cellref->rng (formula-name formula)) "Text"))
        '((error/empty-formula 1)))
       (else (empty-unit)))]
    [($ named-cell-ref name cell-name actual-name)
     (cond
       ((in-hash? hashed-units name)
        (hash-table-get hashed-units name))
       ((string=? "" (com-get-property (cellref->rng (formula-name formula)) "Text"))
        '((error/empty-formula 1)))
       (else (empty-unit)))]
    [($ binary-op name deps op arg1 arg2)
     (let ([arg1-unit (compute-formula arg1 cell-loc)]
           [arg2-unit (compute-formula arg2 cell-loc)])
       (case op
         [(+ -) (check-equal-units (list arg1-unit arg2-unit))]
         [(*) (cond
                ((empty-unit? arg1-unit) arg2-unit)
                ((empty-unit? arg2-unit) arg1-unit)
                (else (let ([result (gen-mult-units arg1-unit arg2-unit)])
                        (cond ((null? result) (empty-unit))
                              (else result)))))]
         [(/) (cond
                ((empty-unit? arg1-unit) (map (lambda(u)
                                                (let ([name (car u)]
                                                      [exp (cadr u)])
                                                  (list (name (- 0 exp)))))
                                              arg2-unit))
                ((empty-unit? arg2-unit) arg1-unit)
                (else (let ([result (gen-div-units arg1-unit arg2-unit)])
                        (cond ((null? result) (empty-unit))
                              (else result)))))]
         [(^) (cond
                ((empty-unit? arg2-unit)
                 (gen-exp-units arg1-unit (formula-name arg2)))
                (else ;;exp not a unitless integer
                 '((error/exponentiation 1))))]))]
    [($ boolean-op name deps op arg1 arg2)
     (let ([arg1-unit (compute-formula arg1 cell-loc)]
           [arg2-unit (compute-formula arg2 cell-loc)])
       (if (equal? arg1-unit arg2-unit)
           (empty-unit)
           '((error/bool-non-empty 1))))]
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
           '((error/table-non-equal-inputs 1))
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
           '((error/table-non-equal-inputs 1))
           formula-unit))]
    [($ application name deps fun args) 
     (let ([arg-units (map (lambda (a)
                             (compute-formula a cell-loc)) args)])
       (case fun
         [(average sum min max) (check-equal-units arg-units)]
         [(not or and)
          (if (andmap empty-unit? arg-units)
              (empty-unit)
              '((error/bool-non-empty 1)))]
         [(if) (cond
                 ((= 3 (length args))
                  (let ([if-unit (second arg-units)]
                        [else-unit (third arg-units)]
                        [test-unit (first arg-units)])
                    (if (empty-unit? test-unit)
                        (check-equal-units (list if-unit else-unit))
                        '((error/if-bool-non-empty 1)))))
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
                 (else '((error/non-empty-precision 1))))))
            (else (error fun "illegal use")))]
         [(large) (let ([split-units (split-list arg-units empty-unit?)])
                    (cond
                      ((= 1 (length (car split-units)))
                       (check-equal-units (cadr split-units)))
                      (else '((error/large-empty-unit 1)))))]
         [(acos acosh asin asinh atan atanh cos cosh sin sinh tan tanh)
          (let ([arg-unit (first arg-units)])
            (cond ((= 1 (length args))
                   (if (empty-unit? arg-unit)
                       arg-unit
                       '((error/non-empty-trig-arg 1))))
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
                          '((error/invalid-sqrt-dimension 1))))
                     (else (error fun "illegal use"))))]
         [(fact) (let ([arg-unit (first arg-units)])
                   (cond
                     ((= 1 (length args))
                      (if (empty-unit? arg-unit)
                          arg-unit
                          '((error/fact-non-unitless 1))))
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
         [(mmult) (let* ([formula-text (lookup-formula-text cell-loc)]
                         [Ms (identify-matrices
                              (substring formula-text 7
                                         (sub1 (string-length formula-text))))])
                    (cond
                      ((= 2 (length Ms))
                       (let* ([cell-xy (get-row-col-num cell-loc)]
                              [M1-row (car cell-xy)]
                              [M2-col (cadr cell-xy)]
                              [M1-row-unit (check-equal-units
                                            (matrix-row-units (car Ms) M1-row))]
                              [M2-col-unit (check-equal-units
                                            (matrix-col-units (cadr Ms) M2-col))])
                         (cond
                           ((is-error-unit? M1-row-unit) M1-row-unit)
                           ((is-error-unit? M2-col-unit) M2-col-unit)
                           (else (gen-mult-units M1-row-unit M2-col-unit)))))
                      (else (error fun "illegal use"))))]
         
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
      ((and (> (string-length str) 6)
            (string=? (substring str 0 6) "error/")) #t)
      (else #f))))

(define (mark-error-cells)
  (hash-table-for-each
   hashed-units
   (lambda (cell unit)
     (if (is-error-unit? unit)
         (let ([cell-rng (cellref->rng cell)]
               [o (open-output-string)])
           (set-range-color! cell-rng (rgb 250 150 0))
           (fprintf o "ERROR unit: ~n~a " (car (first unit)))
           (fprintf o "|| cell: ~a~n" (lookup-formula-text cell))
           (insert-comment cell-rng (get-output-string o))
           (hash-table-put! error-cells cell 'on)
           (update-status! (format "Cell ~a error in computed units!~n" cell))
           (printf "Cell ~a error in computed units!~n" cell))))))

(define (compare-actual-annotated)
  (for-each 
   (lambda (annot)         
     (let* ([cell (car annot)]
            [actual (hash-table-get hashed-units cell)])
       (if (not (or (in-hash? error-cells cell)
                    (equal? actual (cadr annot))))
           (let ([cell-rng (cellref->rng cell)]
                 [o (open-output-string)])
             (set-range-color! cell-rng (rgb 0 200 190))
             (fprintf o "MISMATCH unit:~n~a " actual)
             (fprintf o "|| cell: ~a~n" (lookup-formula-text cell))
             (insert-comment cell-rng (get-output-string o))
             (hash-table-put! error-cells cell 'on)
             (update-status! 
              (format "Cell ~a mismatch between annotated and computed units!~n"
                      cell))
             (printf "Cell ~a mismatch between annotated and computed units!~n"
                     cell)))))
   all-units))

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
    (let* ([cell-rng (cellref->rng (first l))]
           [col (get-dependent-color (first l) action cell-rng)])
      (set-range-color! cell-rng col))
    (color-dependents (rest l) action)))

(define (unmark-errors)
  (let ([orig-col (com-get-property
                   (com-get-property (get-cell 1 1) "Interior")
                   "Color")])
    (hash-table-for-each error-cells
                         (lambda (k v)
                           (let ([rng (cellref->rng k)])
                             (when (eq? v 'off)
                               (begin
                                 (color-dependents
                                  (formula-dependencies (cadr (assq k all-formulas))) 'off)
                                 (com-invoke rng "ShowPrecedents" #t)))
                             (let ([orig (original-comment (get-comment k))])
                               (cond 
                                 ((and (>= (string-length orig) 4)
                                       (string=? "(())" (substring orig 0 4)))
                                  (delete-comment k))
                                 (else (set-comment k orig))))
                             (set-range-color! rng orig-col))))
    (set! error-cells #f)
    (set! colored-dependents #f)))

(define (original-comment comment)
  (letrec ([loop (lambda (str i max)
                   (cond ((= i max) str)
                         ((equal? #\; (string-ref str i)) (substring str 0 i))
                         (else (loop str (+ i 1) max))))])
    (loop comment 0 (string-length comment))))

(define (unit-check)
  (compute-formulas)
  (mark-error-cells)
  (compare-actual-annotated)
  (update-status! (format "-----------------UNIT CHECKING DONE!---------------~n")))

(define (cellref->rng cell)
  (let ([xy (cellref->numbers cell)])
    (get-cell (cadr xy) (car xy))))

(define (get-comment cell)
  (com-invoke (com-get-property (cellref->rng cell) "Comment") "Text"))

(define (delete-comment cell)
  (let* ([rng (cellref->rng cell)]
         [comment-obj (com-get-property rng "Comment")])
    (with-handlers ([void (lambda _ "")])
      (com-invoke comment-obj "Delete"))))

(define (set-comment cell comment-text)
  (let* ([rng (cellref->rng cell)]
         [comment-obj (com-get-property rng "Comment")])
    (with-handlers ([void (lambda _ "")])
      (com-invoke comment-obj "Delete"))
    (com-invoke rng "AddComment"
                (format "~a~n" comment-text))))

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
                        scheme-comment))))

(define (get-row-col-num cell)
  (let* ([rng (cellref->rng cell)]
         [M (com-get-property rng "CurrentArray")]
         [first-row (com-get-property M "Row")]
         [first-col (com-get-property M "Column")]
         [cell-row (com-get-property rng "Row")]
         [cell-col (com-get-property rng "Column")])
    (list (- cell-row first-row) (- cell-col first-col))))

(define (identify-matrices str)
  (map (lambda (range)
         (map (lambda (cell)
                (string->symbol cell))
              (split-string range #\:)))
       (split-string (to-lower str) #\,)))

(define (matrix-row-units M row)
  (let ([row-cells (get-range-cells (first-cell-in-row M row)
                                    (last-cell-in-row M row))])
    (map (lambda (cellref)
           (cond ((in-hash? hashed-units cellref)
                  (hash-table-get hashed-units cellref))
                 (else (empty-unit))))
         row-cells)))

(define (first-cell-in-row M row)
  (let ([xy (cellref->numbers (car M))])
    (numbers->cellref (list (sub1 (car xy)) (+ (cadr xy) row)))))

(define (last-cell-in-row M row)
  (let ([xy-upper (cellref->numbers (car M))]
        [xy-lower (cellref->numbers (cadr M))])
    (numbers->cellref (list (sub1 (car xy-lower)) (+ (cadr xy-upper) row)))))

(define (matrix-col-units M col)
  (let ([col-cells (get-range-cells (first-cell-in-col M col)
                                    (last-cell-in-col M col))])
    (map (lambda (cellref)
           (cond ((in-hash? hashed-units cellref)
                  (hash-table-get hashed-units cellref))
                 (else (empty-unit))))
         col-cells)))

(define (first-cell-in-col M col)
  (let ([xy (cellref->numbers (car M))])
    (numbers->cellref (list (+ (sub1 (car xy)) col) (cadr xy)))))

(define (last-cell-in-col M col)
  (let ([xy-upper (cellref->numbers (car M))]
        [xy-lower (cellref->numbers (cadr M))])
    (numbers->cellref (list (+ (sub1 (car xy-upper)) col) (cadr xy-lower)))))

(define (get-ranges-from-txt str)
  (let* ([s (list->string (filter (lambda (c) (not (char-whitespace? c)))
                                 (string->list str)))]
         [ranges-txt (split-string s #\,)]
         [ranges (map (lambda (r) (split-string r #\:)) ranges-txt)]
         [valid-ranges (filter (lambda (r) (not (empty? r)))
                               (map (lambda (r)
                                      (filter (lambda (c)
                                                (cell-ref? (call-parser parser c)))
                                              r))
                                    ranges))])
    (map (lambda (r)
           (cond ((= 2 (length r)) (get-range-cells (first r) (second r)))
                 (else r)))
         (map (lambda (r)
                (map (lambda (c) (string->symbol (to-lower c))) r))
              valid-ranges))))

(define (get-units-from-txt str)
  (let ([max (string-length str)])
    (cond
      ((string=? "" str) '())
      ((and (eq? (string-ref str 0) #\()
            (eq? (string-ref str (sub1 max)) #\)))
       (let* ([us (split-string (substring str 1 (sub1 max)) #\))]
              [us_s (map (lambda (u) (split-string u #\ )) us)]
              [valid-us (filter (lambda (u)
                                  (and (= (length u) 2)
                                       (eq? (string-ref (first u) 0) #\()
                                       (> (string-length (first u)) 1)
                                       (string->number (second u))))
                                us_s)])
         (if (= (length valid-us) (length us_s))
             (map (lambda (u)
                    (list (string->symbol (substring (first u)
                                                     1 
                                                     (string-length (first u))))
                          (string->number (second u))))
                  valid-us)
             '())))
      (else '()))))