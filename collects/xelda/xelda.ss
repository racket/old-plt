;;(module xelda mzscheme
(require (lib "class.ss"))
(require (lib "list.ss"))
(require (lib "match.ss"))
(require (lib "pretty.ss"))
(require (lib "mred.ss" "mred"))
(require (lib "mysterx.ss" "mysterx"))

(require "private/xl-util.ss")
(require "private/formula.ss")
(require "private/parser.ss")
(require "private/xelda-lib.ss")

;;  (provide
;;   load-xl-file
;;   unit-check-xl-file
;;   unit-recheck-xl-file)

;;
;; GLOBALS
;;
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
(define all-units #f)
(define hashed-units #f)
(define hashed-vars #f)
(define hashed-constraints-vars #f)
(define constraints #f)
(define error-cells #f)
(define colored-dependents #f)
(define all-non-circular-formulas #f)
(define all-circular-formulas #f)
(define bad-format-units empty)
(define bad-format-units-colors #f)

(define-struct dependent (count orig-color) (make-inspector))

;;
;; Constraint functions
;;
(define (init-constraints) (set! constraints (list empty empty)))

(define (push-constraint c)
  (cond ([eq? (first c) '=] (set! constraints (list (cons c (first constraints))
                                                    (second constraints))))
        [else (set! constraints (list (first constraints)
                                      (cons c (second constraints))))]))

(define (pop-eq-constraint)
  (let ([c (first (first constraints))])
    (set! constraints (list (rest (first constraints)) (second constraints)))
    c))

(define (pop-fun-constraint)
  (let ([c (first (second constraints))])
    (set! constraints (list (first constraints) (rest (second constraints))))
    c))

(define (constraint-var c) (second c))
(define (constraint-operator c) (first c))
(define (constraint-left-side c) (third c))
(define (constraint-right-side c) (fourth c))

(define (eq-constraints-left) (length (first constraints)))

(define (fun-constraints-left) (length (second constraints)))

(define (eq-constraints-left?) (not (empty? (first constraints))))

(define (fun-constraints-left?) (not (empty? (second constraints))))

(define (show-constraints)
  (for-each (lambda (c) (printf "constraint: ~a~n" c)) (first constraints))
  (for-each (lambda (c) (printf "constraint: ~a~n" c)) (second constraints)))

;;
;; Event handlers
;;
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
(define *range-text-editor* #f)

(define (set-status! str)
  (send *status-text-editor* erase)
  (send *status-text-editor* insert str))

(define (update-status! str)
  (send *status-text-editor* insert str))

(define (next-update-status! str)
  (update-status! (format "done~n~a.... " str)))

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
                (init-value (format "Ready to load Excel file~n"))
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
        (set! *range-text-editor* (send range-text get-editor))
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
                                     "Improper range format"
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
                            (close-frame!)))))
        (set! check-button
              (instantiate button% ()
                (label "Analyze")
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
                (label "Clear")
                (parent buttons-panel)
                (enabled #f)
                (callback (lambda (b ev)
                            (unmark-errors)
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
        (set! *file-opened* #f)
        (set-status! (format "Load Exel file~n"))
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
  (set! ws (com-get-property wb "ActiveSheet"))
  (com-register-event-handler
   wb "BeforeClose"
   (lambda (cancel) (when *file-opened* (send frame close-frame!))))
  (com-register-event-handler
   ws "SelectionChange"
   (lambda (rng)
     (when *file-opened*
       (let* ([row (com-get-property rng "Row")]
              [col (com-get-property rng "Column")]
              [rows (com-get-property rng "Rows" "Count")]
              [cols (com-get-property rng "Columns" "Count")]
              [left-top (numbers->cellref (list (sub1 col) row))]
              [right-bottom (numbers->cellref (list (- (+ col cols) 2) (sub1 (+ row rows))))])
         (send *range-text-editor* erase)
         (send *range-text-editor* insert (format "~a:~a" left-top right-bottom)))))))

(define (unit-check-xl-file)
  (when (not (empty? bad-format-units))
    (clear-bad-format-units))
  (preprocess)
  (cond [(empty? bad-format-units) (unit-check)]
        [else (mark-bad-format-units)]))

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
          (printf "Preprocessing all formula texts.... ")
          (update-status! (format "+++SPREADSHEET PREPROCESSING BEGIN+++~n"))
          (update-status! "Preprocessing all formula texts.... ")
          (iterate-over-worksheet
           (lambda (cell) (com-get-property cell "Formula"))
           (lambda (formula)
             (and (not (string=? formula ""))
                  (string=? (substring formula 0 1) "="))))))
  (set! all-names
        (begin
          (printf "done~nPreprocessing all names.... ")
          (next-update-status! "Preprocessing all names")
          (iterate-over-worksheet
           (lambda (cell) (with-handlers
                              ([void (lambda _ "")])
                            (com-get-property
                             (com-get-property cell "Name")
                             "Name"))) ; who thought of this one?
           (lambda (s) (not (string=? s ""))))))
  (set! symbol-table
        (begin
          (printf "done~nPreprocessing symbol-table.... ")
          (next-update-status! "Preprocessing symbol-table")
          (map (lambda (pr) (list (cadr pr) (car pr))) all-names)))
  (set! parser (make-parser symbol-table))
  (let ([formulas
         (begin
           (printf "done~nPreprocessing all formulas.... ")
           (next-update-status! "Preprocessing all formulas")
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
                     all-formula-texts)))])
    (set! all-formulas (formula-sort formulas)))
  (set! all-formulas (formula-sort all-formulas))
  (compute-all-circular-formulas)
  ; assumes comment containing Scheme pair is a unit annotation
  (set! all-units
        (let ([good-and-bad
               (split-list
                (begin
                  (printf "done~nPreprocessing all units.... ")
                  (next-update-status! "Preprocessing all units")
                  (iterate-over-worksheet
                   (lambda (cell)
                     (with-handlers
                         ([void (lambda _ '((bad-unit-format bad-unit-format)))])
                       (parse-unit                          
                        (with-handlers
                            ([void (lambda _ "empty_unit")])
                          (com-invoke (com-get-property cell "Comment") 
                                      "Text")))))
                   pair?))
                (lambda (e) (dim? (second e))))])
          (set! bad-format-units (second good-and-bad))
          (map (lambda (entry)
                 (list (car entry) (canonicalize-units (cadr entry))))
               (first good-and-bad))))
  (set! hashed-units (make-hash-table))
  (set! hashed-vars (make-hash-table))
  (set! hashed-constraints-vars (make-hash-table))
  (init-constraints)
  (printf "done~n")
  (update-status! (format "done~n"))
  (update-status! (format "+++SPREADSHEET PREPROCESSING END+++~n"))
  (init-hash
   hashed-units
   (filter (lambda (u) (not (assq (car u) all-formulas)))
           all-units))
  (set! error-cells (make-hash-table))
  (set! colored-dependents (make-hash-table)))

(define (compute-all-circular-formulas)
  (letrec ([is-circular?
            (lambda (f fs)
              (ormap 
               (lambda (dep)
                 (or (in-list? dep fs)
                     (let ([dep_f (assq dep all-formulas)])
                       (and dep_f
                            (is-circular? dep_f (cons (car dep_f) fs))))))
               (formula-dependencies (cadr f))))])
    (set! all-circular-formulas empty)
    (set! all-non-circular-formulas empty)
    (for-each
     (lambda (f)
       (if (is-circular? f (list (car f)))
           (set! all-circular-formulas (cons f all-circular-formulas))
           (set! all-non-circular-formulas (cons f all-non-circular-formulas))))
     all-formulas)
    (set! all-non-circular-formulas (reverse all-non-circular-formulas))))

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
   "Color" rgb)
  (com-set-property!
   (com-get-property rng "Borders")
   "Color" 11842740))

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

(define (empty-unit) (list (list 'empty_unit 1)))

(define (empty-unit? u)
  (and (= (length u) 1)
       (eq? (car (first u)) 'empty_unit)
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
    (when (not (in-hash? hashed-units cell-name))
      (hash-table-put! 
       hashed-units cell-name
       (let ([unit (compute-formula formula cell-name)])
         (cond ((> (length unit) 1)
                (filter (lambda (u) (not (eq? (car u) 'empty_unit)))
                        unit))
               (else unit))))
      (printf "Computing unit for cell ~a~n" cell-name))
    (hash-table-get hashed-units cell-name)))

(define (compute-formula formula cell-loc)
  (match formula
    [($ xl-number name deps val) (empty-unit)]
    [($ cell-ref name cell-name)
     (cond
       ((in-hash? hashed-units name)
        (hash-table-get hashed-units name))
       ((not (not (assq name all-formulas)))
        (let ([u (compute-formula (cadr (assq name all-formulas)) cell-loc)])
          (hash-table-put! hashed-units name u)
          u))
       ((string=? "" (com-get-property (cellref->rng (formula-name formula)) "Text"))
        (list (list 'error/empty-formula cell-loc)))
       (else (empty-unit)))]
    [($ named-cell-ref name cell-name actual-name)
     (cond
       ((in-hash? hashed-units name)
        (hash-table-get hashed-units name))
       ((not (not (assq name all-formulas)))
        (let ([u (compute-formula (cadr (assq name all-formulas)) cell-loc)])
          (hash-table-put! hashed-units name u)
          u))
       ((string=? "" (com-get-property (cellref->rng (formula-name formula)) "Text"))
        (list (list 'error/empty-formula cell-loc)))
       (else (empty-unit)))]
    [($ binary-op name deps op arg1 arg2)
     (let ([arg1-unit (compute-formula arg1 cell-loc)]
           [arg2-unit (compute-formula arg2 cell-loc)])
       (case op
         [(+ -) (check-equal-units (list arg1-unit arg2-unit) cell-loc)]
         [(*) (cond
                ((empty-unit? arg1-unit) arg2-unit)
                ((empty-unit? arg2-unit) arg1-unit)
                (else (let ([result (gen-mult-units arg1-unit arg2-unit cell-loc)])
                        (cond ((null? result) (empty-unit))
                              (else result)))))]
         [(/) (cond
                ((empty-unit? arg1-unit) (map (lambda(u)
                                                (let ([name (car u)]
                                                      [exp (cadr u)])
                                                  (list name (- 0 exp))))
                                              arg2-unit))
                ((empty-unit? arg2-unit) arg1-unit)
                (else (let ([result (gen-div-units arg1-unit arg2-unit cell-loc)])
                        (cond ((null? result) (empty-unit))
                              (else result)))))]
         [(^) (cond
                [(empty-unit? arg2-unit)
                 (gen-exp-units arg1-unit
                                (cond
                                  [(xl-number? arg2) (xl-number-val arg2)]
                                  [(or (cell-ref? arg2)
                                       (named-cell-ref? arg2))
                                   (com-get-property (cellref->rng (formula-name arg2))
                                                     "Value")]
                                  [else 'bad-exp]) cell-loc)]
                [else ;;exp not a unitless integer
                 (list (list 'error/exponentiation cell-loc))])]))]
    [($ boolean-op name deps op arg1 arg2)
     (let ([arg1-unit (compute-formula arg1 cell-loc)]
           [arg2-unit (compute-formula arg2 cell-loc)])
       (if (equal? arg1-unit arg2-unit)
           (empty-unit)
           (list (list 'error/bool-non-empty cell-loc))))]
    [($ unary-op name deps op arg)
     (let ([arg-unit (compute-formula arg cell-loc)])
       (case op
         [(+ -) arg-unit]
         [else (error op "Unknown unary op")]))]
    [($ tbl-left name deps input-cell)
     (let ([left-cell (left-of cell-loc)]
           [formula-cell (get-formula-loc-up cell-loc)]
           [left-cell-unit empty]
           [input-cell-unit (compute-formula input-cell cell-loc)])
       (if (in-hash? hashed-units left-cell)
           (set! left-cell-unit (hash-table-get hashed-units left-cell))
           (set! left-cell-unit (compute-cell-unit (assoc left-cell all-formulas))))
       (compute-formula (replace-in-formula
                         (cadr (assq formula-cell all-formulas))
                         (formula-name input-cell) left-cell) cell-loc))]
    [($ tbl-top name deps input-cell)
     (let ([input-cell-unit (compute-formula input-cell cell-loc)]
           [top-cell (top-of cell-loc)]
           [formula-cell (get-formula-loc-left cell-loc)]
           [top-cell-unit empty])
       (if (in-hash? hashed-units top-cell)
           (set! top-cell-unit (hash-table-get hashed-units top-cell))
           (set! top-cell-unit (compute-cell-unit (assoc top-cell all-formulas))))
       (compute-formula (replace-in-formula
                         (cadr (assq formula-cell all-formulas))
                         (formula-name input-cell) top-cell) cell-loc))]
    [($ application name deps fun args) 
     (let ([arg-units (map (lambda (a)
                             (compute-formula a cell-loc)) args)])
       (case fun
         [(average sum min max) (check-equal-units arg-units cell-loc)]
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
                      (if (andmap (lambda (s_u)
                                    (= (modulo (cadr s_u) 2) 0))
                                  arg-unit)
                          (map (lambda (s_u)
                                 (list (car s_u) (/ (cadr s_u) 2)))
                               arg-unit)
                          (list (list 'error/invalid-sqrt-dimension cell-loc))))
                     (else (error fun "illegal use"))))]
         [(fact) (let ([arg-unit (first arg-units)])
                   (cond
                     ((= 1 (length args))
                      (if (empty-unit? arg-unit)
                          arg-unit
                          (list (list 'error/fact-non-unitless cell-loc))))
                     (else (error fun "illegal use"))))]
         [(isnumber)
          (cond ((= 1 (length arg-units)) (empty-unit))
                (else (error fun "illegal use")))]
         [(median stdev) (check-equal-units arg-units cell-loc)]
         [(frequency)
          (let ([args-unit (check-equal-units arg-units cell-loc)])
            (cond [(is-error-unit? args-unit) args-unit]
                  [else (empty-unit)]))]
         [(mod)
          (cond ((= 2 (length arg-units)) (first arg-units))
                (else (error fun "illegal use")))]
         [(na pi today now) (cond ((empty? args) (empty-unit))
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
                                            (matrix-row-units (car Ms) M1-row)
                                            cell-loc)]
                              [M2-col-unit (check-equal-units
                                            (matrix-col-units (cadr Ms) M2-col)
                                            cell-loc)])
                         (cond
                           ((is-error-unit? M1-row-unit) M1-row-unit)
                           ((is-error-unit? M2-col-unit) M2-col-unit)
                           (else (gen-mult-units M1-row-unit M2-col-unit cell-loc)))))
                      (else (error fun "illegal use"))))]
         
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

(define (compute-formulas)
  (for-each (lambda (f)
              (begin
                (compute-cell-unit f)
                (printf "Computed unit for cell ~a is ~a~n" 
                        (car f) (hash-table-get hashed-units (car f)))))
            all-non-circular-formulas))

(define (compute-formulas-circular)
  (compute-formulas)
  (printf "Assigning dimension variables for formulas.... ")
  (update-status! (format "+++UNIT CHECKING BEGIN+++~n"))
  (update-status! "Assigning dimension variables for formulas");
  (assign-variables)
  (printf "done~nCreating constraints.... ")
  (next-update-status! "Creating constraints")
  (create-constraints)
  (printf "done~nPruning constraints.... ")
  (next-update-status! "Pruning constraints")
  (prune-constraints)
  (replace-equiv-in-constraints)
  (printf "done~nFlattening constraints.... ")
  (next-update-status! "Flattening constraints")
  (flatten-constraints)
  (printf "done~nSolving constraints.... ")
  (next-update-status! "Solving constraints")
  (solve-constraints)
  (printf "done~n")
  (update-status! (format "done~n"))
  (update-status! (format "+++UNIT CHECKING END+++~n")))


(define (gen-alpha-var)
  (string->symbol (string-append "a_" (symbol->string (gensym)))))

(define (gen-beta-var)
  (string->symbol (string-append "b_" (symbol->string (gensym)))))

(define (assign-variables)
  (for-each (lambda (f)
              (let ([a_s (gen-alpha-var)])
                (hash-table-put! hashed-vars a_s 
                                 (list (car f) (list (list 'error/missing-dimensions (car f)))))
                (hash-table-put! hashed-units (car f) a_s)))
            all-circular-formulas))

(define (create-constraints)
  (for-each (lambda (f)
              (let ([a_s (hash-table-get hashed-units (car f))])
                (create-constraints-for-formula a_s (cadr f) (car f))))
            all-circular-formulas))

(define (create-constraints-for-formula sym formula cell-loc)
  (match formula
    [($ xl-number name deps val) (push-constraint (list '= sym (empty-unit)))]
    [($ cell-ref name cell-name)
     (cond [(in-hash? hashed-units name)
            (push-constraint
             (list '= sym (hash-table-get hashed-units name)))]
           [else (push-constraint (list '= sym (empty-unit)))])]
    [($ named-cell-ref name cell-name actual-name)
     (cond [(in-hash? hashed-units name)
            (push-constraint
             (list '= sym (hash-table-get hashed-units name)))]
           [else (push-constraint (list '= sym (empty-unit)))])]
    [($ binary-op name deps op arg1 arg2)
     (case op
       [(+ -)
        (push-constraint 
         (list '= sym (create-constraints-for-formula (gen-beta-var) arg1 cell-loc)))
        (push-constraint
         (list '= sym (create-constraints-for-formula (gen-beta-var) arg2 cell-loc)))]
       [(*) 
        (push-constraint 
         (list '@ sym 
               (create-constraints-for-formula (gen-beta-var) arg1 cell-loc)
               (create-constraints-for-formula (gen-beta-var) arg2 cell-loc)))]
       [(/) 
        (push-constraint 
         (list '@/ sym
               (create-constraints-for-formula (gen-beta-var) arg1 cell-loc)
               (create-constraints-for-formula (gen-beta-var) arg2 cell-loc)))]
       [(^)
        (push-constraint
         (list '= (create-constraints-for-formula
                   (gen-beta-var) arg2 cell-loc) (empty-unit)))])]
    [($ boolean-op name deps op arg1 arg2)
     (push-constraint 
      (list '= 
            (create-constraints-for-formula (gen-beta-var) arg1 cell-loc)
            (create-constraints-for-formula (gen-beta-var) arg2 cell-loc)))
     (push-constraint (list '= sym (empty-unit)))]
    [($ unary-op name deps op arg)
     (push-constraint
      (list '= sym (create-constraints-for-formula (gen-beta-var) arg cell-loc)))]
    [($ tbl-left name deps input-cell)
     (let* ([left-cell (left-of cell-loc)]
            [formula-cell (get-formula-loc-up cell-loc)]
            [formula-sym (create-constraints-for-formula 
                          (gen-beta-var) 
                          (replace-in-formula (cadr (assq formula-cell all-formulas))
                                              (formula-name input-cell) left-cell)
                          formula-cell)])
       (push-constraint (list '= sym formula-sym)))]
    [($ tbl-top name deps input-cell)
     (let* ([top-cell (top-of cell-loc)]
            [formula-cell (get-formula-loc-left cell-loc)]
            [formula-sym (create-constraints-for-formula
                          (gen-beta-var) 
                          (replace-in-formula (cadr (assq formula-cell all-formulas))
                                              (formula-name input-cell) top-cell)
                          formula-cell)])
       (push-constraint (list '= sym formula-sym)))]
    [($ application name deps fun args) 
     (let ([arg-syms (map (lambda (a)
                            (create-constraints-for-formula
                             (gen-beta-var) a cell-loc)) args)])
       (case fun
         [(average sum min max)
          (for-each (lambda (s)
                      (push-constraint (list '= sym s))) arg-syms)]
         [(not or and)
          (for-each (lambda (s)
                      (push-constraint (list '= s empty-unit))) arg-syms)
          (push-constraint (list '= sym (empty-unit)))]
         [(if) (cond
                 [(= 3 (length args))
                  (let ([if-sym (second arg-syms)]
                        [else-sym (third arg-syms)]
                        [test-sym (first arg-syms)])
                    (push-constraint (list '= test-sym (empty-unit)))
                    (push-constraint (list '= if-sym else-sym))
                    (push-constraint (list '= sym if-sym)))]
                 [else (error fun "illegal use")])]
         [(abs) (cond
                  [(= 1 (length args)) (push-constraint (list '= sym (first arg-syms)))]
                  [else (error fun "illegal use")])]
         [(ceiling round)
          (cond
            [(= 2 (length args))
             (let ([num-sym (first arg-syms)]
                   [prec-sym (second arg-syms)])
               (push-constraint (list '= prec-sym (empty-unit)))
               (push-constraint (list '= sym num-sym)))]
            [else (error fun "illegal use")])]
         [(large) (let* ([k-sym (first (reverse arg-syms))]
                         [array-syms (reverse (rest (reverse arg-syms)))])
                    (push-constraint (list '= k-sym (empty-unit)))
                    (for-each (lambda (s)
                                (push-constraint (list '= sym s)))
                              array-syms))]
         [(acos acosh asin asinh atan atanh cos cosh sin sinh tan tanh exp)
          (let ([arg-sym (first arg-syms)])
            (cond [(= 1 (length args))
                   (push-constraint (list '= arg-sym (empty-unit)))
                   (push-constraint (list '= sym (empty-unit)))]
                  [else (error fun "illegal use")]))]
         [(sqrt) (let ([arg-sym (first arg-syms)])
                   (cond
                     [(= 1 (length args))
                      (push-constraint (list '@/ sym arg-sym sym))]
                     [else (error fun "illegal use")]))]
         [(fact) (let ([arg-sym (first arg-syms)])
                   (cond
                     [(= 1 (length args))
                      (push-constraint (list '= arg-sym (empty-unit)))
                      (push-constraint (list '= sym (empty-unit)))]
                     [else (error fun "illegal use")]))]
         [(isnumber)
          (cond [(= 1 (length arg-syms)) (push-constraint (list '= sym (empty-unit)))]
                [else (error fun "illegal use")])]
         [(median stdev)
          (for-each (lambda (s) (push-constraint (list '= sym s))) arg-syms)]
         [(frequency)
          (when (not (empty? arg-syms))
            (let ([first-sym (first arg-syms)])
              (for-each (lambda (s) (push-constraint (list '= s first-sym))) (rest arg-syms))))
          (push-constraint (list '= sym (empty-unit)))]
         [(mod)
          (cond [(= 2 (length args)) (push-constraint (list '= sym (first arg-syms)))]
                [else (error fun "illegal use")])]
         [(na pi today now) 
          (cond [(empty? args) (push-constraint (list '= sym (empty-unit)))]
                [else (error fun "illegal use")])]
         [(mmult) (let* ([formula-text (lookup-formula-text cell-loc)]
                         [Ms (identify-matrices
                              (substring formula-text 7
                                         (sub1 (string-length formula-text))))])
                    (cond
                      [(= 2 (length Ms))
                       (let* ([cell-xy (get-row-col-num cell-loc)]
                              [M1-row (car cell-xy)]
                              [M2-col (cadr cell-xy)]
                              [M1-row-sym (gen-beta-var)]
                              [M2-col-sym (gen-beta-var)])
                         (for-each (lambda (s)
                                     (push-constraint (list '= M1-row-sym s)))
                                   (matrix-row-syms (car Ms) M1-row))
                         (for-each (lambda (s)
                                     (push-constraint (list '= M2-col-sym s)))
                                   (matrix-col-syms (cadr Ms) M2-col))
                         (push-constraint (list '@ sym M1-row-sym M2-col-sym)))]
                      [else (error fun "illegal use")]))]         
         [else (push-constraint (list '= sym (list (list 'error/unimplemented-function 1))))]))])
  sym)

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

(define (prune-constraints)
  (letrec ([loop (lambda ()
                   (when (eq-constraints-left?)
                     (let* ([c (pop-eq-constraint)]
                            [get-syms 
                             (lambda (c)
                               (cond [(or (dim? (second c)) (is-error-unit? (second c)))
                                      (list (third c) (second c))]
                                     [else (list (second c) (third c))]))]
                            [syms (get-syms c)]
                            [sym1 (first syms)]
                            [sym2 (second syms)])
                       (if (or (dim? sym2) (is-error-unit? sym2))
                           (resolve-var-dim sym1 sym2)
                           (resolve-var-var sym1 sym2))
                       (loop))))])
    (loop)))

(define (replace-equiv-in-constraints)
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
                 (let* ([rep (first 
                              (find-rep 
                               (first (hash-table-get hashed-constraints-vars op))))]
                        [rep-u (second (hash-table-get hashed-constraints-vars rep))])
                   (if (empty? rep-u)
                       rep
                       rep-u))
                 op))
            operands))]
         [map-replace 
          (lambda (c)
            (cons (constraint-operator c)
                  (cons (replace-head (second c))
                        (replace-operands (rest (rest c))))))])
    (set! constraints 
          (list empty (remove-duplicates (map map-replace (second constraints)))))))

(define (resolve-var-dim var u)
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
                            [else (list (list 'error/equality var))])])])
          (hash-table-put! hashed-constraints-vars  var
                           (list (first tbl-entry) var-u))
          (let ([rep-u
                 (cond [(empty? (second rep-tbl-entry)) var-u]
                       [(not (or (equal? (second rep-tbl-entry) u)
                                 (is-error-unit? (second rep-tbl-entry))))
                        (cond [(is-error-unit? u) u]
                              [else (list (list 'error/equality rep))])])])
            (hash-table-put! hashed-constraints-vars rep
                             (list (first rep-tbl-entry) rep-u)))))))

(define (resolve-var-var var1 var2)
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

(define (flatten-constraints)
  (let ([flat-constraints 
         (map (lambda (c)
                (cons (constraint-var c) (cons '= (flatten-constraint empty c))))
              (second constraints))])
    (set! constraints (list (first constraints) (normalize-constraints flat-constraints)))))

(define (flatten-constraint deps constraint)
  (let ([left-side (constraint-left-side constraint)]
        [right-side (constraint-right-side constraint)]
        [operator (constraint-operator constraint)]
        [var (constraint-var constraint)])
    (cond
      [(and (dim-var? left-side) (dim-var? right-side))
       (append (expand-var left-side (cons var deps))
               (cons operator 
                     (expand-var right-side (cons var deps))))]
      [(dim-var? left-side)
       (append (expand-var left-side (cons var deps))
               (list operator right-side))]
      [(dim-var? right-side)
       (append (list left-side operator)
               (expand-var right-side (cons var deps)))]
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

(define (solve-constraints)
  (let* ([cs (second constraints)]
         [solved-cs (second-pass-solving-constraints
                     (first-pass-solving-constraints cs))])
    (set! constraints (list (first constraints) solved-cs)))
  (resolve-constraints-vars)
  (replace-constraints-vars))

(define (first-pass-solving-constraints cs)
  (let ([1st-pass-values (make-hash-table)])
    (for-each
     (lambda (c)
       (let ([var (first c)]
             [right-side (rest (rest c))])
         (when (not (not (assq var right-side)))
           (let ([var_exp (- (second (assq var right-side)) 1)]
                 [filtered-right-side (filter (lambda (u)
                                                (not (eq? (first u) var)))
                                              right-side)])
             (cond [(= 0 var_exp)
                    (let* ([vars-dims (split-list filtered-right-side
                                                 (lambda (u) (dim-var? (first u))))]
                           [vars (first vars-dims)]
                           [dims (second vars-dims)])
                      (when (= 1 (length vars))
                        (let ([new-var (first (first vars))]
                              [exp (- 0 (second (first vars)))])
                          (hash-table-put! 1st-pass-values new-var 'assigned-value)
                          (if (andmap (lambda (u) (integer? (/ (second u) exp))) dims)
                              (let ([u (map
                                        (lambda (u)
                                          (list (first u) (/ (second u) (- 0 exp))))
                                        dims)])
                                (resolve-var-dim var u))
                              (resolve-var-dim var (list (list 'error/non-integer-exp var)))))))]
                   [(andmap (lambda (u) (dim-var? (first u))) right-side)
                    (for-each 
                     (lambda (u)
                       (hash-table-put! 1st-pass-values (first u) 'assigned-values)
                       (resolve-var-dim (first u) (empty-unit)))
                     right-side)]
                   [(and (andmap (lambda (u) (not (dim-var? (first u))))
                                 filtered-right-side))
                    (hash-table-put! 1st-pass-values var 'assigned-value)
                    (if (andmap (lambda (u) (integer? (/ (second u) var_exp)))
                                 filtered-right-side)
                        (let ([u (map
                                  (lambda (u)
                                    (list (first u) (/ (second u) (- 0 var_exp))))
                                  filtered-right-side)])
                          (resolve-var-dim var u))
                        (resolve-var-dim var (list (list 'error/non-integer-exp var))))])))))
     cs)
    (replace-1st-pass-values 
     (filter 
      (lambda (c) (not (assq (first c) (rest (rest c)))))
      cs)
     1st-pass-values)))

(define (replace-1st-pass-values cs ht)
  (letrec ([my_expand 
            (lambda (result input)
              (cond [(empty? input) (reverse result)]
                    [else
                     (let ([u (first input)])
                       (cond [(and (list? u) (list? (first u)))
                              (my_expand (append u result) (rest input))]
                             [else (my_expand (cons u result) (rest input))]))]))])
    (hash-table-for-each 
     ht
     (lambda (var val)
       (set! cs
             (map 
              (lambda (c)
                (cons
                 (first c)
                 (cons
                  (second c)
                  (let ([u (second (hash-table-get hashed-constraints-vars var))])
                    (my_expand 
                     empty
                     (map 
                      (lambda (un)
                        (cond [(eq? (first un) var)
                               (map (lambda (_u)
                                      (list (first _u) (* (second _u) (second un))))
                                    u)]
                              [else un]))
                      (rest (rest c))))))))
              cs))))
    cs))

(define (second-pass-solving-constraints cs)
  (for-each
   (lambda (c)
     (let ([right-side (rest (rest c))])
       (cond [(andmap (lambda (u) (not (dim-var? (first u)))) right-side)
              (resolve-var-dim (first c) (simplify right-side))]
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
                                   [else (gen-mult-units left-side-u dims (first c))])]
                            [new-var (first (first vars))]
                            [new-exp (- 0 (second (first vars)))])
                        (if (andmap (lambda (u) (integer? (/ (second u) new-exp)))
                                    new-dims)
                            (let ([u (map
                                  (lambda (u)
                                    (list (first u) (/ (second u) (- 0 new-exp))))
                                  new-dims)])
                              (when (not (in-hash? hashed-constraints-vars new-var))
                                (hash-table-put! 1st-pass-values new-var u))
                          (resolve-var-dim new-var u))
                        (resolve-var-dim new-var 
                                         (list (list 'error/non-integer-exp new-var)))))))))])))
   cs)
  empty)

(define (simplify l)
  (let ([u_l (map (lambda (u) (list u)) l)])
    (letrec ([mu_l
              (lambda (mu l_u)
                (cond [(empty? l_u) mu]
                      [else (mu_l (gen-mult-units mu (first l_u) 'a0) (rest l_u))]))])
      (mu_l (first u_l) (rest u_l)))))

(define (resolve-constraints-vars)
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

(define (replace-constraints-vars)
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

(define (expand-var var deps)
  (if (and (in-constraints? var)
           (not (in-list? var deps)))
      (flatten-constraint deps (get-constraint var))
      (list var)))

(define (in-constraints? var)
  (ormap (lambda (c) (eq? var (second c))) (second constraints)))

(define (get-constraint var)
  (first (filter (lambda (c) (eq? var (second c))) (second constraints))))

(define (mark-error-cells)
  (hash-table-for-each
   hashed-units
   (lambda (cell unit)
     (if (is-error-unit? unit)
         (let ([cell-rng (cellref->rng cell)]
               [o (open-output-string)])
           (cond [(equal? 'error/propagated (first (first unit)))
                  (set-range-color! cell-rng (rgb 250 100 180))
                  (update-status! 
                   (format "Cell ~a error propagation from cell~a~n"
                           cell
                           (second (first unit))))
                  (fprintf o "ERROR propagated from cell: ~a"
                           (second (first unit)))]
                 [else
                  (set-range-color! cell-rng (rgb 0 250 0))
                  (update-status! (format "Cell ~a error in computed units!~n" cell))
                  (fprintf o "ERROR computed: ~n~a " (car (first unit)))
                  (fprintf o ", cell formula: ~a~n" (lookup-formula-text cell))])
           (insert-comment cell-rng (get-output-string o))
           (hash-table-put! error-cells cell 'on)
           (printf "Cell ~a error!~n" cell))))))

(define (unit-pp unit)
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
                          (substring bottom-string 1 bottom-len) ")")])))

(define (compare-actual-annotated)
  (for-each 
   (lambda (annot)         
     (let* ([cell (car annot)]
            [actual (hash-table-get hashed-units cell)])
       (if (not (or (in-hash? error-cells cell)
                    (empty-unit? (cadr annot))
                    (equal? actual (cadr annot))))
           (let ([cell-rng (cellref->rng cell)]
                 [o (open-output-string)])
             (set-range-color! cell-rng (rgb 0 200 190))
             (fprintf o "MISMATCH computed:~n~a " (unit-pp actual))
             (fprintf o "cell-formula: ~a~n" (lookup-formula-text cell))
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

(define (mark-bad-format-units)
  (when (not (empty? bad-format-units))
    (set! bad-format-units-colors (make-hash-table))
    (update-status! 
     (format "+++PREPROCESSING ERROR++~nImproper unit format for the following cells:~n"))
    (for-each 
     (lambda (bad-cell-with-unit)
       (let* ([bad-cell (first bad-cell-with-unit)]
              [bad-unit (second bad-cell-with-unit)]
              [bad-cell-rng (cellref->rng bad-cell)]
              [orig-col (com-get-property
                         (com-get-property bad-cell-rng "Interior")
                         "Color")])
         (update-status! (format "cell: ~a - Improper unit format~n" bad-cell))
         (hash-table-put! bad-format-units-colors bad-cell orig-col)
         (set-range-color! bad-cell-rng (rgb 120 170 120))))
     bad-format-units)))

(define (clear-bad-format-units)
  (when (not (empty? bad-format-units))
    (for-each
     (lambda (bad-cell-with-unit)
       (let* ([bad-cell (first bad-cell-with-unit)]
              [bad-unit (second bad-cell-with-unit)]
              [bad-cell-rng (cellref->rng bad-cell)]
              [orig-col (hash-table-get bad-format-units-colors bad-cell)])
         (set-range-color! bad-cell-rng orig-col)))
     bad-format-units)
    (set! bad-format-units-colors #f)
    (set! bad-format-units empty)))

(define (unmark-errors)
  (when (hash-table? error-cells)
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
                                   ((and (>= (string-length orig) 2)
                                         (string=? "()" (substring orig 0 2)))
                                    (delete-comment k))
                                   (else (set-comment k orig))))
                               (set-range-color! rng orig-col))))
      (set! error-cells #f)
      (set! colored-dependents #f))))

(define (original-comment comment)
  (letrec ([loop (lambda (str i max)
                   (cond ((= i max) str)
                         ((equal? #\; (string-ref str i)) (substring str 0 i))
                         (else (loop str (+ i 1) max))))])
    (loop comment 0 (string-length comment))))

(define (unit-check)
  (compute-formulas-circular)
  (update-status! (format "+++ERROR REPORTING+++~n"))
  (mark-error-cells)
  (compare-actual-annotated)
  (update-status! (format "+++XeLda DONE!+++~n")))

(define (unit-check-circular)
  (compute-formulas-circular)
  (update-status! (format "+++ERROR REPORTING+++~n"))
  (mark-error-cells)
  (compare-actual-annotated))

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
         [comment-text (with-handlers
                           ([void (lambda _ "()")])
                         (com-invoke comment-obj
                                     "Text"))])
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

(define (matrix-row-syms M row)
  (let ([row-cells (get-range-cells (first-cell-in-row M row)
                                    (last-cell-in-row M row))])
    (map (lambda (cellref)
           (let ([sym (gen-beta-var)])
             (cond [(in-hash? hashed-units cellref)
                    (let ([u (hash-table-get hashed-units cellref)])
                      (cond [(in-hash? hashed-vars u) u]
                            [else (push-constraint (list '= sym u)) 
                                  sym]))]
                   [else (push-constraint (list '= sym (empty-unit)))
                         sym])))
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

(define (matrix-col-syms M col)
  (let ([col-cells (get-range-cells (first-cell-in-col M col)
                                    (last-cell-in-col M col))])
    (map (lambda (cellref)
           (let ([sym (gen-beta-var)])
             (cond [(in-hash? hashed-units cellref)
                    (let ([u (hash-table-get hashed-units cellref)])
                      (cond [(in-hash? hashed-vars u) u]
                            [else (push-constraint (list '= sym u)) 
                                  sym]))]
                   [else (push-constraint (list '= sym (empty-unit)))
                         sym])))
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
  (with-handlers ([void (lambda _ '())])
    (parse-unit str)))

(define (split-large-args l)
  (let ([reversed-l (reverse l)])
    (list (list (first reversed-l)) (reverse (rest reversed-l)))));;)
