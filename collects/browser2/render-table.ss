
; See tables-test.ss for TEST CASES.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A structural-matrix
; (listof (listof cell))
; - The columns and rows may or may not be uniform.
;
; A naive-structural-matrix is a
; - structural-matrix without a uniform number of columns
;   per row, uniform widths per column, or uniform heights 
;   per row.
;
; An even-structural-matrix is a
; - structural-matrix with a uniform number of columns per row,
;   uniform widths per column, and uniform heights per row.
;
; > A cell is 
;  (make-cell width height tbody? snip row-ext col-ext)
; where width, height are integers
;       tbody? is boolean
;       snip is an editor-snip% OR false (implies cell padding)
;       row-ext, col-ext are cell-ext
(define-struct cell (width height tbody? snip row-ext col-ext))

; > A cell-ext is either
; - #f (implies rowspan=1 colspan=1)
; - (make-cell-ext width height tbody? rowspan colspan row-ext col-ext)
; where width, height, rowspan, colspan are integers
;       tbody? is boolean
;       row-ext, col-ext are cell-ext.
(define-struct cell-ext (width height tbody? rowspan colspan row-ext col-ext))

; A telt is either
; - tbody
; - tfoot
; - thead
;
; A colelt is either
; - col
; - colgroup
;
; Definitions for %Pixel, %Length, and %Multilength can be found
; here:  http://www.w3.org/TR/REC-html40/sgml/dtd.html#MultiLength
;
; A flattened-pasteboard is a pasteboard whose elements it contains
; are flattened, ie, can not be dragged or selected.
(define flattened-pasteboard%
  (class pasteboard% ()
    (override
      [can-delete? (lambda (snip) #f)]
      [can-interactive-move? (lambda (event) #f)]
      [can-interactive-resize? (lambda (snip) #f)]
      [can-resize? (lambda (snip w h) #f)]
      [can-move-to? (lambda (snip x y dragging)
                      #f)]
      [can-select? (lambda (snip on?)
                     #f)])
    (sequence (super-init))))

; The pasteboard used to get snip sizes for cell widths/heights during
; generate-naive-structural-matrix.  Later, snips rearranged and this
; pasteboard is inserted into the text passed into render-table.
(define the-pasteboard (make-object flattened-pasteboard%))

; A table-snip is an editor-snip% whose border width and cellspacing 
; can be specified.
(define table-snip%
  (class editor-snip% (editor border cellspacing table-width table-height)
    (rename [super-draw draw])
    (inherit get-admin)
    (sequence
      (super-init editor #f  (+ border cellspacing) (+ border cellspacing)  (+ border cellspacing) (+ border cellspacing)
                  0 0 0 0 table-width table-width table-height table-height))
    (private
      [l (box 0)]
      [t (box 0)]
      [r (box 0)]
      [b (box 0)])
    (override
      [draw 
       (lambda (dc x y left top right bottom dx dy draw-caret)
         (super-draw dc x y left top right bottom dx dy draw-caret)
         (local [(define admin (get-admin))]
           (when admin
             (send (send admin get-editor) get-snip-location this l t #f)
             (send (send admin get-editor) get-snip-location this r b #t)
             (local [(define w (- (unbox r) (unbox l)))
                     (define h (- (unbox b) (unbox t)))
                     (define (draw-rectangle-and-fill l t b r border)
                       (if (and (> border 0)
                                (> (- r l) 0)
                                (> (- b t) 0))
                           (send dc draw-rectangle l t (- r l) (- b t))
                           (void)))
                     (define (draw-border)
                       ; draws top side
                       (draw-rectangle-and-fill (+ border x)
                                             y
                                             (+ border y)
                                             (- (+ (unbox r) dx) border)
                                             border)
                       ; draws left side
                       (draw-rectangle-and-fill x
                                             (+ y border)
                                             (- (+ (unbox b) dy) border)
                                             (+ x border)
                                             border)
                       ;draws right side
                       (draw-rectangle-and-fill (- (+ (unbox r) dx) border)
                                             (+ y border)
                                             (- (+ (unbox b) dy) border)
                                             (+ (unbox r) dx)
                                             border)
                       ; draws bottom side
                       (draw-rectangle-and-fill (+ border x)
                                             (- (+ (unbox b) dy) border)
                                             (+ (unbox b) dy)
                                             (- (+ (unbox r) dx) border)
                                             border)
                       ; draws left-top square
                       (draw-rectangle-and-fill x
                                             y
                                             (+ y border)
                                             (+ x border)
                                             border)
                       ; draws right-top square
                       (draw-rectangle-and-fill (- (+ (unbox r) dx) border)
                                             y
                                             (+ y border)
                                             (+ (unbox r) dx)
                                             border)
                       ; draws left-bottom square
                       (draw-rectangle-and-fill x
                                             (- (+ (unbox b) dy) border)
                                             (+ (unbox b) dy)
                                             (+ x border)
                                             border)
                       ; draws right-bottom square
                       (draw-rectangle-and-fill (- (+ (unbox r) dx) border)
                                             (- (+ (unbox b) dy) border)
                                             (+ (unbox b) dy)
                                             (+ (unbox r) dx)
                                             border))]
               (when (and (< (+ cellspacing border) w)
                          (< (+ cellspacing border) h))
                 (local [(define orig-pen (send dc get-pen))
                         (define orig-brush (send dc get-brush))]
                   (send dc set-pen (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
                   (send dc set-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))
                   (draw-border)
                   (send dc set-brush orig-brush)
                   (send dc set-pen orig-pen)))))))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM DEFS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; transpose : structural-matrix -> structural-matrix
; Tranposes (x,y) to (y, x), such that a matrix of 
; n-columns and m-rows becomes the matrix of m-columns
; and n-rows.  Each row must have the same number of columns.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (transpose a-matrix)
  (cond [(empty? a-matrix) empty]
        [else (apply map list a-matrix)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-table : text% html:table -> void
; To render a-table on a-text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-table a-text a-table)
  (local [(define how-to-align (get-attribute-value (html:html-element-attributes a-table)
                                                    'align
                                                    "left"))]
    (align-paragraph a-text a-table (lambda (a-table)
                                      (render-even-structural-matrix 
                                       (generate-even-structural-matrix a-table a-text)
                                       a-table
                                       a-text))
                     how-to-align)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generate-even-structural-matrix : (struct: html:table) text% -> even-structural-matrix
; To generate an even-structural-matrix - a structural representation of a-table renderable on a-text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-even-structural-matrix a-table a-text)
  (local [(define naive (generate-naive-structural-matrix a-table))]
    (process-explicit-definitions naive a-table a-text)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GENERATE NAIVE STRUCTURAL MATRIX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generate-naive-structural-matrix : table -> naive-structural-matrix
; To produce a naive-structural-matrix which reflects the table's 
; contents row by row and cell by cell.  It is the representation of the
; most basic, elemental parts of the table:  the cell definitions, or TDs.
; Each TD defined in the table is represented by a listof cell, with one 
; cell-struct and 0 or more cell-ext for "merged cells" (cells spanning 
; multiple columns and/or rows).  A table row, or TR, is represented by
; a listof these cells; the final result, a list of list of cells, 
; represents the table cell-wise only, with the information gleaned from
; the TD's in a-table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-naive-structural-matrix a-table)
  (local [(define attributes (html:html-element-attributes a-table))
          (define cellpadding (string->exact-non-negative-integer
                               (get-attribute-value attributes 'cellpadding "0")
                               0))
          (define rules? (cond [(string-ci=? (get-attribute-value attributes 'rules "void") "all")
                                #t]
                               [else #f]))
          (define contents-thead (filter html:thead?
                                         (html:html-full-content a-table)))
          (define contents-tfoot (filter html:tfoot?
                                         (html:html-full-content a-table)))
          (define contents-tbody (filter html:tbody?
                                         (html:html-full-content a-table)))]
    (setup-matrix/lotelt contents-tfoot
                         cellpadding
                         rules?
                         #f
                         (setup-matrix/lotelt contents-tbody
                                              cellpadding
                                              rules?
                                              #t
                                              (setup-matrix/lotelt contents-thead cellpadding rules? #f empty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup-matrix/lotelt : (listof telt) num boolean boolean naive-structural-matrix -> naive-structural-matrix
; To create a naive-structural-matrix by combining a previously accumulated naive-structural-
; matrix - accum, with the naive-structural-matrix representing the lotelt.  If tbody? is true,
; alotelt is a (listof tbody), else either (listof thead) or (listof tfoot).
; invariant: accum is the naive-structural-matrix already processed in the traversal of the lotlelt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-matrix/lotelt lotelt cellpadding rules? tbody? accum)
  (setup-matrix/lotr (filter html:tr? (apply append (map html:html-full-content lotelt)))
                     cellpadding rules? tbody? accum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup-matrix/lotr : (listof tr) num boolean boolean naive-structural-matrix -> naive-structural-matrix
; To create a naive-structural-matrix by combining a previously accumulated naive-structural-
; matrix - accum, with the naive-structural-matrix representing alotr.  If tbody? is true,
; alotr proceeds from a TBODY rowgroup, else either THEAD or TFOOT.
; invariant: accum is the already processed naive-structural-matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-matrix/lotr alotr cellpadding rules? tbody? accum)
  (cond [(empty? alotr) accum]
        [else (setup-matrix/lotr (rest alotr)
                                 cellpadding rules? tbody?
                                 (setup-matrix/tr (first alotr) cellpadding rules? tbody? accum))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup-matrix/tr : tr num rules? boolean naive-structural-matrix- -> naive-structural-matrix
; To create a naive-structural-matrix based on a tr, ie a table row, and append it to the 
; accumulated naive-structural-matrix.  If tbody? is true, this row proceeds from a tbody
; row group.
; invariant: accum is the already processed naive-structural-matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-matrix/tr a-tr cellpadding rules? tbody? accum)
  (local [(define trcontents (filter (lambda (content) 
                                       (or (html:td? content)
                                           (html:th? content)))
                                     (html:html-full-content a-tr)))]
    (setup-matrix/lotd trcontents cellpadding rules? tbody? accum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup-matrix/lotd : (listof td | th) num boolean boolean naive-structural-matrix -> naive-structural-matrix
; Constructs a new (listof cell) based on alotd and the last row of the accumulated naive-structural-
; matrix.  Appends the new row to the end of the accumulated naive-structural-matrix.  If tbody? is
; true, this row proceeds from a TBODY rowgroup, otherwise THEAD or TFOOT.
; invariant: accum is an accumulated naive-structural-matrix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-matrix/lotd alotd cellpadding rules? tbody? accum)
  (local [(define last-row (cond [(empty? accum) empty]
                                 [else (car (last-pair accum))]))]
    (cond [(empty? alotd) accum]
          [else 
           (append accum (list (setup-matrix-row alotd last-row cellpadding rules? tbody?)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup-matrix-row : (listof td | th) (listof cell) num boolean boolean -> (listof cell)
; Creates a listof cell from alotd, consulting along the way prev-row - the last row from the previous 
; accumulated naive-structural-matrix.  Definitions of and references to cell-ext's are placed in 
; strategically appropriate locations for creating "merged cells" in the naive-structural-matrix.  In
; the error case, when a column in alotd spans across a row span, the column span is simply picked over 
; the row span.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-matrix-row alotd prev-row cellpadding rules? tbody?)
  (cond [(empty? alotd) 
         empty]
        [(empty? prev-row)
         (expand-cell (create-cell (first alotd) cellpadding rules? tbody?)
                      (setup-matrix-row (rest alotd) prev-row cellpadding rules? tbody?))]
        [(boolean? (cell-dim 'row-ext (first prev-row)))
         (expand-cell (create-cell (first alotd) cellpadding rules? tbody?)
                      (setup-matrix-row (rest alotd) (rest prev-row) cellpadding rules? tbody?))]
        [else 
         (expand-cell (cell-dim 'row-ext (first prev-row))
                           (setup-matrix-row alotd (rest prev-row) cellpadding rules? tbody?))]))

; expand-cell : cell (listof cell) -> (listof cell)
; Extracts "linked" cell-ext and places it in the list.  
; When no more cell-ext exist, therest is returned.
(define (expand-cell a-cell therest)
  (local [(define ext (cell-dim 'col-ext a-cell))]
    (cond [(boolean? ext) (cons a-cell therest)]
          [else (cons a-cell 
                      (expand-cell ext therest))])))

; create-cell : td num boolean boolean -> cell
; Creates a cell belonging (or not) to the tbody rowgroup, checking first for cell-ext in the
; cell above.
(define (create-cell a-td cellpadding rules? tbody?)
  (local [(define td-attribs (html:html-element-attributes a-td))
          (define rowspan (string->number (get-attribute-value td-attribs 'rowspan "1")))
          (define colspan (string->number (get-attribute-value td-attribs 'colspan "1")))
          (define listof-G2 (html:html-full-content a-td))
          (define (create-col-ext span)
            (cond [(= 1 span) #f]
                  [else (make-cell-ext 0 0 tbody? 1 (sub1 span) #f (create-col-ext (sub1 span)))]))
          (define (create-row-ext span col-ext)
            (cond [(= 1 span) #f]
                  [else (make-cell-ext 0 0 tbody? (sub1 span) 1 (create-row-ext (sub1 span) col-ext) col-ext)]))
          (define col-ext (if (<= colspan 1) #f (create-col-ext colspan)))
          (define row-ext (if (<= rowspan 1) #f (create-row-ext rowspan col-ext)))
          (define editor (make-object text%))
          (define initpos (send editor get-start-position))]
    (send editor set-styles-sticky #f)
    (send editor auto-wrap #t)
    (if tbody?
        (for-each (lambda (aG2) (render-G2 editor aG2)) listof-G2)
        (begin
          (for-each
           (lambda (aG2)
             (align-paragraph editor aG2 (lambda (a-G2)
                                           (render-G2 editor a-G2)) "center")
             (boldify editor initpos (send editor get-start-position)))
           listof-G2)
          (if (zero? (send editor line-length (send editor last-line)))
              (send editor delete (send editor get-start-position) 'back #f))))
    (local [(define frame (make-object frame% "" #f 800 800))
            (define text (make-object text%))
            (define editor-canvas (make-object editor-canvas% frame text))
            (define temp-esnip (make-object editor-snip% editor rules? cellpadding cellpadding cellpadding cellpadding
                                 0 0 0 0 'none 'none 'none 'none))]
      (send frame reflow-container)
      (send text insert temp-esnip)
      (local [(define-values (editor-width editor-height)
                (send editor get-max-view-size))]
        (send temp-esnip resize (add1 editor-width) editor-height)
        (send temp-esnip release-from-owner)
        (make-cell (+ (add1 editor-width)
                      (* 2 cellpadding))
                   (+ editor-height
                      (* 2 cellpadding))
                   tbody? temp-esnip row-ext col-ext)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROCESS EXPLICIT DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; process-explicit-definitions : naive-structural-matrix table text% -> even-structural-matrix
; Modifies a-matrix to reflect explicit structural information in a-table and produce a 
; renderable structural-matrix.  Explicitly defined column groupings, table cellspacing and 
; cellpadding, and explicitly defined table width are incorporated into a-matrix here.
;
; For each column, differences between explicitly and implicitly defined attributes
; of the table are reconciled with preference for the explicit definition.  However, the
; implicit definition will be used finally in the case of inconsistent, erroneous, or illogical
; explicit definitions.
;
; For each column, differences in the number of columns are reconciled by padding:
; for-each-row(not (= 0 (abs (- TotalExplicitColumnDefs TotalImplicitColumnDefs))))
; with 0x0 cells, in the extend-col-spans function.

; Secondly, each column's width is calculated and then resized relative to the explicit table 
; width.  Two passes across a-matrix are made: first, to resolve all column widths specified as 
; percentages.  After this pass, the minimum table width can then be found.  In order
; to complete pass two, a table width is required.  The table-width is the maximum of the implied width,
; found AFTER pass1, and the explicit width specified in the attributes.
;
; For pass 2, all column widths specified as proportions can now be resolved.
; Because the proportion is a length relative to the width of the entire table, in order to 
; resolve a proportional-width column, all other column widths must be resolved so that the 
; left over space can be alotted to the proportional-length columns. If no proportional-width 
; columns exist AND there is left over space in the table (controlled by the explicitly defined 
; table width), all unused space will then be alotted proportionally to all columns.
; (see scale-matrix-to-proportion!).
;
; Finally, each row's height and column's width will be resized to the maximum in that grouping,
; and the result is a renderable, even-structural-matrix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (process-explicit-definitions a-matrix a-table a-text)
  (local [(define table-contents (html:html-full-content a-table))
          (define table-attribs (html:html-element-attributes a-table))
          (define listofcols (filter (lambda (x) (or (html:col? x)
                                                     (html:colgroup? x)))
                                     table-contents))
          (define-values (text-width text-height) (send a-text get-max-view-size))
          (define cellspacing (string->exact-non-negative-integer
                               (get-attribute-value table-attribs 'cellspacing "0")
                               0))
          (define cellpadding (string->exact-non-negative-integer
                               (get-attribute-value table-attribs 'cellpadding "0")
                               0))
          (define border (string->exact-non-negative-integer
                          (get-attribute-value table-attribs 'border "0")
                          0))
          (define explicit-table-width (length->pixels
                                           (get-attribute-value table-attribs 'width "0")
                                           text-width
                                           0))
          ; padded structural-matrix
          (define extended-structural-matrix
            (extend-col-spans a-matrix listofcols))
          
          ; maximum width, in pixels, of the display available to table within contraints imposed by table's author
          (define available-space (cond [(zero? explicit-table-width)
                                         (- text-width (* cellspacing (add1 (count-columns extended-structural-matrix))))]
                                        [else
                                         (- explicit-table-width (* cellspacing (add1 (count-columns extended-structural-matrix))))]))
          ; resizes columns of percentage length
          (define matrix-with-resolved-%length 
            (resize-columns extended-structural-matrix
                            listofcols  
                            (lambda (attribs default-width-value)
                              (determine-percent-length attribs
                                                        available-space
                                                        default-width-value))
                            resize-column-logic/percent))]
    ; evens up all row heights and column widths
    (make-rows-and-columns-even! matrix-with-resolved-%length)
    
    ; removes "ghost" columns and makes row heights and column widths even
    (local [(define a-matrix-without-ghost-columns (remove-ghost-columns matrix-with-resolved-%length))
            (define implicit-table-width (get-implicit-table-width matrix-with-resolved-%length cellspacing border))
            (define total-portions (cond [(empty? listofcols) 0]
                                         [else (apply + (map get-portions listofcols))]))
            (define total-columns (length (transpose matrix-with-resolved-%length)))
            ; visible space remaining in the frame
            (define remaining-available-space (- explicit-table-width implicit-table-width))
            
            (define scale (cond [(zero? total-columns) 0]
                                [(zero? total-portions) (inexact->exact (round (/ remaining-available-space total-columns)))]
                                [else (inexact->exact (round (/ remaining-available-space total-portions)))]))]
      ; resizes columns of proportional width OR all columns in order to stretch table
      (if (and (> total-portions 0) (> explicit-table-width implicit-table-width))
          (resize-columns 
           a-matrix-without-ghost-columns
           listofcols 
           (lambda (attribs default)
             (* scale (get-attributes-width-proportion attribs)))
           resize-column-logic/proportion!)
          (if (and (<= total-portions 0) (> explicit-table-width implicit-table-width))
              (begin 
                (scale-matrix-to-proportion! a-matrix-without-ghost-columns remaining-available-space)
                a-matrix-without-ghost-columns)))

      ; resizes cells and snips for merged-cells
      (setup-merged-cells! a-matrix-without-ghost-columns cellspacing)
      a-matrix-without-ghost-columns)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; resize-columns : structural-matrix (listof colelt) ((listof attribute) num -> num) ((listof cell) num -> (listof cell)) -> structural-matrix
; Abstract function for traversing a structural-matrix and list of explicit column definitions (that is, col or colgroup tags) simultaneously. 
; Resize the columns using factor-func to calculate the new width in pixels, and using resize-func to perform the resizing on a listof cells.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resize-columns a-matrix a-listof-colelt factor-func resize-func)
  (local [(define lo-matrix-columns (transpose a-matrix))
          (define (resize/transposed transposed-matrix locols)
            (cond [(or (empty? transposed-matrix)
                       (empty? locols)) transposed-matrix]
                  [else
                   (append
                    (resize-columns/elt (build-list (get-span (first locols))
                                                    (lambda (i)
                                                      (list-ref transposed-matrix i)))
                                        (first locols)
                                        0
                                        factor-func
                                        resize-func)
                    (resize/transposed (list-tail transposed-matrix (get-span (first locols)))
                                       (rest locols)))]))]
    (transpose (resize/transposed lo-matrix-columns a-listof-colelt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; resize-columns/elt : (listof (listof cell)) colelt num ((listof attribute) num -> num) ((listof cell) num -> (listof cell)) -> (listof (listof cell))
; Abstract function for resizing the column widths of a transposed structural-matrix, a-lo-columns, according to the explicit column definition given in
; a-col-elt, using factor-func to calculate the magnitude of the resizing and using resize-func to perform the resizing on a column.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resize-columns/elt a-lo-columns a-col-elt default-value factor-func resize-func)
  (local [(define contents (cond [(html:colgroup? a-col-elt) (filter (lambda (content)
                                                                       (or (html:col? content)
                                                                           (html:colgroup? content)))
                                                                     (html:html-full-content a-col-elt))]
                                 [else empty]))
          (define attribs (html:html-element-attributes a-col-elt))
          (define implicit-span (get-implicit-span a-col-elt))
          (define explicit-span (get-explicit-span a-col-elt))
          (define span-def-diff (abs (- explicit-span implicit-span)))
          (define greater-span (cond [(> explicit-span implicit-span) explicit-span]
                                     [else implicit-span]))
          (define lesser-span (cond [(> explicit-span implicit-span) implicit-span]
                                     [else explicit-span]))
          (define factor (factor-func attribs default-value))
          (define (resize-columns/transposed transposed-matrix a-listof-colelt default factorfunc resizefunc)
            (cond [(or (empty? a-listof-colelt)
                       (empty? transposed-matrix)) transposed-matrix]
                  [else
                   (append
                    (resize-columns/elt (build-list (get-span (first a-listof-colelt))
                                                    (lambda (i)
                                                      (list-ref transposed-matrix i)))
                                        (first a-listof-colelt)
                                        default
                                        factorfunc
                                        resizefunc)
                    (resize-columns/transposed (list-tail transposed-matrix (get-span (first a-listof-colelt)))
                                               (rest a-listof-colelt) default factorfunc resizefunc))]))]
    (cond [(empty? contents)
           (map (lambda (a-column)
                  (resize-func a-column factor))
                a-lo-columns)]
          [(or (= 0 span-def-diff)
               (>= implicit-span explicit-span))
           (resize-columns/transposed a-lo-columns contents factor factor-func resize-func)]
          [else
           (append
            (resize-columns/transposed 
             (build-list implicit-span
                         (lambda (i) (list-ref a-lo-columns i)))
             contents
             default-value
             factor-func
             resize-func)
            (map (lambda (a-column)
                   (resize-func a-column factor))
                 (list-tail a-lo-columns implicit-span)))])))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; determine-percent-length : (listof attribute) num num -> num
; If the explicit width definition in attribs is a percentage,
; returns the percentage of the display-length.
; Otherwise, returns the default-value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (determine-percent-length attribs display-length default-value )
  (local [(define default-value-string (number->string default-value))]
    (cond [(empty? attribs) default-value]
          [else
           (length->pixels
            (get-attribute-value attribs 'width default-value-string)
            display-length
            default-value)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; resize-column-logic/percent : (listof cell) num -> (listof cell)
; To produce a list of cell whose width value is the largest between the explicit-width - width-in-pixels, 
; and the implicit width, the value in the cell-width field.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resize-column-logic/percent alocd width-in-pixels)
  (local [(define (make-new-cell)
            (cond [(cell? (first alocd))
                   (set-cell-width! (first alocd) width-in-pixels)
                   (first alocd)]
                  [(cell-ext? (first alocd))
                   (set-cell-ext-width! (first alocd) width-in-pixels)
                   (first alocd)]))]
    (cond [(empty? alocd) empty]
          [(>= (cell-dim 'width (first alocd)) width-in-pixels)
           (cons (first alocd) (resize-column-logic/percent (rest alocd) width-in-pixels))]
          [else 
           (cons (make-new-cell)
                 (resize-column-logic/percent (rest alocd) width-in-pixels))])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cell-dim : symbol cell -> object
; If a-cell is a cell-struct, field is one of: '(width height tbody? row-ext col-ext)
; If a-cell is a cell-ext-struct, field is one of: '(width height tbody? colspan rowspan row-ext col-ext)
; Returns the value from the selector of the field.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cell-dim field a-cell)
  (cond [(and (symbol=? 'width field)
              (cell? a-cell))
         (cell-width a-cell)]
        [(and (symbol=? 'width field)
              (cell-ext? a-cell))
         (cell-ext-width a-cell)]
        [(and (symbol=? 'height field)
              (cell? a-cell))
         (cell-height a-cell)]
        [(and (symbol=? 'height field)
              (cell-ext? a-cell))
         (cell-ext-height a-cell)]
        [(and (symbol=? 'tbody? field)
              (cell? a-cell))
         (cell-tbody? a-cell)]
        [(and (symbol=? 'tbody? field)
              (cell-ext? a-cell))
         (cell-ext-tbody? a-cell)]
        [(and (symbol=? 'row-ext field)
              (cell? a-cell))
         (cell-row-ext a-cell)]
        [(and (symbol=? 'row-ext field)
              (cell-ext? a-cell))
         (cell-ext-row-ext a-cell)]
        [(and (symbol=? 'col-ext field)
              (cell? a-cell))
         (cell-col-ext a-cell)]
        [(and (symbol=? 'col-ext field)
              (cell-ext? a-cell))
         (cell-ext-col-ext a-cell)]
        [(symbol=? 'rowspan field)
         (cell-ext-rowspan a-cell)]
        [(symbol=? 'colspan field)
         (cell-ext-colspan a-cell)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; resize-column-logic/proportion! : (listof cell) num -> (listof cell)
; To produce a list of cell whose width value is the sum of the implicit column width and the scale, 
; that is, the magnitude of change from the implicit column's width.
;
; EFFECT: For each aloc, the width is increased by scale.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (resize-column-logic/proportion! aloc scale)
  (local [(define implicit-column-width (apply max (map cell-width aloc)))
          (define (make-new-cell cell)
            (cond [(cell? cell)
                   (set-cell-width! cell (+ scale implicit-column-width))]
                  [(cell-ext? cell)
                   (set-cell-ext-width! cell (+ scale implicit-column-width))]))]
    (for-each make-new-cell aloc)
    aloc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scale-matrix-to-proportion! : structural-matrix num -> structural-matrix
; Stretches a-matrix proportionally such that its new width is the implicit-table-width plus the
; remaining-available-space.
; EFFECT: For each column, its width is changed to the sum of the column's current width and
;         the percentage of remaining/unused table space proportional to the percentage of
;         space occupied by that column in the minimum space of the table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (scale-matrix-to-proportion! a-matrix remaining-available-space)
  (local [(define a-lo-columns (transpose a-matrix))
          (define matrix-width (apply max (map get-row-width
                                               a-matrix)))
          (define (make-new-cell cell)
            (cond [(cell? cell)
                   (set-cell-width! cell (round (+ (cell-width cell)
                                                   (* (/ (cell-width cell) matrix-width)
                                                      remaining-available-space))))]
                  [(cell-ext? cell)
                   (set-cell-ext-width! cell (round (+ (cell-ext-width cell)
                                                       (* (/ (cell-ext-width cell) matrix-width)
                                                          remaining-available-space))))]))]
    (transpose (map (lambda (a-column) (for-each make-new-cell a-column)
                                        a-column) a-lo-columns))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-implicit-table-width : structural-matrix num num -> num
; Returns the width of the table implied by the minimum width
; of each column plus cellspacing and cellpadding used on that
; row.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-implicit-table-width a-matrix cellspacing border)
  (local [(define lo-min-row-width (map (lambda (a-row)
                                          (get-minimum-row-width a-row cellspacing))
                                        a-matrix))]
    (cond [(empty? lo-min-row-width) 0]
          [else 
           (+ (* 2 border)
                   (apply max lo-min-row-width))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-minimum-row-width : (listof cell) num -> num
; Returns the minimum width of the row defined by the 
; width of each cell and cellspacing and cellpadding.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-minimum-row-width a-row cellspacing)
  (local [(define total-cellspacing (* cellspacing (length a-row)))]
    (+ (apply + (map (lambda (a-cell)
                       (cell-dim 'width a-cell))
                     a-row))
       total-cellspacing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-portions : colelt -> num
; Returns the total number of portions (single column definition) in a-colelt.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-portions a-colelt)
  (max (get-explicit-portions a-colelt)
       (get-implicit-portions a-colelt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-explicit-portions : colelt -> num
; Returns the total number of portions explicitly defined in a-colelt.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-explicit-portions a-colelt)
  (local [(define attribs (html:html-element-attributes a-colelt))]
    (get-attributes-width-proportion attribs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-implicit-portions : colelt -> num
; Returns the total number of portions implicitly defined in a-colelt,
; or 0 for none.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-implicit-portions a-colelt)
  (local [(define contents (cond [(html:colgroup? a-colelt) (filter (lambda (content)
                                                                      (or (html:colgroup? content)
                                                                          (html:col? content)))
                                                                    (html:html-full-content a-colelt))]
                                 [else empty]))]
    (cond [(empty? contents) 0]
          [else (apply + (map get-portions contents))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-attributes-width-proportion : (listof attribute) -> num
; Returns the width's proportion, ie, relative length, or 0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-attributes-width-proportion attribs)
  (local [(define width (get-attribute-value attribs 'width "0"))
          (define last-char (string-ref width (sub1 (string-length width))))]
    (cond [(empty? attribs) 0]
          [(char=? last-char #\*)
           (string->exact-non-negative-integer
            (substring width 0 (sub1 (string-length width)))
            0)]
          [else 0])))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extend-col-spans : structural-matrix (listof colelt) -> structural-matrix
; Extends the number of columns to include the difference between the implictly 
; and explicitly defined column groups.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (extend-col-spans a-matrix listofcols)
  (local [(define implicit-number-of-columns (count-columns a-matrix))
          (define explicit-number-of-columns (sum-of-columns listofcols))
          (define greater (cond [(> explicit-number-of-columns implicit-number-of-columns) explicit-number-of-columns]
                                [else implicit-number-of-columns]))
          (define lesser (cond [(> explicit-number-of-columns implicit-number-of-columns) implicit-number-of-columns]
                               [else explicit-number-of-columns]))
          (define difference (abs (- greater lesser)))]
    (cond [(> difference 0) (map (lambda (a-row) (pad-a-row a-row greater)) a-matrix)]
          [else (map (lambda (a-row) (pad-a-row a-row lesser)) a-matrix)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sum-of-columns : (listof colelt) -> num
; To count the total number of columns represented in the list
; of col/colgroup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-of-columns alocols)
  (cond [(empty? alocols) 0]
        [else (apply + (map get-span alocols))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-span : colelt -> number
; Returns the span of a-colelt.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-span a-colelt)
  (max (get-explicit-span a-colelt) (get-implicit-span a-colelt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-explicit-span : colelt -> number
; Returns the span defined by the attributes of the COL or COLGROUP.  Default value is 1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-explicit-span a-colelt)
  (local [(define attributes (html:html-element-attributes a-colelt))]
    (string->exact-non-negative-integer
     (get-attribute-value (html:html-element-attributes a-colelt) 'span "1")
     1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-implicit-span : colelt -> number
; Returns the implicit span defined by the contents of the colelt.  Only applies to COLGROUP.  
; Otherwise, returns 1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-implicit-span a-colelt)
  (local [(define colelt-contents (cond [(html:colgroup? a-colelt)  (filter (lambda (content)
                                                                              (or (html:col? content)
                                                                                  (html:colgroup? content)))
                                                                            (html:html-full-content a-colelt))]
                                        [else empty]))]
    (cond [(empty? colelt-contents) 1]
          [else (apply + (map get-span colelt-contents))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pad-a-row : (listof cell) num -> (listof cell)
; To insert filler cells in remaining empty columns of the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pad-a-row alocd cols)
  (local [(define diff (- cols (length alocd)))
          (define therest (build-list diff (lambda (i) (make-cell 0 0 #t #f #f #f))))]
    (append alocd therest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; count-columns : structural-matrix -> num
; To count the total number of implicitly defined columns in a structural-matrix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count-columns a-matrix)
  (cond [(empty? a-matrix) 0]
        [else 
         (apply max (map length a-matrix))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RENDER STRUCTURAL-MATRIX ON A TEXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-even-structural-matrix : structural-matrix table text% -> void
; To render a structural-matrix derived from table, which contains
; the table's attributes, on a text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render-even-structural-matrix a-matrix a-table a-text)
  (local [(define attributes (html:html-element-attributes a-table))
          (define cellspacing (string->exact-non-negative-integer
                               (get-attribute-value attributes 'cellspacing "0")
                               0))
          (define cellpadding (string->exact-non-negative-integer
                               (get-attribute-value attributes 'cellpadding "0")
                               0))
          (define border (string->number (get-attribute-value attributes 'border "0")))
          (define frame? (cond [(or (>= border 1)
                                    (string-ci=? (get-attribute-value attributes 'frame "none") "box")) #t]))
          (define table-width (get-matrix-width a-matrix cellspacing cellpadding))
          (define table-height (+ 2 (get-matrix-height a-matrix cellspacing cellpadding)))
          (define table-editorsnip (make-object table-snip% the-pasteboard border cellspacing table-width table-height))
          (define-values (text-width text-height) (send a-text get-max-view-size))]
    (send a-text insert table-editorsnip)
    (setup-table-pasteboard a-matrix 1 cellpadding cellspacing)
    (set! the-pasteboard (make-object flattened-pasteboard%))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup-table-pasteboard : structural-matrix num num -> void
; Starting y pixels down the canvas, each row in the structural-matrix is positioned on
; the pasteboard as it will be seen by the user, exactly as it will be rendered.  Each
; cell has margins of cellpadding and all cells are spaced from each other a distance of 
; cellspacing.  The table will include cellspacing between all cells and its edges.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-table-pasteboard a-matrix y cellpadding cellspacing )
  (cond [(empty? a-matrix) (void)]
        [else 
         (setup-table-row-on-pasteboard (first a-matrix) 1 y cellspacing cellpadding)
         (setup-table-pasteboard 
               (rest a-matrix)
               (+ y (get-min-row-height (first a-matrix)) cellspacing)
               cellpadding cellspacing)]))

; get-min-row-height : (listof cell) -> num
; Returns the smallest height of all cells in the row.
(define (get-min-row-height aloc)
  (apply min (map (lambda (cell)
                    (cell-dim 'height cell)) aloc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; setup-table-row-on-pasteboard : (listof cell) num num num num -> void
; Returns a pasteboard with the list of editor snips inserted beginning at x y on the pasteboard with cellspacing padding cells in 
; the row.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-table-row-on-pasteboard alocells x y cellspacing cellpadding)
  (local [(define (col-ext? a-cell)
            (not (boolean? (col-ext a-cell))))
          (define (col-ext a-cell)
            (cond [(cell? a-cell) (cell-col-ext a-cell)]
                  [(cell-ext? a-cell) (cell-ext-col-ext a-cell)]))]
    (cond [(empty? alocells) (void)]
          [(cell-ext? (first alocells))
           (setup-table-row-on-pasteboard (rest alocells)
                                          (+ x 
                                             (cell-dim 'width (first alocells))
                                             cellspacing)
                                          y
                                          cellspacing
                                          cellpadding)]
          [(col-ext? (first alocells))
           (send the-pasteboard insert (cell-snip (first alocells)) x y)
           (setup-table-row-on-pasteboard (list-tail alocells (count-links (first alocells)))
                                          (+ x
                                             (cell-dim 'width (first alocells))
                                             cellspacing)
                                          y
                                          cellspacing
                                          cellpadding)]
          [(object? (cell-snip (first alocells))) 
           (send the-pasteboard insert (cell-snip (first alocells)) x y)
           (setup-table-row-on-pasteboard (rest alocells)
                                               (+ x 
                                                  (cell-dim 'width (first alocells))
                                                  cellspacing)
                                               y
                                               cellspacing
                                               cellpadding)]
          [else
           (setup-table-row-on-pasteboard (rest alocells)
                                          (+ x
                                             (cell-dim 'width (first alocells))
                                             cellspacing)
                                          y
                                          cellspacing
                                          cellpadding)])))

; count-links : cell -> num
; Returns the number of column links to this cell, in 
; addition to itself.
(define (count-links a-cell)
  (if (boolean? a-cell)
      0
      (if (boolean? (cell-dim 'col-ext a-cell))
          1
          (add1 (count-links (cell-dim 'col-ext a-cell))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; count-cells : cell -> num
; Returns the number of cells this cell contains, including itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count-cells a-cell)
  (local [(define (count-cell-ext a-cell-ext)
            (cond [(empty? (cell-ext-col-ext a-cell-ext)) 1]
                  [else (add1 (apply + (map count-cell-ext (cell-ext-col-ext a-cell-ext))))]))]
    (cond [(cell? a-cell)
           (add1 (apply + (map count-cell-ext (cell-col-ext a-cell))))]
          [else (count-cell-ext a-cell)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-linked-dim-of-cell : symbol cell num -> num
; If dim is 'height, sums the height values of each link of the cell.  
; If dim is 'width, sums the width values of each link of the cell.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-linked-dim-of-cell dim a-cell cellspacing)
  (local [(define (get-linked-dim-of-cell* a-cell)
            (cond [(boolean? a-cell) 0]
                  [(and (symbol=? 'width dim)
                        (cell? a-cell))
                   (+ (cell-width a-cell)
                      (get-linked-dim-of-cell* (cell-col-ext a-cell)))]
                  [(and (symbol=? 'width dim)
                        (cell-ext? a-cell))
                   (+ (cell-ext-width a-cell)
                      cellspacing
                      (get-linked-dim-of-cell* (cell-ext-col-ext a-cell)))]
                  [(and (symbol=? 'height dim)
                        (cell? a-cell))
                   (+ (cell-height a-cell)
                      (get-linked-dim-of-cell* (cell-row-ext a-cell)))]
                  [(and (symbol=? 'height dim)
                        (cell-ext? a-cell))
                   (+ (cell-ext-height a-cell)
                      cellspacing
                      (get-linked-dim-of-cell* (cell-ext-row-ext a-cell)))]
                  [else (printf "error: cell : ~s~n dim : ~s~n" a-cell dim)]))]
    (get-linked-dim-of-cell* a-cell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-row-height : (listof cell) -> num
; Returns the uniform height of the row.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-row-height alocell)
  (apply max (map (lambda (cell) (cell-dim 'height cell)) alocell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-row-width : (listof cell) -> num
; Returns the uniform width of the row.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-row-width aloc)
  (apply + (map (lambda (cell) (if (cell? cell)
                                   (add1 (cell-dim 'width cell))
                                   0)) aloc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-matrix-width : structural-matrix num num ->num
; Returns the uniform width of the matrix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-matrix-width a-matrix cellspacing cellpadding)
  (cond [(empty? a-matrix) 0]
        [else 
         (+ (apply max (map get-row-width a-matrix))
            (* cellspacing (sub1 (count-columns a-matrix))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-matrix-height : structural-matrix num num -> num
; Returns the uniform height of the matrix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-matrix-height a-matrix cellspacing cellpadding)
  (cond [(empty? a-matrix) 0]
        [else
         (+ (apply + (map get-min-row-height a-matrix))
            (* (sub1 (length a-matrix)) cellspacing))]))

; get-col-width : (listof cell) -> num
; Gets the minimum width of the list of cells.
(define (get-col-width aloc)
  (apply max (map (lambda (a-cell)
                    (cell-dim 'width a-cell)) aloc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make-rows-and-columns-even! : naive-structural-matrix -> even-structural-matrix
; Consumes a structural-matrix with presumably even number of columns per row,
; and returns an even-structural-matrix.
; EFFECT : changes the width and height of all cells in a-matrix permanently.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-rows-and-columns-even! a-matrix)
  (for-each (lambda (a-row height)
              (for-each (lambda (a-cell width)
                          (if (cell? a-cell)
                              (local [(define snip (cell-snip a-cell))]
                                (if (object? snip)
                                    (send snip resize width height))
                                (set-cell-width! a-cell width)
                                (set-cell-height! a-cell height))
                              (begin (set-cell-ext-width! a-cell width)
                                     (set-cell-ext-height! a-cell height))))
                        a-row (map get-col-width (transpose a-matrix))))
            a-matrix (map get-row-height a-matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; remove-ghost-columns : even-structural-matrix -> even-structural-matrix
; Removes columns with width 0, as those columns aren't visible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (remove-ghost-columns a-matrix)
  (local [(define (ghost-column? a-cell)
            (cond [(empty? a-cell) #f]
                  [(zero? (cell-dim 'width a-cell)) #t]
                  [else #f]))
          (define ghost-columns (cond [(empty? a-matrix) empty]
                                      [else (map ghost-column? (first a-matrix))]))
          (define transposed-matrix (transpose a-matrix))
          ; invariant: accum is the list of columns kept so far
          ;            in the traversal of the list
          (define (remove-columns a-lo-columns ghost-list accum)
            (cond [(or (empty? ghost-list)
                       (empty? transposed-matrix)) accum]
                  [(boolean=? (first ghost-list)
                              #t)
                   (remove-columns (rest a-lo-columns)
                                   (rest ghost-list)
                                   accum)]
                  [else 
                   (remove-columns (rest a-lo-columns)
                                   (rest ghost-list)
                                   (append accum (list (first a-lo-columns))))]))]
    (transpose 
     (remove-columns transposed-matrix ghost-columns empty))))


; setup-merged-cells! : structural-matrix num -> void
; Accumulates the individual widths and heights of cell-ext
; and transfers the sum to the appropriate dimension of the
; cell
(define (setup-merged-cells! a-matrix cellspacing)
  (for-each (lambda (a-row)
              (for-each (lambda (a-cell)
                          (local [(define new-width (get-linked-dim-of-cell 'width a-cell cellspacing))
                                  (define new-height (get-linked-dim-of-cell 'height a-cell cellspacing))]
                            (if (cell? a-cell)
                                (local [(define snip (cell-snip a-cell))]
                                  (if (object? snip)
                                      (send snip resize new-width new-height))
                                  (set-cell-width! a-cell new-width)
                                  (set-cell-height! a-cell new-height))
                                (void))))
                        a-row))
            a-matrix))