(module debug-tool mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "string.ss")
           (prefix srfi: (lib "search.ss" "srfi" "1"))
           ;(lib "math.ss")
           (lib "class.ss")
           (lib "unitsig.ss")
           ;(lib "contract.ss")
           (lib "mred.ss" "mred")
           (prefix drscheme:arrow: (lib "arrow.ss" "drscheme"))
           (lib "tool.ss" "drscheme")
           (lib "marks.ss" "skipper")
           (lib "boundmap.ss" "syntax")
           (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "bitmap-label.ss" "mrlib")
           ;(lib "framework.ss" "framework")
           #;(lib "string-constant.ss" "string-constants"))
  
  (provide tool@)
  
  ; QUESTIONS/IDEAS
  ; what is the right way to deal with macros?
  ; how can the three tool classes communicate with each other safely
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define phase1 void)
      (define phase2 void)
      
      (define (arglist-bindings arglist-stx)
        (syntax-case arglist-stx ()
          [var
           (identifier? arglist-stx)
           (list arglist-stx)]
          [(var ...)
           (syntax->list arglist-stx)]
          [(var . others)
           (cons #'var (arglist-bindings #'others))]))
      
      (define (annotate-stx stx breakpoints break-wrap record-bound-id)
        
        (define (top-level-annotate stx)
          (kernel:kernel-syntax-case
           stx #f
           [(module identifier name (#%plain-module-begin . module-level-exprs))
            (quasisyntax/loc stx (module identifier name
                                   (#%plain-module-begin 
                                    #,@(map module-level-expr-iterator
                                            (syntax->list #'module-level-exprs)))))]
           [else-stx
            (general-top-level-expr-iterator stx)]))
        
        (define (module-level-expr-iterator stx)
          (kernel:kernel-syntax-case
           stx #f
           [(provide . provide-specs)
            stx]
           [else-stx
            (general-top-level-expr-iterator stx)]))
        
        (define (general-top-level-expr-iterator stx)
          (kernel:kernel-syntax-case
           stx #f
           [(define-values (var ...) expr)
            #`(define-values (var ...)
                #,(annotate #`expr (syntax->list #`(var ...)) #t))]
           [(define-syntaxes (var ...) expr)
            stx]
           [(begin . top-level-exprs)
            (quasisyntax/loc stx (begin #,@(map (lambda (expr)
                                                  (module-level-expr-iterator expr))
                                                (syntax->list #'top-level-exprs))))]
           [(require . require-specs)
            stx]
           [(require-for-syntax . require-specs)
            stx]
           [else
            (annotate stx '() #f)]))
        
        (define (annotate expr bound-vars is-tail?)
          
          (define annotate-break?
            (let ([pos (syntax-position expr)]
                  [src (syntax-source expr)])
              (and src
                   (is-a? src object%) ; FIX THIS
                   pos
                   (hash-table-get breakpoints pos (lambda () #t))
                   (kernel:kernel-syntax-case
                    expr #f
                    [(if test then) #t]
                    [(if test then else) #t]
                    [(begin . bodies) #t]
                    [(begin0 . bodies) #t]
                    [(let-values . clause) #t]
                    [(letrec-values . clause) #t]
                    [(set! var val) #t]
                    [(with-continuation-mark key mark body) #t]
                    [(#%app . exprs) #t]
                    [_ #f])
                   (begin
                     (hash-table-put! breakpoints pos #f)
                     (when (not is-tail?)
                       (hash-table-put! breakpoints (+ pos (syntax-span expr) -1) #f))
                     #t))))
          
          (define (let/rec-values-annotator letrec?)
            (kernel:kernel-syntax-case
             expr #f
             [(label (((var ...) rhs) ...) . bodies)
              (let* ([new-bindings (apply append
                                          (map syntax->list
                                               (syntax->list #`((var ...) ...))))]
                     [new-rhs (if letrec?
                                  (map (lambda (expr)
                                         (annotate expr (append new-bindings bound-vars) #f))
                                       (syntax->list #`(rhs ...)))
                                  (map (lambda (expr) (annotate expr bound-vars #f))
                                       (syntax->list #`(rhs ...))))]
                     [last-body (car (reverse (syntax->list #`bodies)))]
                     [all-but-last-body (reverse (cdr (reverse (syntax->list #`bodies))))]
                     [bodies (append (map (lambda (expr)
                                            (annotate expr
                                                      (append new-bindings bound-vars) #f))
                                          all-but-last-body)
                                     (list (annotate
                                            last-body
                                            (append new-bindings bound-vars) is-tail?)))])
                (with-syntax ([(new-rhs/trans ...) new-rhs])
                  (quasisyntax/loc expr
                    (label (((var ...) new-rhs/trans) ...)
                           #,@bodies))))]))
          
          (define (lambda-clause-annotator clause)
            (kernel:kernel-syntax-case
             clause #f
             [(arg-list . bodies)
              (let* ([new-bound-vars (append (arglist-bindings #`arg-list) bound-vars)]
                     [new-bodies (let loop ([bodies (syntax->list #`bodies)])
                                   (if (equal? '() (cdr bodies))
                                       (list (annotate (car bodies) new-bound-vars #t))
                                       (cons (annotate (car bodies) new-bound-vars #f)
                                             (loop (cdr bodies)))))])
                (quasisyntax/loc clause
                  (arg-list #,@new-bodies)))]))
          
          (define annotated
            (syntax-recertify
             (kernel:kernel-syntax-case
              expr #f
              [var-stx (identifier? (syntax var-stx))
                       (let ([binder (and (syntax-original? expr)
                                          (srfi:member expr bound-vars module-identifier=?))])
                         (when binder
                           (let ([f (first binder)])
                             (record-bound-id f f)
                             (record-bound-id expr f)))
                         expr)]
              
              [(lambda . clause)
               (quasisyntax/loc expr 
                 (lambda #,@(lambda-clause-annotator #`clause)))]
              
              [(case-lambda . clauses)
               (quasisyntax/loc expr
                 (case-lambda #,@(map lambda-clause-annotator (syntax->list #`clauses))))]
              
              [(if test then)
               (quasisyntax/loc expr (if #,(annotate #`test bound-vars #f)
                                         #,(annotate #`then bound-vars is-tail?)))]
              
              [(if test then else)
               (quasisyntax/loc expr (if #,(annotate #`test bound-vars #f)
                                         #,(annotate #`then bound-vars is-tail?)
                                         #,(annotate #`else bound-vars is-tail?)))]
              
              [(begin . bodies)
               (letrec ([traverse
                         (lambda (lst)
                           (if (and (pair? lst) (equal? '() (cdr lst)))
                               `(,(annotate (car lst) bound-vars is-tail?))
                               (cons (annotate (car lst) bound-vars #f)
                                     (traverse (cdr lst)))))])
                 (quasisyntax/loc expr (begin #,@(traverse (syntax->list #`bodies)))))]
              
              [(begin0 . bodies)
               (quasisyntax/loc expr (begin0 #,@(map (lambda (expr)
                                                       (annotate expr bound-vars #f))
                                                     (syntax->list #`bodies))))]
              
              [(let-values . clause)
               (let/rec-values-annotator #f)]
              
              [(letrec-values . clause) 
               (let/rec-values-annotator #t)]
              
              [(set! var val)
               (let ([binder (and (syntax-original? #'var)
                                  (srfi:member #'var bound-vars module-identifier=?))])
                 (when binder
                   (let ([f (first binder)])
                     (record-bound-id f f)
                     (record-bound-id expr f)))
                 (quasisyntax/loc expr (set! var #,(annotate #`val bound-vars #f))))]
              
              [(quote _) expr]
              
              [(quote-syntax _) expr]
              
              [(with-continuation-mark key mark body)
               (quasisyntax/loc expr (with-continuation-mark key
                                       #,(annotate #`mark bound-vars #f)
                                       #,(annotate #`body bound-vars is-tail?)))]
              
              [(#%app . exprs)
               (let ([subexprs (map (lambda (expr) 
                                      (annotate expr bound-vars #f))
                                    (syntax->list #`exprs))])
                 (if is-tail?
                     (quasisyntax/loc expr #,subexprs)
                     (wcm-wrap (make-debug-info expr bound-vars bound-vars 'normal #f)
                               (quasisyntax/loc expr #,subexprs))))]
              
              [(#%datum . _) expr]
              
              [(#%top . var) expr]
              
              [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                           (syntax-object->datum expr))])
             expr
             (current-code-inspector)
             #f))
          
          (if annotate-break?
              (break-wrap
               (make-debug-info expr bound-vars bound-vars 'at-break #f)
               annotated
               expr
               is-tail?)
              annotated))
        
        (top-level-annotate stx))
      
      (define (break-at bp p)
        (hash-table-get bp p))
      
      (define (truncate str n)
        (if (< (string-length str) n)
            str
            (if (>= n 3)
                (string-append
                 (substring str 0 (- n 3))
                 "...")
                (substring str 0 (min n (string-length str))))))
      
      (define (string-map! f str)
        (let loop ([i 0])
          (when (< i (string-length str))
            (string-set! str i (f (string-ref str i)))
            (loop (add1 i)))
          str))
      
      (define (newlines->spaces str)
        (string-map! (lambda (chr)
                       (case chr
                         [(#\newline) #\space]
                         [else chr]))
                     str))
      
      (define (index-of chr str)
        (let loop ([i 0])
          (if (< i (string-length str))
              (if (char=? chr (string-ref str i))
                  i
                  (loop (add1 i)))
              #f)))
          
      (define (trim-expr-str str)
        (cond
          [(index-of #\newline str) => (lambda (i)
                                         (string-append
                                          (substring str 0 i)
                                          (if (char=? (string-ref str 0) #\()
                                              " ...)"
                                              " ...")))]
          [str]))
      
      (define (average . values)
        (/ (apply + values) (length values)))
      
      (define (debug-definitions-text-mixin super%)
        (class super%
          
          (inherit dc-location-to-editor-location
                   editor-location-to-dc-location
                   invalidate-bitmap-cache
                   begin-edit-sequence
                   end-edit-sequence
                   get-canvas
                   get-top-level-window)
          
          (define parent #f)
          (define debug? #f)
          (define/public (set-parent! p)
            (set! parent p)
            (set! debug? (send parent debug?)))
          (define mouse-over-pos #f)
          (define bp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define bp-brush (send the-brush-list find-or-create-brush "red" 'solid))
          (define bp-mo-pen (send the-pen-list find-or-create-pen "darkgray" 1 'solid))
          (define bp-mo-brush (send the-brush-list find-or-create-brush "pink"
                                    'solid))
          (define bp-tmp-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define bp-tmp-brush (send the-brush-list find-or-create-brush "yellow"
                                    'solid))
          (define pc-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define pc-brush (send the-brush-list find-or-create-brush "forestgreen" 'solid))
          (define pc-err-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define pc-err-brush (send the-brush-list find-or-create-brush "red" 'solid))
          (define pc-brk-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
          (define pc-brk-brush (send the-brush-list find-or-create-brush "gray" 'solid))
          
          (super-instantiate ())
          
          (define/augment (on-delete start len)
            (begin-edit-sequence)
            (inner (void) on-delete start len))
          (define/augment (after-delete start len)
            (inner (void) after-delete start len)
            (clean-up)
            (end-edit-sequence))
          
          (define/augment (on-insert start len)
            (begin-edit-sequence)
            (inner (void) on-insert start len))
          (define/augment (after-insert start len)
            (inner (void) after-insert start len)
            (clean-up)
            (end-edit-sequence))

          (define/private (clean-up)
            (set! debug? #f)
            (invalidate-bitmap-cache))
          
          (define/private (get-pos/text event)
            (let ([event-x (send event get-x)]
                  [event-y (send event get-y)]
                  [on-it? (box #f)])
              (let loop ([editor this])
                (let-values ([(x y) (send editor dc-location-to-editor-location
                                          event-x event-y)])
                  (cond
                    [(is-a? editor text%)
                     (let ([pos (send editor find-position x y #f on-it?)])
                       (cond
                         [(not (unbox on-it?)) (values #f #f)]
                         [else
                          (let ([snip (send editor find-snip pos 'after-or-none)])
                            (if (and snip
                                     (is-a? snip editor-snip%))
                                (loop (send snip get-editor))
                                (values pos editor)))]))]
                    [(is-a? editor pasteboard%)
                     (let ([snip (send editor find-snip x y)])
                       (if (and snip
                                (is-a? snip editor-snip%))
                           (loop (send snip get-editor))
                           (values #f #f)))]
                    [else (values #f #f)])))))
          
          (define/private (find-char-box text left-pos right-pos)
            (let ([xlb (box 0)]
                  [ylb (box 0)]
                  [xrb (box 0)]
                  [yrb (box 0)])
              (send text position-location left-pos xlb ylb #t)
              (send text position-location right-pos xrb yrb #f)
              (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location
                                                   (unbox xlb) (unbox ylb))]
                            [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                            [(xr-off yr-off) (send text editor-location-to-dc-location
                                                   (unbox xrb) (unbox yrb))]
                            [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                (values xl yl xr yr))))
          
          (define/override (on-event event)
            (if (and parent debug?)
                (let ([breakpoints (send parent get-breakpoints)])
                  (cond
                    [(send event leaving?)
                     (when mouse-over-pos
                       (set! mouse-over-pos #f)
                       (invalidate-bitmap-cache))]
                    [(or (send event moving?)
                         (send event entering?))
                     (let-values ([(pos text) (get-pos/text event)])
                       (when (and pos text)
                         (let ([pos (add1 pos)])
                           (cond
                             ; mouse on breakable pos and hasn't moved significantly
                             [(eq? pos mouse-over-pos)]
                             ; mouse on new breakable pos
                             [(not (eq? (hash-table-get
                                       breakpoints
                                       pos (lambda () 'invalid)) 'invalid))
                              (set! mouse-over-pos pos)
                              (invalidate-bitmap-cache)]
                             ; moved off breakable pos
                             [mouse-over-pos
                              (set! mouse-over-pos #f)
                              (invalidate-bitmap-cache)])
                           (let* ([frames (send parent get-stack-frames)]
                                  [pos-vec (send parent get-pos-vec)]
                                  [id (vector-ref pos-vec pos)]
                                #;[_ (printf "frames = ~a~npos-vec = ~a~nid = ~a~n"
                                             frames pos-vec id)])
                             (send parent
                                   set-mouse-over-msg
                                   (cond
                                     [(and id frames
                                           (let/ec k
                                             (let* ([id-sym (syntax-e id)]
                                                    [binding (lookup-first-binding
                                                              (lambda (id2) (module-identifier=? id id2))
                                                              frames (lambda () (k #f)))]
                                                    [val (mark-binding-value
                                                          binding)])
                                               (truncate (format "~a = ~a" id-sym val) 200))))]
                                     [""])))
                           (super on-event event))))]
                    [(send event button-down? 'right)
                     (let-values ([(pos text) (get-pos/text event)])
                       (if (and pos text)
                           (let* ([pos (add1 pos)]
                                  [break-status (hash-table-get breakpoints pos (lambda () 'invalid))])
                             (case break-status
                               [(#t #f)
                                (let ([menu (make-object popup-menu% #f)])
                                  (make-object menu-item%
                                    "Toggle breakpoint"
                                    menu
                                    (lambda (item evt)
                                      (hash-table-put! breakpoints pos (not break-status))
                                      (invalidate-bitmap-cache)))
                                  (let ([pc (send parent get-pc)])
                                    (if (and pc (= pos pc))
                                        (let ([stat (send parent get-break-status)]
                                              [f (get-top-level-window)])
                                          (when (cons? stat)
                                            (send (make-object menu-item%
                                                    (truncate
                                                     (if (= 2 (length stat))
                                                         (format "value = ~a" (cadr stat))
                                                         (format "~a" (cons 'values (rest stat))))
                                                     200)
                                                    menu
                                                    void) enable #f))
                                          (when (not (eq? stat 'break))
                                            (make-object menu-item%
                                              (if (cons? stat)
                                                  "Change return value..."
                                                  "Skip expression...")
                                              menu
                                              (lambda (item evt)
                                                (let ([tmp
                                                       (get-text-from-user "Return value" #f)])
                                                  (when tmp
                                                    (send parent set-break-status
                                                          (list 'exit-break (eval-string tmp)))))))))
                                        (make-object menu-item%
                                          "Run up to this location"
                                          menu
                                          (lambda (item evt)
                                            (hash-table-put!
                                             breakpoints pos
                                             (lambda () (hash-table-put! breakpoints pos #f) #t))
                                            (invalidate-bitmap-cache)
                                            (when (send parent get-stack-frames)
                                              (send parent resume))))))
                                  (send (get-canvas) popup-menu menu
                                        (+ 1 (inexact->exact (floor (send event get-x))))
                                        (+ 1 (inexact->exact (floor (send event get-y))))))]
                               [(invalid)
                                (let* ([frames (send parent get-stack-frames)]
                                       [pos-vec (send parent get-pos-vec)]
                                       [id (vector-ref pos-vec pos)]
                                       #;[_ (printf "frames = ~a~npos-vec = ~a~nid = ~a~n"
                                                    frames pos-vec id)])
                                  (unless (and
                                           id frames
                                           (let/ec k
                                             (let* ([id-sym (syntax-e id)]
                                                    [binding (lookup-first-binding
                                                              (lambda (id2) (module-identifier=? id id2))
                                                              frames (lambda () (k #f)))]
                                                        [val (mark-binding-value
                                                              binding)]
                                                        [menu (make-object popup-menu% #f)])
                                               (send (make-object menu-item%
                                                       (truncate
                                                        (format "~a = ~a" id-sym val)
                                                        200)
                                                       menu
                                                       (lambda (item evt)
                                                         (printf "~a" val))) enable #f)
                                               (make-object menu-item%
                                                 (format "(set! ~a ...)" id-sym)
                                                 menu
                                                 (lambda (item evt)
                                                   (let ([tmp
                                                          (get-text-from-user
                                                               (format "New value for ~a" id-sym) #f #f
                                                               (format "~a" val))])
                                                     (when tmp
                                                       (mark-binding-set! binding (eval-string tmp))))))
                                               (send (get-canvas) popup-menu menu
                                                     (+ 1 (inexact->exact (floor (send event get-x))))
                                                     (+ 1 (inexact->exact (floor (send event get-y)))))
                                               #t)))
                                    (super on-event event)))]))
                           (super on-event event)))]
                    [else (super on-event event)]))
                (super on-event event)))
          
          (define/override (on-paint before dc left top right bottom dx dy draw-caret)
            (super on-paint before dc left top right bottom dx dy draw-caret)
            (when (and parent debug? (not before))
              (let ([breakpoints (send parent get-breakpoints)])
                (hash-table-for-each
                 breakpoints
                 (lambda (pos enabled?)
                   (when (and (>= pos 0) (or enabled? (and mouse-over-pos (= mouse-over-pos pos))))
                     (let*-values ([(xl yl xr yr) (find-char-box this (sub1 pos) pos)]
                                   [(diameter) (- xr xl)]
                                   [(yoff) (/ (- yr yl diameter) 2)])
                       (let ([op (send dc get-pen)]
                             [ob (send dc get-brush)])
                         (case enabled?
                           [(#t) (send dc set-pen bp-pen)
                                 (send dc set-brush bp-brush)]
                           [(#f) (send dc set-pen bp-mo-pen)
                                 (send dc set-brush bp-mo-brush)]
                           [else (send dc set-pen bp-tmp-pen)
                                   (send dc set-brush bp-tmp-brush)])
                         ;(drscheme:arrow:draw-arrow dc xl yl xr yr dx dy)
                         (send dc draw-ellipse (+ xl dx) (+ yl dy yoff) diameter diameter)
                         #;(send dc draw-polygon stop-sign
                                 (+ xl dx)
                                 (+ yl dy 2))
                         (send dc set-pen op)
                         (send dc set-brush ob)))))))
              (let ([pos (send parent get-pc)])
                (when pos
                  (let*-values ([(xl yl xr yr) (find-char-box this (sub1 pos) pos)]
                                [(ym) (average yl yr)])
                    (let ([op (send dc get-pen)]
                          [ob (send dc get-brush)])
                      (case (send parent get-break-status)
                        [(error) (send dc set-pen pc-err-pen)
                                 (send dc set-brush pc-err-brush)]
                        [(break) (send dc set-pen pc-brk-pen)
                                 (send dc set-brush pc-brk-brush)]
                        [else    (send dc set-pen pc-pen)
                                 (send dc set-brush pc-brush)]))
                    (drscheme:arrow:draw-arrow dc xl ym xr ym dx dy))
                  #;(let loop ([end-pos pos]
                             [marks (send parent get-stack-frames)])
                    (when (cons? marks)
                      (let*-values ([(start-pos) (syntax-position (mark-source (first marks)))]
                                    [(xl0 yl0 xr0 yr0) (find-char-box this (sub1 start-pos) start-pos)]
                                    [(xm0) (average xl0 xr0)]
                                    [(ym0) (average yl0 yr0)]
                                    [(xl yl xr yr) (find-char-box this (sub1 end-pos) end-pos)]
                                    [(xm) (average xl xr)]
                                    [(ym) (average yl yr)])
                        (let ([op (send dc get-pen)]
                              [ob (send dc get-brush)])
                          (case (send parent get-break-status)
                            [(error) (send dc set-pen pc-err-pen)
                                     (send dc set-brush pc-err-brush)]
                            [(break) (send dc set-pen pc-brk-pen)
                                     (send dc set-brush pc-brk-brush)]
                            [else    (send dc set-pen pc-pen)
                                     (send dc set-brush pc-brush)]))
                        (drscheme:arrow:draw-arrow dc xm0 ym0 xr ym dx dy)
                        (loop start-pos (rest marks)))))))))))
      
      (define (debug-interactions-text-mixin super%)
        (class super%
          
          (inherit run-in-evaluation-thread
                   display-results)
          
          (define parent #f)
          (define/public (set-parent! p)
            (set! parent p))
          
          (super-instantiate ())
          
          ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
          ;; adds debugging information to `sexp' and calls `oe'
          (define/private (make-debug-eval-handler oe break? break)
            (lambda (orig-exp)
              (if (or (compiled-expression? (if (syntax? orig-exp)
                                                (syntax-e orig-exp)
                                                orig-exp))
                      (not parent)
                      (not (syntax-source orig-exp))
                      (not (eq? (syntax-source orig-exp)
                                (send parent get-definitions-text))))
                  (oe orig-exp)
                  (let loop ([exp (if (syntax? orig-exp)
                                      orig-exp
                                      (namespace-syntax-introduce
                                       (datum->syntax-object #f orig-exp)))])
                    (let ([top-e (expand-syntax-to-top-form exp)])
                      (syntax-case top-e (begin)
                        [(begin expr ...)
                         ;; Found a `begin', so expand/eval each contained 
                         ;; expression one at a time 
                         (let i-loop ([exprs (syntax->list #'(expr ...))]
                                      [last-one (list (void))])
                           (cond
                             [(null? exprs) (apply values last-one)]
                             [else (i-loop (cdr exprs)
                                           (call-with-values
                                            (lambda () (loop (car exprs)))
                                            list))]))]
                        [_else
                         ;; Not `begin', so proceed with normal expand and eval 
                         (let* ([breakpoints (send parent get-breakpoints)]
                                [pos-vec (send parent get-pos-vec)]
                                [annotated
                                 (annotate-stx
                                  (expand-syntax top-e)
                                  breakpoints
                                  (lambda (debug-info annotated raw is-tail?)
                                    (let* ([start (syntax-position raw)]
                                           [end (+ start (syntax-span raw) -1)])
                                      (if is-tail?
                                          #`(let-values ([(value-list) #f])
                                              (if (#,break? #,start)
                                                  (set! value-list (#,break (current-continuation-marks)
                                                                      'entry-break #,debug-info)))
                                              (if (not value-list)
                                                  #,annotated
                                                  (apply values value-list)))
                                          #`(let-values ([(value-list) #f])
                                              (if (#,break? #,start)
                                                  (set! value-list (#,break (current-continuation-marks)
                                                                      'entry-break #,debug-info)))
                                              (if (not value-list)
                                                  (call-with-values
                                                   (lambda () #,annotated)
                                                   (case-lambda
                                                     [(val) (if (#,break? #,end)
                                                                (apply values
                                                                   (#,break (current-continuation-marks)
                                                                      (list 'exit-break val) #,debug-info))
                                                                val)]
                                                     [vals (apply values
                                                                  (if (#,break? #,end)
                                                                  (#,break (current-continuation-marks)
                                                                     (cons 'exit-break vals) #,debug-info)
                                                                  vals))]))
                                                  (apply values
                                                         (if (#,break? #,end)
                                                             (#,break (current-continuation-marks)
                                                                (cons 'exit-break value-list) #,debug-info)
                                                             value-list)))))))
                                  (lambda (bound binding)
                                    ;(display-results (list bound))
                                    (let loop ([i 0])
                                      (when (< i (syntax-span bound))
                                        (vector-set! pos-vec (+ i (syntax-position bound)) binding)
                                        (loop (add1 i))))))])
                           ;(display-results (list orig-exp))
                           (oe annotated))]))))))
          
          (define/override (reset-console)
            (super reset-console)
            (when (and parent (send parent debug?))
              (let ([breakpoints (send parent get-breakpoints)])
                (run-in-evaluation-thread
                 (lambda ()
                   (let ([self (current-thread)]
                         [oeh (current-exception-handler)]
                         [err-hndlr (error-display-handler)])
                     (error-display-handler
                      (lambda (msg exn)
                        (err-hndlr msg exn)
                        (if (and (eq? self (current-thread)) (exn:fail? exn))
                            (send parent suspend oeh
                                  (continuation-mark-set->list (exn-continuation-marks exn) debug-key)
                                  'error)))) ; this breaks the buttons because it looks like we can resume
                     (current-eval
                      (make-debug-eval-handler
                       (current-eval)
                       (lambda (pos)
                         (or (hash-table-get breakpoints -1)
                             (let ([bp (hash-table-get breakpoints pos)])
                               (if (procedure? bp)
                                   (bp)
                                   bp))))
                       (lambda (ccm kind info)
                         (let* ([debug-marks (continuation-mark-set->list ccm debug-key)])
                           (send parent suspend oeh (cons info debug-marks) kind)))))
                     (current-exception-handler
                      (lambda (exn)
                        (if (and (exn:break? exn) (send parent suspend-on-break?))
                            (let ([marks (exn-continuation-marks exn)]
                                  [cont (exn:break-continuation exn)])
                              (send parent suspend oeh (continuation-mark-set->list marks debug-key) 'break)
                              (cont))
                            (oeh exn))))))))))))
      
      (define (debug-unit-frame-mixin super%)
        (class super%
          
          (inherit get-button-panel
                   get-definitions-text
                   get-interactions-text
                   get-menu-bar
                   break-callback
                   reset-offer-kill
                   get-top-level-window)
          
          (define breakpoints (make-hash-table))
          (hash-table-put! breakpoints -1 #f)
          (define resume-sem (make-semaphore))
          (define want-suspend-on-break? #f)
          (define want-debug? #f)
          (define/public (debug?)
            want-debug?)
          (define stack-frames #f)
          (define pos-vec (make-vector 1))
          (define/public (suspend-on-break?)
            want-suspend-on-break?)
          (define/public (get-stack-frames)
            stack-frames)
          (define/public (get-pos-vec)
            pos-vec)
          (define/public (get-breakpoints)
            breakpoints)
          (define break-status #f)
          (define/public (get-break-status)
            break-status)
          (define/public (set-break-status stat)
            (set! break-status stat))
          (define control-panel #f)
          (define/public (resume)
            (semaphore-post resume-sem))
          (define/public (set-mouse-over-msg msg)
            (when (not (string=? msg (send mouse-over-message get-label)))
              (send mouse-over-message set-label msg)))
          
          (define/public (get-pc)
            (if (cons? stack-frames)
                (let* ([src-stx (mark-source (first stack-frames))]
                       [start (syntax-position src-stx)]
                       [end (+ start (syntax-span src-stx) -1)])
                  (if (cons? break-status)
                      end
                      start))
                #f))
          
          (define/public suspend
            (opt-lambda (break-handler frames [status #f])
              (set! want-suspend-on-break? #f)
              (hash-table-put! breakpoints -1 #f)
              (send pause-button enable #f)
              (send step-button enable #t)
              (send resume-button enable #t)
              ;(fprintf (current-error-port) "break: ~a~n" (map expose-mark frames))
              ;(printf "status = ~a~n" status)
              (let ([osf stack-frames]
                    [obs break-status])
                (set! stack-frames frames)
                (set! break-status status)
                (when (cons? status)
                  (let ([expr (mark-source (first frames))])
                    (send status-message set-label
                                        (truncate
                                         (format "~a ==> ~a"
                                                 (trim-expr-str
                                                  (send (get-definitions-text) get-text
                                                        (sub1 (syntax-position expr))
                                                        (+ -1 (syntax-position expr) (syntax-span expr))))
                                                 (if (= 2 (length status))
                                                     (cadr status)
                                                     (cons 'values (rest status))))
                                         200))))
                (cond [(get-pc) => (lambda (pc) (send (get-definitions-text) scroll-to-position pc))])
                (send (get-definitions-text) invalidate-bitmap-cache)
                (with-handlers ([exn:break?
                                 (lambda (exn)
                                   (set! stack-frames osf)
                                   (set! break-status obs)
                                   (send status-message set-label "")
                                   (send (get-definitions-text) invalidate-bitmap-cache)
                                   (break-handler exn))])
                  (semaphore-wait/enable-break resume-sem))
                (begin0
                  (if (cons? break-status)
                      (rest break-status)
                      #f)
                  (set! stack-frames osf)
                  (set! break-status obs)
                  (send pause-button enable #t)
                  (send step-button enable #f)
                  (send resume-button enable #f)
                  (send status-message set-label "")
                  (send (get-definitions-text) invalidate-bitmap-cache)))))
          
          (define (my-execute debug?)
            (set! want-debug? debug?)
            (if debug?
                (show-debug)
                (hide-debug))
            (set! breakpoints (make-hash-table))
            (hash-table-put! breakpoints -1 #t)
            (set! pos-vec (make-vector (add1 (send (get-definitions-text) last-position)) #f))
            (set! resume-sem (make-semaphore))
            (set! want-suspend-on-break? #f)
            (set! stack-frames #f)
            (send (get-definitions-text) set-parent! this)
            (send (get-interactions-text) set-parent! this)
            (super execute-callback))
          
          (define/override (execute-callback)
            (my-execute #f))
          
          (define/augment (enable-evaluation)
            (send debug-button enable #t)
            (inner (void) enable-evaluation))
          
          (define/augment (disable-evaluation)
            (send debug-button enable #f)
            (inner (void) disable-evaluation))
          
          (define debug-parent-panel 'uninitialized-debug-parent-panel)
          (define debug-panel 'uninitialized-debug-panel)
          (define/override (get-definitions/interactions-panel-parent)
            (set! debug-parent-panel
                  (make-object vertical-panel%
                    (super get-definitions/interactions-panel-parent)))
            (set! debug-panel (instantiate horizontal-panel% ()
                                (parent debug-parent-panel)
                                (stretchable-height #f)
                                (alignment '(center center))
                                (style '(border))))
            (send debug-parent-panel change-children (lambda (l) null))
            #;(instantiate button% ()
              (label "Hide")
              (parent debug-panel)
              (callback (lambda (x y) (hide-debug)))
              (stretchable-height #t))
            (make-object vertical-panel% debug-parent-panel))

          (define/private (hide-debug)
            (when (member debug-panel (send debug-parent-panel get-children))
              (send debug-parent-panel change-children
                    (lambda (l) (remq debug-panel l)))))
          
          (define/private (show-debug)
            (unless (member debug-panel (send debug-parent-panel get-children))
              (send debug-parent-panel change-children
                    (lambda (l) (cons debug-panel l)))))
                    
          (super-new)
          
          (define status-message
            (instantiate message% ()
              [label ""]
              [parent debug-panel]
              [stretchable-width #t]))
          
          (define debug-button
            (make-object button%
              ((bitmap-label-maker
                "Debug"
                (build-path (collection-path "skipper") "icon-small.png")) this)
              (get-button-panel)
              (lambda (button evt)
                (my-execute #t))))
          
          (define pause-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Pause"
                       (build-path (collection-path "skipper") "pause.png")) this)]
              [parent debug-panel]
              [callback (lambda (button evt)
                          (if stack-frames
                              (bell)
                              (begin
                                (set! want-suspend-on-break? #t)
                                (break-callback)
                                (reset-offer-kill))))]
              [enabled #t]))
          
          (define resume-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Continue"
                       (build-path (collection-path "skipper") "resume.png")) this)]
              [parent debug-panel]
              [callback (lambda (button evt)
                          (if stack-frames
                              (semaphore-post resume-sem)
                              (bell)))]
              [enabled #f]))
          
          (define step-button
            (instantiate button% ()
              [label ((bitmap-label-maker
                       "Step"
                       (build-path (collection-path "skipper") "step.png")) this)]
              [parent debug-panel]
              [callback (lambda (btn evt)
                          (if stack-frames
                              (begin
                                (hash-table-put! breakpoints -1 #t)
                                (semaphore-post resume-sem))
                              (bell)))]
              [enabled #f]))

          (define mouse-over-message
            (instantiate message% ()
              [label ""]
              [parent debug-panel]
              [stretchable-width #t]))
          
          (send (get-button-panel) change-children
                (lambda (_)
                  (cons debug-button
                        (remq debug-button _))))))
      
      (drscheme:get/extend:extend-definitions-text debug-definitions-text-mixin)
      (drscheme:get/extend:extend-interactions-text debug-interactions-text-mixin)
      (drscheme:get/extend:extend-unit-frame debug-unit-frame-mixin))))