(module world mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "unit.ss")
           (lib "list.ss"))
  
  (provide make-world
           demo-world)

  (define cell-size 32)
  
  (define step-msec 50)
  
  (define person-x-inset 8)
  (define person-top-inset 4)
  
  (define jump-v -18)
  
  (define (minpos a b)
    (if (negative? b)
        a
        (min a b)))
  (define (maxneg a b)
    (if (positive? b)
        a
        (max a b)))
  (define (rnde v)
    (inexact->exact (round v)))

  ;;(define wait-for-vertical-retrace
  ;;  (with-handlers ([not-break-exn? (lambda (x) void)])
  ;;    (dynamic-require '(lib "vr.ss" "ollie-world") 'wait-for-vertical-retrace)))
  
  (define (icon-file f)
    (build-path (collection-path "ollie-world") "icons" f))

  (define static-object-snip%
    (class image-snip%
      (init filename)
      (init-field [has-top? #t]
                  [has-bottom? #t]
                  [has-left? #t]
                  [has-right? #t])
      (define/public (collide-adjust-delta mx my x y w h dx dy)
        (let ([mxe (+ mx cell-size)]
              [mye (+ my cell-size)]
              [xe (+ x w)]
              [ye (+ y h)])
          (if (zero? dx)
              ;; Special case for vertical movement only:
              (if (or (< mx x mxe) (= x mx)
                      (< mx xe mxe))
                  ;; Hits top or bottom?
                  (let ([hit-top (and has-top?
                                      (positive? dy)
                                      (<= ye my)
                                      (> (+ ye dy) my)
                                      (- my ye))]
                        [hit-bottom (and has-bottom?
                                         (negative? dy)
                                         (>= y mye)
                                         (< (+ y dy) mye)
                                         (- mye y))])
                    (values 0 
                            (or hit-top hit-bottom dy) 
                            (or hit-top
                                (and (= ye my) (zero? dy)))))
                  ;; Doesn't hit:
                  (values 0 dy #f))
              ;; Calculate intersections:
              (let ([islope (/ (exact->inexact dy) dx)])
                (let ([hit-left
                       (and has-left?
                            (positive? dx)
                            (<= xe mx)
                            (> (+ xe dx) mx)
                            (let ([left-y (rnde (+ y (* (- mx xe) islope)))])
                              (or (< my left-y mye) (= my left-y)
                                  (< my (+ left-y h) mye)
                                  (<= left-y my mye (+ left-y h))))
                            (- mx xe))]
                      [hit-right
                       (and has-right?
                            (negative? dx)
                            (>= x mxe)
                            (< (+ x dx) mxe)
                            (let ([right-y (rnde (+ y (* (- mxe x) islope)))])
                              (or (< my right-y mye) (= my right-y)
                                  (< my (+ right-y h) mye)
                                  (<= right-y my mye (+ right-y h))))
                            (- mxe x))]
                      [hit-top
                       (and has-top?
                            (positive? dy)
                            (<= ye my)
                            (> (+ ye dy) my)
                            (let ([top-x (rnde (+ x (/ (- my ye) islope)))])
                              (or (< mx top-x mxe) (= mx top-x)
                                  (< mx (+ top-x w) mxe)))
                            (- my ye))]
                      [hit-bottom
                       (and has-bottom?
                            (negative? dy)
                            (>= y mye)
                            (< (+ y dy) mye)
                            (let ([bottom-x (rnde (+ x (/ (- mye y) islope)))])
                              (or (< mx bottom-x mxe) (= mx bottom-x)
                                  (< mx (+ bottom-x w) mxe)))
                            (- mye y))])
                  (if (or hit-left
                          hit-right
                          hit-top
                          hit-bottom)
                      ;; Collision
                      (values (or hit-left hit-right dx)
                              (or hit-top hit-bottom dy)
                              hit-top)
                      ;; No collision
                      (values dx dy #f)))))))
      (super-make-object filename)))

  (define (mk-rock)
    (make-object static-object-snip% (icon-file "rock.gif")))
    
  (define (mk-ground)
    (instantiate static-object-snip% ((icon-file "ground.gif")) [has-left? #f] [has-right? #f] [has-bottom? #f]))
  
  (define min-x-speed 4)
  (define max-x-speed 8)
  (define x-speed min-x-speed)
  
  (define max-y-speed 12)
  (define gravity-acceleration 3)
  
  (define transparent-pen (make-object pen% "white" 1 'transparent))
      (define sky-brush (make-object brush% "sky blue" 'solid))
      (define sky-pasteboard (class pasteboard%
                               (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
                                 (when before?
                                   (let ([pen (send dc get-pen)]
                                         [brush (send dc get-brush)])
                                     (send dc set-pen transparent-pen)
                                     (send dc set-brush sky-brush)
                                     (send dc draw-rectangle (+ dx left) (+ dy top) (- right left) (- bottom top))
                                     (send dc set-brush brush)
                                     (send dc set-pen pen))))
                               (super-make-object)))
  
  (define person-table (make-hash-table 'equal))
      
  (define world-unit
    (unit
      (import world-width world-height start-x start-y layout)
      (export)
      
      (define y-speed 0)

      (define moving? #f)
      (define left? #f)
      (define jumping? #f)
      (define can-jump? #f)
      (define jump-count 0)
      
      (define timers null)
      (define (remember-timer! t)
        (set! timers (cons t timers)))
      (define (stop-timers!)
        (let ([l timers])
          (set! timers null)
          (map (lambda (t) (send t stop)) l)))
      
      (define (mk-person)
        (let ([bm-cons (lambda (file)
                         (hash-table-get
                          person-table
                          file
                          (lambda ()
                            (let* ([bm (make-object bitmap% (icon-file file))]
                                   [w (send bm get-width)]
                                   [h (send bm get-height)]
                                   [mask (make-object bitmap% w h #t)]
                                   [tmp-src (make-object bitmap-dc% bm)]
                                   [tmp-dest (make-object bitmap-dc% mask)]
                                   [c (make-object color%)])
                              (let loop ([i 0])
                                (unless (= i w)
                                  (let loop ([j 0])
                                    (unless (= j h)
                                      (send tmp-src get-pixel i j c)
                                      (send c set
                                            (if (= 255 (send c red)) 255 0)                                        
                                            (if (= 255 (send c green)) 255 0)
                                            (if (= 255 (send c blue)) 255 0))
                                      (send tmp-dest set-pixel i j c)
                                      (loop (add1 j))))
                                  (loop (add1 i))))
                              (send tmp-src set-bitmap #f)
                              (send tmp-dest set-bitmap #f)
                              (let ([p (cons bm mask)])
                                (hash-table-put! person-table file p)
                                p)))))])
          (let* ([rotation (list (cons (bm-cons "p-left.gif") (bm-cons "p-right.gif"))
                                 (cons (bm-cons "p-left-r.gif") (bm-cons "p-right-r.gif"))
                                 (cons (bm-cons "p-left-rr.gif") (bm-cons "p-right-rr.gif"))
                                 (cons (bm-cons "p-left-rr.gif") (bm-cons "p-right-rr.gif"))
                                 (cons (bm-cons "p-left-rr.gif") (bm-cons "p-right-rr.gif"))
                                 (cons (bm-cons "p-left-r.gif") (bm-cons "p-right-r.gif"))
                                 (cons (bm-cons "p-left.gif") (bm-cons "p-right.gif"))
                                 (cons (bm-cons "p-left-l.gif") (bm-cons "p-right-l.gif"))
                                 (cons (bm-cons "p-left-ll.gif") (bm-cons "p-right-ll.gif"))
                                 (cons (bm-cons "p-left-ll.gif") (bm-cons "p-right-ll.gif"))
                                 (cons (bm-cons "p-left-ll.gif") (bm-cons "p-right-ll.gif"))
                                 (cons (bm-cons "p-left-l.gif") (bm-cons "p-right-l.gif")))]
                 [top rotation])
            (set-cdr! (last-pair rotation) rotation)
            (make-object
                (class image-snip%
                  (inherit set-bitmap)
                  (remember-timer!
                   (instantiate timer% ()
                     [notify-callback
                      (lambda ()
                        (let-values ([(dx dy) (values
                                               (if moving? 
                                                   (* (if left? -1 1) x-speed)
                                                   0)
                                               (if (and jumping? can-jump?)
                                                   jump-v
                                                   (+ y-speed gravity-acceleration)))]
                                     [(x y) (let ([xb (box 0)]
                                                  [yb (box 0)])
                                              (send pb get-snip-location this xb yb)
                                              (values (unbox xb) (unbox yb)))])
                          ;; Assumptions:
                          ;;   1. Ollie is two cells high and no more than one cell wide
                          ;;   2. We move less than one cell in each direction on each step
                          ;; Find potential hits and adjust delta:
                          (let ([potential-snips
                                 (let loop ([corners 
                                             (let ([pw (- cell-size person-x-inset)]
                                                   [pi (- person-x-inset)])
                                               (let ([now-on (list 
                                                              (cons (+ x pi) (+ y person-top-inset))
                                                              (cons (+ x pw) (+ y person-top-inset))
                                                              (cons (+ x pi) (+ y cell-size))
                                                              (cons (+ x pw) (+ y cell-size))
                                                              (cons (+ x pi) (+ y (* 2 cell-size)))
                                                              (cons (+ x pw) (+ y (* 2 cell-size))))])
                                                 ;; Might hit:
                                                 (append
                                                  now-on
                                                  (map (lambda (p) (cons (+ dx (car p)) (cdr p))) now-on)
                                                  (map (lambda (p) (cons (+ dx (car p)) (+ dy (cdr p)))) now-on)
                                                  (map (lambda (p) (cons (car p) (+ dy (cdr p)))) now-on))))])
                                   (if (null? corners)
                                       null
                                       (let ([l (loop (cdr corners))])
                                         (let ([snip (send pb find-snip (caar corners) (cdar corners) this)])
                                           (if (and (snip . is-a? . static-object-snip%) 
                                                    (not (memq snip l)))
                                               (cons snip l)
                                               l)))))]
                                [orig-dx dx])
                            (let-values ([(dx dy jump-ok?)
                                          (let loop ([snips potential-snips][dx orig-dx][dy dy][j? #f][first-pass? #f])
                                            (if (null? snips)
                                                (begin
                                                  ;(printf "~a ~a ~a ~a~n" first-pass? dx dy orig-dx)
                                                  (if first-pass?
                                                      (loop potential-snips orig-dx dy #f #f)
                                                      (values dx dy j?)))
                                                (let-values ([(dx dy jump?)
                                                              (let ([xb (box 0)]
                                                                    [yb (box 0)])
                                                                (send pb get-snip-location (car snips) xb yb)
                                                                (send (car snips) collide-adjust-delta 
                                                                      (unbox xb) (unbox yb)
                                                                      (+ x person-x-inset) (+ y person-top-inset)
                                                                      (- cell-size person-x-inset person-x-inset) 
                                                                      (- (* 2 cell-size) person-top-inset)
                                                                      dx dy))])
                                                  (loop (cdr snips) dx dy (or j? jump?) first-pass?))))])
                              (set! y-speed dy)
                              (if (and jumping? can-jump?)
                                  (set! jump-count (add1 jump-count))
                                  (set! jump-count 0))
                              (set! can-jump? (or jump-ok? (= jump-count 1)))
                              ;; Do the move:
                              (set! rotation (if moving? (cdr rotation) top))
                              (send pb begin-edit-sequence)
                              (let ([p ((if left? caar cdar) rotation)])
                                (set-bitmap (car p) (cdr p)))
                              (unless (and (zero? dx) (zero? dy))
                                (send pb move this dx dy)
                                (set! x-speed (min max-x-speed (add1 x-speed))))
                              (when (send c scroll-to 
                                          (- (+ x dx) (* 2 cell-size))
                                          (- (+ y dy) (/ cell-size 2))
                                          (* 5 cell-size)
                                          (* 3 cell-size) #f)
                                (send pb invalidate-bitmap-cache 0 0 'end 'end))
                              (send pb end-edit-sequence)))))]
                     [interval step-msec]))
                  (super-make-object (caaar rotation)))))))
      
      (define f (make-object (class frame% 
                               (rename [super-on-close on-close])
                               (define/override (on-close)
                                 (stop-timers!)
                                 (super-on-close))
                               (super-make-object "World"))))
      (define pb (make-object pasteboard%))
      (define c
        (instantiate (class editor-canvas% 
                       (define/override (on-event e)
                         ;; Ignore mouse events
                         (void))
                       (define/override (on-char e)
                         (case (send e get-key-code)
                           [(left) (set! left? #t) (set! moving? #t)]
                           [(right) (set! left? #f) (set! moving? #t)]
                           [(#\space) (set! jumping? #t)]
                           [else (cond
                                   [(eq? 'release (send e get-key-code))
                                    (case (send e get-key-release-code)
                                      [(left right)
                                       (set! moving? #f)
                                       (set! x-speed min-x-speed)]
                                      [(#\space)
                                       (set! jumping? #f)])])]))
                       (super-instantiate ()))
          (f)
          [editor pb]
          [stretchable-width #f]
          [stretchable-height #f]
          [style '(hide-hscroll hide-vscroll)]))
      
      ;; Kindof a hack: we know that the canvas adds a 5-pixel border:
      (send c min-client-width (+ 10 (* world-width cell-size)))
      (send c min-client-height (+ 10 (* world-height cell-size)))
      
      (define (add obj x y)    
        (send pb insert obj (* x cell-size) (* y cell-size)))
      
      (for-each (lambda (i)
                  (add
                   (case (car i)
                     [(rock) (mk-rock)]
                     [(ground) (mk-ground)])
                   (cadr i)
                   (caddr i)))
                layout)
      
      (add (mk-person) start-x start-y)
      
      (send f show #t)))
  
  (define demo-layout
    `((rock 0 10)
      (rock 0 9)
      (rock 0 8)
      (rock 19 10)
      (rock 19 9)
      
      (rock 6 10)
      (rock 6 9)

      (rock 8 6.5)
      
      ,@(let loop ([i 0])
          (cons
           `(ground ,i ,11)
           (if (= i 19)
               null
               (loop (add1 i)))))))
    
  (define (make-world world-width world-height layout start-x start-y)
    (invoke-unit world-unit world-width world-height start-x start-y layout))
  
  (define (demo-world)
    (make-world 15 12 demo-layout 3 7))
  
  '(demo-world))
