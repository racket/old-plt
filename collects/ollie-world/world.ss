(module world mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "list.ss"))

  (define cell-size 32)
  
  (define person-x-inset 5)
  
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

  (define static-object-snip%
    (class image-snip%
      (init filename)
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
                  (let ([hit-top (and (positive? dy)
                                      (<= ye my)
                                      (> (+ ye dy) my)
                                      (- my ye))]
                        [hit-bottom (and (negative? dy)
                                         (>= y mye)
                                         (< (+ y dy) mye)
                                         (- mye dy))])
                    (values 0 
                            (or hit-top hit-bottom dy) 
                            (or hit-top
                                (and (= ye my) (zero? dy)))))
                  ;; Doesn't hit:
                  (values 0 dy #f))
              ;; Calculate intersections:
              (let ([islope (/ (exact->inexact dy) dx)])
                (let ([hit-left
                       (and (positive? dx)
                            (<= xe mx)
                            (> (+ xe dx) mx)
                            (let ([left-y (rnde (+ y (* (- mx xe) islope)))])
                              (or (< my left-y mye) (= my left-y)
                                  (< my (+ left-y h) mye)
                                  (<= left-y my mye (+ left-y h))))
                            (- mx xe))]
                      [hit-right
                       (and (negative? dx)
                            (>= x mxe)
                            (< (+ x dx) mxe)
                            (let ([right-y (rnde (+ y (* (- x mxe) islope)))])
                              (or (< my right-y mye) (= my right-y)
                                  (< my (+ right-y h) mye)
                                  (<= right-y my mye (+ right-y h))))
                            (- mxe x))]
                      [hit-top
                       (and (positive? dy)
                            (<= ye my)
                            (> (+ ye dy) my)
                            (let ([top-x (rnde (+ x (/ (- my ye) islope)))])
                              (or (< mx top-x mxe) (= mx top-x)
                                  (< mx (+ top-x w) mxe)))
                            (- my ye))]
                      [hit-bottom
                       (and (negative? dy)
                            (>= y mye)
                            (< (+ y dy) mye)
                            (let ([bottom-x (rnde (+ x (/ (- y mye) islope)))])
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

  (define (mk-img file)
    (make-object static-object-snip% (build-path (collection-path "ollie-world") "icons" file)))

  (define (mk-rock) (mk-img "rock.gif"))
  (define (mk-ground) (mk-img "ground.gif"))
  
  (define moving? #f)
  (define left? #f)
  (define can-jump? #f)
  
  (define min-x-speed 4)
  (define max-x-speed 8)
  (define x-speed min-x-speed)
  
  (define y-speed 0)
  (define max-y-speed 12)
  (define gravity-acceleration 3)
  
  (define person-table (make-hash-table 'equal))
  
  (define (mk-person)
    (let ([bm-cons (lambda (file)
                     (hash-table-get
                      person-table
                      file
                      (lambda ()
                        (let* ([bm (make-object bitmap% (build-path (collection-path "ollie-world") "icons" file))]
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
              (instantiate timer% ()
                [notify-callback
                 (lambda ()
                   (let-values ([(dx dy) (values
                                          (if moving? 
                                              (* (if left? -1 1) x-speed)
                                              0)
                                          (+ y-speed gravity-acceleration))]
                                [(x y) (let ([xb (box 0)]
                                             [yb (box 0)])
                                         (send pb get-snip-location this xb yb)
                                         (values (unbox xb) (unbox yb)))])
                     ;; Assumptions:
                     ;;   1. ollie = 2 cells high, one cell wide
                     ;;   2. we move less than a cell in each direction on each step
                     ;; Find potential hits and adjust delta:
                     (let ([potential-snips
                            (let loop ([corners 
                                        (list 
                                         ;; shift x by 1 so that snip search doesn't find the character
                                         (cons (- x 1) y)
                                         (cons (+ x cell-size 1) y)
                                         (cons (- x 1) (+ y cell-size))
                                         (cons x (+ y cell-size 1))
                                         (cons (+ x cell-size 1) (+ y cell-size))
                                         (cons (- x 1) (+ y (* 2 cell-size)))
                                         (cons x (+ y 1 (* 2 cell-size)))
                                         (cons (+ x cell-size 1) (+ y (* 2 cell-size))))])
                              (if (null? corners)
                                  null
                                  (let ([snip1 (send pb find-snip (caar corners) (cdar corners))]
                                        [snip2 (send pb find-snip (+ dx (caar corners)) (+ dy (cdar corners)))])
                                    (let ([l (loop (cdr corners))])
                                      (let ([l (if (snip2 . is-a? . static-object-snip%) (cons snip2 l) l)])
                                        (if (snip1 . is-a? . static-object-snip%) (cons snip1 l) l))))))]
                           [orig-dx dx])
                       (let-values ([(dx dy jump?)
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
                                                                 (+ x person-x-inset) y
                                                                 (- cell-size person-x-inset person-x-inset) (* 2 cell-size)
                                                                 dx dy))])
                                             (loop (cdr snips) dx dy (or j? jump?) first-pass?))))])
                         (set! y-speed dy)
                         (set! can-jump? jump?)
                         ;; Do the move:
                         (set! rotation (if moving? (cdr rotation) top))
                         (send pb begin-edit-sequence)
                         (let ([p ((if left? caar cdar) rotation)])
                           (set-bitmap (car p) (cdr p)))
                         (unless (and (zero? dx) (zero? dy))
                           (send pb move this dx dy)
                           (set! x-speed (min max-x-speed (add1 x-speed))))
                         (send pb end-edit-sequence)))))]
                [interval 50])
              (super-make-object (caaar rotation)))))))
  
  (define f (make-object frame% "World"))
  (define pb (make-object pasteboard%))
  (define c (instantiate (class editor-canvas% 
                           (define/override (on-char e)
                             (case (send e get-key-code)
                               [(left) (set! left? #t) (set! moving? #t)]
                               [(right) (set! left? #f) (set! moving? #t)]
                               [(#\space) (when can-jump? (set! y-speed -18))]
                               [else (when (or (not (eq? 'release (send e get-key-code)))
                                               (memq (send e get-key-release-code) '(left right)))
                                       (set! moving? #f) (set! x-speed min-x-speed))]))
                           (super-instantiate ()))
              (f)
              [editor pb]
              [stretchable-width #f]
              [stretchable-height #f]
              [style '(hide-hscroll hide-vscroll)]))

  ;; Kindof a hack: we know that the canvas adds a 5-pixel border:
  (send c min-client-width (+ 10 (* 20 cell-size)))
  (send c min-client-height (+ 10 (* 12 cell-size)))
  
  (define (add obj x y)    
    (send pb insert obj (* x cell-size) (* y cell-size)))
          
  (add (mk-rock) 0 10)
  (add (mk-rock) 19 10)
  (let loop ([i 0])
    (add (mk-ground) i 11)
    (unless (= i 19)
      (loop (add1 i))))
  
  (send pb insert (mk-person) (* 2 cell-size) (- (* 9 cell-size) 2))
  
  (send f show #t))
