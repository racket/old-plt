(module snip mzscheme
  (require (lib "unitsig.ss")
           "drsig.ss"
	   (lib "etc.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "class100.ss")
           (lib "framework.ss" "framework")
           (lib "zodiac.ss" "syntax"))
  
  (provide snip@)
  
  (define snip@
    (unit/sig drscheme:snip^
      (import)
      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      
      (define body-pen (send the-pen-list find-or-create-pen "BLACK" 0 'solid))
      (define body-brush (send the-brush-list find-or-create-brush "BLACK" 'solid))
      (define bw? (< (get-display-depth) 3))
      (define-values (shadow-pen shadow-brush)
	(if bw?
	    (let* ([pen (make-object pen% "BLACK" 0 'solid)]
		   [brush (make-object brush%)]
		   [a (integer->char #b01010101)]
		   [b (integer->char #b10101010)]
		   [bitmap (make-object bitmap%
			     (list a b a b a b a b)
			     8 8 1)])
	      (send* pen
                (set-colour "BLACK")
                (set-stipple bitmap))
	      (send* brush
                (set-colour "BLACK")
                (set-stipple bitmap))
	      (values pen brush))
	    (values
	     (send the-pen-list find-or-create-pen
		   "GRAY" 0 'solid)
	     (send the-brush-list find-or-create-brush
		   "GRAY" 'solid))))
      
      (define separator-snipclass
        (make-object
            (class100 snip-class% ()
              (override
                [read (lambda (s) 
                        (let ([size-box (box 0)])
                          (send s get size-box)
                          (make-object separator-snip%)))])
              (sequence (super-init)))))
      (send* separator-snipclass
        (set-version 1)
        (set-classname "drscheme:sepatator-snip%"))
      (send (get-the-snip-class-list) add separator-snipclass)
      
      
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
      (define separator-snip%
        (class100 snip% ()
          (inherit get-style set-snipclass set-flags get-flags get-admin)
          (private-field
           [width 500]
           [height 1]
           [white-around 2])
          (override
            [write (lambda (s) 
                     (send s put (char->integer #\r)))]
            [copy (lambda () 
                    (let ([s (make-object separator-snip%)])
                      (send s set-style (get-style))
                      s))]
            [get-extent
             (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
               (for-each (lambda (box) (set-box/f! box 0))
                         (list descent-box space-box lspace-box rspace-box))
               (let* ([admin (get-admin)]
                      [reporting-media (send admin get-editor)]
                      [reporting-admin (send reporting-media get-admin)]
                      [widthb (box 0)]
                      [space 2])
                 (send reporting-admin get-view null null widthb null)
                 (set! width (- (unbox widthb) 
                                space
                                2)))
               (set! height 1)
               (set-box/f! w-box width)
               (set-box/f! h-box (+ (* 2 white-around) height)))]
            [draw
             (lambda (dc x y left top right bottom dx dy draw-caret)
               (let ([orig-pen (send dc get-pen)]
                     [orig-brush (send dc get-brush)])
                 (send dc set-pen body-pen)
                 (send dc set-brush body-brush)
                 
                 (send dc draw-rectangle (+ x 1)
                       (+ white-around y) width height)
                 
                 (send dc set-pen orig-pen)
                 (send dc set-brush orig-brush)))])
          (sequence
            (super-init)
            (set-flags (cons 'hard-newline (get-flags)))
            (set-snipclass separator-snipclass))))
      
      (define make-snip-class
        (lambda (name get-%)
          (let ([ans (make-object
                         (class100 snip-class% ()
                           (override
                             [read (lambda (s) 
                                     (let ([size-box (box 0)])
                                       (send s get size-box)
                                       (make-object (get-%) (unbox size-box))))])
                           (sequence (super-init))))])  
            (send* ans
              (set-version 1)
              (set-classname name))
            (send (get-the-snip-class-list) add ans)
            ans)))
      
      (define prompt-snip-class
        (make-snip-class "drscheme:prompt-snip%" 
                         (lambda () prompt-snip%)))
      
      (define equal-snip-class
        (make-snip-class "drscheme:equal-snip%" 
                         (lambda () equal-snip%)))
      
      (define make-snip%
        (lambda (snip-class draw-snip)
          (rec
              this%
            (class100 snip% ([initial-size 12])
              (inherit get-style set-snipclass set-style)
              (private-field
               [allowed-size initial-size])
              (override
                [write (lambda (s) 
                         (send s put allowed-size))]
                [copy (lambda () 
                        (let ([s (make-object this% allowed-size)])
                          (send s set-style (get-style))
                          s))]
                [get-extent
                 (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
                   (for-each (lambda (box) (set-box/f! box 2))
                             (list descent-box space-box lspace-box rspace-box))
                   (set! allowed-size (send (get-style) get-size))
                   (set-box/f! w-box allowed-size)
                   (set-box/f! h-box allowed-size))]
                [draw
		 (lambda (dc x y left top right bottom dx dy draw-caret)
		   (let* ([shadow-size (max 1 (floor (/ allowed-size 10)))]
			  [size (-  allowed-size shadow-size)]
			  [line-width (/ size 3)]
			  [orig-brush (send dc get-brush)]
			  [orig-pen (send dc get-pen)])
		     
		     (send dc set-pen shadow-pen)
		     (send dc set-brush shadow-brush)
		     
		     (draw-snip dc (+ x shadow-size) (+ y shadow-size)
				size line-width)
		     
		     (send dc set-pen body-pen)
		     (send dc set-brush body-brush)
		     
		     (draw-snip dc x y size line-width)
		     
		     (send dc set-pen orig-pen)
		     (send dc set-brush orig-brush)))])
              (sequence
                (super-init)
                (set-snipclass snip-class))))))
      
      (define equal-snip%
        (make-snip%
         equal-snip-class
         (lambda (dc x y size line-width)
           (let ([top (- (+ y (/ size 2)) (* 5/4 line-width))])
             (send dc draw-rectangle x (+ top (* 3/2 line-width)) size line-width)
             (send dc draw-rectangle x top size line-width)))))
      
      
      (define prompt-snip%
        (make-snip%
         prompt-snip-class
         (lambda (dc x y size line-width)
           (send dc draw-rectangle x y line-width size)
           (let ([top-pos (- (+ y (/ size 2)) (/ line-width 2))])
             (send dc draw-rectangle 
                   x
                   top-pos
                   size
                   line-width)))))
      
      (define whole/part-number-snipclass
        (make-object 
            (class100 snip-class% ()
              (override
                [read
                 (lambda (p)
                   (make-object whole/part-number-snip%
                     (string->number (send p get-string))))])
              (sequence (super-init)))))
      (send whole/part-number-snipclass set-version 1)
      (send whole/part-number-snipclass set-classname 
            "drscheme:whole/part-number-snip")
      (send (get-the-snip-class-list) add whole/part-number-snipclass)
      
      (define whole/part-number-snip%
        (class100* snip% (gui-utils:text-snip<%>) (_number . args)
	  (private-field
           [number _number])
          (override
            [get-text
             (case-lambda
              [(offset num) (get-text offset num #f)]
              [(offset num flattened?) (number->string number)])])
          (public
            [get-number (lambda () number)]
            [get-formatted-string (lambda ()
                                    (if (or (string=? "" wholes)
                                            (string=? "-" wholes))
                                        (format "~a~a/~a" wholes nums dens)
                                        (format "~a ~a/~a" wholes nums dens)))]
            [get-string
             (lambda ()
               (format " ~a " number))])
          (private-field
           [wholes (cond
                     [(= (floor number) 0) ""]
                     [(= (ceiling number) 0) "-"]
                     [(< number 0)
                      (number->string (ceiling number))]
                     [else
                      (number->string (floor number))])]
           [nums (number->string (numerator (- (abs number) (floor (abs number)))))]
           [dens (number->string (denominator (- (abs number) (floor (abs number)))))])
          (inherit get-style)
          (override
            [write
             (lambda (p)
               (send p put (number->string number)))]
            [copy
             (lambda ()
               (make-object whole/part-number-snip% number))]
            [get-extent
             (lambda (dc x y w h descent space lspace rspace)
               (let* ([style (get-style)]
                      [th (send style get-text-height dc)]
                      [old-font (send dc get-font)])
                 (send dc set-font (send style get-font))
                 (let-values ([(nw nh na nd) (send dc get-text-extent nums)]
                              [(dw dh da dd) (send dc get-text-extent dens)]
                              [(ww wh wa wd) (send dc get-text-extent wholes)])
                   (set-box/f! h (+ nh dh 1))
                   (set-box/f! w (+ ww (max nw dw)))
                   (set-box/f! descent (+ wd (/ dh 2)))
                   (set-box/f! space  (+ wa (/ nh 2)))
                   (set-box/f! lspace 0)
                   (set-box/f! rspace 0))))]
            [draw
             (lambda (dc x y left top right bottom dx dy draw-caret)
               (let-values ([(nw nh na nd) (send dc get-text-extent nums)]
                            [(dw dh da dd) (send dc get-text-extent dens)]
                            [(ww wh wa wd) (send dc get-text-extent wholes)])
                 (let ([frac-w (max nw dw)])
                   (send dc draw-text nums (+ x ww (- (/ nw 2)) (/ frac-w 2)) y)
                   (send dc draw-text dens (+ x ww (- (/ dw 2)) (/ frac-w 2)) (+ y nh 1))
                   (send dc draw-text wholes x (+ y (/ nh 2)))
                   (send dc draw-line
                         (+ x ww) (+ y dh)
                         (+ x ww (max nw dw) -1) (+ y dh)))))])
          (inherit set-snipclass)
          (sequence 
            (super-init)
            (set-snipclass whole/part-number-snipclass)))))))
