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
      
      (define special<%>
        (interface ()
          read-special))
      
      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      (define bw? (< (get-display-depth) 3))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                                                                  ;;
      ;;                         fraction snip                            ;;
      ;;                                                                  ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
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
        (class100* snip% (special<%>) (_number . args)
	  (private-field
           [number _number])
          (public
            [read-special
             (lambda (file line col pos)
               (values
                ;(datum->syntax-object #f number (list file line col pos 1))
                number
                1))])
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
                                        (format "~a ~a/~a" wholes nums dens)))])
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
