#|

TODO: 

fix the repeating decimal snip so that copies
carry over the computation of the original
(maybe just track the number of clicks?)

|#
(module snip mzscheme
  (require (lib "unitsig.ss")
           "drsig.ss"
	   (lib "etc.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "class100.ss")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants"))
  
  (provide snip@)
  
  (define snip@
    (unit/sig drscheme:snip^
      (import)
      
      (define special<%>
        (interface ()
          read-special))
      
      ;; make-repeating-decimal-snip : number boolean -> snip
      (define (make-repeating-decimal-snip number e-prefix?)
        (instantiate number-snip% ()
          [number number]
          [decimal-prefix (if e-prefix? "#e" "")]))
      
      ;; make-fraction-snip : number boolean -> snip
      (define (make-fraction-snip number e-prefix?)
        (let ([n (instantiate number-snip% ()
                   [number number]
                   [decimal-prefix (if e-prefix? "#e" "")])])
          (send n set-fraction-view #t)
          n))

      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      (define bw? (< (get-display-depth) 3))
      
      (define number-snip-class%
        (class snip-class%
          (define/override (read f)
            (let* ([number (string->number (send f get-string))]
                   [decimal-prefix (send f get-string)]
                   [fraction-view? (string=? "#t" (send f get-string))]
                   [expansions (string->number (send f get-string))]
                   [snip
                    (instantiate number-snip% ()
                      [number number]
                      [decimal-prefix decimal-prefix])])
              (send snip iterate (max 0 (- expansions 1))) ;; one iteration is automatic
              (send snip set-fraction-view fraction-view?)
              snip))
          (super-instantiate ())))
      
      (define number-snipclass (make-object number-snip-class%))
      (send number-snipclass set-version 3)
      (send number-snipclass set-classname "drscheme:number")
      (send (get-the-snip-class-list) add number-snipclass)

      (define arrow-cursor (make-object cursor% 'arrow))

      ;; cut-off : number
      ;; indicates how many digits to fetch for each click
      (define cut-off 25)
      
      (define number-snip%
        (class* snip% (special<%>)
          ;; number : number
          ;; this is the number to show
          (init-field number)
          (define/public (get-number) number)
          
           ;; decimal-prefix : string
           ;; this prefix is shown on the string when it is viewed in
           ;; the decimal view
          (init-field [decimal-prefix ""])
        
          ;; fraction-view? : boolean
          ;; this field holds the current view state
          (field [fraction-view? #f])
          
          ;; these fields are for the drawing code for decimal printing
          (field 
           ;; clickable-portion : (union #f string)
           [clickable-portion #f]
           ;; unbarred-portion : string
           [unbarred-portion ""]
           ;; barred-portion : (union #f string)
           [barred-portion #f])
          
          (field
           ;; wholes/frac : string
           ;; the whole-number portion of the number as a fraction
           [wholes/frac
            (cond
              [(= (floor number) 0) ""]
              [(= (ceiling number) 0) "-"]
              [(< number 0)
               (number->string (ceiling number))]
              [else
               (number->string (floor number))])])
          
          (field
           ;; wholes/dec : string
           ;; the whole-number portion of decimal expansion
           [wholes/dec
            (cond
              [(= (floor number) 0) "0"]
              [(= (ceiling number) 0) "-0"]
              [(< number 0)
               (number->string (ceiling number))]
              [else
               (number->string (floor number))])])

          ;; these fields are for the fractional printing view
          (field
           ;; nums : string
           ;; the numerator, as a string
           [nums (number->string (numerator (- (abs number) (floor (abs number)))))]

           ;; dens : string
           ;; the denominator, as a string
           [dens (number->string (denominator (- (abs number) (floor (abs number)))))])

          ;; these fields are for the decimal expansion calculation code
          (field
           [init-num (* 10 (numerator (- (abs number) (floor (abs number)))))]
           [den (denominator (- (abs number) (floor (abs number))))])
          
          ;; ht : number -o> (cons digit number)
          ;; this maps from divisors of the denominator to
          ;; digit and new divisor pairs. Use this
          ;; to read off the decimal expansion.
          (field
           [ht (make-hash-table 'equal)]
           [expansions 0])
          
          ;; this field holds the state of the current computation
          ;; of the numbers digits. If it is a number, it corresponds
          ;; to the next starting divisor in the iteration.
          ;; if it is #f, it means that the string of digits is
          ;; fully computed.
          (field [state init-num])
          
          ;; repeat : (union 'unk number #f)
          ;; this field correlates with `state'. If `state' is a number,
          ;; this field is 'unk. Otherwise, this is either a number of #f.
          ;; #f indicates no repeat.
          ;; a number indiates a repeat starting at `number' in `ht'.
          (field [repeat 'unk])
          
          ;; reverse-fraction-view : -> void
          ;; toggles the view
          (define/public (reverse-fraction-view)
            (set-fraction-view (not fraction-view?)))

          ;; set-fraction-view : boolean -> void
          ;; sets the view based on the input
          (define/public (set-fraction-view b)
            (set! fraction-view? b)
            (let ([admin (get-admin)])
              (when admin
                (send admin resized this #t))))

          ;; get-fraction-view : -> boolean
          ;; returns the current fraction view settings
          (define/public (get-fraction-view) fraction-view?)
          
          ;; iterate : number -> void
          ;; computes the next sequence of digits (`n' times)
          ;; and update the strings for GUI drawing
          (define/public (iterate n)
            (let loop ([n n])
              (unless (zero? n)
                (expand-number)
                (loop (- n 1))))
            (update-drawing-fields))
          
          (inherit get-admin)
          
          ;; iterate/reflow : -> void
          ;; iterates the fraction and tells the administrator to redraw the numbers
          (define (iterate/reflow)
            (iterate 1)
            (let ([admin (get-admin)])
              (when admin
                (send admin resized this #t))))
          
          ;; one-step-division : number -> number number
          ;; given a numerator and denominator,
          ;; returns a digits and a new numerator to consider
          (define/private (one-step-division num)
            (cond
              [(num . < . den) (values 0 (* 10 num))]
              [else
               (let ([qu (quotient num den)])
                 (values qu (* 10 (- num (* qu den)))))]))
          
          ;; expand-number : -> void
          ;; iterates until the numbers decimal expansion is completely computed,
          ;; or the number's decimal expansion terminates.
          (define/public (expand-number)
            (when state
              (set! expansions (+ expansions 1))
              (let loop ([num state]
                         [counter cut-off])
                (cond
                  [(hash-table-bound? ht num)
                   (set! state #f)
                   (set! repeat num)]
                  [(zero? counter) 
                   (set! state num)]
                  [else
                   (let-values ([(dig next-num) (one-step-division num)])
                     (if (zero? next-num)
                         (begin
                           (hash-table-put! ht num (cons dig #t))
                           (set! state #f)
                           (set! repeat #f))
                         (begin
                           (hash-table-put! ht num (cons dig next-num))
                           (loop next-num (- counter 1)))))]))))
          
          ;; update-drawing-fields : -> void
          (define/public (update-drawing-fields)
            (cond
              [(number? state) 
               (set! unbarred-portion
                     (string-append
                      decimal-prefix
                      wholes/dec
                      "."
                      (apply string-append (map number->string (extract-non-cycle)))))
               (set! barred-portion #f)
               (set! clickable-portion "...")]
              [(number? repeat)
               (set! unbarred-portion
                     (string-append
                      decimal-prefix
                      wholes/dec
                      "."
                      (apply string-append 
                             (map number->string (extract-non-cycle)))))
               (set! barred-portion (apply string-append (map number->string (extract-cycle))))
               (set! clickable-portion #f)]
              [else
               (set! unbarred-portion
                     (string-append
                      decimal-prefix
                      wholes/dec
                      "."
                      (apply string-append
                             (map number->string (extract-non-cycle)))))
               (set! barred-portion #f)
               (set! clickable-portion #f)]))
          
          ;; extract-cycle : -> (listof digit)
          ;; pre: (number? repeat)
          (define (extract-cycle)
            (let ([pr (hash-table-get ht repeat)])
              (cons (car pr)
                    (extract-helper (cdr pr)))))
          
          ;; extract-non-cycle : -> (listof digit)
          (define/private (extract-non-cycle) (extract-helper init-num))
          
          (define/private (extract-helper start)
            (let loop ([ind start])
              (cond
                [(equal? ind repeat) null]
                [else
                 (let* ([iter (hash-table-get ht ind)]
                        [dig (car iter)]
                        [next-num (cdr iter)])
                   (cons dig
                         (if (hash-table-bound? ht next-num)
                             (loop next-num)
                             null)))])))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;                                                                  ;;
          ;;                       snip infrastructure                        ;;
          ;;                                                                  ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          
          (define/public (read-special file line col pos)
            (values number 1))
          
          (define/override get-text
            (case-lambda
              [(offset num) (get-text offset num #f)]
              [(offset num flattened?) 
               (if fraction-view?
                   (string-append wholes/frac " " nums "/" dens)
                   (string-append 
                    unbarred-portion
                    (or barred-portion "")
                    (or clickable-portion "")))]))
          
          (define/override (write f)
            (send f put (number->string number))
            (send f put decimal-prefix)
            (send f put (format "~a" fraction-view?))
            (send f put (number->string expansions)))
          
          (define/override (copy)
            (let ([snip (instantiate number-snip% ()
                          [number number]
                          [decimal-prefix decimal-prefix])])
              (send snip iterate (max 0 (- expansions 1))) ;; one iteration is automatic
              (send snip set-fraction-view fraction-view?)
              snip))
          
          (inherit get-style)
          
          (define/override (get-extent dc x y wb hb descent space lspace rspace)
            (if fraction-view?
                (get-fraction-extent dc x y wb hb descent space lspace rspace)
                (get-decimal-extent dc x y wb hb descent space lspace rspace)))
          
          (define (get-fraction-extent dc x y w h descent space lspace rspace)
            (let* ([style (get-style)]
                   [th (send style get-text-height dc)]
                   [old-font (send dc get-font)])
              (send dc set-font (send style get-font))
              (let-values ([(nw nh na nd) (send dc get-text-extent nums)]
                           [(dw dh da dd) (send dc get-text-extent dens)]
                           [(ww wh wa wd) (send dc get-text-extent wholes/frac)])
                (set-box/f! h (+ nh dh 1))
                (set-box/f! w (+ ww (max nw dw)))
                (set-box/f! descent (+ wd (/ dh 2)))
                (set-box/f! space  (+ wa (/ nh 2)))
                (set-box/f! lspace 0)
                (set-box/f! rspace 0))))
          
          (define (get-decimal-extent dc x y wb hb descent space lspace rspace)
            (let ([font (send (get-style) get-font)])
              (let-values ([(w1 h1 d1 a1) (get-text-extent/f dc unbarred-portion font)]
                           [(w2 h2 d2 a2) (get-text-extent/f dc barred-portion font)]
                           [(w3 h3 d3 a3) (get-text-extent/f dc clickable-portion font)])
                (set-box/f! wb (+ w1 w2 w3))
                (set-box/f! hb (if barred-portion 
                                   (+ h1 2)
                                   h1))
                (set-box/f! descent d1)
                (set-box/f! space (if barred-portion
                                      (+ a1 2)
                                      a1))
                (set-box/f! lspace 0)
                (set-box/f! rspace 0))))
          
          (define/private (get-text-extent/f dc str font)
            (if str
                (let-values ([(w h d a) (send dc get-text-extent str font)])
                  (values w h d a))
                (values 0 0 0 0)))
          
          (define/override (draw dc x y left top right bottom dx dy draw-caret?)
            (if fraction-view?
                (draw-fraction dc x y)
                (draw-decimals dc x y)))
          
          (define (draw-fraction dc x y)
            (let-values ([(nw nh na nd) (send dc get-text-extent nums)]
                         [(dw dh da dd) (send dc get-text-extent dens)]
                         [(ww wh wa wd) (send dc get-text-extent wholes/frac)])
              (let ([frac-w (max nw dw)])
                (send dc draw-text nums (+ x ww (- frac-w nw)) y)
                (send dc draw-text dens (+ x ww (- (/ dw 2)) (/ frac-w 2)) (+ y nh 1))
                (send dc draw-text wholes/frac x (+ y (/ nh 2)))
                (send dc draw-line
                      (+ x ww) (+ y dh)
                      (+ x ww (max nw dw) -1) (+ y dh)))))
          
          (define (draw-decimals dc x y)
            (define (draw-digits digits x)
              (if digits
                  (let-values ([(w h a d) (send dc get-text-extent digits)])
                    (send dc draw-text digits x (if barred-portion (+ y 2) y))
                    (+ x w))
                  x))
            (let* ([unbarred-end (draw-digits unbarred-portion x)]
                   [barred-end (draw-digits barred-portion unbarred-end)]
                   [clickable-end (draw-digits clickable-portion barred-end)])
              (when barred-portion
                (send dc draw-line unbarred-end y (- barred-end 1) y))))
          
          (define/override (adjust-cursor dc x y editorx editory evt)
            (let ([sx (- (send evt get-x) x)]
                  [sy (- (send evt get-y) y)])
              (if (in-clickable-portion? dc sx sy)
                  arrow-cursor
                  #f)))
          
          (define/override (on-event dc x y editor-x editor-y evt)
            (let ([sx (- (send evt get-x) x)]
                  [sy (- (send evt get-y) y)])
              (cond
                [(send evt button-down? 'right)
                 (let ([admin (get-admin)])
                   (when admin
                     (let ([popup-menu (make-right-clickable-menu)])
                       (send admin popup-menu popup-menu this (+ sx 1) (+ sy 1)))))]
                [(send evt button-up? 'left)
                 (when (in-clickable-portion? dc sx sy)
                   (iterate/reflow))]
                [else (void)])))

          (define (make-right-clickable-menu)
            (let ([menu (make-object popup-menu%)])
              (make-object menu-item% 
                (if fraction-view?
                    (string-constant show-decimal-expansion)
                    (string-constant show-fraction-view))
                menu
                (lambda (x y)
                  (reverse-fraction-view)))
              (when (and (not fraction-view?)
                         clickable-portion)
                (make-object menu-item% 
                  (string-constant show-more-decimal-places)
                  menu
                  (lambda (x y)
                    (iterate/reflow))))
              menu))

          (define (in-clickable-portion? dc sx sy)
            (and clickable-portion
                 (let ([font (send (get-style) get-font)])
                   (let-values ([(w1 h1 d1 a1) (get-text-extent/f dc unbarred-portion font)]
                                [(w2 h2 d2 a2) (get-text-extent/f dc barred-portion font)]
                                [(w3 h3 d3 a3) (get-text-extent/f dc clickable-portion font)])
                     (and (<= (+ w1 w2) sx (+ w1 w2 w3))
                          (<= 0 sy h3))))))

          (super-instantiate ())
          (inherit set-snipclass set-flags get-flags)
          (set-flags (cons 'handles-events (get-flags)))
          (set-snipclass number-snipclass)
          (iterate 1))) ;; calc first digits
      
      ;; hash-table-bound? : hash-table TST -> boolean
      (define (hash-table-bound? ht key)
        (let/ec k
          (hash-table-get ht key (lambda () (k #f)))
          #t)))))
