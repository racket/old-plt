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

      
                                                               
                                             ;                 
                                     ;                         
                                     ;                         
 ; ;;;   ;;;  ; ;;;    ;;;   ;;;;   ;;;;;  ;;;   ; ;;;    ;;; ;
  ;     ;   ;  ;   ;  ;   ;      ;   ;       ;    ;;  ;  ;   ; 
  ;     ;;;;;  ;   ;  ;;;;;   ;;;;   ;       ;    ;   ;  ;   ; 
  ;     ;      ;   ;  ;      ;   ;   ;       ;    ;   ;  ;   ; 
  ;     ;   ;  ;   ;  ;   ;  ;   ;   ;   ;   ;    ;   ;  ;   ; 
 ;;;;    ;;;   ;;;;    ;;;    ;;; ;   ;;;  ;;;;; ;;;  ;;  ;;;; 
               ;                                             ; 
               ;                                             ; 
              ;;;                                         ;;;  

                                                 
    ;;                  ;                  ;;;   
     ;                                       ;   
     ;                                       ;   
  ;;;;   ;;;    ;;;   ;;;   ;;; ;   ;;;;     ;   
 ;   ;  ;   ;  ;   ;    ;    ; ; ;      ;    ;   
 ;   ;  ;;;;;  ;        ;    ; ; ;   ;;;;    ;   
 ;   ;  ;      ;        ;    ; ; ;  ;   ;    ;   
 ;   ;  ;   ;  ;   ;    ;    ; ; ;  ;   ;    ;   
  ;;; ;  ;;;    ;;;   ;;;;; ;; ; ;;  ;;; ; ;;;;;;
                                                 
                                                 
      
      (define repeating-decimal-snip-class%
        (class snip-class%
          (define/override (read f)
            (instantiate repeating-decimal-number% ()
              [number (string->number (send f get-string))]
              [prefix (send f get-string)]))
          (super-instantiate ())))
      
      (define repeating-decimal-snipclass (make-object repeating-decimal-snip-class%))
      (send repeating-decimal-snipclass set-version 2)
      (send repeating-decimal-snipclass set-classname "drscheme:repeating-decimal")
      
      (define arrow-cursor (make-object cursor% 'arrow))

      ;; cut-off : number
      ;; indicates how many digits to fetch for each click
      (define cut-off 25)
      
      (define repeating-decimal-number%
        (class snip%
          (init-field number
                      [prefix ""])
          
          ;; these fields are for the drawing code
          (field 
           ;; clickable-portion : (union #f string)
           [clickable-portion #f]
           ;; unbarred-portion : string
           [unbarred-portion ""]
           ;; barred-portion : (union #f string)
           [barred-portion #f])
          
          ;; these fields are for the expansion calculation code
          (field [whole-part (floor number)]
                 [init-num (* 10 (numerator (- number whole-part)))]
                 [den (denominator (- number whole-part))])
          
          ;; ht : number -o> (cons digit number)
          ;; this maps from divisors of the denominator to
          ;; digit and new divisor pairs. Use this
          ;; to read off the decimal expansion.
          (field [ht (make-hash-table 'equal)])
          
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
          
          ;; iterate : -> void
          ;; computes the next sequence of digits
          ;; and update the GUI aspects of the snip
          (define (iterate)
            (expand-number)
            (update-drawing-fields))
          
          (inherit get-admin)
          
          ;; iterate/reflow : -> void
          ;; iterates the fraction and tells the administrator to redraw the numbers
          (define (iterate/reflow)
            (iterate)
            (let ([admin (get-admin)])
              (when admin
                (let ([dc (send admin get-dc)]
                      [font (send (get-style) get-font)])
                  (let-values ([(w1 h1 d1 a1) (get-text-extent/f dc unbarred-portion font)]
                               [(w2 h2 d2 a2) (get-text-extent/f dc barred-portion font)]
                               [(w3 h3 d3 a3) (get-text-extent/f dc clickable-portion font)])
                    (let ([sw (+ w1 w2 w3)]
                          [sh (if barred-portion (+ h1 2) h1)])
                      (send admin resized this #t)
                      '(send admin needs-update this 0 0 sw sh)))))))
          
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
                      prefix
                      (if (zero? whole-part) "" (number->string whole-part))
                      "."
                      (apply string-append (map number->string (extract-non-cycle)))))
               (set! barred-portion #f)
               (set! clickable-portion "...")]
              [(number? repeat)
               (set! unbarred-portion
                     (string-append
                      prefix
                      (if (zero? whole-part) "" (number->string whole-part))
                      "."
                      (apply string-append 
                             (map number->string (extract-non-cycle)))))
               (set! barred-portion (apply string-append (map number->string (extract-cycle))))
               (set! clickable-portion #f)]
              [else
               (set! unbarred-portion
                     (string-append
                      prefix
                      (if (zero? whole-part) "" (number->string whole-part))
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
               (string-append 
                unbarred-portion
                (or barred-portion "")
                (or clickable-portion ""))]))
          
          (define/override (write f)
            (send f put (number->string number))
            (send f put prefix))
          
          (define/override (copy)
            (instantiate repeating-decimal-number% ()
              [number number]
              [prefix prefix]))
          
          (inherit get-style)
          (define/override (get-extent dc x y wb hb descent space lspace rspace)
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
                  [sy (- (send evt get-y) y)]
                  [font (send (get-style) get-font)])
              (let-values ([(w1 h1 d1 a1) (get-text-extent/f dc unbarred-portion font)]
                           [(w2 h2 d2 a2) (get-text-extent/f dc barred-portion font)]
                           [(w3 h3 d3 a3) (get-text-extent/f dc clickable-portion font)])
                (let ([in-region? (<= (+ w1 w2) sx (+ w1 w2 w3))])
                  (if in-region?
                      arrow-cursor
                      #f)))))
          
          (define/override (on-event dc x y editor-x editor-y evt)
            (let ([sx (- (send evt get-x) x)]
                  [sy (- (send evt get-y) y)]
                  [font (send (get-style) get-font)])
              (let-values ([(w1 h1 d1 a1) (get-text-extent/f dc unbarred-portion font)]
                           [(w2 h2 d2 a2) (get-text-extent/f dc barred-portion font)]
                           [(w3 h3 d3 a3) (get-text-extent/f dc clickable-portion font)])
                (let ([in-region? (<= (+ w1 w2) sx (+ w1 w2 w3))])
                  (cond
                    [(send evt button-up?) (iterate/reflow)]
                    [(or (send evt moving?)
                         (send evt leaving?)
                         (send evt entering?))
                     '...]
                    [else (void)])))))            
          
          (super-instantiate ())
          (inherit set-snipclass set-flags get-flags)
          (set-flags (cons 'handles-events (get-flags)))
          (set-snipclass repeating-decimal-snipclass)
          (iterate))) ;; calc first digits
      
      ;; hash-table-bound? : hash-table TST -> boolean
      (define (hash-table-bound? ht key)
        (let/ec k
          (hash-table-get ht key (lambda () (k #f)))
          #t))                                                

      ;; make-repeating-fraction-snip : number boolean -> snip
      (define (make-repeating-fraction-snip number e-prefix?)
        (instantiate repeating-decimal-number% ()
          [number number]
          [prefix (if e-prefix? "#e" "")]))


                                                        
   ;;;                                ;                 
  ;                           ;                         
  ;                           ;                         
 ;;;;;  ; ;;;  ;;;;    ;;;   ;;;;;  ;;;     ;;;  ; ;;;  
  ;      ;         ;  ;   ;   ;       ;    ;   ;  ;;  ; 
  ;      ;      ;;;;  ;       ;       ;    ;   ;  ;   ; 
  ;      ;     ;   ;  ;       ;       ;    ;   ;  ;   ; 
  ;      ;     ;   ;  ;   ;   ;   ;   ;    ;   ;  ;   ; 
 ;;;;   ;;;;    ;;; ;  ;;;     ;;;  ;;;;;   ;;;  ;;;  ;;
                                                        
                                                        
                                                        
      
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
               (let ([s (make-object whole/part-number-snip% number)])
                 (send s set-style (get-style))
                 s))]
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
                   (send dc draw-text nums (+ x ww (- frac-w nw)) y)
                   (send dc draw-text dens (+ x ww (- (/ dw 2)) (/ frac-w 2)) (+ y nh 1))
                   (send dc draw-text wholes x (+ y (/ nh 2)))
                   (send dc draw-line
                         (+ x ww) (+ y dh)
                         (+ x ww (max nw dw) -1) (+ y dh)))))])
          (inherit set-snipclass)
          (sequence 
            (super-init)
            (set-snipclass whole/part-number-snipclass)))))))
