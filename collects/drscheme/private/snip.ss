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
                                                 
                                                 
                                                 

      ;; find-repeat : 0 <= n < 1 -> (values (listof digit) (listof digit))
      ;; finds the repeating and non-repeating portion of an exact number
      (define (find-repeat n)
        (let loop ([n n]
                   [priors (list)]
                   [non-repeating-digits (list)])
          (let* ([d (floor (* n 10))]
                 [m (- (* n 10) d)])
            (cond
              [(zero? m)
               (values (reverse (cons d non-repeating-digits)) null)]
              [else
               (let* ([new-digits (cons d non-repeating-digits)]
                      [new-priors (cons n priors)]
                      [rep (extract-repeat m new-priors new-digits)])
                 (if rep
                     (values (car rep) (cadr rep))
                     (loop m
                           new-priors
                           new-digits)))]))))
      
      ;; extract-repeat : number[0<=n<1] (listof digit) (listof digit) ->
      ;;                  (union #f (list (listof digit) (listof digit)))
      (define (extract-repeat m priors non-repeating-digits)
        (let loop ([priors (reverse priors)]
                   [digits (reverse non-repeating-digits)]
                   [digit-front null])
          (cond
            [(null? priors) #f]
            [else
             (let ([pm (car priors)])
               (if (= m pm)
                   (list (reverse digit-front) digits)
                   (loop (cdr priors)
                         (cdr digits)
                         (cons (car digits) digit-front))))])))
      
      (define repeat-snip-class%
        (class snip-class%
          (define/override (read f)
            (instantiate repeat-snip% ()
              [number (string->number (send f get-string))]
              [whole-digits (send f get-string)]
              [non-repeat-digits (send f get-string)]
              [repeat-digits (send f get-string)]))
          (super-instantiate ())))
      
      (define repeat-snipclass (make-object repeat-snip-class%))
      (send repeat-snipclass set-version 1)
      (send repeat-snipclass set-classname "drscheme:repeating-decimal")
      (send (get-the-snip-class-list) add repeat-snipclass)
      
      (define repeat-snip%
        (class* snip% (special<%>)
          (init-field number whole-digits non-repeat-digits repeat-digits)
          
          (define/public (get-number) number)
          (define/public (get-whole-digits) whole-digits)
          (define/public (get-non-repeat-digits) non-repeat-digits)
          (define/public (get-repeat-digits) repeat-digits)
          
          (define/public (read-special file line col pos)
            (values number 1))
          
          (define/override (copy)
            (instantiate repeat-snip% ()
              [number number]
              [whole-digits whole-digits]
              [non-repeat-digits non-repeat-digits]
              [repeat-digits repeat-digits]))
          
          (define/override (write f)
            (send f put (number->string number))
            (send f put whole-digits)
            (send f put non-repeat-digits)
            (send f put repeat-digits))
          
          (inherit get-style)
          (define/override (get-extent dc x y wb hb descent space lspace rspace)
            (let ([font (send (get-style) get-font)]
                  [old-font (send dc get-font)])
              (send dc set-font font)
              (let-values ([(w1 h1 d1 a1) (send dc get-text-extent whole-digits font)]
                           [(w2 h2 d2 a2) (send dc get-text-extent "." font)]
                           [(w3 h3 d3 a3) (send dc get-text-extent non-repeat-digits font)]
                           [(w4 h4 d4 a4) (send dc get-text-extent repeat-digits font)])
                (set-box/f! wb (+ w1 w2 w3 w4))
                (set-box/f! hb (+ h1 2))
                (set-box/f! descent d1)
                (set-box/f! space (+ a1 2))
                (set-box/f! lspace 0)
                (set-box/f! rspace 0))
              (send dc set-font old-font)))
          
          (define/override (draw dc x y left top right bottom dx dy draw-caret?)
            (define (draw-digits digits x)
              (let-values ([(w h a d) (send dc get-text-extent digits)])
                (send dc draw-text digits x (+ y 2))
                (+ x w)))
            (let* ([whole-end (draw-digits whole-digits x)]
                   [decimal-end (draw-digits "." whole-end)]
                   [non-repeat-end (draw-digits non-repeat-digits decimal-end)]
                   [repeat-end (draw-digits repeat-digits non-repeat-end)])
              (unless (string=? "" repeat-digits)
                (send dc draw-line non-repeat-end y (- repeat-end 1) y))))
          
          (super-instantiate ())
          
          (inherit set-snipclass)
          (set-snipclass repeat-snipclass)))
      
      ;; make-repeating-fraction-snip : number boolean -> snip
      (define (make-repeating-fraction-snip number e-prefix?)
        (let* ([whole-part (floor number)]
               [fraction-part (- number whole-part)])
          (let-values ([(non-repeating repeating) (find-repeat fraction-part)])
            (instantiate repeat-snip% ()
              [number number]
              [whole-digits (let ([raw (if (zero? whole-part)
                                           ""
                                           (format "~a" whole-part))])
                              (if e-prefix?
                                  (string-append "#e" raw)
                                  raw))]
              [repeat-digits
               (apply string-append (map number->string repeating))]
              [non-repeat-digits
               (apply string-append (map number->string non-repeating))]))))

#|

;;; test code

(define f (make-object frame% "frame" #f 300 150))
(define t (make-object text%))
(define ec (make-object editor-canvas% f t))
(send t insert (make-repeating-fraction-snip 10/7))
(send f show #t)

(define (test-find-repeat frac digits repeat)
  (let-values ([(got-digits got-repeat) (find-repeat frac)])
    (unless (and (equal? got-digits digits)
                 (equal? got-repeat repeat))
      (printf "expected ~a -> ~a ~a\n     got ~a -> ~a ~a\n"
              frac digits repeat
              frac got-digits got-repeat))))

(test-find-repeat 1/2 '(5) #f)
(test-find-repeat 1/4 '(2 5) #f)
(test-find-repeat #e.1234567 '(1 2 3 4 5 6 7) #f)
(test-find-repeat 1/3 '() '(3))
(test-find-repeat 1/30 '(0) '(3))
(test-find-repeat 1/300 '(0 0) '(3))
(test-find-repeat (+ 1/30000 #e.1234) '(1 2 3 4) '(3))
(test-find-repeat 1/7 '() '(1 4 2 8 5 7))

|#
      
      

                                                        
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
