(load "comma-delimited-text-parser.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-attribute-value : (listof attributes) symbol string -> string
; To get an attribute's value, or default value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-attribute-value attribs name default-value) 
  (local [(define (find-name attriblist)
            (cond [(empty? attriblist) default-value]
                  [(symbol=? 
                    (attribute-name (first attriblist))
                    name)
                   (attribute-value (first attriblist))]
                  [else (find-name (rest attriblist))]))]
    (find-name attribs)))


;; string->exact-non-negative-integer : string num -> exact-non-negative-integer
;; if the string does not represent an exact positive integer, default is returned
(define (string->exact-non-negative-integer str default)
  (let ([num (string->number str)])
    (if (and (number? num)
             (exact? num)
             (integer? num)
             (>= num 0))
        num
        default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; length->pixels : string num num -> exact-positive-integer
; Consumes a fixed or percent length and a quantity representing 
; a space.  A percentage length is based on the percentage 
; screen space available. 
; Ex: (length->pixels "770" 0 0) -> 770
;     (length->pixels "500" 200 20) -> 500
;     (length->pixels "50%" 0 30) -> 0
;     (length->pixels "25%" 12 45) -> 3
;     (length->pixels "5*" 12 0) -> 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (length->pixels a-length space default-value)
  (local [(define (length-type-char) (string-ref a-length (sub1 (string-length a-length))))
          (define (constant)
            (string->number (substring a-length 0 (sub1 (string-length a-length)))))
          (define rounded-default-value (round default-value))
          (define rounded-exact-default-value (cond [(inexact? rounded-default-value) (inexact->exact rounded-default-value)]
                                                    [(exact? rounded-default-value) rounded-default-value]))]
    (cond [(= 0 (string-length a-length)) rounded-exact-default-value]
          [(char=? #\% (length-type-char))
           (inexact->exact (round (* (/ (constant) 100) space)))]
          [(char=? #\* (length-type-char))
           0]
          [(boolean? (string->number a-length))
           rounded-exact-default-value]
          [else (inexact->exact (round (string->number a-length)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; x^y : num num -> num
; Exponentiates x to the power of y and returns the product.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (x^y x y)
    (cond [(= 0 y) 1]
          [else (* x (x^y x (sub1 y)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hex->integer : string -> integer
; Consumes a-string, stripped of any notation and representing 
; only the hexadecimal digits.  Returns the base-10 equivalent.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXAMPLES:
; (hex->integer "a") -> 10
; (hex->integer "f") -> 15
; (hex->integer "ff") -> 255
; (hex->integer "FF") -> 255
; (hex->integer "00") -> 0
; (hex->integer "10E5") -> 4325
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (hex->integer a-string)
  (local [(define listofhexchars (string->list a-string))
          (define (helper a-char sofar/power)
            (let ([sofar (car sofar/power)]
                  [power (cdr sofar/power)]
                  [n (cond
                       [(char<=? #\0 a-char #\9)
                        (- (char->integer a-char) (char->integer #\0))]
                       [(char<=? #\a a-char #\f)
                        (+ 10 (- (char->integer a-char) (char->integer #\a)))]
                       [(char<=? #\A a-char #\F)
                        (+ 10 (- (char->integer a-char) (char->integer #\A)))]
                       [else 0])])
              (cons (+ sofar (* n (x^y 16 power)))
                    (+ power 1))))]
    (car (foldr helper (cons 0 0) listofhexchars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; get-file-suffix : string -> string
; Returns the substring after the last "." in a-string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-file-suffix a-string)
  (local [(define listofchars (string->list a-string))
          (define (helper a-list)
            (local [(define thesuffix (foldr (lambda (char found?/sofar)
                                               (cond [(boolean=? #t (car found?/sofar)) found?/sofar]
                                                     [(char=? #\. char) (cons #t (cdr found?/sofar))]
                                                     [else (cons #f (cons char (cdr found?/sofar)))]))
                                             (cons #f empty)
                                             a-list))]
              (cond [(boolean=? #f (car thesuffix)) empty]
                    [(boolean=? #t (car thesuffix)) (cdr thesuffix)])))]
    (list->string (helper listofchars))))

; color-value->object : string -> color%
; Returns a color-object with RGB levels or color-name
; as specified in a-color.
(define (color-value->object a-color)
  (local [(define lochars (string->list a-color))
          (define new-color (make-object color% a-color))]
    (cond [(char=? #\# (string-ref a-color 0))
           (make-object color% 
             (hex->integer (substring a-color 0 2))
             (hex->integer (substring a-color 2 4))
             (hex->integer (substring a-color 4 6)))]
          [(send new-color ok?) new-color]
          [else (make-object color% "black")])))

; accumulated-errors is a (listof str) of error messages 
; displayed thus far since the last reset.
(define accumulated-errors
  empty)

; show-internal-error-once : string -> void
; Displays error_msg to the user.
(define (show-internal-error-once error_msg)
  (local [(define title "error")
          (define msg (string-append "internal browser error: " error_msg))
          (define title-len (string-length title))]
    (if (not (empty? (filter (lambda (accum-msg)
                               (string-ci=? error_msg accum-msg)) accumulated-errors)))
        (void)
        (begin
          (set! accumulated-errors (cons error_msg accumulated-errors))
          (message-box title
                       msg
                       #f
                       '(ok))))))