#|

At Jon Jacky's request, this is a teachpack-ized version of simply.scm.

He writes:
> simply.scm comes from ftp://anarres.cs.berkeley.edu/pub/scheme/simply.scm
> via the link on Brian Harvey's site at http://www.cs.berkeley.edu/~bh/

|#

;;; simply.scm version 3.13 (8/11/98) (teachpackized 9/20/2000 --robby)
;;; (Numbered to agree with berkeley.scm version.)

(unit/sig (whoops
	   word?
	   sentence?
	   empty?
	   char-rank
	   string->word
	   char->word
	   word->string
	   count
	   word
	   se
	   sentence
	   first
	   last
	   bf
	   butfirst
	   bl
	   butlast
	   equal?
	   member?
	   before?
	   filter
	   keep
	   appearances
	   every
	   accumulate
	   reduce
	   repeated
	   make-node
	   datum
	   children
	   show
	   show-line
	   read-string
	   read-line
	   *the-open-inports*
	   *the-open-outports*
	   open-output-file
	   open-input-file
	   remove!
	   close-input-port
	   close-output-port
	   close-all-ports
	   maybe-num
	   logoize
	   logoize-1
	   logoize-2
	   strings-are-numbers

	   *
	   +
	   -
	   /
	   <
	   <=
	   =
	   >
	   >=
	   abs
	   acos
	   asin
	   atan
	   ceiling
	   cos
	   even?
	   exp
	   expt
	   floor
	   align
	   gcd
	   integer?
	   item
	   lcm
	   list-ref
	   log
	   make-vector
	   max
	   min
	   modulo
	   negative?
	   number?
	   odd?
	   positive?
	   quotient
	   random
	   remainder
	   round
	   sin
	   sqrt
	   tan
	   truncate
	   vector-ref
	   vector-set!
	   zero?
	   number->string)
  (import plt:userspace^)

  (rename [-empty? empty?]
	  [-filter filter]
	  [-first first]
	  [-quotient quotient]
	  [-remainder remainder])

;;; This file uses Scheme features we don't talk about in _Simply_Scheme_.
;;; Read at your own risk.

(define number->string
  (lambda args
    (if (string? (car args))
	(car args)
	(apply #%number->string args))))

;; Get strings in error messages to print nicely (especially "")

(define whoops
  (let ()
    (define (error-printform x)
      (if (string? x)
	  (string-append "\"" x "\"")
	  x))
    (lambda (string . args)
      (apply error (cons string (map error-printform args))))))


;; ROUND returns an inexact integer if its argument is inexact,
;; but we think it should always return an exact integer.
;; (It matters because some Schemes print inexact integers as "+1.0".)
;; The (exact 1) test is for PC Scheme, in which nothing is exact.
(define orig-round
  (lambda (number)
    (inexact->exact (#%round number))))

;; Remainder and quotient blow up if their argument isn't an integer.
;; Unfortunately, in SCM, (* 365.25 24 60 60) *isn't* an integer.

(define -remainder
  (lambda (x y)
    (#%remainder (if (integer? x) (inexact->exact x) x)
		 (if (integer? y) (inexact->exact y) y))))

(define -quotient
  (lambda (x y)
    (#%quotient (if (integer? x) (inexact->exact x) x)
		(if (integer? y) (inexact->exact y) y))))


;;; Logo-style word/sentence implementation

(define word?
  (lambda (x)
    (or (symbol? x) (number? x) (string? x))))

(define sentence?
  (let ()
    (define (list-of-words? l)
      (cond ((null? l) #t)
	    ((pair? l)
	     (and (word? (car l)) (list-of-words? (cdr l))))
	    (else #f)))
    list-of-words?))

(define -empty?
  (lambda (x)
    (or (null? x)
	(and (string? x) (string=? x "")))))

(define char-rank
  ;; 0 Letter in good case or special initial
  ;; 1 ., + or -
  ;; 2 Digit
  ;; 3 Letter in bad case or weird character
  (let ((*the-char-ranks* (#%make-vector 256 3)))
    (define (rank-string str rank)
      (define (helper i len)
	(if (#%= i len)
	    'done
	    (begin (#%vector-set! *the-char-ranks*
				  (char->integer (string-ref str i))
				  rank)
		   (helper (#%+ i 1) len))))
      (helper 0 (string-length str)))
    (rank-string "abcdefghijklmnopqrstuvwxyz" 0)
    (rank-string "!$%&*/:<=>?~_^" 0)
    (rank-string "+-." 1)
    (rank-string "0123456789" 2)
    (lambda (char)		    ;; value of char-rank
      (#%vector-ref *the-char-ranks* (char->integer char)))))

(define string->word
  (lambda (string)
    (define (subsequents? string i length)
      (cond ((= i length) #t)
	    ((<= (char-rank (string-ref string i)) 2)
	     (subsequents? string (+ i 1) length))
	    (else #f)))
    (define (special-id? string)
      (or (string=? string "+")
	  (string=? string "-")
	  (string=? string "...")))
    (define (ok-symbol? string)
      (if (string=? string "")
	  #f
	  (let ((rank1 (char-rank (string-ref string 0))))
	    (cond ((= rank1 0) (subsequents? string 1 (string-length string)))
		  ((= rank1 1) (special-id? string))
		  (else #f)))))
    (define (nn-helper string i len seen-point?)
      (cond ((= i len)
	     (if seen-point?
		 (not (char=? (string-ref string (- len 1)) #\0))
		 #t))
	    ((char=? #\. (string-ref string i))
	     (cond (seen-point? #f)
		   ((= (+ i 2) len) #t)  ; Accepts "23.0"
		   (else (nn-helper string (+ i 1) len #t))))
	    ((= 2 (char-rank (string-ref string i)))
	     (nn-helper string (+ i 1) len seen-point?))
	    (else #f)))
    (define (narrow-number? string)
      (if (string=? string "")
	  #f
	  (let* ((c0 (string-ref string 0))
		 (start 0)
		 (len (string-length string))
		 (cn (string-ref string (- len 1))))
	    (if (and (char=? c0 #\-) (not (= len 1)))
		(begin
		  (set! start 1)
		  (set! c0 (string-ref string 1)))
		#f)
	    (cond ((not (= (char-rank cn) 2)) #f)  ; Rejects "-" among others
		  ((char=? c0 #\.) #f)
		  ((char=? c0 #\0)
		   (cond ((= len 1) #t)  ; Accepts "0" but not "-0"
			 ((= len 2) #f)  ; Rejects "-0" and "03"
			 ((char=? (string-ref string (+ start 1)) #\.)
			  (nn-helper string (+ start 2) len #t))
			 (else #f)))
		  (else (nn-helper string start len #f)))))) 

    ;; The body of string->word:
    (cond ((narrow-number? string) (string->number string))
	  ((ok-symbol? string) (string->symbol string))
	  (else string))))

(define char->word
  (lambda (char)
    (let ((rank (char-rank char))
	  (string (make-string 1 char)))
      (cond ((= rank 0) (string->symbol string))
	    ((= rank 2) (string->number string))
	    ((char=? char #\+) '+)
	    ((char=? char #\-) '-)
	    (else string)))))

(define word->string
  (lambda (wd)
    (cond ((string? wd) wd)
	  ((number? wd) (number->string wd))
	  (else (symbol->string wd)))))

(define count
  (lambda (stuff)
    (if (word? stuff)
	(string-length (word->string stuff))
	(length stuff))))

(define word
  (lambda x
    (string->word
     (apply string-append
	    (map (lambda (arg)
		   (if (word? arg)
		       (word->string arg)
		       (whoops "Invalid argument to WORD: " arg)))
		 x)))))

(define se
  (let ()
    (define (paranoid-append a original-a b)
      (cond ((null? a) b)
	    ((word? (car a))
	     (cons (car a) (paranoid-append (cdr a) original-a b)))
	    (else (whoops "Argument to SENTENCE not a word or sentence"
			  original-a ))))
    (define (combine-two a b)                ;; Note: b is always a list
      (cond ((pair? a) (paranoid-append a a b))
	    ((null? a) b)
	    ((word? a) (cons a b))
	    (else (whoops "Argument to SENTENCE not a word or sentence:" a))))
    ;; Helper function so recursive calls don't show up in TRACE
    (define (real-se args)
      (if (null? args)
	  '()
	  (combine-two (car args) (real-se (cdr args)))))
    (lambda args
      (real-se args))))

(define sentence se)

(define -first
  (let ()
    (define (word-first wd)
      (char->word (string-ref (word->string wd) 0)))
    (lambda (x)
      (cond ((pair? x) (car x))
	    ((empty? x) (whoops "Invalid argument to FIRST: " x))
	    ((word? x) (word-first x))
	    (else (whoops "Invalid argument to FIRST: " x))))))

(define last
  (let ()
    (define (word-last wd)
      (let ((s (word->string wd)))
	(char->word (string-ref s (- (string-length s) 1)))))
    (define (list-last lst)      
      (if (empty? (cdr lst))
	  (car lst)
	  (list-last (cdr lst))))
    (lambda (x)
      (cond ((pair? x) (list-last x))
	    ((empty? x) (whoops "Invalid argument to LAST: " x))
	    ((word? x) (word-last x))
	    (else (whoops "Invalid argument to LAST: " x))))))

(define bf
  (let ()
    (define string-bf
      (lambda (s)
	(substring s 1 (string-length s))))
    (define (word-bf wd)
      (string->word (string-bf (word->string wd))))
    (lambda (x)
      (cond ((pair? x) (cdr x))
	    ((empty? x) (whoops "Invalid argument to BUTFIRST: " x))
	    ((word? x) (word-bf x))
	    (else (whoops "Invalid argument to BUTFIRST: " x))))))

(define butfirst bf)

(define bl
  (let ()
    (define (list-bl list)
      (if (null? (cdr list))
	  '()
	  (cons (car list) (list-bl (cdr list)))))
    (define (string-bl s)
      (substring s 0 (- (string-length s) 1)))  
    (define (word-bl wd)
      (string->word (string-bl (word->string wd))))
    (lambda (x)
      (cond ((pair? x) (list-bl x))
	    ((empty? x) (whoops "Invalid argument to BUTLAST: " x))
	    ((word? x) (word-bl x))
	    (else (whoops "Invalid argument to BUTLAST: " x))))))

(define butlast bl)

(define orig-item
  (let ()
    (define (word-item n wd)
      (char->word (string-ref (word->string wd) (- n 1))))
    (lambda (n stuff)
      (cond ((not (integer? n))
	     (whoops "Invalid first argument to ITEM (must be an integer): "
		     n))
	    ((< n 1)
	     (whoops "Invalid first argument to ITEM (must be positive): "
		     n))
	    ((> n (count stuff))
	     (whoops "No such item: " n stuff))
	    ((word? stuff) (word-item n stuff))
	    ((list? stuff) (list-ref stuff (- n 1)))
	    (else (whoops "Invalid second argument to ITEM: " stuff))))))

(define equal?
  (let ()
    (define (vector-equal? v1 v2)
      (let ((len1 (vector-length v1))
	    (len2 (vector-length v2)))
	(define (helper i)
	  (if (= i len1)
	      #t
	      (and (equal? (vector-ref v1 i) (vector-ref v2 i))
		   (helper (+ i 1)))))
	(if (= len1 len2)
	    (helper 0)
	    #f)))
    (lambda (x y)
      (cond ((null? x) (null? y))
	    ((null? y) #f)
	    ((pair? x)
	     (and (pair? y)
		  (equal? (car x) (car y))
		  (equal? (cdr x) (cdr y))))
	    ((pair? y) #f)
	    ((symbol? x)
	     (or (and (symbol? y) (eq? x y))
		 (and (string? y) (string=? (symbol->string x) y))))
	    ((symbol? y)
	     (and (string? x) (string=? x (symbol->string y))))
	    ((number? x)
	     (or (and (number? y) (= x y))
		 (and (string? y)
		      (let ((possible-num (string->word y)))
			(and (number? possible-num)
			     (= x possible-num))))))
	    ((number? y)
	     (and (string? x)
		  (let ((possible-num (string->word x)))
		    (and (number? possible-num)
			 (= possible-num y)))))
	    ((string? x) (and (string? y) (string=? x y)))
	    ((string? y) #f)
	    ((vector? x) (and (vector? y) (vector-equal? x y)))
	    ((vector? y) #f)
	    (else (eqv? x y))))))

(define member?
  (let ()
    (define (symbol-in-list? symbol string lst)
      (cond ((null? lst) #f)
	    ((and (symbol? (car lst))
		  (eq? symbol (car lst))))
	    ((string? (car lst))
	     (cond ((not string)
		    (symbol-in-list? symbol (symbol->string symbol) lst))
		   ((string=? string (car lst)) #t)
		   (else (symbol-in-list? symbol string (cdr lst)))))
	    (else (symbol-in-list? symbol string (cdr lst)))))
    (define (word-in-list? wd lst)
      (cond ((null? lst) #f)
	    ((equal? wd (car lst)) #t)
	    (else (word-in-list? wd (cdr lst)))))
    (define (word-in-word? small big)
      (let ((one-letter-str (word->string small)))
	(if (> (string-length one-letter-str) 1)
	    (whoops "Invalid arguments to MEMBER?: " small big)
	    (let ((big-str (word->string big)))
	      (char-in-string? (string-ref one-letter-str 0)
			       big-str
			       (- (string-length big-str) 1))))))
    (define (char-in-string? char string i)
      (cond ((< i 0) #f)
	    ((char=? char (string-ref string i)) #t)
	    (else (char-in-string? char string (- i 1)))))
    (lambda (x stuff)
      (cond ((empty? stuff) #f)
	    ((word? stuff) (word-in-word? x stuff))
	    ((not (list? stuff))
	     (whoops "Invalid second argument to MEMBER?: " stuff))
	    ((symbol? x) (symbol-in-list? x #f stuff))
	    ((or (number? x) (string? x))
	     (word-in-list? x stuff))
	    (else (whoops "Invalid first argument to MEMBER?: " x))))))

(define before?
  (lambda (wd1 wd2)
    (cond ((not (word? wd1))
	   (whoops "Invalid first argument to BEFORE? (not a word): " wd1))
	  ((not (word? wd2))
	   (whoops "Invalid second argument to BEFORE? (not a word): " wd2))
	  (else (string<? (word->string wd1) (word->string wd2))))))


;;; Higher Order Functions

(define -filter
  (lambda (pred l)
    ;; Helper function so recursive calls don't show up in TRACE
    (define (real-filter l)
      (cond ((null? l) '())
	    ((pred (car l))
	     (cons (car l) (real-filter (cdr l))))
	    (else (real-filter (cdr l)))))
    (cond ((not (procedure? pred))
	   (whoops "Invalid first argument to FILTER (not a procedure): "
		   pred))
	  ((not (list? l))
	   (whoops "Invalid second argument to FILTER (not a list): " l))
	  (else (real-filter l)))))

(define keep
  (lambda (pred w-or-s)
    (define (keep-string in i out out-len len)
      (cond ((= i len) (substring out 0 out-len))
	    ((pred (char->word (string-ref in i)))
	     (string-set! out out-len (string-ref in i))
	     (keep-string in (+ i 1) out (+ out-len 1) len))
	    (else (keep-string in (+ i 1) out out-len len))))
    (define (keep-word wd)
      (let* ((string (word->string wd))
	     (len (string-length string)))
	(string->word
	 (keep-string string 0 (make-string len) 0 len))))
    (cond ((not (procedure? pred))
	   (whoops "Invalid first argument to KEEP (not a procedure): "
		   pred))
	  ((pair? w-or-s) (filter pred w-or-s))
	  ((word? w-or-s) (keep-word w-or-s))
	  ((null? w-or-s) '())
	  (else
	   (whoops "Bad second argument to KEEP (not a word or sentence): "
		   w-or-s)))))

(define appearances
  (lambda (item aggregate)
    (count (keep (lambda (element) (equal? item element)) aggregate))))

(define every
  (lambda (fn stuff)
    (define (string-every string i length)
      (if (= i length)
	  '()
	  (se (fn (char->word (string-ref string i)))
	      (string-every string (+ i 1) length))))
    (define (sent-every sent)
      ;; This proc. can't be optimized or else it will break the
      ;; exercise where we ask them to reimplement sentences as
      ;; vectors and then see if every still works.
      (if (empty? sent)
	  sent		; Can't be '() or exercise breaks.
	  (se (fn (first sent))    
	      (sent-every (bf sent)))))
    (cond ((not (procedure? fn))
	   (whoops "Invalid first argument to EVERY (not a procedure):"
		   fn))
	  ((word? stuff)
	   (let ((string (word->string stuff)))
	     (string-every string 0 (string-length string))))
	  (else (sent-every stuff)))))

(define accumulate
  (lambda (combiner stuff)
    (define (real-accumulate stuff)
      (if (empty? (bf stuff))
	  (first stuff)
	  (combiner (first stuff) (real-accumulate (bf stuff)))))
    (cond ((not (procedure? combiner))
	   (whoops "Invalid first argument to ACCUMULATE (not a procedure):"
		   combiner))
	  ((not (empty? stuff)) (real-accumulate stuff))
	  ((member combiner (list + * word se)) (combiner))
	  (else
	   (whoops "Can't accumulate empty input with that combiner")))))

(define reduce
  (lambda (combiner stuff)
    (define (real-reduce stuff)
      (if (null? (cdr stuff))
	  (car stuff)
	  (combiner (car stuff) (real-reduce (cdr stuff)))))
    (cond ((not (procedure? combiner))
	   (whoops "Invalid first argument to REDUCE (not a procedure):"
		   combiner))
	  ((not (null? stuff)) (real-reduce stuff))
	  ((member combiner (list + * word se append)) (combiner))
	  (else (whoops "Can't reduce empty input with that combiner")))))

(define orig-repeated
  (lambda (fn number)
    (if (= number 0)
	(lambda (x) x)
	(lambda (x)
	  ((orig-repeated fn (- number 1)) (fn x))))))


  ;; Tree stuff
(define make-node cons)
(define datum car)
(define children cdr)

  ;; I/O
  
(define show
  (lambda args
    (cond
     ((= (length args) 1)
      (display (car args))
      (newline))
     ((= (length args) 2)
      (if (not (output-port? (car (cdr args))))
	  (whoops "Invalid second argument to SHOW (not an output port): "
		  (car (cdr args))))
      (apply display args)
      (newline (car (cdr args))))
     (else (whoops "Incorrect number of arguments to procedure SHOW")))))

(define show-line
  (lambda (line . args)
    (if (>= (length args) 2)
	(whoops "Too many arguments to show-line")
	(let ((port (if (null? args) (current-output-port) (car args))))
	  (cond ((not (list? line))
		 (whoops "Invalid argument to SHOW-LINE (not a list):" line))
		((null? line) #f)
		(else
		 (display (car line) port)
		 (for-each (lambda (wd) (display " " port) (display wd port))
			   (cdr line))))
	  (newline port)))))

(define read-string
  (let ()
    (define (read-string-helper chars all-length chunk-length port)
      (let ((char (read-char port))
	    (string (car chars)))
	(cond ((or (eof-object? char) (eqv? char #\newline))
	       (apply string-append
		      (reverse
		       (cons
			(substring (car chars) 0 chunk-length)
			(cdr chars)))))
	      ((>= chunk-length 80)
	       (let ((newstring (make-string 80)))
		 (string-set! newstring 0 char)
		 (read-string-helper (cons newstring chars)
				     (+ all-length 1)
				     1
				     port)))
	      (else
	       (string-set! string chunk-length char)
	       (read-string-helper chars
				   (+ all-length 1)
				   (+ chunk-length 1)
				   port)))))
    (lambda args
      (if (>= (length args) 2)
	  (whoops "Too many arguments to read-string")
	  (let ((port (if (null? args) (current-input-port) (car args))))
	    (if (eof-object? (peek-char port))
		(read-char port)
		(read-string-helper (list (make-string 80)) 0 0 port)))))))

(define read-line
  (lambda args
    (define (tokenize string)
      (define (helper i start len)
	(cond ((= i len)
	       (if (= i start)
		   '()
		   (list (string->word (substring string start i)))))
	      ((char-whitespace? (string-ref string i))
	       (if (= i start)
		   (helper (+ i 1) (+ i 1) len)
		   (cons (string->word (substring string start i))
			 (helper (+ i 1) (+ i 1) len))))
	      (else (helper (+ i 1) start len))))
      (if (eof-object? string)
	  string
	  (helper 0 0 (string-length string))))
    (tokenize (apply read-string args))))

(define *the-open-inports* '())
(define *the-open-outports* '())

(define orig-align
  (lambda (obj width . rest)
    (define (align-number obj width rest)
      (let* ((sign (< obj 0))
	     (num (abs obj))
	     (prec (if (null? rest) 0 (car rest)))
	     (big (round (* num (expt 10 prec))))
	     (cvt0 (number->string big))
	     (cvt (if (< num 1) (string-append "0" cvt0) cvt0))
	     (pos-str (if (>= (string-length cvt0) prec)
			  cvt
			  (string-append
			   (make-string (- prec (string-length cvt0)) #\0)
			   cvt)))
	     (string (if sign (string-append "-" pos-str) pos-str))
	     (length (+ (string-length string)
			(if (= prec 0) 0 1)))
	     (left (- length (+ 1 prec)))
	     (result (if (= prec 0)
			 string
			 (string-append
			  (substring string 0 left)
			  "."
			  (substring string left (- length 1))))))
	(cond ((= length width) result)
	      ((< length width)
	       (string-append (make-string (- width length) #\space) result))
	      (else (let ((new (substring result 0 width)))
		      (string-set! new (- width 1) #\+)
		      new)))))
    (define (align-word string)
      (let ((length (string-length string)))
	(cond ((= length width) string)
	      ((< length width)
	       (string-append string (make-string (- width length) #\space)))
	      (else (let ((new (substring string 0 width)))
		      (string-set! new (- width 1) #\+)
		      new)))))
    (if (number? obj)
	(align-number obj width rest)
	(align-word (word->string obj)))))

(define open-output-file
  (lambda (filename)
    (let ((port (#%open-output-file filename)))
      (set! *the-open-outports* (cons port *the-open-outports*))
      port)))

(define open-input-file
  (lambda (filename)
    (let ((port (#%open-input-file filename)))
      (set! *the-open-inports* (cons port *the-open-inports*))
      port)))

(define remove!
  (lambda (thing lst)
    (define (r! prev)
      (cond ((null? (cdr prev)) lst)
	    ((eq? thing (car (cdr prev)))
	     (set-cdr! prev (cdr (cdr prev)))
	     lst)
	    (else (r! (cdr prev)))))
    (cond ((null? lst) lst)
	  ((eq? thing (car lst)) (cdr lst))
	  (else (r! lst)))))

(define close-input-port
  (lambda (port)
    (set! *the-open-inports* (remove! port *the-open-inports*))
    (#%close-input-port port)))

(define close-output-port
  (lambda (port)
    (set! *the-open-outports* (remove! port *the-open-outports*))
    (#%close-output-port port)))

(define close-all-ports
  (lambda ()
    (for-each close-input-port *the-open-inports*)
    (for-each close-output-port *the-open-outports*)
    'closed))

;; Make arithmetic work on numbers in string form:
(define maybe-num
  (lambda (arg)
    (if (string? arg)
	(let ((num (string->number arg)))
	  (if num num arg))
	arg)))

(define logoize
  (lambda (fn)
    (lambda args
      (apply fn (map maybe-num args)))))

;; special case versions of logoize, since (lambda args ...) is expensive
(define logoize-1
  (lambda (fn)
    (lambda (x) (fn (maybe-num x)))))

(define logoize-2
  (lambda (fn)
    (lambda (x y) (fn (maybe-num x) (maybe-num y)))))

(define internal-* void)
(define internal-+ void)
(define internal-- void)
(define internal-/ void)
(define internal-< void)
(define internal-<= void)
(define internal-= void)
(define internal-> void)
(define internal->= void)
(define internal-abs void)
(define internal-acos void)
(define internal-asin void)
(define internal-atan void)
(define internal-ceiling void)
(define internal-cos void)
(define internal-even? void)
(define internal-exp void)
(define internal-expt void)
(define internal-floor void)
(define internal-align void)
(define internal-gcd void)
(define internal-integer? void)
(define internal-item void)
(define internal-lcm void)
(define internal-list-ref void)
(define internal-log void)
(define internal-make-vector void)
(define internal-max void)
(define internal-min void)
(define internal-modulo void)
(define internal-negative? void)
(define internal-number? void)
(define internal-odd? void)
(define internal-positive? void)
(define internal-quotient void)
(define internal-random void)
(define internal-remainder void)
(define internal-repeated void)
(define internal-round void)
(define internal-sin void)
(define internal-sqrt void)
(define internal-tan void)
(define internal-truncate void)
(define internal-vector-ref void)
(define internal-vector-set! void)
(define internal-zero? void)

(define (* . args) (apply internal-* args))
(define (+ . args) (apply internal-+ args))
(define (- . args) (apply internal-- args))
(define (/ . args) (apply internal-/ args))
(define (< . args) (apply internal-< args))
(define (<= . args) (apply internal-<= args))
(define (= . args) (apply internal-= args))
(define (> . args) (apply internal-> args))
(define (>= . args) (apply internal->= args))
(define (abs . args) (apply internal-abs args))
(define (acos . args) (apply internal-acos args))
(define (asin . args) (apply internal-asin args))
(define (atan . args) (apply internal-atan args))
(define (ceiling . args) (apply internal-ceiling args))
(define (cos . args) (apply internal-cos args))
(define (even? . args) (apply internal-even? args))
(define (exp . args) (apply internal-exp args))
(define (expt . args) (apply internal-expt args))
(define (floor . args) (apply internal-floor args))
(define (align . args) (apply internal-align args))
(define (gcd . args) (apply internal-gcd args))
(define (integer? . args) (apply internal-integer? args))
(define (item . args) (apply internal-item args))
(define (lcm . args) (apply internal-lcm args))
(define (list-ref . args) (apply internal-list-ref args))
(define (log . args) (apply internal-log args))
(define (make-vector . args) (apply internal-make-vector args))
(define (max . args) (apply internal-max args))
(define (min . args) (apply internal-min args))
(define (modulo . args) (apply internal-modulo args))
(define (negative? . args) (apply internal-negative? args))
(define (number? . args) (apply internal-number? args))
(define (odd? . args) (apply internal-odd? args))
(define (positive? . args) (apply internal-positive? args))
(define (quotient . args) (apply internal-quotient args))
(define (random . args) (apply internal-random args))
(define (remainder . args) (apply internal-remainder args))
(define (repeated . args) (apply internal-repeated args))
(define (round . args) (apply internal-round args))
(define (sin . args) (apply internal-sin args))
(define (sqrt . args) (apply internal-sqrt args))
(define (tan . args) (apply internal-tan args))
(define (truncate . args) (apply internal-truncate args))
(define (vector-ref . args) (apply internal-vector-ref args))
(define (vector-set! . args) (apply internal-vector-set! args))
(define (zero? . args) (apply zero? args))

(define are-they? #f)

(define strings-are-numbers
  (lambda (yesno)
    (cond ((and are-they? (eq? yesno #t))
	   (show "Strings are already numbers"))
	  ((eq? yesno #t)
	   (set! are-they? #t)
	   (set! internal-* (logoize #%*))
	   (set! internal-+ (logoize #%+))
	   (set! internal-- (logoize #%-))
	   (set! internal-/ (logoize #%/))
	   (set! internal-< (logoize #%<))
	   (set! internal-<= (logoize #%<=))
	   (set! internal-= (logoize #%=))
	   (set! internal-> (logoize #%>))
	   (set! internal->= (logoize #%>=))
	   (set! internal-abs (logoize-1 #%abs))
	   (set! internal-acos (logoize-1 #%acos))
	   (set! internal-asin (logoize-1 #%asin))
	   (set! internal-atan (logoize #%atan))
	   (set! internal-ceiling (logoize-1 #%ceiling))
	   (set! internal-cos (logoize-1 #%cos))
	   (set! internal-even? (logoize-1 #%even?))
	   (set! internal-exp (logoize-1 #%exp))
	   (set! internal-expt (logoize-2 #%expt))
	   (set! internal-floor (logoize-1 #%floor))
	   (set! internal-align (logoize orig-align))
	   (set! internal-gcd (logoize #%gcd))
	   (set! internal-integer? (logoize-1 #%integer?))
	   (set! internal-item (lambda (n stuff)
				 (orig-item (maybe-num n) stuff)))
	   (set! internal-lcm (logoize #%lcm))
	   (set! internal-list-ref (lambda (lst k) 
				     (#%list-ref lst (maybe-num k))))
	   (set! internal-log (logoize-1 #%log))
	   (set! internal-max (logoize #%max))
	   (set! internal-min (logoize #%min))
	   (set! internal-modulo (logoize-2 #%modulo))
	   (set! internal-negative? (logoize-1 #%negative?))
	   (set! internal-number? (logoize-1 #%number?))
	   (set! internal-odd? (logoize-1 #%odd?))
	   (set! internal-positive? (logoize-1 #%positive?))
	   (set! internal-quotient (logoize-2 #%quotient))
	   (set! internal-random (logoize #%random))
	   (set! internal-remainder (logoize-2 #%remainder))
	   (set! internal-round (logoize-1 orig-round))
	   (set! internal-sin (logoize-1 #%sin))
	   (set! internal-sqrt (logoize-1 #%sqrt))

	   (set! internal-tan (logoize-1 #%tan))
	   (set! internal-truncate (logoize-1 #%truncate))
	   (set! internal-zero? (logoize-1 #%zero?))
	   (set! internal-vector-ref
		 (lambda (vec i) (#%vector-ref vec (maybe-num i))))
	   (set! internal-vector-set!
		 (lambda (vec i val)
		   (#%vector-set! vec (maybe-num i) val)))
	   (set! internal-make-vector
		 (lambda (num . args)
		   (apply #%make-vector (cons (maybe-num num)
					      args))))
	   (set! internal-list-ref
		 (lambda (lst i) (#%list-ref lst (maybe-num i))))
	   (set! internal-repeated
		 (lambda (fn n) (orig-repeated fn (maybe-num n)))))
	  ((and (not are-they?) (not yesno))
	   (show "Strings are already not numbers"))
	  ((not yesno)
	   (set! are-they? #f)
	   (set! internal-* #%*) (set! internal-+ #%+)
	   (set! internal-- #%-) (set! internal-/ #%/) (set! internal-< #%<)
	   (set! internal-<= #%<=) (set! internal-= #%=) (set! internal-> #%>)
	   (set! internal->= #%>=) (set! internal-abs #%abs) (set! internal-acos #%acos)
	   (set! internal-asin #%asin) (set! internal-atan #%atan)
	   (set! internal-ceiling #%ceiling) (set! internal-cos #%cos)
	   (set! internal-even? #%even?)
	   (set! internal-exp #%exp) (set! internal-expt #%expt)
	   (set! internal-floor #%floor) (set! internal-align orig-align)
	   (set! internal-gcd #%gcd) (set! internal-integer? #%integer?)
	   (set! internal-item orig-item)
	   (set! internal-lcm #%lcm) (set! internal-list-ref #%list-ref)
	   (set! internal-log #%log) (set! internal-max #%max) (set! internal-min #%min)
	   (set! internal-modulo #%modulo) (set! internal-odd? #%odd?)
	   (set! internal-quotient #%quotient) (set! internal-random #%random)
	   (set! internal-remainder #%remainder) (set! internal-round orig-round)
	   (set! internal-sin #%sin) (set! internal-sqrt #%sqrt) (set! internal-tan #%tan)
	   (set! internal-truncate #%truncate) (set! internal-zero? #%zero?)
	   (set! internal-positive? #%positive?) (set! internal-negative? #%negative?)
	   (set! internal-number? #%number?) (set! internal-vector-ref #%vector-ref)
	   (set! internal-vector-set! #%vector-set!)
	   (set! internal-make-vector #%make-vector)
	   (set! internal-list-ref #%list-ref) (set! internal-item orig-item)
	   (set! internal-repeated orig-repeated))
	  (else (whoops "Strings-are-numbers: give a #t or a #f")))
    are-they?))

;; By default, strings are numbers:
(strings-are-numbers #t))