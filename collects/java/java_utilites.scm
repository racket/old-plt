;; Mario Latendresse, 18 May 2000
;;
;; Several utility functions copied from my personnal library lib.scm

;; Redefinition of some basic primitive to have shorter names.

(define : cons)
(define :~ assoc)
(define (.cdr x) (if (pair? x) (cdr x) #f))

(define third  caddr)
(define fourth cadddr)

;; compose
(define (c* f g)  (lambda (x) (f (g x))))

;; Fix the first argument
(define (c.1 f x) (lambda l (apply f `(,x ,@l))))

;; Fix the second argument
(define (c.2 f x) (lambda l (apply f `(,(car l) ,x ,@(cdr l)))))

(define (:~. x y) (.cdr (:~ x y)))

(define (display-ln . l) (for-each display l)(newline))
(define (display-l . l)  (for-each display l))
(define (write-ln . l)   (for-each write l)  (newline))
(define (write-l . l)    (for-each write x l))

(define (:~-s e l s)
  (let loop ((l l))
    (if (pair? l)
	(if (equal? e (s (car l)))
	    (car l)
	    (loop (cdr l)))
	#f)))

(define (:~-equ-s equ e l s)
  (let loop ((l l))
    (if (pair? l)
	(let ((equivalent (equ e (s (car l)))))
	  (if equivalent
	      (cons equivalent (cdr (car l)))
	      (loop (cdr l))))
	#f)))

;; Return first element of l for which p is true.
(define (:~-p p l)
  (let loop ((l l))
    (if (pair? l)
	(if (p (car l))
	    (car l)
	    (loop (cdr l)))
	#f)))

;; I: p, a function of n arguments.
;;    ls, a list of n>0 lists of same length.
;; O: #t iff for all (p a1 ... an). 
(define (all p . ls)
  (if (pair? (car ls))
      (and (apply p (map car ls)) (apply all p (map cdr ls)))
      #t))

(define (included? l1 l2) (all (lambda (x) (member x l2)) l1))

(define (duplicates? l) 
  (let loop ((l l))
    (if (pair? l)
	(if (member (car l) (cdr l))
	    #t
	    (loop (cdr l)))
	#f
	)))

;; Return a list of duplicate elements in l.
(define (duplicates l)
  (if (pair? l)
      (if (member (car l) (cdr l))
	  (cons (car l) (duplicates (cdr l)))
	  (duplicates (cdr l))) 
      '()))


(define (foldr f a l)
  (if (pair? l)
      (f (car l) (foldr f a (cdr l)))
      a))
(define (foldl f a l)
  (if (pair? l)
      (foldl f (f a (car l)) (cdr l))
      a))

;; starting at position i
(define (foldli f a l i)
  (if (pair? l)
      (foldli f (f a (car l) i) (cdr l) (+ i 1))
      a))

(define (foldrp f a l p)
  (if (and (pair? l) (p (car l)))
      (f (car l) (foldrp f a (cdr l) p))
      a))

(define (list-last l)
  (if (pair? l)
      (list-ref l (- (length l) 1))
      l))

(define (list-but-last l)
  (if (pair? l)
      (reverse (cdr (reverse l)))
      l))

;; I: lex, (s lexeme)
;; O: (s n-src)
(define (ls->ns lex)
  (make-n-src (lexeme-line lex) (lexeme-col lex)))

(define (ns->ns node)
  (if (pair? node)
      (list-last node)
      (cond
       ((n-qName? node) (n-qName-src node))
       ((n-typeSpecifier? node) (n-typeSpecifier-src node))
       ((n-arrayAlloc? node) (n-arrayAlloc-src node))
       ((n-classAlloc? node) (n-classAlloc-src node))
       (else (error "ns->ns, unknown node "))
       )))

(define (intersect? e1 e2)
  (or (and (pair? e1)
	   (or (member (car e1) e2) (intersect? (cdr e1) e2)))))
    
(define (intersection e1 e2)
  (foldr (lambda (x y) (if (member x e2) (: x y) y)) '() e1))

(define (union e1 e2)
  (let loop ((l e1) (r e2))
    (if (pair? l)
	(if (member (car l) r)
	    (loop (cdr l) r)
	    (loop (cdr l) (cons (car l) r)))
	r)))

(define (list-string-append l) (foldr (lambda (x y) (++ x y)) "" l))

;; I: s, str.
;;    l, int. Length of suffix.
;; O: str.
(define (string-suffix s l) 
  (substring s (max 0 (- (string-length s) l)) (string-length s)))

;; Remove last char from string.
;; I: s, str
;; O: str.
(define (remove-suffix s)  (substring s 0 (- (string-length s) 1)))

;; Removes the first two charaters of an hex constant.
;; I: s, str.
;; O: str.
(define (remove-hex-prefix s)  (substring s 2 (string-length s)))

(define (string-bin-int->number s b)
  (let* ((n (string->number s b)))
    (if (and (<= n 4294967295) (> n 2147483647))
	;; It is in fact negative
	(- n 4294967296)
	n
	)))

(define (string-bin-long->number s b)
  (let* ((n (string->number s b)))
    (if (and (<= n 18446744073709551615) (> n 9223372036854775807))
	;; It is in fact negative
	(- n 18446744073709551616)
	n
	)))

(define (for-eachi p l i)
  (let loop ((l l) (i i))
    (if (pair? l)
	(begin
	  (p (car l) i)
	  (loop (cdr l) (+ i 1))))))

(define (p-exists? p l)
  (if (pair? l)
      (or (p (car l)) (p-exists? p (cdr l)))
      #f))

(define (exists? l)
  (if (pair? l)
      (or (car l) (exists? (cdr l)))
      #f))

(define (elem-all-eqv e l) (filter (lambda (x) (not (eqv? e x))) l))

;; Eliminate duplicates in the list l.
(define (set l)
  (let loop ((l l) (r '()))
    (if (pair? l)
	(if (member (car l) r)
	    (loop (cdr l) r)
	    (loop (cdr l) (cons (car l) r)))
	(reverse r)
	)))

;; (list-append '((1 2 3) (a b c) (d))) = (1 2 3 a b c d)
;;
(define (list-append l) (foldr (lambda (e L) (append e L)) '() l))

(define (my-length l)
  (if (not (pair? l))
      1
      (length l)))

;; Always return a list.
(define (my-append l1 l2)
  (cond
   ((and (pair? l1) (pair? l2))
    (append l1 l2))
   ((and (pair? l1) (not (pair? l2)))
    (append l1 (list l2)))
   ((and (not (pair? l1)) (pair? l2))
    (cons l1 l2))
   (else (list l1 l2))))
      
(define (list-scan f e l)
  (let loop ((l l) (r (list e)) (d e))
    (if (pair? l)
	(let ((nd (f d (car l))))
	  (loop (cdr l) (cons nd r) nd)) 
	(reverse r))))

;; (list-scan2 + 0 '(1 2 3)) = (0 1 3)
(define (list-scan2 f e l) (list-head (list-scan f e l) (length l)))

(define (list-sum l) (foldr + 0 l))

(define (make-list e l)
  (let loop ((i 0) (r '()))
    (if (< i l)
	(loop (+ i 1) (cons e r))
	r)))

;; v is positive.
(define (complement2 v nb-bits)
  (if (= 0 v) 0 (- (expt 2 nb-bits) v)))

;; I: op, (u '& 'or '^)
;;    v1, v2. integer values
;;    n>0. Number of bits of v1, v2, and the result.
;; O: integer. The binary operator `op' between v1 and v2.
;;             This value may be negative.
(define (binary-op op v1 v2 n)
  (define (op-or a b)  (or (odd? a) (odd? b)))
  (define (op-and a b) (and (odd? a) (odd? b)))
  (define (op-xor a b) (not (eqv? (odd? a) (odd? b))))
  (define (op-op) (cdr (:~ op `((or . ,op-or) 
				(& . ,op-and)
				(^ . ,op-xor)))))
  (let* ((v1p (if (< v1 0) (complement2 (- v1) n) v1))
	 (v2p (if (< v2 0) (complement2 (- v2) n) v2)))
    (let loop ((v1p v1p) (v2p v2p) (i 0) (r '()))
      (if (< i n)
	  (loop (quotient v1p 2) (quotient v2p 2) (+ i 1)
		(cons (if ((op-op) v1p v2p) #\1 #\0) r))
	  (if (char=? #\1 (car r))
	      (- (complement2 (string->number (list->string r) 2)
			      n))
	      (string->number (list->string r) 2))
	  ))))

(define (increasing-sort l . s) 
  (define (partitionne l e)
    (let loop ((l l) (gauche '()) (droite '()))
      (if (pair? l)
	  (if (< (if (pair? s)
		     ((car s) (car l))
		     (car l))
		 (if (pair? s)
		     ((car s) e)
		     e
		     ))
	      (loop (cdr l) (cons (car l) gauche) droite)
	      (loop (cdr l) gauche (cons (car l) droite)))
	  (cons gauche droite)
	  )))

  (if (pair? l)
      (let* ((partitionnee (partitionne (cdr l) (car l)))
	     (gauche       (car partitionnee))
	     (droite       (cdr partitionnee)))
	(append (if (pair? s) 
		    (increasing-sort gauche (car s))
		    (increasing-sort gauche))
		(cons (car l) (if (pair? s)
				  (increasing-sort droite (car s))
				  (increasing-sort droite)
				  ))))
      l))

(define 2^32 (expt 2 32))
(define 2^16 (expt 2 16))
(define 2^20 (expt 2 20))
(define 2^31 (expt 2 31))
(define 2^24 (expt 2 24))
(define 2^23 (expt 2 23))
(define 2^52 (expt 2 52))
(define 2^63 (expt 2 63))

(define log_2     (log 2))
(define (log2 x)  (/ (log x) log_2))

;; Return an integer (32 bits) representing the float value in IEEE.
;; Mantissa has 23 bits, exponent 8 bits, 1 bit sign.
;; TBF: -infinite or +infinite, NANs, Denormalized.
;; O: a positive integer value representable on 32 bits.
(define (float->ieee32bits v)
  (if (= v 0)  0
      (let* ((vp   (abs v))
	     (s    (if (< v 0) 2^31 0))
	     (e    (min 127 (max -126 (log2 vp))))
	     (ei   (inexact->exact (floor e)))
	     (ef   (- e ei))
	     (m    (inexact->exact (round (* (- (expt 2 ef) 1) 2^23)))))

	(+ s (* (+ 127 ei) 2^23) m))))

;; Return an integer (64 bits) representing in IEEE the value v.
;; Mantissa has 52 bits, exponent 11 bits, 1 bit sign.
;; Infinites and Denormalized TBF.
;; There may be imprecision errors due to the underlying 
;; implementation.
;; O: a positive integer value representable on 64 bits.
(define (double->ieee64bits v)
  (if (= v 0)
      0
      (let* ((vp   (abs v))
	     (s    (if (< v 0) 2^63 0))
	     (e    (min 1023 (max -1022 (log2 vp))))
	     (ei   (inexact->exact (floor e)))
	     (ef   (- e ei))
	     (m    (max 0 (inexact->exact (round (* (- (expt 2 ef) 1) 2^52))))))
	(+ s (* (+ 1023 ei) 2^52) m))))

;; Returns a couple (high low) representing the 64 bits value v.
(define (long->highLow long)
  (let* ((high (quotient  long 2^32))
	 (low  (remainder long 2^32)))
    `(,high ,low)))

;; The couple high low represents a 64 bits IEEE double
;; number. Returns the value of that number.
;; Does not handle NAN, denormalizes and infinities. TBF
(define (highLow->double highLow)
  (let* ((high   (car highLow))
	 (low    (cadr highLow))
	 (sign   (if (>= high 2^32) -1.0 1.0))
	 (high2  (if (>= high 2^32) (- high 2^32) high)) 
	 (ebias  (quotient high2 2^20))
	 (ereal  (- ebias 1023))
	 (high3  (remainder high2 2^20))
	 (mantissa (+ (* high3 2^32) low)))
    (if (= ereal 0)
	0.0
	(if (>= ereal 52)
	    (if (< sign 0) 
		(- (* (+ mantissa 2^52) (expt 2 (- ereal 52))))
		(* (+ mantissa 2^52) (expt 2 (- ereal 52))))
	    (* sign (+ (expt 2 ereal) 
		       (/ high3 (expt 2 (- 20 ereal)))
		       (/ low (expt 2 (- 52 ereal)))))))
    ))

;; The high low represents a 64 bits integer. Returns the value of it.
(define (highLow->long high low)  (+ (* high 2^32) low))

(define (mapi p l)
  (let loop ((l l) (i 0) (r '()))
    (if (pair? l)
	(loop (cdr l) (+ i 1) (cons (p (car l) i) r))
	(reverse r))))

;; Returns bigendian two's complement byte codification of the integer
;; value of v. Generates at least n bytes, and at most m bytes,
;; representing value v in two's complement. The bytes are not
;; negative. If value v is positive, the first byte is < 128, if
;; negative the first byte is >= 128 unless m is too small, in which
;; case the list of bytes is truncated.
;;
;; I: v, int. n, int.  
;; O: [byte]
(define (int->bytes v n m)
  (if (= m 0)
      '()
      (if (= v 0)
	  (make-list 0 n)
	  (if (< v 0)
	      (let* ((r (bytes-complement2 (int->bytes (- v) n m))))
		(if (and (< (car r) 128) (< (length r) m))
		    (cons 255 r)
		    r))
	      (let* ((r (append 
			 (int->bytes (quotient v 256) (- n 1) (- m 1))
			 (list (remainder v 256)))))
		(if (and (>= (car r) 128) (< (length r) m))
		    (cons 0 r)
		    r))))))

(define (bytes-complement1 l)  (map (lambda (v) (- 255 v)) l))

;; I: l, [byte]. m, int. l is at most of length m.
;;    l is encoding a positive value. It is not empty.
;; O: [byte] at most of length m.
;;    The list always starts with a value >= 128.
;;
(define (bytes-complement2 l)
  (let loop ((l2 (reverse (bytes-complement1 l))) 
	     (r  '()) 
	     (carry 1))
    (if (pair? l2)
	(loop (cdr l2)
	      (cons (remainder (+ carry (car l2)) 256)
		    r)
	      (quotient (+ carry (car l2)) 256))
	r)))

;; I: l, [byte]. Not empty.
;; O: int.
(define (bytes->int l)

  (define (bytes-positive->int l)
    (if (pair? l)
	(+ (car l) (* (bytes-positive->int (cdr l)) 256))
	0))

  (if (< (car l) 128)
      ;; It is positive
      (bytes-positive->int (reverse l))
      (- (bytes-positive->int (reverse (bytes-complement2 l))))))
  
(define (remove-ends s)  (substring s 1 (- (string-length s) 1)))

(define (isOctalDigit? c) (and (char<=? c #\7) (char>=? c #\0)))
(define (isTetraDigit? c) (and (char<=? c #\3) (char>=? c #\0)))

;; 3.10.6 (This does not handle \uxxxx of 3.3)
;; Convert the string s, replacing all escape characters
;; by their ASCII values.
(define (javaConstString->str s)
  (let loop ((l (string->list s)) (r '()))
    (if (pair? l)
	(let* ((c-l (javaChar->char l)))
	  (loop (car c-l) (cons (cadr c-l) r)))
	(list->string (reverse r)))))

;; I: l, [char]
;; O: (rest char)
;;     rest is the tail of unprocessed l. 
;;     char is the resulting character seen at the head of l.
(define (javaChar->char l)
  (if (char=? #\\ (car l))
      (if (pair? (cdr l))
	  ;; The lexer verified  that in the case of three octal digits
	  ;; the first one is in the range [0,3].
	  (if (or (isOctalDigit? (cadr l)) (isTetraDigit? (cadr l)))
	      (if (and (pair? (cddr l)) (isOctalDigit? (caddr l)))
		  (if (and (pair? (cdddr l)) (isOctalDigit? (cadddr l)))
		      `(,(cddddr l) 
			,(integer->char (octal->v (cadr l) (caddr l) (cadddr l))))
		      `(,(cdddr l) 
			,(integer->char (octal->v #f (cadr l) (caddr l)))))
		  `(,(cddr l)
		    ,(integer->char (octal->v #f #f (cadr l)))))
	      (case (cadr l)
		;; These are all non-octal escape characters
		((#\b) `(,(cddr l) ,(integer->char 8)))
		((#\t) `(,(cddr l) ,(integer->char 9)))
		((#\n) `(,(cddr l) ,(integer->char 10)))
		((#\f) `(,(cddr l) ,(integer->char 12)))
		((#\r) `(,(cddr l) ,(integer->char 13)))
		((#\") `(,(cddr l) ,(integer->char 34)))
		((#\') `(,(cddr l) ,(integer->char 39)))
		((#\\) `(,(cddr l) ,(integer->char 92)))
		(else `(,(cdr l) ,(car l)))))
	  `(,(cdr l) ,(car l)))
      `(,(cdr l) ,(car l))))

;; I: s, str. This the lexeme as read by the lexer without the quotes.
;; O: int.
(define (javaCharConst->char s)
  (char->integer (cadr (javaChar->char (string->list s)))))

;; O: [0,255]
(define (octal->v o1 o2 o3)
  (define (c->v c) (if c (-  (char->integer c) (char->integer #\0)) 0))
    (max 0 (min (+ (* 64 (c->v o1)) (* 8 (c->v o2)) (c->v o3)) 255)))

;; String-append s1, s2 and s3 only if s1 is not the null string.
(define (++C s1 s2 s3) (if (string=? s1 "") s3 (++ s1 s2 s3)))

(define (++del l s)
  (if (string? l)  l
      (foldl (lambda (x y) (if (string=? x "") y (++ x s y))) ""  l)))

(define (++ . l)

  (define sta string-append)
  (define (++r l)
    (if (pair? l)
	(cond ((string? (car l)) (sta (car l) (++r (cdr l))))
	      ((number? (car l)) (sta (number->string (car l)) (++r (cdr l))))
	      ((symbol? (car l)) (sta (symbol->string (car l)) (++r (cdr l))))
	      ((pair? (car l))   (sta (++r (car l)) (++r (cdr l))))
	      ((null? (car l))   "")
	      (else (error "++, unknown type " (car l))))
	""))
  (++r l))


(define (list-head l n)
  (let loop ((l l) (n n) (r '()))
    (if (and (pair? l) (> n 0))
	(loop (cdr l) (- n 1) (cons (car l) r))
	(reverse r))))

(define (list-head-p l p)
  (let loop ((l l) (r '()))
    (if (pair? l)
	(if (p (car l))
	    (reverse r)
	    (loop (cdr l) (cons (car l) r)))
	(reverse r))))

;; Multiple assoc.
(define (:~-multiple c l)
  (let loop ((l l) (r '()))
    (if (pair? l)
	(if (equal? (caar l) c)
	    (loop (cdr l) (: (car l) r))
	    (loop (cdr l) r))
	(reverse r))))

(define (all-true ls)
  (if (pair? ls)
      (and (car ls) (all-true (cdr ls)))
      #t))

(define (prefix=? p l2)
  (and (<= (length p) (length l2)) (equal? p (list-head l2 (length p)))))

(define (string-prefix? prefix s)
  (and (>= (string-length s) (string-length prefix))
       (string=? (string-prefix s (string-length prefix)) prefix)))

(define (string-prefix s n) (substring s 0 (min (string-length s) n)))

;; Breaks string s based on c.  "abc/def/g/h/" -> ("abc" "def" "g" "h" "")
;; O: [str]
(define (string-breaks s c)
  (let* ((l (string->list s)))
    (let loop ((l l) (dir '()) (r '()))
      (if (pair? l)
	  (if (char=? c (car l))
	      (loop (cdr l) '() (: (list->string (reverse dir)) r))
	      (loop (cdr l) (: (car l) dir) r))
	  (reverse (if (null? dir)
		       r
		       (: (list->string (reverse dir)) r))))
      )))


(define (show d) (display d) d)

;; These two functions are used for debugging.
(define (trace! . l)
  (apply display-ln l)
  (list-last l))

(define (trace . l)  #t)



