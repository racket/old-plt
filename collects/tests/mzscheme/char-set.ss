
(load-relative "loadtest.ss")

(SECTION 'char-set/SRFI-14)

(require (lib "char-set.ss" "srfi" "14"))

;; NOTE: tests assume that ! functions are actually functional

;; Check char sets ----------------------------------------

(test #t char-set-contains? char-set:lower-case #\a)
(test #f char-set-contains? char-set:lower-case #\A)
(test #t char-set-contains? char-set:lower-case #\u00E0)
(test #f char-set-contains? char-set:lower-case #\u00C2)
(test #t char-set-contains? char-set:lower-case #\u00B5)

(test #t char-set-contains? char-set:upper-case #\A)
(test #f char-set-contains? char-set:upper-case #\a)
(test #t char-set-contains? char-set:upper-case #\u00C2)
(test #f char-set-contains? char-set:upper-case #\u00E0)

(test #t char-set-contains? char-set:title-case #\u01C5)
(test #t char-set-contains? char-set:title-case #\u1FA8)
(test #f char-set-contains? char-set:title-case #\a)
(test #f char-set-contains? char-set:title-case #\A)

(test #t char-set-contains? char-set:letter #\a)
(test #t char-set-contains? char-set:letter #\A)
(test #f char-set-contains? char-set:letter #\1)
(test #t char-set-contains? char-set:letter #\u00AA)
(test #t char-set-contains? char-set:letter #\u00BA)

(test #f char-set-every (lambda (c) (char-set-contains? char-set:lower-case c)) char-set:letter)
(test #t char-set-any (lambda (c) (char-set-contains? char-set:lower-case c)) char-set:letter)
(test #f char-set-every (lambda (c) (char-set-contains? char-set:upper-case c)) char-set:letter)
(test #t char-set-any (lambda (c) (char-set-contains? char-set:upper-case c)) char-set:letter)
;; Not true?
;; (test #t char-set<= char-set:letter (char-set-union char-set:lower-case char-set:upper-case char-set:title-case))

(test #t char-set-contains? char-set:digit #\1)
(test #f char-set-contains? char-set:digit #\a)

(test #t char-set-contains? char-set:hex-digit #\1)
(test #t char-set-contains? char-set:hex-digit #\a)
(test #t char-set-contains? char-set:hex-digit #\A)
(test #f char-set-contains? char-set:hex-digit #\g)

(test #t char-set-contains? char-set:letter+digit #\1)
(test #t char-set-contains? char-set:letter+digit #\a)
(test #t char-set-contains? char-set:letter+digit #\z)
(test #t char-set-contains? char-set:letter+digit #\A)
(test #t char-set-contains? char-set:letter+digit #\Z)

(test 90547 char-set-size char-set:letter)

(test #t char-set= char-set:letter+digit (char-set-union char-set:letter char-set:digit))
;; Slow!:
;;  (test #t char-set-every (lambda (c) (char-set-contains? char-set:letter+digit c)) char-set:letter)
;; Slow!:
;;  (test #t char-set-every (lambda (c) (char-set-contains? char-set:letter+digit c)) char-set:digit)
(test #f char-set-every (lambda (c) (char-set-contains? char-set:letter c)) char-set:letter+digit)
(test #f char-set-every (lambda (c) (char-set-contains? char-set:digit c)) char-set:letter+digit)
(test #t char-set-any (lambda (c) (char-set-contains? char-set:letter c)) char-set:letter+digit)

(define char-set:latin-1 (ucs-range->char-set 0 256))

(test #t char-set= 
      (char-set-intersection char-set:graphic char-set:latin-1)
      (char-set-intersection (char-set-union char-set:letter char-set:digit char-set:punctuation char-set:symbol)
			     char-set:latin-1))

(test #t char-set= char-set:printing (char-set-union char-set:graphic char-set:whitespace))

(test #t char-set-contains? char-set:whitespace #\u0009)
(test #t char-set-contains? char-set:whitespace #\u000D)
(test #f char-set-contains? char-set:whitespace #\a)

(test #t char-set= char-set:iso-control 
      (char-set-union (ucs-range->char-set #x0000 #x0020)
		      (ucs-range->char-set #x007F #x00A0)))

(test #t char-set-contains? char-set:punctuation #\!)
(test #t char-set-contains? char-set:punctuation #\u00A1)
(test #f char-set-contains? char-set:punctuation #\a)

(test #t char-set-contains? char-set:symbol #\$)
(test #t char-set-contains? char-set:symbol #\u00A2)
(test #f char-set-contains? char-set:symbol #\a)

(test #t char-set-contains?  char-set:blank #\space)
(test #t char-set-contains?  char-set:blank #\u3000)
(test #f char-set-contains?  char-set:blank #\a)

;; General procedures ----------------------------------------

(test #t char-set= char-set:letter char-set:letter char-set:letter)
(test #f char-set= char-set:letter char-set:digit)
(test #f char-set= char-set:letter char-set:letter char-set:digit)
(test #f char-set= char-set:letter char-set:digit char-set:letter)

(test #t char-set<= char-set:graphic char-set:printing)
(test #f char-set<= char-set:printing char-set:graphic)
(test #t char-set<= char-set:graphic char-set:printing char-set:full)
(test #f char-set<= char-set:graphic char-set:full char-set:printing)
  
(test (char-set-hash char-set:graphic) char-set-hash char-set:graphic)

;; Iterating over character sets ----------------------------------------

(test 612 char-set-size char-set:digit)

(test #t char-set=
      char-set:digit
      (list->char-set
       (let loop ([c (char-set-cursor char-set:digit)][l null])
	 (if (end-of-char-set? c)
	     l
	     (loop (char-set-cursor-next char-set:digit c)
		   (cons (char-set-ref char-set:digit c)
			 l))))))

(test #t char-set=
      (ucs-range->char-set 10 20)
      (char-set-unfold integer->char (lambda (x) (= x 20)) add1 10))
(test #t char-set=
      (ucs-range->char-set 10 21)
      (char-set-unfold integer->char (lambda (x) (= x 20)) add1 10 (char-set #\u14)))
(test #t char-set=
      (ucs-range->char-set 10 20)
      (char-set-unfold! integer->char (lambda (x) (= x 20)) add1 10 char-set:empty))

(test #t char-set= char-set:digit
      (let ([cs char-set:empty])
	(char-set-for-each 
	 (lambda (c) 
	   (set! cs (char-set-adjoin cs c)))
	 char-set:digit)
	cs))

(test #t char-set= char-set:digit
      (list->char-set
       (char-set-map
	(lambda (c) c)
	char-set:digit)))
(test #t char-set= char-set:digit
      (list->char-set!
       (char-set-map
	(lambda (c) c)
	char-set:digit)
       char-set:empty))
(test #t char-set= (char-set-adjoin char-set:digit #\A)
      (list->char-set
       (char-set-map
	(lambda (c) c)
	char-set:digit)
       (char-set #\A)))

;; Creating character sets ----------------------------------------

(test #t char-set= char-set:digit (char-set-copy char-set:digit))

(let ([abc (char-set #\a #\b #\c)])
  (test #t char-set= abc (char-set #\c #\a #\b))
  (test #t char-set= abc (string->char-set "cba"))
  (test #t char-set= abc (string->char-set! "cba" char-set:empty))
  (test #t char-set= abc (string->char-set "cb" (char-set #\a)))
  (test #t char-set= (char-set #\b) (char-set-filter (lambda (c) (char=? c #\b)) abc))
  (test #t char-set= abc (->char-set "abc"))
  (test #t char-set= abc (->char-set abc))
  (test #t char-set= (char-set #\a) (->char-set #\a)))

(printf "x~n")
(test #t char-set= 
      (ucs-range->char-set 0 #x20000)
      (char-set-union (ucs-range->char-set 0 #xD800)
		      (ucs-range->char-set #xE000 #x20000)))
(test #t char-set= 
      (ucs-range->char-set 0 #xD801)
      (ucs-range->char-set 0 #xD800))
(test #t char-set= 
      (ucs-range->char-set 0 #xDFFF)
      (ucs-range->char-set 0 #xD800))
(test #t char-set= 
      (ucs-range->char-set #xD800 #xD810)
      char-set:empty)
(test #t char-set= 
      (ucs-range->char-set #xD810 #xE000)
      char-set:empty)
(test #t char-set= 
      (ucs-range->char-set #xE000 #xE001)
      (ucs-range->char-set #xD810 #xE001))
(test #t char-set= 
      (ucs-range->char-set #xD7FF #xE001)
      (char-set #\uD7FF #\uE000))

(err/rt-test (ucs-range->char-set -1 10))
(err/rt-test (ucs-range->char-set 2 2))
(err/rt-test (ucs-range->char-set 2 1))
(err/rt-test (ucs-range->char-set 0 #x300000))
(err/rt-test (ucs-range->char-set #x300000 #x300001))

;; Querying character sets ------------------------------

(test 3 char-set-count (lambda (x) (char<=? #\0 x #\2)) char-set:digit)

(test #t char-set= char-set:digit (list->char-set (char-set->list char-set:digit)))
(test #t char-set= char-set:digit (string->char-set (char-set->string char-set:digit)))

;; Character-set algebra ----------------------------------------

(let* ([cs1 (char-set #\U #\t #\a #\h)]
       [cs2 (char-set-union cs1 (char-set #\S #\L #\C))])
  (test #t char-set= cs2 (char-set-adjoin cs1 #\S #\L #\C))
  (test #t char-set= cs2 (char-set-adjoin! cs1 #\S #\L #\C))
  (test #t char-set= (char-set-delete cs2 #\S #\L #\C) cs1)
  (test #t char-set= (char-set-delete! cs2 #\S #\L #\C) cs1)

  (let ([go 
	 (lambda (char-set-union)
	   (test #t char-set= cs1 (char-set-union (char-set #\U #\t #\a #\h)))
	   (test #t char-set= cs1 (char-set-union (char-set #\U #\t) (char-set #\a #\h)))
	   (test #t char-set= cs1 (char-set-union (char-set #\U) (char-set #\t #\a) (char-set #\h))))])
    (go char-set-union)
    (go char-set-union!))

  (let ([go 
	 (lambda (char-set-intersect)
	   (test #t char-set= cs1 (char-set-intersect (char-set #\U #\t #\a #\h)))
	   (test #t char-set= cs1 (char-set-intersect (char-set #\U #\t #\a #\h #\v) (char-set #\U #\t #\a #\h #\w)))
	   (test #t char-set= cs1 (char-set-intersect (char-set #\U #\t #\a #\h #\v) 
						      (char-set #\U #\t #\a #\h #\w)
						      (char-set #\a #\b #\U #\t #\a #\h #\w))))])
    (go char-set-intersection)
    (go char-set-intersection!))

  (let ([go 
	 (lambda (char-set-diff)
	   (test #t char-set= cs1 (char-set-diff (char-set #\U #\t #\a #\h #\v) 
						 (char-set #\v #\b)))
	   (test #t char-set= cs1 (char-set-diff (char-set #\U #\t #\a #\h #\v #\w) 
						 (char-set #\v #\b)
						 (char-set #\w))))])
    (go char-set-difference)
    (go char-set-difference!))

  (let ([go 
	 (lambda (char-set-xor)
	   (test #t char-set= cs1 (char-set-xor (char-set #\U #\t #\a #\v) 
						(char-set #\v #\h)))
	   (test #t char-set= cs1 (char-set-xor (char-set #\U #\h #\v #\w) 
						(char-set #\v #\a #\b)
						(char-set #\t #\w #\b))))])
    (go char-set-xor)
    (go char-set-xor!))

  (let ([go 
	 (lambda (char-set-diff+i)
	   (test #t andmap char-set= (list cs1 (char-set #\v))
		 (call-with-values
		     (lambda ()
		       (char-set-diff+i (char-set #\U #\t #\a #\h #\v) 
					(char-set #\v #\b)))
		   list))
	   (test #t andmap char-set= (list cs1 char-set:empty)
		 (call-with-values
		     (lambda ()
		       (char-set-diff+i (char-set #\U #\t #\a #\h #\v #\w) 
					(char-set #\v #\b)
					(char-set #\w)))
		   list)))])
    (go char-set-diff+intersection)
    (go char-set-diff+intersection!))

  )

  



;; ----------------------------------------

(report-errs)
