;pregexp.ss
;Portable regular expressions for Scheme
;Dorai Sitaram
;http://www.ccs.neu.edu/~dorai/pregexp/pregexp.html
;Oct 2, 1999
;Last mod: Nov 30, 2002

(module pregexp mzscheme
        (provide pregexp
                 pregexp-match-positions
                 pregexp-match
                 pregexp-split
                 pregexp-replace
                 pregexp-replace*
                 pregexp-quote
                 *pregexp-comment-char*)

;Configured for Scheme dialect plt by scmxlate, v 1a3,
;(c) Dorai Sitaram, 
;http://www.ccs.neu.edu/~dorai/scmxlate/scmxlate.html

(define *pregexp-comment-char* #\;)

(define *pregexp-space-sensitive?* #t)

(define pregexp-read-pattern
  (lambda (s i n)
    (if (>= i n)
      (list (list ':or (list ':seq)) i)
      (let loop ((branches '()) (i i))
        (if (or (>= i n) (char=? (string-ref s i) #\)))
          (list (cons ':or (reverse! branches)) i)
          (let ((vv
                 (pregexp-read-branch
                   s
                   (if (char=? (string-ref s i) #\|) (+ i 1) i)
                   n)))
            (loop (cons (car vv) branches) (cadr vv))))))))

(define pregexp-read-branch
  (lambda (s i n)
    (let loop ((pieces '()) (i i))
      (cond
       ((>= i n) (list (cons ':seq (reverse! pieces)) i))
       ((let ((c (string-ref s i))) (or (char=? c #\|) (char=? c #\))))
        (list (cons ':seq (reverse! pieces)) i))
       (else
        (let ((vv (pregexp-read-piece s i n)))
          (loop (cons (car vv) pieces) (cadr vv))))))))

(define pregexp-read-piece
  (lambda (s i n)
    (let ((c (string-ref s i)))
      (case c
        ((#\^) (list ':bos (+ i 1)))
        ((#\$) (list ':eos (+ i 1)))
        ((#\.) (pregexp-wrap-quantifier-if-any (list ':any (+ i 1)) s n))
        ((#\[)
         (pregexp-wrap-quantifier-if-any
           (case (string-ref s (+ i 1))
             ((#\^)
              (let ((vv (pregexp-read-char-list s (+ i 2) n)))
                (list (list ':neg-char (car vv)) (cadr vv))))
             (else (pregexp-read-char-list s (+ i 1) n)))
           s
           n))
        ((#\()
         (pregexp-wrap-quantifier-if-any
           (pregexp-read-subpattern s (+ i 1) n)
           s
           n))
        ((#\\)
         (pregexp-wrap-quantifier-if-any
           (cond
            ((pregexp-read-escaped-number s i n)
             =>
             (lambda (num-i) (list (list ':backref (car num-i)) (cadr num-i))))
            ((pregexp-read-escaped-char s i n)
             =>
             (lambda (char-i) (list (car char-i) (cadr char-i))))
            (else (error 'pregexp-read-piece "backslash")))
           s
           n))
        (else
         (if (or *pregexp-space-sensitive?*
                 (and (not (char-whitespace? c))
                      (not (char=? c *pregexp-comment-char*))))
           (pregexp-wrap-quantifier-if-any (list c (+ i 1)) s n)
           (let loop ((i i) (in-comment? #f))
             (if (>= i n)
               (list ':empty i)
               (let ((c (string-ref s i)))
                 (cond
                  (in-comment? (loop (+ i 1) (not (char=? c #\newline))))
                  ((char-whitespace? c) (loop (+ i 1) #f))
                  ((char=? c *pregexp-comment-char*) (loop (+ i 1) #t))
                  (else (list ':empty i))))))))))))

(define pregexp-read-escaped-number
  (lambda (s i n)
    (and (< (+ i 1) n)
         (let ((c (string-ref s (+ i 1))))
           (and (char-numeric? c)
                (let loop ((i (+ i 2)) (r (list c)))
                  (if (>= i n)
                    (list (string->number (list->string (reverse! r))) i)
                    (let ((c (string-ref s i)))
                      (if (char-numeric? c)
                        (loop (+ i 1) (cons c r))
                        (list
                         (string->number (list->string (reverse! r)))
                         i))))))))))

(define pregexp-read-escaped-char
  (lambda (s i n)
    (and (< (+ i 1) n)
         (let ((c (string-ref s (+ i 1))))
           (case c
             ((#\b) (list ':wbdry (+ i 2)))
             ((#\B) (list ':not-wbdry (+ i 2)))
             ((#\d) (list ':digit (+ i 2)))
             ((#\D) (list '(:neg-char :digit) (+ i 2)))
             ((#\n) (list #\newline (+ i 2)))
             ((#\r) (list #\return (+ i 2)))
             ((#\s) (list ':space (+ i 2)))
             ((#\S) (list '(:neg-char :space) (+ i 2)))
             ((#\t) (list #\tab (+ i 2)))
             ((#\w) (list ':word (+ i 2)))
             ((#\W) (list '(:neg-char :word) (+ i 2)))
             (else (list c (+ i 2))))))))

(define pregexp-read-posix-char-class
  (lambda (s i n)
    (let ((neg? #f))
      (let loop ((i i) (r (list #\:)))
        (if (>= i n)
          (error 'pregexp-read-posix-char-class)
          (let ((c (string-ref s i)))
            (cond
             ((char=? c #\^) (set! neg? #t) (loop (+ i 1) r))
             ((char-alphabetic? c) (loop (+ i 1) (cons c r)))
             ((char=? c #\:)
              (if (or (>= (+ i 1) n) (not (char=? (string-ref s (+ i 1)) #\])))
                (error 'pregexp-read-posix-char-class)
                (let ((posix-class
                        (string->symbol (list->string (reverse! r)))))
                  (list
                   (if neg? (list ':neg-char posix-class) posix-class)
                   (+ i 2)))))
             (else (error 'pregexp-read-posix-char-class)))))))))

(define pregexp-read-cluster-type
  (lambda (s i n)
    (let ((c (string-ref s i)))
      (case c
        ((#\?)
         (let ((i (+ i 1)))
           (case (string-ref s i)
             ((#\:) (list '() (+ i 1)))
             ((#\=) (list '(:lookahead) (+ i 1)))
             ((#\!) (list '(:neg-lookahead) (+ i 1)))
             ((#\>) (list '(:no-backtrack) (+ i 1)))
             ((#\<)
              (list
               (case (string-ref s (+ i 1))
                 ((#\=) '(:lookbehind))
                 ((#\!) '(:neg-lookbehind))
                 (else (error 'pregexp-read-cluster-type)))
               (+ i 2)))
             (else
              (let loop ((i i) (r '()) (inv? #f))
                (let ((c (string-ref s i)))
                  (case c
                    ((#\-) (loop (+ i 1) r #t))
                    ((#\i)
                     (loop
                      (+ i 1)
                      (cons (if inv? ':case-sensitive ':case-insensitive) r)
                      #f))
                    ((#\x)
                     (set! *pregexp-space-sensitive?* inv?)
                     (loop (+ i 1) r #f))
                    ((#\:) (list r (+ i 1)))
                    (else (error 'pregexp-read-cluster-type)))))))))
        (else (list '(:sub) i))))))

(define pregexp-read-subpattern
  (lambda (s i n)
    (let* ((remember-space-sensitive? *pregexp-space-sensitive?*)
           (ctyp-i (pregexp-read-cluster-type s i n))
           (ctyp (car ctyp-i))
           (i (cadr ctyp-i))
           (vv (pregexp-read-pattern s i n)))
      (set! *pregexp-space-sensitive?* remember-space-sensitive?)
      (let ((vv-re (car vv)) (vv-i (cadr vv)))
        (if (and (< vv-i n) (char=? (string-ref s vv-i) #\)))
          (list
           (let loop ((ctyp ctyp) (re vv-re))
             (if (null? ctyp) re (loop (cdr ctyp) (list (car ctyp) re))))
           (+ vv-i 1))
          (error 'pregexp-read-subpattern))))))

(define pregexp-wrap-quantifier-if-any
  (lambda (vv s n)
    (let ((re (car vv)))
      (let loop ((i (cadr vv)))
        (if (>= i n)
          vv
          (let ((c (string-ref s i)))
            (if (and (char-whitespace? c) (not *pregexp-space-sensitive?*))
              (loop (+ i 1))
              (case c
                ((#\* #\+ #\? #\{)
                 (let* ((new-re
                          (list ':between 'minimal? 'at-least 'at-most re))
                        (new-vv (list new-re 'next-i)))
                   (case c
                     ((#\*)
                      (set-car! (cddr new-re) 0)
                      (set-car! (cdddr new-re) #f))
                     ((#\+)
                      (set-car! (cddr new-re) 1)
                      (set-car! (cdddr new-re) #f))
                     ((#\?)
                      (set-car! (cddr new-re) 0)
                      (set-car! (cdddr new-re) 1))
                     ((#\{)
                      (let ((pq (pregexp-read-nums s (+ i 1) n)))
                        (if (not pq)
                          (error
                           'pregexp-wrap-quantifier-if-any
                           "left bracket must be followed by number"))
                        (set-car! (cddr new-re) (car pq))
                        (set-car! (cdddr new-re) (cadr pq))
                        (set! i (caddr pq)))))
                   (let loop ((i (+ i 1)))
                     (if (>= i n)
                       (begin
                         (set-car! (cdr new-re) #f)
                         (set-car! (cdr new-vv) i))
                       (let ((c (string-ref s i)))
                         (cond
                          ((and (char-whitespace? c)
                                (not *pregexp-space-sensitive?*))
                           (loop (+ i 1)))
                          ((char=? c #\?)
                           (set-car! (cdr new-re) #t)
                           (set-car! (cdr new-vv) (+ i 1)))
                          (else
                           (set-car! (cdr new-re) #f)
                           (set-car! (cdr new-vv) i))))))
                   new-vv))
                (else vv)))))))))

(define pregexp-read-nums
  (lambda (s i n)
    (let loop ((p '()) (q '()) (k i) (reading 1))
      (if (>= k n) (error 'pregexp-read-nums))
      (let ((c (string-ref s k)))
        (cond
         ((char-numeric? c)
          (if (= reading 1)
            (loop (cons c p) q (+ k 1) 1)
            (loop p (cons c q) (+ k 1) 2)))
         ((and (char-whitespace? c) (not *pregexp-space-sensitive?*))
          (loop p q (+ k 1) reading))
         ((and (char=? c #\,) (= reading 1)) (loop p q (+ k 1) 2))
         ((char=? c #\})
          (let ((p (string->number (list->string (reverse! p))))
                (q (string->number (list->string (reverse! q)))))
            (cond
             ((and (not p) (= reading 1)) (list 0 #f k))
             ((= reading 1) (list p p k))
             (else (list p q k)))))
         (else #f))))))

(define pregexp-invert-char-list
  (lambda (vv) (set-car! (car vv) ':none-of-chars) vv))

(define pregexp-read-char-list
  (lambda (s i n)
    (let loop ((r '()) (i i))
      (if (>= i n)
        (error 'pregexp-read-char-list "character class ended too soon")
        (let ((c (string-ref s i)))
          (case c
            ((#\])
             (if (null? r)
               (loop (cons c r) (+ i 1))
               (list (cons ':one-of-chars (reverse! r)) (+ i 1))))
            ((#\\)
             (let ((char-i (pregexp-read-escaped-char s i n)))
               (if char-i
                 (loop (cons (car char-i) r) (cadr char-i))
                 (error 'pregexp-read-char-list "backslash"))))
            ((#\-)
             (let ((c-prev (car r)))
               (if (char? c-prev)
                 (loop
                  (cons
                   (list ':char-range c-prev (string-ref s (+ i 1)))
                   (cdr r))
                  (+ i 2))
                 (loop (cons c r) (+ i 1)))))
            ((#\[)
             (if (char=? (string-ref s (+ i 1)) #\:)
               (let ((posix-char-class-i
                       (pregexp-read-posix-char-class s (+ i 2) n)))
                 (loop
                  (cons (car posix-char-class-i) r)
                  (cadr posix-char-class-i)))
               (loop (cons c r) (+ i 1))))
            (else (loop (cons c r) (+ i 1)))))))))

(define pregexp-string-match
  (lambda (s1 s i n sk fk)
    (let ((n1 (string-length s1)))
      (if (> n1 n)
        (fk)
        (let loop ((j 0) (k i))
          (cond
           ((>= j n1) (sk k))
           ((>= k n) (fk))
           ((char=? (string-ref s1 j) (string-ref s k)) (loop (+ j 1) (+ k 1)))
           (else (fk))))))))

(define pregexp-char-word?
  (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))))

(define pregexp-at-word-boundary?
  (lambda (s i n)
    (or (= i 0)
        (>= i n)
        (let ((c/i (string-ref s i)) (c/i-1 (string-ref s (- i 1))))
          (let ((c/i/w? (pregexp-check-if-in-char-class? c/i ':word))
                (c/i-1/w? (pregexp-check-if-in-char-class? c/i-1 ':word)))
            (or (and c/i/w? (not c/i-1/w?)) (and (not c/i/w?) c/i-1/w?)))))))

(define pregexp-check-if-in-char-class?
  (lambda (c char-class)
    (case char-class
      ((:any) (not (char=? c #\newline)))
      ((:alnum) (or (char-alphabetic? c) (char-numeric? c)))
      ((:alpha) (char-alphabetic? c))
      ((:ascii) (< (char->integer c) 128))
      ((:blank) (or (char=? c #\space) (char=? c #\tab)))
      ((:cntrl) (< (char->integer c) 32))
      ((:digit) (char-numeric? c))
      ((:graph) (and (>= (char->integer c) 32) (not (char-whitespace? c))))
      ((:lower) (char-lower-case? c))
      ((:print) (>= (char->integer c) 32))
      ((:punct)
       (and (>= (char->integer c) 32)
            (not (char-whitespace? c))
            (not (char-alphabetic? c))
            (not (char-numeric? c))))
      ((:space) (char-whitespace? c))
      ((:upper) (char-upper-case? c))
      ((:word) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))
      ((:xdigit)
       (or (char-numeric? c)
           (char-ci=? c #\a)
           (char-ci=? c #\b)
           (char-ci=? c #\c)
           (char-ci=? c #\d)
           (char-ci=? c #\e)
           (char-ci=? c #\f)))
      (else (error 'pregexp-check-if-in-char-class?)))))

(define pregexp-list-ref
  (lambda (s i)
    (let loop ((s s) (k 0))
      (cond ((null? s) #f) ((= k i) (car s)) (else (loop (cdr s) (+ k 1)))))))

(define pregexp-match-positions-aux
  (lambda (re s start n i)
    (let ((case-sensitive? #t))
      (let sub ((re re) (i i) (backrefs '()) (sk list) (fk (lambda () #f)))
        (cond
         ((eqv? re ':bos) (if (= i start) (sk i backrefs) (fk)))
         ((eqv? re ':eos) (if (>= i n) (sk i backrefs) (fk)))
         ((eqv? re ':empty) (sk i backrefs))
         ((eqv? re ':wbdry)
          (if (pregexp-at-word-boundary? s i n) (sk i backrefs) (fk)))
         ((eqv? re ':not-wbdry)
          (if (pregexp-at-word-boundary? s i n) (fk) (sk i backrefs)))
         ((and (char? re) (< i n))
          (if ((if case-sensitive? char=? char-ci=?) (string-ref s i) re)
            (sk (+ i 1) backrefs)
            (fk)))
         ((and (not (pair? re)) (< i n))
          (if (pregexp-check-if-in-char-class? (string-ref s i) re)
            (sk (+ i 1) backrefs)
            (fk)))
         ((and (pair? re) (eqv? (car re) ':char-range) (< i n))
          (let ((c (string-ref s i)))
            (if (let ((c< (if case-sensitive? char<=? char-ci<=?)))
                  (and (c< (cadr re) c) (c< c (caddr re))))
              (sk (+ i 1) backrefs)
              (fk))))
         ((pair? re)
          (case (car re)
            ((:char-range)
             (if (>= i n) (fk) (error 'pregexp-match-positions-aux)))
            ((:one-of-chars)
             (if (>= i n)
               (fk)
               (let loup-one-of-chars ((chars (cdr re)))
                 (if (null? chars)
                   (fk)
                   (sub
                    (car chars)
                    i
                    backrefs
                    sk
                    (lambda () (loup-one-of-chars (cdr chars))))))))
            ((:neg-char)
             (if (>= i n)
               (fk)
               (sub
                (cadr re)
                i
                backrefs
                (lambda (i1 backrefs1) (fk))
                (lambda () (sk (+ i 1) backrefs)))))
            ((:seq)
             (let loup-seq ((res (cdr re)) (i i) (backrefs backrefs))
               (if (null? res)
                 (sk i backrefs)
                 (sub
                  (car res)
                  i
                  backrefs
                  (lambda (i1 backrefs1) (loup-seq (cdr res) i1 backrefs1))
                  fk))))
            ((:or)
             (let loup-or ((res (cdr re)))
               (if (null? res)
                 (fk)
                 (sub
                  (car res)
                  i
                  backrefs
                  (lambda (i1 backrefs1)
                    (or (sk i1 backrefs1) (loup-or (cdr res))))
                  (lambda () (loup-or (cdr res)))))))
            ((:backref)
             (let ((backref (pregexp-list-ref backrefs (cadr re))))
               (if backref
                 (pregexp-string-match
                   (substring s (car backref) (cdr backref))
                   s
                   i
                   n
                   (lambda (i) (sk i backrefs))
                   fk)
                 (sk i backrefs))))
            ((:sub)
             (let* ((sub-backref (cons i i))
                    (backrefs (append backrefs (list sub-backref))))
               (sub
                (cadr re)
                i
                backrefs
                (lambda (i1 backrefs1)
                  (set-cdr! sub-backref i1)
                  (sk i1 backrefs1))
                fk)))
            ((:lookahead)
             (let ((found-it? (sub (cadr re) i backrefs list (lambda () #f))))
               (if found-it? (sk i backrefs) (fk))))
            ((:neg-lookahead)
             (let ((found-it? (sub (cadr re) i backrefs list (lambda () #f))))
               (if found-it? (fk) (sk i backrefs))))
            ((:lookbehind)
             (let ((n-actual n))
               (set! n i)
               (let ((found-it?
                       (sub
                        (list ':seq '(:between #f 0 #f :any) (cadr re) ':eos)
                        0
                        backrefs
                        list
                        (lambda () #f))))
                 (set! n n-actual)
                 (if found-it? (sk i backrefs) (fk)))))
            ((:neg-lookbehind)
             (let ((n-actual n))
               (set! n i)
               (let ((found-it?
                       (sub
                        (list ':seq '(:between #f 0 #f :any) (cadr re) ':eos)
                        0
                        backrefs
                        list
                        (lambda () #f))))
                 (set! n n-actual)
                 (if found-it? (fk) (sk i backrefs)))))
            ((:no-backtrack)
             (let ((found-it? (sub (cadr re) i backrefs list (lambda () #f))))
               (if found-it? (sk (car found-it?) (cadr found-it?)) (fk))))
            ((:case-sensitive :case-insensitive)
             (let ((old case-sensitive?))
               (set! case-sensitive? (eqv? (car re) ':case-sensitive))
               (sub
                (cadr re)
                i
                backrefs
                (lambda (i1 backrefs1)
                  (set! case-sensitive? old)
                  (sk i1 backrefs1))
                (lambda () (set! case-sensitive? old) (fk)))))
            ((:between)
             (let* ((maximal? (not (cadr re)))
                    (p (caddr re))
                    (q (cadddr re))
                    (re (car (cddddr re)))
                    (subpat? (and (pair? re) (eqv? (car re) ':sub))))
               (let loup-p ((k 0) (i i) (cbackrefs 'no-match-yet))
                 (if (< k p)
                   (sub
                    re
                    i
                    backrefs
                    (lambda (i1 backrefs1) (loup-p (+ k 1) i1 backrefs1))
                    fk)
                   (let ((q (and q (- q p))))
                     (let loup-q ((k 0) (i i) (cbackrefs cbackrefs))
                       (let ((fk
                              (lambda ()
                                (sk
                                 i
                                 (if (eqv? cbackrefs 'no-match-yet)
                                   (if subpat?
                                     (append backrefs (list #f))
                                     backrefs)
                                   cbackrefs)))))
                         (if (and q (>= k q))
                           (fk)
                           (if maximal?
                             (sub
                              re
                              i
                              backrefs
                              (lambda (i1 backrefs1)
                                (or (loup-q (+ k 1) i1 backrefs1) (fk)))
                              fk)
                             (or (fk)
                                 (sub
                                  re
                                  i
                                  backrefs
                                  (lambda (i1 backrefs1)
                                    (loup-q (+ k 1) i1 backrefs1))
                                  fk)))))))))))
            (else (error 'pregexp-match-positions-aux))))
         ((>= i n) (fk))
         (else (error 'pregexp-match-positions-aux)))))))

(define pregexp-replace-aux
  (lambda (str ins n backrefs)
    (let loop ((i 0) (r ""))
      (if (>= i n)
        r
        (let ((c (string-ref ins i)))
          (if (char=? c #\\)
            (let* ((br-i (pregexp-read-escaped-number ins i n))
                   (br
                    (if br-i
                      (car br-i)
                      (if (char=? (string-ref ins (+ i 1)) #\&) 0 #f)))
                   (i (if br-i (cadr br-i) (if br (+ i 2) (+ i 1)))))
              (if (not br)
                (let ((c2 (string-ref ins i)))
                  (loop
                   (+ i 1)
                   (if (char=? c2 #\$) r (string-append r (string c2)))))
                (loop
                 i
                 (let ((backref (pregexp-list-ref backrefs br)))
                   (if backref
                     (string-append
                       r
                       (substring str (car backref) (cdr backref)))
                     r)))))
            (loop (+ i 1) (string-append r (string c)))))))))

(define pregexp
  (lambda (s)
    (set! *pregexp-space-sensitive?* #t)
    (list ':sub (car (pregexp-read-pattern s 0 (string-length s))))))

(define pregexp-match-positions
  (lambda (pat str . opt-args)
    (let* ((pat (if (string? pat) (pregexp pat) pat))
           (start
            (if (null? opt-args)
              0
              (let ((start (car opt-args)))
                (set! opt-args (cdr opt-args))
                start)))
           (end (if (null? opt-args) (string-length str) (car opt-args))))
      (let loop ((i start))
        (and (<= i end)
             (let ((vv (pregexp-match-positions-aux pat str start end i)))
               (if vv (cadr vv) (loop (+ i 1)))))))))

(define pregexp-match
  (lambda (pat str . opt-args)
    (let ((ix-prs (apply pregexp-match-positions pat str opt-args)))
      (and ix-prs
           (map
            (lambda (ix-pr)
              (and ix-pr (substring str (car ix-pr) (cdr ix-pr))))
            ix-prs)))))

(define pregexp-split
  (lambda (pat str)
    (let ((n (string-length str)))
      (let loop ((i 0) (r '()) (picked-up-one-undelimited-char? #f))
        (cond
         ((>= i n) (reverse! r))
         ((pregexp-match-positions pat str i n)
          =>
          (lambda (y)
            (let ((jk (car y)))
              (let ((j (car jk)) (k (cdr jk)))
                (cond
                 ((= j k) (loop (+ k 1) (cons (substring str i (+ j 1)) r) #t))
                 ((and (= j i) picked-up-one-undelimited-char?) (loop k r #f))
                 (else (loop k (cons (substring str i j) r) #f)))))))
         (else (loop n (cons (substring str i n) r) #f)))))))

(define pregexp-replace
  (lambda (pat str ins)
    (let* ((n (string-length str)) (pp (pregexp-match-positions pat str 0 n)))
      (if (not pp)
        str
        (let ((ins-len (string-length ins)) (m-i (caar pp)) (m-n (cdar pp)))
          (string-append
            (substring str 0 m-i)
            (pregexp-replace-aux str ins ins-len pp)
            (substring str m-n n)))))))

(define pregexp-replace*
  (lambda (pat str ins)
    (let ((pat (if (string? pat) (pregexp pat) pat))
          (n (string-length str))
          (ins-len (string-length ins)))
      (let loop ((i 0) (r ""))
        (let ((pp (pregexp-match-positions pat str i n)))
          (cond
           (pp
            (loop
             (cdar pp)
             (string-append
               r
               (substring str i (caar pp))
               (pregexp-replace-aux str ins ins-len pp))))
           ((= i 0) str)
           (else (string-append r (substring str i n)))))))))

(define pregexp-quote
  (lambda (s)
    (let loop ((i (- (string-length s) 1)) (r '()))
      (if (< i 0)
        (list->string r)
        (loop
         (- i 1)
         (let ((c (string-ref s i)))
           (if (memv c '(#\\ #\. #\? #\* #\+ #\| #\[ #\] #\{ #\} #\( #\)))
             (cons #\\ (cons c r))
             (cons c r))))))))


)