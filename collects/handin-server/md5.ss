;;; md5.scm  --  Jens Axel Søgaard, 16 oct 2002  

;;; History

; 14-10-2002  
;   - Bored. Initial attempt. Done. Well, except for faulty output.
; 15-10-2002  
;   - It works at last
; 16-10-2002  
;   - Added R5RS support

;;; Summary

; This is an implementation of the md5 message-digest algorithm
; in R5RS Scheme. The algorithm takes an arbitrary string and 
; returns a 128-bit "fingerprint". 

; The algorithm was invented by Ron Rivest, RSA Security, INC.

; Reference:  RFC 1321, <http://www.faqs.org/rfcs/rfc1321.html>

;;; Contact

; Email jensaxel@soegaard.net if you have problems,
; suggestions, code for 32 bit arithmetic for your
; favorite implementation.

; Check <http://www.scheme.dk/md5/> for new versions.


;;; Technicalities

; The algorithm is designed to be efficiently implemented
; using 32 bit arithmetic. If your implementation supports
; 32 bit arithmetic directly, you should substitute the
; portable 32 operations with primitives of your implementation.

; See the PLT version below for an example. 

;;; Word aritmetic (32 bit)

; Terminology

;    word:  32 bit unsigned integer
;    byte:   8 bit unsigned integer

(module md5 mzscheme
  (provide md5)
  
  ;; mod32 : integer -> word
  (define-syntax mod32
    (syntax-rules ()
      ((mod32 n) (modulo n 4294967296))))
  
  ; word+ : word word -> word
  (define (word+ w1 w2)
    (mod32 (+ w1 w2)))
  
  ; word->bits : word -> (list (union 0 1))
  (define (word->bits w)
    (define (bit i)  
      (modulo (quotient w (expt 2 i)) 2))
    (map bit (iota 0 31)))
  
  ; bits->integer : (list (union 0 1)) -> integer
  (define (bits->integer bs)
    (apply + (map * bs (map (lambda (i) (expt 2 i))
                            (iota 0 31)))))
  
  ; map-bitwise (bit -> bit) word word -> word
  (define (map-bitwise f w1 w2)
    (bits->integer (map f (word->bits w1) (word->bits w2))))
  
  ;;; PLT-Versions (DrScheme, MzScheme)
  
  ; Remove the comments to use the PLT primitives.
  
  (define word-or  bitwise-ior)
  (define word-not bitwise-not)
  (define word-xor bitwise-xor)
  (define word-and bitwise-and)
  (define (word<<< n s)
    (bitwise-ior (arithmetic-shift n s) 
                 (arithmetic-shift n (- s 32))))
  
  ;;; Bytes and words
  
  ; The least significant byte of a word is the first
  
  ; bytes->word : (list byte*) -> word
  (define (bytes->word bs)
    (define (bs->w akk mul bs)
      (cond
        ((empty? bs) akk)
        (else        (bs->w (+ akk (* (first bs) mul)) (* 256 mul) (rest bs)))))
    (bs->w 0 1 bs))
  
  ; word->bytes : word -> "(list byte byte byte byte)"
  (define (word->bytes word)
    (define (extract w i)
      (remainder (quotient w (expt 256 i)) 256))
    (list (extract word 0) (extract word 1) (extract word 2) (extract word 3)))
  
  ; bytes->words : (list byte) -> (list word)
  (define (bytes->words bytes)
    (define (loop bs l)
      (cond
        ((empty? l)         (list (bytes->word (reverse bs))))
        ((< (length bs) 4)  (loop (cons (first l) bs)  (rest l)))
        (else               (cons (bytes->word (reverse bs))  (loop '() l)))))
    (if (empty? bytes)
        '()
        (loop '() bytes)))
  
  ; string->bytes : string -> (list byte)
  (define (string->bytes s)
    (map char->integer (string->list s)))
  
  
  ;;; Personal idiosyncrasies
  
  ; These are all part of PLT Scheme.
  ; Thus comment them out, if you use PLT.
  
  (define empty? null?)
  (define rest   cdr)
  (define first  car)
  (define second cadr)
  (define third  caddr)
  (define fourth cadddr)
  
  (define (iota m n)
    (if (> m n)
        '()
        (cons m (iota (+ m 1) n))))
  
  
  ;;; List Helper
  
  ; block/list : list -> (values vector list)
  ;  return a vector of the first 16 elements of the list,
  ;         and the rest of the list
  (define (block/list l)
    (let* (( v0 (first  l))  ( l0 (rest l))
           ( v1 (first l0))  ( l1 (rest l0))
           ( v2 (first l1))  ( l2 (rest l1))
           ( v3 (first l2))  ( l3 (rest l2))
           ( v4 (first l3))  ( l4 (rest l3))
           ( v5 (first l4))  ( l5 (rest l4))
           ( v6 (first l5))  ( l6 (rest l5))
           ( v7 (first l6))  ( l7 (rest l6))
           ( v8 (first l7))  ( l8 (rest l7))
           ( v9 (first l8))  ( l9 (rest l8))
           (v10 (first l9))  (l10 (rest l9))
           (v11 (first l10)) (l11 (rest l10))
           (v12 (first l11)) (l12 (rest l11))
           (v13 (first l12)) (l13 (rest l12))
           (v14 (first l13)) (l14 (rest l13))
           (v15 (first l14)) (l15 (rest l14)))
      (values (vector v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
              l15)))
  
  
  ;;;;; MD5
  
  ; The algorithm consists of five steps.
  ; All we need to do, is to call them in order.
  
  ; md5 : string -> string
  (define (md5 str)
    (step5 (step4 (step2 (* 8 (string-length str)) 
                         (step1 (string->bytes str))))))
  
  
  ;;; Step 1  -  Append Padding Bits
  
  ; The message is padded so the length (in bits) becomes 448 modulo 512.
  ; We allways append a 1 bit and then append the proper numbber of 0's.
  
  ; NB: 448 bits is 14 words and 512 bits is 16 words
  
  ; step1 : (list byte) -> (list byte)
  (define (step1 message)
    (let ((zero-bits-to-append (modulo (- 448 (* 8 (length message))) 512)))
      (append message 
              (cons #x80   ; The byte containing the 1 bit => one less 0 byte to append 
                    (vector->list (make-vector (quotient (- zero-bits-to-append 1) 8) 0))))))
  
  ;;; Step 2  -  Append Length
  
  ; A 64 bit representation of the bit length b of the message before
  ; the padding of step 1is appended. Lower word first.
  
  ; step2 : number (list byte) -> (list word)
  ;  org-len is the length of the original message in number of bits
  (define (step2 org-len padded-message)
    (let* ((b  org-len)
           (lo (mod32 b))
           (hi (mod32 (quotient b (expt 2 32)))))
      (bytes->words 
       (append padded-message 
               (append (word->bytes lo)
                       (word->bytes hi))))))
  
  
  ;;; Step 3  -  Initialize MD Buffer
  
  ; These magic constants are used to initialize the loop
  ; in step 4.
  ;
  ;          word A: 01 23 45 67
  ;          word B: 89 ab cd ef
  ;          word C: fe dc ba 98
  ;          word D: 76 54 32 10
  
  ;;; Step 4  -  Process Message in 16-Word Blocks
  
  ; For each 16 word block, go through a round one to four.
  
  ; step4 : (list word) -> "(list word word word word)"
  (define (step4 message)
    (define (loop A B C D message)
      (if (empty? message)
          (list A B C D)
          (let-values (((X rest) (block/list message)))
            (let* ((result (apply round4 
                                  (apply round3 
                                         (apply round2 
                                                (round1 A B C D X)))))
                   (A (word+ (list-ref result 0) A)) 
                   (B (word+ (list-ref result 1) B))
                   (C (word+ (list-ref result 2) C)) 
                   (D (word+ (list-ref result 3) D)))
              (loop A B C D rest)))))
    
    ; Step 3 :-) (magic constants)
    (loop #x67452301 #xefcdab89 #x98badcfe #x10325476 message))
  
  ; Each round consists of the application of the following
  ; basic functions. They functions on a word bitwise, as follows.
  
  ;          F(X,Y,Z) = XY v not(X) Z  (NB: or can be replaced with + in F)
  ;          G(X,Y,Z) = XZ v Y not(Z)
  ;          H(X,Y,Z) = X xor Y xor Z
  ;          I(X,Y,Z) = Y xor (X v not(Z))
  
  (define (F x y z)
    (word-or (word-and x y) (word-and (word-not x) z)))
  (define (G x y z)
    (word-or (word-and x z) (word-and y (word-not z))))
  (define (H x y z)
    (word-xor x (word-xor y z)))
  (define (II x y z)
    (word-xor y (word-or x (word-not z))))
  
  ; The rounds furthermore use values from this sine table,
  ; which we precompute.
  
  (define T
    (let* ((precompute (lambda (i) (inexact->exact (floor (* 4294967296 (abs (sin i)))))))
           (v (list->vector (map precompute (iota 1 64)))))
      (lambda (i)
        (vector-ref v (- i 1)))))
  
  ; The rounds are specified using the notation (abcd k s i).
  ; This is a shorthand for respectively:    
  
  ;   Round 1:   a = b + ((a + F(b,c,d) + X(k) + T(i)) <<< s) 
  ;   Round 2:   a = b + ((a + G(b,c,d) + X(k) + T(i)) <<< s) 
  ;   Round 3:   a = b + ((a + H(b,c,d) + X(k) + T(i)) <<< s) 
  ;   Round 4:   a = b + ((a + I(b,c,d) + X(k) + T(i)) <<< s) 
  
  ; Example: (DABC  1 12  2) in round 1 is shothand for this operation
  ;          D = A + ((D + F(A,B,C) + X(1) + T(2)) <<< 12) 
  
  ; To use the specifications, we need to replace the symbols
  ; with permutation vectors.
  
  ; prepare : operations -> operations'
  ;  symbols are substituted with indices, e.g. 'DABC |-> (list 3 0 1 2)
  (define (prepare ops)
    (define (symbol->indices s)
      (list->vector (map (lambda (n) (- n (char->integer #\a)))
                         (map char->integer (string->list (symbol->string s))))))
    (map (lambda (l)
           (cons (symbol->indices (first l)) (rest l)))
         ops))
  
  (define round1-operations
    (prepare
     '((ABCD  0  7  1)  (DABC  1 12  2)  (CDAB  2 17  3)  (BCDA  3 22  4)
       (ABCD  4  7  5)  (DABC  5 12  6)  (CDAB  6 17  7)  (BCDA  7 22  8)
       (ABCD  8  7  9)  (DABC  9 12 10)  (CDAB 10 17 11)  (BCDA 11 22 12)
       (ABCD 12  7 13)  (DABC 13 12 14)  (CDAB 14 17 15)  (BCDA 15 22 16))))
  
  (define round2-operations
    (prepare
     '((ABCD  1  5 17)  (DABC  6  9 18)  (CDAB 11 14 19)  (BCDA  0 20 20)
       (ABCD  5  5 21)  (DABC 10  9 22)  (CDAB 15 14 23)  (BCDA  4 20 24)
       (ABCD  9  5 25)  (DABC 14  9 26)  (CDAB  3 14 27)  (BCDA  8 20 28)
       (ABCD 13  5 29)  (DABC  2  9 30)  (CDAB  7 14 31)  (BCDA 12 20 32))))
  
  (define round3-operations
    (prepare
     '((ABCD  5  4 33)  (DABC  8 11 34)  (CDAB 11 16 35)  (BCDA 14 23 36)
       (ABCD  1  4 37)  (DABC  4 11 38)  (CDAB  7 16 39)  (BCDA 10 23 40)
       (ABCD 13  4 41)  (DABC  0 11 42)  (CDAB  3 16 43)  (BCDA  6 23 44)
       (ABCD  9  4 45)  (DABC 12 11 46)  (CDAB 15 16 47)  (BCDA  2 23 48))))
  
  (define round4-operations
    (prepare
     '((ABCD  0  6 49)  (DABC  7 10 50)  (CDAB 14 15 51)  (BCDA  5 21 52)
       (ABCD 12  6 53)  (DABC  3 10 54)  (CDAB 10 15 55)  (BCDA  1 21 56)
       (ABCD  8  6 57)  (DABC 15 10 58)  (CDAB  6 15 59)  (BCDA 13 21 60)
       (ABCD  4  6 61)  (DABC 11 10 62)  (CDAB  2 15 63)  (BCDA  9 21 64))))
  
  ; The operation without permutation is given by (respectively).
  
  (define (rf1 a b c d X k i s)
    (word+ b (word<<< (word+ a (word+ (F b c d)  (word+ (vector-ref X k) (T i)))) s)))
  (define (rf2 a b c d X k i s)
    (word+ b (word<<< (word+ a (word+ (G b c d)  (word+ (vector-ref X k) (T i)))) s)))
  (define (rf3 a b c d X k i s)
    (word+ b (word<<< (word+ a (word+ (H b c d)  (word+ (vector-ref X k) (T i)))) s)))
  (define (rf4 a b c d X k i s)
    (word+ b (word<<< (word+ a (word+ (II b c d) (word+ (vector-ref X k) (T i)))) s)))
  
  ; Uncomment these to see what happens in the rounds
  ; (define (trace func name)
  ;   (lambda (a b c d X k i s)
  ;     (display (list name (hex a) (hex b) (hex c) (hex d) 
  ;                    (hex (vector-ref X k)) (hex (T i)) (hex s)))
  ;     (let ((r (func a b c d X k i s)))
  ;       (display " -> ") (display (hex r)) (newline)
  ;       r)))
  ;
  ; (define rf1 (trace rf1 'f))
  ; (define rf2 (trace rf2 'g))
  ; (define rf3 (trace rf3 'h))
  ; (define rf4 (trace rf4 'i))
  
  ; To execute a round, one goes through the list of
  ; operations. The above functions rf1,...,rf4 are called
  ; after the permutation is done.
  
  (define (xround j  a b c d X)
    
    (define (loop a b c d X rf ops)
      (define (indirect v w i)
        (vector-ref v (vector-ref w i)))
      (if (empty? ops)
          (list a b c d x)
          (let* ((op      (first ops))
                 (indices (first op))
                 (k       (second op))
                 (s       (third op))
                 (i       (fourth op))
                 ; permute
                 (v  (vector a b c d))
                 (a  (indirect v indices 0))
                 (b  (indirect v indices 1))
                 (c  (indirect v indices 2))
                 (d  (indirect v indices 3))
                 (a  (rf a b c d X k i s)))
            ; make the assignment
            (vector-set! v (vector-ref indices 0) a)
            (let ((a (vector-ref v 0)) 
                  (b (vector-ref v 1))
                  (c (vector-ref v 2))
                  (d (vector-ref v 3)))
              (apply loop (list a b c d X rf (rest ops)))))))
    
    (cond 
      ((= j 1) (loop a b c d X rf1 round1-operations))
      ((= j 2) (loop a b c d X rf2 round2-operations))
      ((= j 3) (loop a b c d X rf3 round3-operations))
      ((= j 4) (loop a b c d X rf4 round4-operations))))
  
  ; For convenience in step 4:
  
  (define (round1  a b c d X)
    (xround 1  a b c d X))
  (define (round2  a b c d X)
    (xround 2  a b c d X))
  (define (round3  a b c d X)
    (xround 3  a b c d X))
  (define (round4  a b c d X)
    (xround 4  a b c d X))
  
  
  ;;; Step 5  -  Output
  
  ; To finish up, we convert the word to hexadecimal string
  ; - and make sure they end up in order.
  
  ; step5 : "(list word word word word)" -> string
  (define (step5 l)
    
    (define (number->hex n)
      (let ((str (number->string n 16)))
        (case (string-length str)
          ((1)  (string-append "0" str))
          (else str))))
    
    (apply string-append
           (map number->hex
                (apply append (map word->bytes l)))))
  
  
  ;;; Test
  
  ; Generic arithmetic
  
  ;'bytes->word
  ;(and (= (bytes->word '(1 0 0 0))    1)
  ;     (= (bytes->word '(0 0 0 128))  (expt 2 31)))
  ;
  ;'word->bytes
  ;(and (equal? '(1 2 3 4) (word->bytes (bytes->word '(1 2 3 4)))))
  ;
  ;'word<<<
  ;(and (= 123  (word<<< (word<<< 123 7) 25))
  ;     (= 123  (word<<< (word<<< 123 0) 32))
  ;     (= 123  (word<<< (word<<< 123 8) 24)))
  ;
  ;'word-not
  ;(and (= (+ 0 (word-not 0))
  ;        (+ 1 (word-not 1))))
  ;
  ;(define (hex n)
  ;  (number->string n 16))
  
  #;
  (define (md5-test)
    (if (and (equal? (md5 "")
                     "d41d8cd98f00b204e9800998ecf8427e")
             (equal? (md5 "a")
                     "0cc175b9c0f1b6a831c399e269772661")
             (equal? (md5 "abc")
                     "900150983cd24fb0d6963f7d28e17f72")
             (equal? (md5 "message digest")
                     "f96b697d7cb7938d525a2f31aaf161d0")
             (equal? (md5 "abcdefghijklmnopqrstuvwxyz")
                     "c3fcd3d76192e4007dfb496cca67e13b")
             (equal? (md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
                     "d174ab98d277d9f5a5611c2c9f419d9f")
             (equal? (md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
                     "57edf4a22be3c955ac49da2e2107b67a"))
        'passed
        'failed))
  
  #;
  (md5-test)
  )