
(load-relative "loadtest.ss")

(SECTION 'utf8)

(define basic-utf8-tests
  '((#(#x3ba #x1f79 #x3c3 #x3bc #x3b5)
     (#\316 #\272 #\341 #\275 #\271 #\317 #\203 #\316 #\274 #\316 #\265))
    (#(0)
     (#\nul))
    (#(#x00000080)
     (#\302 #\200))
    (#(#x00000800)
     (#\340 #\240 #\200))
    (#(#x00010000)
     (#\360 #\220 #\200 #\200))
    (#(#x00200000)
     (#\370 #\210 #\200 #\200 #\200))
    (#(#x04000000)
     (#\374 #\204 #\200 #\200 #\200 #\200))
    (#(#x0000007F)
     (#\rubout))
    (#(#x000007FF)
     (#\337 #\277))
    ;; Should this one be allowed? We check below that it's
    ;;  disallowed!
    ;; (#(#x0000FFFF)
    ;;  (#\357 #\277 #\277))
    (#(#x001FFFFF)
     (#\367 #\277 #\277 #\277))
    (#(#x03FFFFFF)
     (#\373 #\277 #\277 #\277 #\277))
    (#(#x7FFFFFFF)
     (#\375 #\277 #\277 #\277 #\277 #\277))
    (#(#x0000D7FF)
     (#\355 #\237 #\277))
    (#(#x0000E000)
     (#\356 #\200 #\200))
    (#(#x0000FFFD)
     (#\357 #\277 #\275))
    (#(#x0010FFFF)
     (#\364 #\217 #\277 #\277))
    (#(#x00110000)
     (#\364 #\220 #\200 #\200))
    (#f
     (#\200))
    (#f
     (#\277))
    (#f
     (#\200 #\277))
    (#f
     (#\200 #\277 #\200))
    (#f
     (#\200 #\277 #\200 #\277))
    (#f
     (#\200 #\277 #\200 #\277 #\200))
    (#f
     (#\200 #\277 #\200 #\277 #\200 #\277))
    (#f
     (#\200 #\277 #\200 #\277 #\200 #\277 #\200))
    (#f
     (#\200))
    (#f
     (#\201))
    (#f
     (#\202))
    (#f
     (#\203))
    (#f
     (#\204))
    (#f
     (#\205))
    (#f
     (#\206))
    (#f
     (#\207))
    (#f
     (#\210))
    (#f
     (#\211))
    (#f
     (#\212))
    (#f
     (#\213))
    (#f
     (#\214))
    (#f
     (#\215))
    (#f
     (#\216))
    (#f
     (#\217))
    (#f
     (#\220))
    (#f
     (#\221))
    (#f
     (#\222))
    (#f
     (#\223))
    (#f
     (#\224))
    (#f
     (#\225))
    (#f
     (#\226))
    (#f
     (#\227))
    (#f
     (#\230))
    (#f
     (#\231))
    (#f
     (#\232))
    (#f
     (#\233))
    (#f
     (#\234))
    (#f
     (#\235))
    (#f
     (#\236))
    (#f
     (#\237))
    (#f
     (#\240))
    (#f
     (#\241))
    (#f
     (#\242))
    (#f
     (#\243))
    (#f
     (#\244))
    (#f
     (#\245))
    (#f
     (#\246))
    (#f
     (#\247))
    (#f
     (#\250))
    (#f
     (#\251))
    (#f
     (#\252))
    (#f
     (#\253))
    (#f
     (#\254))
    (#f
     (#\255))
    (#f
     (#\256))
    (#f
     (#\257))
    (#f
     (#\260))
    (#f
     (#\261))
    (#f
     (#\262))
    (#f
     (#\263))
    (#f
     (#\264))
    (#f
     (#\265))
    (#f
     (#\266))
    (#f
     (#\267))
    (#f
     (#\270))
    (#f
     (#\271))
    (#f
     (#\272))
    (#f
     (#\273))
    (#f
     (#\274))
    (#f
     (#\275))
    (#f
     (#\276))
    (#f
     (#\277))
    ;; 2-byte seqs with no continuation
    (#f
     (#\300 #\space))
    (#f
     (#\301 #\space))
    (#f
     (#\302 #\space))
    (#f
     (#\303 #\space))
    (#f
     (#\304 #\space))
    (#f
     (#\305 #\space))
    (#f
     (#\306 #\space))
    (#f
     (#\307 #\space))
    (#f
     (#\310 #\space))
    (#f
     (#\311 #\space))
    (#f
     (#\312 #\space))
    (#f
     (#\313 #\space))
    (#f
     (#\314 #\space))
    (#f
     (#\315 #\space))
    (#f
     (#\316 #\space))
    (#f
     (#\317 #\space))
    (#f
     (#\320 #\space))
    (#f
     (#\321 #\space))
    (#f
     (#\322 #\space))
    (#f
     (#\323 #\space))
    (#f
     (#\324 #\space))
    (#f
     (#\325 #\space))
    (#f
     (#\326 #\space))
    (#f
     (#\327 #\space))
    (#f
     (#\330 #\space))
    (#f
     (#\331 #\space))
    (#f
     (#\332 #\space))
    (#f
     (#\333 #\space))
    (#f
     (#\334 #\space))
    (#f
     (#\335 #\space))
    (#f
     (#\336 #\space))
    (#f
     (#\337 #\space))
    ;; 3-byte seqs with no continuation
    (#f
     (#\340 #\space))
    (#f
     (#\341 #\space))
    (#f
     (#\342 #\space))
    (#f
     (#\343 #\space))
    (#f
     (#\344 #\space))
    (#f
     (#\345 #\space))
    (#f
     (#\346 #\space))
    (#f
     (#\347 #\space))
    (#f
     (#\350 #\space))
    (#f
     (#\351 #\space))
    (#f
     (#\352 #\space))
    (#f
     (#\353 #\space))
    (#f
     (#\354 #\space))
    (#f
     (#\355 #\space))
    (#f
     (#\356 #\space))
    (#f
     (#\357 #\space))
    ;; 3-byte seqs with partial continuation
    (#f
     (#\340 #\203 #\space))
    (#f
     (#\341 #\203 #\space))
    (#f
     (#\342 #\203 #\space))
    (#f
     (#\343 #\203 #\space))
    (#f
     (#\344 #\203 #\space))
    (#f
     (#\345 #\203 #\space))
    (#f
     (#\346 #\203 #\space))
    (#f
     (#\347 #\203 #\space))
    (#f
     (#\350 #\203 #\space))
    (#f
     (#\351 #\203 #\space))
    (#f
     (#\352 #\203 #\space))
    (#f
     (#\353 #\203 #\space))
    (#f
     (#\354 #\203 #\space))
    (#f
     (#\355 #\203 #\space))
    (#f
     (#\356 #\203 #\space))
    (#f
     (#\357 #\203 #\space))
    ;; 4-byte seq with no continuations
    (#f
     (#\360 #\space))
    (#f
     (#\361))
    (#f
     (#\362 #\space))
    (#f
     (#\363 #\space))
    (#f
     (#\364 #\space))
    (#f
     (#\365 #\space))
    (#f
     (#\366 #\space))
    (#f
     (#\367 #\space))
    ;; 4-byte seq with only 1 continuation
    (#f
     (#\360 #\203 #\space))
    (#f
     (#\361 #\203))
    (#f
     (#\362 #\203 #\space))
    (#f
     (#\363 #\203 #\space))
    (#f
     (#\364 #\203 #\space))
    (#f
     (#\365 #\203 #\space))
    (#f
     (#\366 #\203 #\space))
    (#f
     (#\367 #\203 #\space))
    ;; 4-byte seq with only 2 continuation
    (#f
     (#\360 #\203 #\203 #\space))
    (#f
     (#\361 #\203 #\203))
    (#f
     (#\362 #\203 #\203 #\space))
    (#f
     (#\363 #\203 #\203 #\space))
    (#f
     (#\364 #\203 #\203 #\space))
    (#f
     (#\365 #\203 #\203 #\space))
    (#f
     (#\366 #\203 #\203 #\space))
    (#f
     (#\367 #\203 #\203 #\space))
    ;; 5-byte seqs with no continuation
    (#f
     (#\370 #\space))
    (#f
     (#\371 #\space))
    (#f
     (#\372 #\space))
    (#f
     (#\373 #\space))
    ;; 5-byte seqs with only 1 continuation
    (#f
     (#\370 #\203 #\space))
    (#f
     (#\371 #\203 #\space))
    (#f
     (#\372 #\203 #\space))
    (#f
     (#\373 #\203 #\space))
    ;; 5-byte seqs with only 2 continuations
    (#f
     (#\370 #\203 #\203 #\space))
    (#f
     (#\371 #\203 #\203 #\space))
    (#f
     (#\372 #\203 #\203 #\space))
    (#f
     (#\373 #\203 #\203 #\space))
    ;; 5-byte seqs with only 3 continuations
    (#f
     (#\370 #\203 #\203 #\203 #\space))
    (#f
     (#\371 #\203 #\203 #\203 #\space))
    (#f
     (#\372 #\203 #\203 #\203 #\space))
    (#f
     (#\373 #\203 #\203 #\203 #\space))
    ;; 6-byte seqs with no continuation
    (#f
     (#\374 #\space))
    (#f
     (#\375 #\space))
    ;; 6-byte seqs with only 1 continuation
    (#f
     (#\374 #\203 #\space))
    (#f
     (#\375 #\203 #\space))
    ;; 6-byte seqs with only 2 continuation
    (#f
     (#\374 #\203 #\203 #\space))
    (#f
     (#\375 #\203 #\203 #\space))
    ;; 6-byte seqs with only 3 continuation
    (#f
     (#\374 #\203 #\203 #\203 #\space))
    (#f
     (#\375 #\203 #\203 #\203 #\space))
    ;; 6-byte seqs with only 4 continuation
    (#f
     (#\374 #\203 #\203 #\203 #\203 #\space))
    (#f
     (#\375 #\203 #\203 #\203 #\203 #\space))
    ;; Sequences with last continuation byte missing, eol instead of space
    (#f
     (#\300))
    (#f
     (#\340 #\200))
    (#f
     (#\340))
    (#f
     (#\360 #\200 #\200))
    (#f
     (#\360 #\200))
    (#f
     (#\360 #\200))
    (#f
     (#\370 #\200 #\200 #\200))
    (#f
     (#\370))
    (#f
     (#\370 #\200))
    (#f
     (#\370 #\200 #\200))
    (#f
     (#\374 #\200 #\200 #\200 #\200))
    (#f
     (#\374))
    (#f
     (#\374 #\200))
    (#f
     (#\374 #\200 #\200))
    (#f
     (#\374 #\200 #\200 #\200))
    (#f
     (#\337))
    (#f
     (#\357 #\277))
    (#f
     (#\367 #\277 #\277))
    (#f
     (#\373 #\277 #\277 #\277))
    (#f
     (#\375 #\277 #\277 #\277 #\277))
    ;; Concatenation of incomplete sequences
    (#f
     (#\300 #\340 #\200 #\360 #\200 #\200 #\370 #\200 #\200 #\200 #\374 #\200 #\200 #\200 #\200 #\337 #\357 #\277 #\367 #\277 #\277 #\373 #\277 #\277 #\277 #\375 #\277 #\277 #\277 #\277))
    ;; Impossible bytes
    (#f
     (#\376))
    (#f
     (#\377))
    (#f
     (#\376 #\376 #\377 #\377))
    ;; Overlong
    (#f
     (#\300 #\257))
    (#f
     (#\340 #\200 #\257))
    (#f
     (#\360 #\200 #\200 #\257))
    (#f
     (#\370 #\200 #\200 #\200 #\257))
    (#f
     (#\374 #\200 #\200 #\200 #\200 #\257))
    (#f
     (#\301 #\277))
    (#f
     (#\340 #\237 #\277))
    (#f
     (#\360 #\217 #\277 #\277))
    (#f
     (#\370 #\207 #\277 #\277 #\277))
    (#f
     (#\374 #\203 #\277 #\277 #\277 #\277))
    (#f
     (#\300 #\200))
    (#f
     (#\340 #\200 #\200))
    (#f
     (#\360 #\200 #\200 #\200))
    (#f
     (#\370 #\200 #\200 #\200 #\200))
    (#f
     (#\374 #\200 #\200 #\200 #\200 #\200))
    ;; illedgal surrogates
    (#f
     (#\355 #\240 #\200))
    (#f
     (#\355 #\255 #\277))
    (#f
     (#\355 #\256 #\200))
    (#f
     (#\355 #\257 #\277))
    (#f
     (#\355 #\260 #\200))
    (#f
     (#\355 #\276 #\200))
    (#f
     (#\355 #\277 #\277))
    (#f
     (#\355 #\240 #\200 #\355 #\260 #\200))
    (#f
     (#\355 #\240 #\200 #\355 #\277 #\277))
    (#f
     (#\355 #\255 #\277 #\355 #\260 #\200))
    (#f
     (#\355 #\255 #\277 #\355 #\277 #\277))
    (#f
     (#\355 #\256 #\200 #\355 #\260 #\200))
    (#f
     (#\355 #\256 #\200 #\355 #\277 #\277))
    (#f
     (#\355 #\257 #\277 #\355 #\260 #\200))
    (#f
     (#\355 #\257 #\277 #\355 #\277 #\277))
    ;; Other illegal code positions
    (#f
     (#\357 #\277 #\276))
    (#f
     (#\357 #\277 #\277))))

(for-each (lambda (p)
	    (let ([code-points (car p)]
		  [s (apply string (cadr p))])
	      (when code-points
		(test (vector-length code-points) string-unicode-length s)
		(test code-points string->unicode-vector s)
		(test s unicode-vector->string code-points))
	      (when (not code-points)
		(test #f string-unicode-length s))))
	  basic-utf8-tests)

(test '(#\302 #\251) string->list (unicode-vector->string (vector 169)))
(test '(#\304 #\250) string->list (unicode-vector->string (vector 296)))

(test '("\302\251") regexp-match #rx"." "\302\251")
(test '("\302") regexp-match #rxb"." "\302\251")

(test #f regexp-match #rx"[a-z]" "\302\251")
(test '("\302\251") regexp-match #rx"\302\251" "\302\251")
(test '("\302\251") regexp-match #rx"\302\251+" "\302\251")
(test '("\302\251\302\251") regexp-match #rx"\302\251+" "\302\251\302\251")
(test '("\302\251\302\251") regexp-match #rx"\302\251+" "x\302\251\302\251y")
(test '("\302\251") regexp-match #rx"[a-z\302\251]" "\302\251mm")
(test '("\302\251mm") regexp-match #rx"[a-z\302\251]+" "\302\251mm")
(test '("\302\251") regexp-match #rx"[\302\251-\304\250]+" "xx\302\251mm")
(test '("\303\251") regexp-match #rx"[\302\251-\304\250]+" "xx\303\251mm")
(test #f regexp-match #rx"[\302\251-\304\250]+" "xx\304\251mm")
(test #f regexp-match #rx"[\302\251-\304\250]+" "xx\302\250mm")
(test '("\303\251\302\251") regexp-match #rx"[\302\251-\304\250]+" "xx\303\251\302\251mm")

;; Nots of patterns and ranges:
(test #f regexp-match #rx"[^a-z\302\251]" "\302\251mm")
(test '("") regexp-match #rx"[^a-d\302\251]*" "\302\251mm")
(test '("xx") regexp-match #rx"[^\302\251-\304\250]+" "xx\302\251mm")
(test '("xx") regexp-match #rx"[^\302\251-\304\250]+" "xx\303\251mm")
(test '("xx\304\251mm") regexp-match #rx"[^\302\251-\304\250]+" "xx\304\251mm")
(test '("xx\302\250mm") regexp-match #rx"[^\302\251-\304\250]+" "xx\302\250mm")
(test '("xx") regexp-match #rx"[^\302\251-\304\250]+" "xx\303\251\302\251mm")

;; 3-char seqs
(test '("\341\275\271") regexp-match #rx"\341\275\271" "a\341\275\271\341\275\271b")
(test '("\341\275\271\341\275\271") regexp-match #rx"\341\275\271+" "a\341\275\271\341\275\271b")

(test '("\341\275\271\302\251\341\275\271r") regexp-match #rx"[c-\341\275\271]+" "a\341\275\271\302\251\341\275\271r")
(test '("d\341\275\271\302\251\341\275\271r") regexp-match #rx"[c-\341\275\271]+" "d\341\275\271\302\251\341\275\271r")

(test '("\342\275\271") regexp-match #rx"[\341\275\271-\343\275\271]" "\342\275\271")
(test '("\341\275\271") regexp-match #rx"[\341\275\271-\343\275\271]" "\341\275\271")
(test #f regexp-match #rx"[\341\275\271-\343\275\271]" "\341\274\271")
(test #f regexp-match #rx"[\341\275\271-\343\275\271]" "\341\275\270")

;; Nots of 3-char seqs:
(test #f regexp-match #rx"[^\341\275\271-\343\275\271]" "\342\275\271")
(test #f regexp-match #rx"[^\341\275\271-\343\275\271]" "\341\275\271")
(test '("\341\274\271") regexp-match #rx"[^\341\275\271-\343\275\271]" "\341\274\271")
(test '("\341\275\270") regexp-match #rx"[^\341\275\271-\343\275\271]" "\341\275\270")


;; Regexps that shouldn't parse:
(err/rt-test (regexp "[a--b\341\275\270]") exn:misc?)
(err/rt-test (regexp "[a-b-c\341\275\270]") exn:misc?)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String comparison. Relies on the default locale knowing
;;  about upper A with hat (\303\202) and lower a with hat (\303\242),
;;  and also relies on a "C" locale that can't encode those
;;  two characters. It doesn't rely on a relative order of A-hat
;;  and a-hat --- only that they're the same case-insensitively.
(let ()
  (define (stest r comp? a b)
    (test r comp? a b)
    (test r comp? (format "xx~ayy" a) (format "xx~ayy" b))
    (test r comp? (format "x\000x~ay" a) (format "x\000x~ay" b))
    (test r comp? (format "x\000~ay" a) (format "x\000~ay" b))
    (test r comp? (format "x\000~a\000y" a) (format "x\000~a\000y" b)))
  (define (go c?)
    (stest #f string=? "\303\202" "\303\242") 
    (stest #f string-ci=? "\303\202" "\303\242")
    (stest #f string-unicode=? "\303\202" "\303\242")
    (stest (if c? #f #t) string-unicode-ci=? "\303\202" "\303\242")
    (stest #f string<? "\303\242" "b")
    (stest (if c? #f #t) string-unicode<? "\303\242" "b")
    (stest #t string>? "\303\242" "b")
    (stest (if c? #t #f) string-unicode>? "\303\242" "b")
    (stest #t string<? "b" "\303\242")
    (stest (if c? #t #f) string-unicode<? "b" "\303\242")
    (stest #f string>? "b" "\303\242")
    (stest (if c? #f #t) string-unicode>? "b" "\303\242"))
  (go #f)
  (parameterize ([current-locale "C"])
    (go #t)))

(report-errs)
