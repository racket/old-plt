; C++ parenthesis wrappers around the general routines

(define mred:c++-paren-pairs '(("(" . ")")
			       ("[" . "]")
			       ("{" . "}")))

(define mred:c++-quote-pairs '(("\"" . "\"")
			       ("'" . "'")
			       ("/*" . "*/")))

(define mred:c++-comments '("//"))

(define mred:c++-forward-match
  (lambda (edit start end)
    (mred:forward-match edit start end
                        mred:c++-paren-pairs
                        mred:c++-quote-pairs
                        mred:c++-comments)))

(define mred:c++-backward-match
  (opt-lambda (edit start end [cache #f])
    (mred:backward-match edit start end
                         mred:c++-paren-pairs
                         mred:c++-quote-pairs
                         mred:c++-comments
                         #f
			 cache)))

(define mred:c++-balanced?
  (lambda (edit start end)
    (mred:balanced? edit start end
                    mred:c++-paren-pairs
                    mred:c++-quote-pairs
                    mred:c++-comments)))

(define mred:c++-backward-containing-exp
  (opt-lambda (edit start end [cache #f])
    (mred:backward-match edit start end
                         mred:c++-paren-pairs
                         mred:c++-quote-pairs
                         mred:c++-comments
                         #t
			 cache)))
