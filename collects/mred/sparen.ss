
; Scheme parenthesis wrappers around the general routines

(define-sigfunctor (mred:scheme-paren@ mred:scheme-paren^)
  (import mred:debug^ mred:paren^)

  (define scheme-paren-pairs '(("(" . ")")
			       ("[" . "]")
			       ("{" . "}")))
  
  (define scheme-quote-pairs '(("\"" . "\"")))
  
  (define scheme-comments '(";"))
  
  (define scheme-forward-match
    (opt-lambda (edit start end [cache #f])
      (mred:paren^:forward-match edit start end
				 scheme-paren-pairs
				 scheme-quote-pairs
				 scheme-comments
				 cache)))

  (define scheme-backward-match
    (opt-lambda (edit start end [cache #f])
      (mred:paren^:backward-match edit start end
				  scheme-paren-pairs
				  scheme-quote-pairs
				  scheme-comments
				  #f
				  cache)))

  (define scheme-balanced?
    (lambda (edit start end)
      (mred:paren^:balanced? edit start end
			     scheme-paren-pairs
			     scheme-quote-pairs
			     scheme-comments)))
  
  (define scheme-backward-containing-sexp
    (opt-lambda (edit start end [cache #f])
      (mred:paren^:backward-match edit start end
				  scheme-paren-pairs
				  scheme-quote-pairs
				  scheme-comments
				  #t
				  cache)))
  )
