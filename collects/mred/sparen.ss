
; Scheme parenthesis wrappers around the general routines

  (unit/sig mred:scheme-paren^
    (import [wx : wx^]
	    [mred:constants : mred:constants^]
	    [mred:paren : mred:paren^])
	    
    (mred:debug:printf 'invoke "mred:scheme-paren@")

    ;; for now the scheme mode assumes that these are all one character long.
    (define scheme-paren-pairs '(("(" . ")")
				 ("[" . "]")
				 ("{" . "}")))
    
    (define scheme-quote-pairs '(("\"" . "\"")
				 ("#|" . "|#")
				 ("|" . "|")))
    
    (define scheme-comments '(";" "#!"))
    
    (define scheme-forward-match
      (opt-lambda (edit start end [cache #f])
	(mred:paren:forward-match edit start end
				   scheme-paren-pairs
				   scheme-quote-pairs
				   scheme-comments
				   cache)))

    (define scheme-backward-match
      (case-lambda
       [(edit start end cache)
	(mred:paren:backward-match edit start end
				   scheme-paren-pairs
				   scheme-quote-pairs
				   scheme-comments
				   #f
				   cache)]
       [(edit start end) (scheme-backward-match edit start end #f)]))

    (define scheme-balanced?
      (lambda (edit start end)
	(mred:paren:balanced? edit start end
			       scheme-paren-pairs
			       scheme-quote-pairs
			       scheme-comments)))
    
    (define scheme-backward-containing-sexp
      (case-lambda
       [(edit start end cache)
	(mred:paren:backward-match edit start end
				    scheme-paren-pairs
				    scheme-quote-pairs
				    scheme-comments
				    #t
				    cache)]
       [(edit start end) (scheme-backward-containing-sexp edit start end #f)])))
