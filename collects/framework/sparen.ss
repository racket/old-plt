; Scheme parenthesis wrappers around the general routines

(unit/sig framework:scheme-paren^
  (import [paren : framework:paren^])

  ;; for now the scheme mode assumes that these are all one character long.
  (define paren-pairs '(("(" . ")")
			("[" . "]")
			("{" . "}")))
  
  (define quote-pairs '(("\"" . "\"")
			("#|" . "|#")
			("|" . "|")))
  
  (define comments '(";" "#!"))

  (define (get-paren-pairs) paren-pairs)
  (define (get-quote-pairs) quote-pairs)
  (define (get-comments) comments)
  
  (define forward-match
    (opt-lambda (edit start end [cache #f])
      (paren:forward-match edit start end
			   paren-pairs
			   quote-pairs
			   comments
			   cache)))

  (define backward-match
    (case-lambda
     [(edit start end cache)
      (paren:backward-match edit start end
			    paren-pairs
			    quote-pairs
			    comments
			    #f
			    cache)]
     [(edit start end) (backward-match edit start end #f)]))

  (define balanced?
    (lambda (edit start end)
      (paren:balanced? edit start end
		       paren-pairs
		       quote-pairs
		       comments)))
  
  (define backward-containing-sexp
    (case-lambda
     [(edit start end cache)
      (paren:backward-match edit start end
			    paren-pairs
			    quote-pairs
			    comments
			    #t
			    cache)]
     [(edit start end) (backward-containing-sexp edit start end #f)])))
