; Parenthesis Matching
; originally by Dan Grossman: 6/19/95
; rewitten by Matthew: 1/28/98

  (unit/sig mred:paren^
    (import [wx : wx^]
	    [mred:constants : mred:constants^])
	    
    (mred:debug:printf 'invoke "mred:paren@")

    (define skip-whitespace
      (lambda (edit pos dir)
	(let ([left (if (= dir 1) pos (sub1 pos))]
	      [okay (if (= dir 1)(<= pos (send edit last-position))
			(> pos 0))])
	  (if okay	
	      (let ([next-char (send edit get-character left)])
		(if (char-whitespace? next-char)
		    (skip-whitespace edit ((if (= dir 1) add1 sub1) pos) dir)
		    pos))
	      pos))))
    

    ;; :balanced? uses match-forward to determine if paren.s are balanced.
    
    (define balanced?
      (lambda (edit start end paren-pairs quote-pairs  eol-comments)
	(if (or (> end (send edit last-position))
		(<= end start))
	    #f
	    (let* ([balance-point                  
		    (forward-match edit start end
				   paren-pairs quote-pairs eol-comments)]
		   [end-point 
		    (and balance-point
			 (skip-whitespace edit balance-point 1))])
	      (and balance-point
		   (or (and (<= balance-point end) (>= end-point end))
		       (balanced? edit end-point end paren-pairs quote-pairs 
				  eol-comments))))))) 

    (define do-match
      (lambda (forward? 
	       bad-match-answer edit 
	       pos end-pos 
	       paren-pairs quote-pairs eol-comment-list ; For backwards, reverse pair orders in paren-pairs and quote-pairs
	       unreversed-quote-pairs
	       paren-cache)
	(let* ([backward? (not forward?)]
	       [eof-answer #f]
	       [next1 (if forward? add1 sub1)]
	       [offset (if forward? + -)]
	       [skip1 (lambda (x) (offset x 2))]
	       [-string-ref (if forward?
				string-ref
				(lambda (s p)
				  (string-ref s (- (string-length s) p 1))))]
	       [init-pos (max 0 pos)]
	       [buffer-end (if forward?
			       (min end-pos (send edit last-position))
			       (max end-pos 0))]
	       [get-character (ivar edit get-character)]
	       [get-char (if forward?
			     get-character
			     ;; DEVIOUS TRICK: we're going to virtually swap
			     ;; each escaping \ with the character it escapes. Now,
			     ;; in a backward read of the buffer, a backslash appears
			     ;; before the character it escapes
			     (letrec ([is-escaping?
				       (lambda (pos)
					 (let ([c (get-character (sub1 pos))])
					   (not (and (char=? c #\\) (is-escaping? (sub1 pos))))))])
			       (lambda (x) 
				 (cond
				  [(= x 1) (let ([c (get-character 0)])
					     (if (char=? c #\\)
						 (get-character 1)
						 c))]
				  [(> x 1) (let ([c-1 (get-character (- x 2))]
						 [c (get-character (sub1 x))])
					     (cond
					      [(char=? c #\\)
					       (if (is-escaping? (sub1 x))
						   (get-character x)
						   c)]
					      [(char=? c-1 #\\)
					       (if (is-escaping? (- x 2))
						   #\\
						   c)]
					      [else c]))]
				  [else #\nul]))))]
	       [paragraph-start-position (ivar edit paragraph-start-position)]
	       [position-paragraph (ivar edit position-paragraph)]
	       [find-string (ivar edit find-string)]
	       [get-cached (if paren-cache
			       (ivar paren-cache get)
			       (lambda (p) #f))]
	       [past-end? (let ([past? (if forward? >= <=)])
			    (lambda (pos) (past? pos buffer-end)))]
	       [opens (map car paren-pairs)]
	       [close-parens (map cdr paren-pairs)]
	       [parentheses (append opens close-parens)]
	       [look-for-comment-starters (append eol-comment-list
						  (map car quote-pairs))]
	       [answer (lambda (s e)
			 (unless (number? e)
				 (error 'paren-match "internal-error: tried to put bad answer in cache"))
			 (when paren-cache
			       (send paren-cache put s e))
			 e)]
	       [normal-close-k (lambda (start-pos pos closer) eof-answer)]
	       [normal-done-k (lambda (pos) pos)]
	       [normal-starters (append parentheses
					(map car quote-pairs)
					eol-comment-list)]
	       [match-string-at-pos*
		(lambda (next1 get-char -string-ref)
		  (lambda (pos c l)
		    (and (ormap (lambda (s) (char=? c (-string-ref s 0))) l) ; quick check
			 (let loop ([pos pos][i 0][c c][l l])
			   (let ([so-far (let loop ([l l][k (lambda (x) x)])
					   (cond
					    [(null? l) (k null)]
					    [(char=? (-string-ref (car l) i) c)
					     (if (= (add1 i) (string-length (car l)))
						 (list (car l))
						 (loop (cdr l)
						       (lambda (r) (k (cons (car l) r)))))]
					    [else (loop (cdr l) k)]))])
			     (cond
			      [(null? so-far) #f]
			      [(and (null? (cdr so-far)) 
				    (let ([s (car so-far)])
				      (and (= (string-length s) (add1 i)) 
					   s)))
			       => (lambda (s) s)]
			      [else (loop (next1 pos) (add1 i) (get-char (next1 pos)) so-far)]))))))]
	       [match-string-at-pos (match-string-at-pos* next1 get-char -string-ref)]
	       [find-comment
		(lambda (pos skip-comment-k)
		  (if forward?
		      (skip-comment-k pos)
		      ;; Even in reverse mode, we have to implement comment-checking
		      ;;  by a forward search. We propogate the assumption that we
		      ;;  started outside of quotes by assuming that the *beginning* of
		      ;;  the current line is outside of quotes (until proven otherwise).
		      (let ([linestart (paragraph-start-position (position-paragraph pos))])
			(if (ormap (lambda (s) (>= (find-string s 1 linestart pos) 0)) eol-comment-list)
			    (let ([forward-match-string (match-string-at-pos* add1 get-character string-ref)])
			      (let loop ([p linestart])
				(if (>= p pos) 
				    (skip-comment-k pos)
				    (let ([c (get-character p)])
				      (cond
				       [(char=? c #\\) (loop (+ p 2))]
				       [(forward-match-string p c look-for-comment-starters)
					=> (lambda (match)
					     (if (member match eol-comment-list)
						 (skip-comment-k p)
						 ;; Not a comment character; now we have to perform
						 ;;  a forward-match for quotes:
						 (let ([r (forward-match edit p pos null unreversed-quote-pairs null #f)])
						   (if r
						       (loop r)
						       ;; Unmatched quotes? The searching semantics
						       ;; assumes that we start out outside of quotes, so
						       ;; jump over the open quote (to where we are, officially)
						       ;; and try again
						       (loop (+ p (string-length match)))))))]
				       [else (loop (add1 p))])))))
			    (skip-comment-k pos)))))])
	  ;; In reverse mode, start by looking for a comment
	  (find-comment 
	   init-pos
	   (lambda (i-pos)
	     (let nloop ([pos i-pos]
			 [unquoted? #t]
			 [starters normal-starters]
			 [closes close-parens]
			 [close-k normal-close-k]
			 [done-k normal-done-k])
	       ;; Skip spaces
	       (let loop ([pos pos])
		 (let ([c (get-char pos)])
		   (if (char-whitespace? c) ; Note: #\null is not whitespace
		       (cond
			[(and backward? unquoted? (char=? c #\newline))
			 ;; Check for a comment
			 (find-comment
			  (next1 pos)
			  (lambda (pos) (loop pos)))]
			[else
			 (loop (next1 pos))])
		       (let ([cached (and unquoted? (get-cached pos))])
			 (if cached
			     (done-k cached)
			     ;; Check for matching open-something (forward) or close-something (backward)
			     (let ([match (match-string-at-pos pos c starters)])
			       (cond
				[(not match)
				 (cond
				  [(past-end? pos)  ; end of file
				   (done-k eof-answer)]
				  [unquoted?
				   ;; Find next space or parenthesis
				   (let ([start-pos pos])
				     (let loop ([pos ((if (char=? c #\\) skip1 next1) pos)])
				       (let ([c (get-char pos)])
					 (cond
					  [(or (char-whitespace? c) (past-end? pos))
					   (done-k 
					    (if (= pos start-pos)
						eof-answer
						(answer start-pos pos)))]
					  [(char=? #\\ c) (loop (skip1 pos))]
					  [(match-string-at-pos pos c normal-starters) (done-k (answer start-pos pos))]
					  [else 		      
					   (loop (next1 pos))]))))]
				  [else
				   ;; Try again
				   (if (char=? #\\ c)
				       (loop (skip1 pos))
				       (loop (next1 pos)))])]
				[(member match closes)
				 ;; pos after close is answer - maybe the final answer
				 (close-k pos (offset pos (string-length match)) match)]
				[else
				 ;; Found open; now find close
				 (cond
				  [(assoc match quote-pairs)
				   ;; Open quote
				   => (lambda (p)
					(nloop (offset pos (string-length match))
					       #f
					       (list (cdr p)) ; no special characters now except the close quote
					       (list (cdr p))
					       (lambda (cl-start-pos cl-pos closer) 
						 (done-k (answer pos cl-pos)))
					       (lambda (pos)
						 (if (or (eq? pos bad-match-answer)
							 (eq? pos eof-answer))
						     (done-k pos)
						     (error 'paren-match "non-failure done-k called while looking for a close quote")))))]
				  [(and forward? (member match eol-comment-list))
				   ;; Start comment to EOL
				   (let ([start-pos pos])
				     (let mloop ([pos (offset pos (string-length match))])
				       (let ([c (get-char pos)])
					 (if (or (char=? c #\newline)
						 (and (past-end? pos)))
					     ;; Skipped comment, now try again
					     (loop pos)
					     ;; Still looking for the end-of-comment
					     (mloop (next1 pos))))))]
				  [else
				   ;; Open parenthesis
				   (let ([paren-pair (assoc match paren-pairs)]
					 [found-pos pos]
					 [start-pos (offset pos (string-length match))])
				     (unless paren-pair
					     (error 'paren-match "internal-error: open parenthesis ~a is not in the list ~a~n"
						    match paren-pairs))
					(let loop ([pos start-pos])
					  (nloop pos
						 #t
						 normal-starters
						 close-parens
						 (lambda (cl-start-pos cl-pos closer)
						   (if (string=? closer (cdr paren-pair))
						       (done-k (answer found-pos cl-pos))
						       (done-k bad-match-answer))) ; wrong closer
						 (lambda (pos)
						   (if (or (eq? pos bad-match-answer)
							   (eq? pos eof-answer))
						       (done-k pos)
						       (loop pos))))))])])))))))))))))

    (define forward-match
      (opt-lambda (edit pos end-pos 
			paren-pairs quote-pairs eol-comment-list
			[paren-cache #f])
	(do-match #t #f edit pos end-pos paren-pairs quote-pairs eol-comment-list quote-pairs paren-cache)))

    (define prev-paren-pairs null)
    (define prev-rev-paren-pairs null)
    (define prev-quote-pairs null)
    (define prev-rev-quote-pairs null)

    (define backward-match
      (opt-lambda (edit pos end-pos 
			paren-pairs quote-pairs eol-comment-list
			[contains? #f]
			[paren-cache #f])
	 (let loop ([old #f][pos pos])
	   (let ([p (do-match #f (if contains? 'bad-match #f)
			      edit pos end-pos 
			      (map (lambda (p) (cons (cdr p) (car p))) paren-pairs)
			      (map (lambda (p) (cons (cdr p) (car p))) quote-pairs)
			      eol-comment-list quote-pairs paren-cache)])
	     (if contains?
		 (cond
		  [(eq? p 'bad-match) #f]
		  [(not p) old]
		  [(and old (= p old)) p] ; what!?
		  [else (loop p p)])
		 p))))))
