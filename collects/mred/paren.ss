; Parenthesis Matching
; originally by Dan Grossman
; 6/19/95

; BALANCER ASSUMES THAT QUOTED STRINGS DO NOT SPAN LINES

  (unit/sig mred:paren^
    (import)
	    
    (mred:debug:printf 'invoke "mred:paren@")

    (define newline-string (string #\newline))

    ;; search-esc wraps find string with a check for preceding \ characters
    ;; using get-character
    (define search-esc
      (lambda (edit pos end str dir)
	(let* ([last (send edit last-position)]
	       [next (if (eq? dir 1) add1 sub1)])
	  (letrec
	      ([candidate (send edit find-string str dir pos end (= dir 1))]
	       [slashes
		(lambda (pos)
		  (if (and (> pos 0)
			   (char=? #\\  (send edit get-character (sub1 pos))))
		      (add1 (slashes (sub1 pos)))
		      0))])
	    (if (negative? candidate)
		#f
		(if (even? (slashes candidate))
		    candidate
		    (search-esc edit (next candidate) end str dir)))))))
    
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
	(if (or (> end (send edit last-position))(<= end start))
	    #f 
	    (let* ([balance-point                  
		    (forward-match edit start end
				   paren-pairs quote-pairs eol-comments)]
		   [end-point(if balance-point(skip-whitespace edit balance-point 1)#f)])
	      (if balance-point
		  (if (and (<= balance-point end)(>= end-point end))
		      #t
		      (balanced? edit end-point end paren-pairs quote-pairs 
				 eol-comments))
		  #f))))) 

    (define forward-match
      (opt-lambda (edit pos end-pos paren-pairs quote-pairs eol-comment-list
			[paren-cache #f])
	(let ([open-spots (make-vector (length paren-pairs) -1)] 
	      [close-spots (make-vector (length paren-pairs) -1)]
	      [quote-spots (make-vector (length quote-pairs) -1)]
	      [eol-spots (make-vector (length eol-comment-list) -1)]
	      [parens (list->vector paren-pairs)]
	      [quotes (list->vector quote-pairs)]
	      [eols (list->vector eol-comment-list)])
	  (letrec ([do-open
		    (lambda (pos paren-type)
		      (vector-set! open-spots paren-type 
				   (search-esc 
				    edit pos end-pos
				    (car (vector-ref parens paren-type)) 1))
		      (find-match pos paren-type))]
		   [do-close
		    (lambda (this-paren-type pos current-paren-type)
		      (let ([new-start (+ pos (string-length
					       (cdr (vector-ref
						     parens this-paren-type))))])
			(vector-set! close-spots this-paren-type
				     (search-esc 
				      edit new-start end-pos
				      (cdr (vector-ref parens this-paren-type)) 1))
			(if (= this-paren-type current-paren-type) 
			    new-start
			    #f)))]
		   [do-quote
		    (lambda (quote-type pos paren-type)
		      (let ([new-pos 
			     (search-esc edit (+ pos (string-length
						      (car (vector-ref 
							    quotes quote-type))))
					 end-pos
					 (cdr (vector-ref quotes quote-type)) 1)])
			(if new-pos
			    (let ([new-start 
				   (+ new-pos (string-length 
					       (cdr (vector-ref quotes quote-type))))])
			      (initialize new-start)
			      (find-match new-start paren-type))
			    #f)))]
		   [do-eol
		    (lambda (eol-type pos paren-type)
		      (let ([new-pos
			     (search-esc edit (+ pos (string-length
						      (vector-ref eols eol-type)))
					 end-pos
					 newline-string 1)])
			(if new-pos
			    (begin (initialize new-pos)
				   (find-match new-pos paren-type))
			    #f)))]
		   [initialize
		    (lambda (pos)
		      (letrec([this (lambda (x) x)]
			      [do-vector
			       (lambda (spots strings type)
				 (letrec 
				     ([iterator
				       (lambda (index)
					 (if (= index (vector-length spots)) #t
					     (let ([curr (vector-ref spots index)])
					       (if 
						(and curr (< curr pos))
						(vector-set! 
						 spots index
						 (search-esc edit pos end-pos
							     (type 
							      (vector-ref strings index))
							     1)))
					       (iterator (add1 index)))))])
				   (iterator 0)))])	
			(do-vector open-spots parens car)
			(do-vector close-spots parens cdr)
			(do-vector quote-spots quotes car)
			(do-vector eol-spots eols this)))]
		   [get-min-ref
		    (lambda (vec index)
		      (if (= index (vector-length vec)) 
			  (cons #f (add1 end-pos))
			  (let ([this (vector-ref vec index)]
				[rest (get-min-ref vec (add1 index))])
			    (if 
			     (or (not (car rest))
				 (and this (< this (cdr rest))))
			     (cons index (if this this (add1 end-pos))) 
			     rest))))]
		   [find-match
		    (lambda (pos paren-type)
		      (let* ([next-open (get-min-ref open-spots 0)]
			     [next-close (get-min-ref close-spots 0)]
			     [next-quote(get-min-ref quote-spots 0)]
			     [next-eol(get-min-ref eol-spots 0)]
			     [next-pos (min (cdr next-open)(cdr next-close)
					    (cdr next-quote)(cdr next-eol))])
			(cond
			 [(> next-pos end-pos) #f]
			 [(= (cdr next-open) next-pos)
			  (let ([known (if paren-cache
					   (send paren-cache get next-pos)
					   #f)])
			    (if known
				(begin
				  (initialize known)
				  (find-match known paren-type))
				(let*([new-type 
				       (vector-ref parens (car next-open))]
				      [nest (do-open (+ next-pos (string-length
								  (car new-type)))
						     (car next-open))])
				  (if nest
				      (begin
					(if paren-cache
					    (send paren-cache put next-pos nest))
					(find-match nest paren-type))
				      #f))))]
			 [(= (cdr next-close) next-pos)
			  (do-close (car next-close) next-pos paren-type)]
			 [(= (cdr next-quote) next-pos)
			  (do-quote (car next-quote) next-pos paren-type)]
			 [(= (cdr next-eol) next-pos)
			  (do-eol (car next-eol) next-pos paren-type)])))])
	    (let* ([pos 
		    (if pos
			(let skip-eols ([pos pos])
			  (if (or (not pos)(>= pos end-pos))
			      end-pos
			      (let ([ew-pos (skip-whitespace edit pos 1)])
				(initialize ew-pos)
				(if (= (cdr (get-min-ref eol-spots 0)) ew-pos)
				    (skip-eols
				     (search-esc edit ew-pos end-pos 
						 (string #\newline) 1))
				    ew-pos))))
			pos)]
		   [first-open (get-min-ref open-spots 0)]
		   [first-close (get-min-ref  close-spots 0)]
		   [first-quote (get-min-ref quote-spots 0)])
	      (cond [(not pos) #f]
		    [(>= pos end-pos) (add1 end-pos)]
		    [(= (cdr first-open) pos)
		     (let ([new-start 
			    (+ pos (string-length 
				    (car (vector-ref parens
						     (car first-open)))))])
		       (vector-set! open-spots (car first-open) 
				    (search-esc 
				     edit new-start end-pos
				     (car(vector-ref parens(car first-open))) 1))
		       (find-match new-start (car first-open)))]
		    [(= (cdr first-close) pos) #f]
		    [(= (cdr first-quote) pos)
		     (let ([closer (search-esc 
				    edit (+ pos (string-length
						 (car (vector-ref 
						       quotes (car first-quote)))))
				    end-pos
				    (cdr (vector-ref quotes 
						     (car first-quote))) 1)])
		       (if closer (+ closer (string-length 
					     (cdr (vector-ref quotes 
							      (car first-quote)))))
			   #f))]
		    [else (min
			   (let ([next-newline (search-esc edit pos end-pos 
							   newline-string 1)])
			     (if next-newline next-newline end-pos))
			   (let ([next-space (search-esc edit pos end-pos " " 1)])
			     (if next-space next-space end-pos))
			   (cdr (get-min-ref eol-spots 0))
			   (cdr first-open)
			   (cdr first-close)
			   (cdr first-quote))]))))))

    
    (define backward-match
      (opt-lambda (edit pos first-pos
			paren-pairs quote-pairs eol-comment-list 
			[contain-flag #f]
			[paren-cache #f])
	(let* ([end-pos (send edit last-position)]
	       [open-spots (make-vector (length paren-pairs) (add1 end-pos))]
	       [close-spots (make-vector (length paren-pairs) (add1 end-pos))]
	       [quote-spots (make-vector (length quote-pairs) (add1 end-pos))]
	       [newline-spot (add1 end-pos)]
	       [parens (list->vector paren-pairs)]
	       [quotes (list->vector quote-pairs)]
	       [newline newline-string]
	       [eol-spots (make-vector (length eol-comment-list)(add1 end-pos))]
	       [eol-temp-spots (make-vector (length eol-comment-list))]
	       [op-quote-temp-spots (make-vector (length quote-pairs))]
	       [cl-quote-temp-spots (make-vector (length quote-pairs))]
	       [eols (list->vector eol-comment-list)]
	       [cache-closes (list (list))]
	       [cache-opens (list #f)])
	  (letrec 
	      ([this (lambda (x) x)]
	       [do-close
		(lambda (pos paren-type)
		  (vector-set! close-spots paren-type
			       (search-esc 
				edit pos first-pos 
				(cdr (vector-ref parens paren-type)) -1))
		  (set-car! cache-closes (cons pos (car cache-closes)))
		  (set! cache-closes (cons () cache-closes))
		  (set! cache-opens (cons #f cache-opens))
		  (find-match pos paren-type))]
	       
	       [do-open
		(lambda (this-paren-type pos current-paren-type)
		  (vector-set! open-spots this-paren-type
			       (search-esc
				edit pos first-pos
				(car (vector-ref parens this-paren-type)) -1))
		  (when (car cache-opens)
		    (let leave-loop ([closes (car cache-closes)])
		      (if (null? closes)
			  #t
			  (begin
			    (if paren-cache
				(send paren-cache put  
				      (car closes)(car cache-opens)))
			    (leave-loop (cdr closes))))))
		  
		  (set! cache-closes (cdr cache-closes))
		  (set! cache-opens (cdr cache-opens))
		  (if (not current-paren-type)
		      (+ pos (string-length 
			      (car (vector-ref parens this-paren-type))))
		      (begin (set-car! cache-opens pos)
			     (if (= this-paren-type current-paren-type)
				 pos
				 #f))))] ;;maybe dump another cache level here???
	       [do-quote
		(lambda (quote-type pos paren-type)
		  (let ([new-pos (search-esc 
				  edit pos first-pos
				  (car (vector-ref quotes quote-type)) -1)])
		    (if new-pos
			(begin 
			  (initialize new-pos)
			  (let* ([line-start (search-esc edit new-pos
							 first-pos newline -1)]
				 [line-start (if line-start line-start 0)]
				 [new-new-pos (do-newline line-start new-pos)])
			    (if (< new-new-pos new-pos)
				(do-quote quote-type new-new-pos paren-type)
				(find-match new-pos paren-type))))
			#f)))]
	       [do-vector
		(lambda (spots strings type pos limit-pos dir)
		  (letrec ([iterator
			    (lambda (index)
			      (if (= index (vector-length spots)) 
				  #t
				  (let ([curr (vector-ref spots index)])
				    (if (and curr ((if (= dir 1) <= >=) curr pos))
					(vector-set!
					 spots index
					 (search-esc edit pos limit-pos
						     (type
						      (vector-ref strings index)) dir)))
				    (iterator (add1 index)))))]) 
		    (iterator 0)))]
	       [initialize
		(lambda (pos)
		  (do-vector open-spots parens car pos first-pos -1)
		  (do-vector close-spots parens cdr pos first-pos -1)
		  (do-vector quote-spots quotes cdr pos first-pos -1)
		  (do-vector eol-spots eols this pos first-pos -1)
		  (if (>= newline-spot pos)
		      (set! newline-spot
			    (let ([res (search-esc edit pos first-pos
						   newline -1)])
			      (if res (if (= res 0) 0 res) -1)))))]
	       [get-min-ref
		(lambda (vec index)
		  (if (= index (vector-length vec)) 
		      (cons #f end-pos)
		      (let ([this (vector-ref vec index)]
			    [rest (get-min-ref vec (add1 index))])
			(if 
			 (or (not (car rest))
			     (and this (< this (cdr rest))))
			 (cons index (if this this end-pos)) 
			 rest))))]
	       [get-max-ref
		(lambda (vec index)
		  (if (= index (vector-length vec)) 
		      (cons #f -1)
		      (let ([this (vector-ref vec index)]
			    [rest (get-max-ref vec (add1 index))])
			(if 
			 (or (not (car rest))
			     (and this (> this (cdr rest))))
			 (cons index (if this this -1)) 
			 rest))))]
	       [do-newline
		(lambda (line-start line-end)
		  ;(display line-start)
		  (let ([needed (get-max-ref eol-spots 0)])
		    ;(display line-start)
		    (if 
		     (and line-start (car needed)(>= (cdr needed) line-start))
		     (begin
		       (vector-fill! eol-temp-spots -1)
		       (do-vector eol-temp-spots eols this 
				  line-start line-end 1)
		       (vector-fill! op-quote-temp-spots -1)
		       (do-vector op-quote-temp-spots quotes car 
				  line-start line-end 1)
		       (vector-fill! cl-quote-temp-spots -1)
		       (do-vector cl-quote-temp-spots quotes cdr 
				  line-start line-end 1)
		       (let* ([first (get-min-ref eol-temp-spots 0)]
			      [op-quote (get-min-ref op-quote-temp-spots 0)]
			      [cl-quote (get-min-ref cl-quote-temp-spots 0)])
			 (cond
			  [(= end-pos (cdr op-quote)(cdr cl-quote))
			   (begin (initialize (cdr first)) (cdr first))]
			  [(< (cdr op-quote)(cdr first))
			   (do-newline
			    (search-esc edit (add1 (cdr op-quote)) line-end
					(car (vector-ref quotes (car op-quote))) 1)
			    line-end)]
			  [(= (cdr op-quote)(cdr cl-quote))
			   (begin (initialize (cdr first)) (cdr first))]
			  [(< (cdr cl-quote)(cdr first))
			   (do-newline (add1 (cdr cl-quote) line-end))]
			  [(= (cdr cl-quote) end-pos)
			   (begin (initialize (cdr first)) (cdr first))]
			  [else
			   (let*
			       ([quote-type (vector-ref quotes (car cl-quote))]
				[next-op 
				 (search-esc edit (sub1 (cdr cl-quote))
					     first-pos (car quote-type) -1)]
				[next-cl
				 (search-esc edit (sub1 (cdr cl-quote))
					     first-pos (cdr quote-type) -1)])
			     (if (and next-op
				      (< next-op (cdr first))
				      (or (not next-cl)
					  (> next-op next-cl)))
				 (do-newline (add1 (cdr cl-quote)) line-end)
				 (begin (initialize (cdr first)) (cdr first)
					)))])))
		     (let ([newnewline
			    (search-esc edit line-end first-pos newline -1)])
		       (set! newline-spot (if newnewline newnewline -1))
		       line-end))))]     
	       [find-match
		(lambda (pos paren-type)
		  (let* ([next-open (get-max-ref open-spots 0)]
			 [next-close (get-max-ref close-spots 0)]
			 [next-quote(get-max-ref quote-spots 0)]
			 [next-pos (max (cdr next-open)(cdr next-close)
					(cdr next-quote) newline-spot)])
		    (cond
		     [(< next-pos first-pos)  #f]
		     [(= (cdr next-open) next-pos)
		      (do-open (car next-open) next-pos paren-type)]
		     [(= (cdr next-close) next-pos)
		      (let ([known (if paren-cache 
				       (send paren-cache get next-pos)
				       #f)])
			(if known
			    (begin (initialize known)(find-match known paren-type))
			    (let ([nest (do-close next-pos (car next-close))])
			      (if nest
				  (find-match nest paren-type)
				  #f))))]
		     [(= (cdr next-quote) next-pos)
		      (do-quote (car next-quote) next-pos paren-type)]
		     [(= newline-spot next-pos)
		      (let ([line-start (search-esc edit (sub1 next-pos)
						    first-pos newline -1)])
			(find-match
			 (do-newline (if line-start line-start 0) 
				     newline-spot)
			 paren-type))])))])
	    (if (> pos end-pos)
		#f
		(begin
		  (initialize pos)
		  (let startup ([pos pos][first? #t])
		    (let* ([line-start (search-esc edit pos first-pos newline -1)]
			   [newline-pos 
			    (do-newline (if line-start line-start 0)
					pos)]
			   [old-pos pos]
			   [pos (skip-whitespace edit pos -1)])
		      (cond 
		       [(and first? (< newline-pos pos)) old-pos]
		       [(< newline-pos pos)
			(startup (skip-whitespace edit newline-pos -1) #f)]
		       [(< (send edit position-line pos)
			   (send edit position-line old-pos))
			(startup pos #f)]
		       [(= pos first-pos) #f]
		       [contain-flag (find-match pos #f)]
		       [else
			(let*([first-open (get-max-ref open-spots 0)]
			      [first-close (get-max-ref  close-spots 0)]
			      [first-quote (get-max-ref quote-spots 0)])
			  (cond 
			   [(= (cdr first-close) 
			       (- pos(string-length 
				      (cdr (vector-ref parens(car first-close))))))
			    (let ([new-start 
				   (- pos(string-length 
					  (cdr(vector-ref parens(car first-close)))
					  ))])
			      (vector-set! 
			       close-spots (car first-close)
			       (search-esc
				edit new-start first-pos
				(cdr(vector-ref parens(car first-close))) -1))
			      (set-car! cache-closes(cons new-start 
							  (car cache-closes)))
			      (set! cache-closes (cons () cache-closes))
			      (set! cache-opens (cons #f cache-opens))
			      (let ([answer
				     (find-match new-start (car first-close))])
				(if paren-cache
				    (send paren-cache put  new-start answer))
				answer))]
			   [(= (cdr first-open) 
			       (- pos (string-length
				       (car (vector-ref parens 
							(car first-open))))))
			    (cdr first-open)]
			   [(= (cdr first-quote) 
			       (- pos (string-length
				       (cdr(vector-ref quotes
						       (car first-quote))))))
			    (search-esc edit (sub1 pos) first-pos
					(car (vector-ref quotes 
							 (car first-quote)))
					-1)]
			   [else  
			    (max
			     (let ([next-newline 
				    (search-esc edit pos first-pos newline -1)])
			       (add1 (if next-newline next-newline -1)))
			     (let ([next-space
				    (search-esc edit pos first-pos " " -1)])
			       (add1 (if next-space next-space -1)))
			     (+ (cdr first-open) 
				(string-length 
				 (car (vector-ref parens(car first-open)))))
			     (+(cdr first-close)
			       (string-length
				(cdr(vector-ref parens(car first-close)))))
			     (+(cdr first-quote)
			       (string-length
				(cdr(vector-ref quotes(car first-quote))))))]
			   ))]))))))))))


	    
			 


