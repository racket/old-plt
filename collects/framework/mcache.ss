; Cache parenthesis-matching
;  Implemented as a splay tree

(unit/sig framework:match-cache^
  (import)
  
  (define-struct node (left right pos jump-to))
  
  (define %
    (class object% ()
      (private
	[tree #f]
	[offset 0])
      (public
	[max-count 0]
	[times 0]
	[sum 0]
	[splay
	 (lambda (pos)
	   (let* ([count 0]
		  [ans
		   (let/ec exit
		     (unless tree
		       (exit #f))
		     (let* ([n (make-node #f #f 0 0)]
			    [r n]
			    [l n]
			    [break-at
			     (lambda (t)
			       (set-node-right! l (node-left t))
			       (set-node-left! r (node-right t))
			       (set-node-left! t (node-right n))
			       (set-node-right! t (node-left n))
			       (set! tree t)
			       (exit #f))])
		       (let loop ([t tree])
			 (set! count (add1 count))
			 (let* ([npos (node-pos t)])
			   (cond
			     ((< pos npos)
			      (let ([left (node-left t)])
				(if (not left)
				    (break-at t)
				    (begin
				      (when (< pos (node-pos left))
					(set-node-left! t
							(node-right 
							 left))
					(set-node-right! left t)
					(if (not (node-left left))
					    (break-at left)
					    (set! t left)))
				      (set-node-left! r t)
				      (set! r t)
				      (loop (node-left t))))))
			     ((> pos npos)
			      (let ([right (node-right t)])
				(if (not right)
				    (break-at t)
				    (begin
				      (when (> pos (node-pos right))
					(set-node-right! t
							 (node-left 
							  right))
					(set-node-left! right t)
					(if (not (node-right right))
					    (break-at right)
					    (set! t right)))
				      (set-node-right! l t)
				      (set! l t)
				      (loop (node-right t))))))
			     (else
			      (break-at t)))))))])
	     (when (< max-count count)
	       (set! max-count count))
	     (set! times (+ 1 times))
	     (set! sum (+ count sum))
	     ;(printf "count: ~a ~a ~a~n" count max-count (exact->inexact (/ sum times)))
	     ans))]
	[put
	 (lambda (pos jump-to)
	   (let ([pos (- pos offset)]
		 [jump-to (if jump-to (- jump-to offset) #f)])
	     ;(printf "put: ~a ~a~n" pos jump-to)
	     (splay pos)
	     (if tree
		 (let ([tpos (node-pos tree)])
		   (if (= pos tpos)
		       (set-node-jump-to! tree jump-to)
		       (let ([new (make-node #f #f pos jump-to)])
			 (if (< pos tpos)
			     (begin
			       (set-node-left! new (node-left tree))
			       (set-node-right! new tree)
			       (set-node-left! tree #f))
			     (begin
			       (set-node-right! new (node-right tree))
			       (set-node-left! new tree)
			       (set-node-right! tree #f)))
			 (set! tree new))))
		 (set! tree (make-node #f #f pos jump-to)))))]
	[get
	 (lambda (pos)
	   (let ([pos (- pos offset)])
	     (splay pos)
	     (let ([ans 
		    (if tree
			(let ([tpos (node-pos tree)])
			  (if (= pos tpos)
			      (let ([jump-to (node-jump-to tree)])
				(if jump-to
				    (+ jump-to offset)
				    #f))
			      #f))
			#f)])
	       ;(printf "get: ~a~n" ans)
	       ans)))]
	[delete
	 (lambda (pos)
	   (let ([pos (- pos offset)])
	     (when tree
	       (splay pos)
	       (if (= pos (node-pos tree))
		   (if (node-left tree)
		       (begin
			 (let ([right (node-right tree)])
			   (set! tree (node-left tree))
			   (splay pos)
			   (set-node-right! tree right)))
		       (set! tree (node-right tree)))))))]
	[invalidate
	 (lambda (pos)
	   ;(printf "invalidate~n")
	   (when tree	
	     (splay pos)
	     (if (<= pos (node-pos tree))
		 (set! tree (node-left tree))
		 (set-node-right! tree #f))))]
	[forward-invalidate
	 (lambda (pos adjust)
	   ;(printf "forward-invalidate~n")
	   (when tree	
	     (let ([pos (- pos offset)])
	       (splay pos)
	       (if (>= pos (node-pos tree))
		   (set! tree (node-right tree))
		   (set-node-left! tree #f))
	       (set! offset (+ offset adjust)))))]
	[contents
	 (lambda ()
	   (let loop ([tree tree])
	     (if tree
		 (list (loop (node-left tree))
		       (node-pos tree)
		       (loop (node-right tree)))
		 '())))])
      (sequence (super-init)))))

