(module servlet-integrity mzscheme

  (require (lib "file.ss"))
  (require (lib "list.ss"))

  (provide run-servlet-check)

  (define num-errors 0)

  (define (partition ls pred?)
    (let loop ([the-partition '(() ())]
	       [elts ls])
      (if (null? elts)
	  (map reverse the-partition)
	  (let* ([elt (car elts)]
		 [left (car the-partition)]
		 [right (cadr the-partition)]
		 [new-partition
		  (if (pred? elt)
		      (list (cons elt left)
			    right)
		      (list left
			    (cons elt right)))])
		(loop new-partition (cdr elts))))))

  (define (run-servlet-check)
    (check-dir (simplify-path 
		(build-path (collection-path "mzlib") 'up 
			    "help" "servlets")))
    (if (> num-errors 0)
	  (fprintf (current-error-port)
		   "*** There were ~a servlet load errors ***~n"
		   num-errors)
	  (printf "There were no servlet load errors~n")))
	      
  (define (check-dir dir)
    (let* ([all-files 
	    (map (lambda (s)
		   (build-path dir s))
		 (filter (lambda (f)
			   (not (string=? f "CVS")))
			 (directory-list dir)))]
	   [directories-and-files
	    (partition all-files directory-exists?)]
	   [dirs (car directories-and-files)]
	   [files (cadr directories-and-files)]
	   [servlets (filter (lambda (f)
			       (let ([len (string-length f)])
				 (string=?
				  (substring f (- len 3) len)
				  ".ss")))
			     files)])
      (for-each 
       (lambda (s) 
	 (printf "Checking servlet: ~a~n" s)
	 (with-handlers
	  ([void (lambda (exn)
		   (set! num-errors (add1 num-errors))
		   (fprintf (current-error-port)
			    "*** Load error *** in servlet ~a~n" s))])
	  (load s)))
       servlets)
      (for-each check-dir dirs))))


					     
