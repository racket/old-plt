
(module pread mzscheme
  (require "readline.ss"
	   (lib "file.ss"))

  (define MAX-HISTORY 100)
  (define leftovers null)

  (define counter 1)

  (define local-history (get-preference 'mzrl-history (lambda () null)))

  (define (do-readline p)
    (let ([s (readline p)])
      (when (string? s)
	(add-history s)
	(if (= (length local-history) MAX-HISTORY)
	    (set! local-history (cdr local-history)))
	(set! local-history (append local-history (list s))))
      s))

  (define (save-history)
    (put-preferences '(mzrl-history) (list local-history)))

  (exit-handler (let ([old (exit-handler)])
		  (lambda (v)
		    (save-history)
		    (old v))))
  
  (for-each add-history (if (list? local-history) local-history null))

  (define (prompt-read-using-readline get-prompt)
    (if (pair? leftovers)
	(begin0
	 (car leftovers)
	 (set! leftovers (cdr leftovers)))
	(let big-loop ()
	  (let loop ([s (do-readline (get-prompt 0))][next-pos 1])
	    (if (eof-object? s)
		(begin
		  (save-history)
		  s)
		(with-handlers ([exn:read:eof?
				 (lambda (exn)
				   (loop (string-append 
					  s
					  (string #\newline)
					  (do-readline (get-prompt next-pos)))
					 (add1 next-pos)))])
		  (let ([p (open-input-string s)])
		    (port-count-lines! p)
		    (let ([rs (let loop ()
				(let ([r (read-syntax (format "repl-~a" counter) p)])
				  (if (eof-object? r)
				      null
				      (cons r (loop)))))])
		      (if (null? rs)
			  (big-loop)
			  (begin0
			   (car rs)
			   (set! counter (add1 counter))
			   (set! leftovers (cdr rs))))))))))))

  (provide prompt-read-using-readline))
