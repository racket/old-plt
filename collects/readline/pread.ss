
(module pread mzscheme
  (require "readline.ss")

  (define .history "~/.mzrl.history")
  (define MAX-HISTORY 100)
  (define leftovers null)

  (define local-history
    (with-handlers ([void (lambda (exn) null)])
      (with-input-from-file .history
	(lambda () (read)))))

  (define (do-readline p)
    (let ([s (readline p)])
      (when (string? s)
	(add-history s)
	(if (= (length local-history) MAX-HISTORY)
	    (set! local-history (cdr local-history)))
	(set! local-history (append local-history (list s))))
      s))

  (define (save-history)
    (with-handlers ([void void])
      (with-output-to-file .history
	(lambda () (write local-history))
	'truncate)))

  (exit-handler (let ([old (exit-handler)])
		  (lambda (v)
		    (save-history)
		    (old v))))
  
  (for-each add-history local-history)

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
		  (let* ([p (open-input-string s)]
			 [rs (let loop ()
			       (let ([r (read p)])
				 (if (eof-object? r)
				     null
				     (cons r (loop)))))])
		    (if (null? rs)
			(big-loop)
			(begin0
			 (car rs)
			 (set! leftovers (cdr rs)))))))))))

  (provide prompt-read-using-readline))
