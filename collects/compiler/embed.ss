
;; This function copies the current binary, appends code to the copy,
;;  then sets the internal command-line hack-string to read the
;;  appended code when the binary is run. The user can also supply
;;  additional command-line arguments as a list of strings; '("-mvq-")
;;  should be a popular choice.

(define (make-embedding-executable dest file cmdline)
  (unless (< (apply + (length cmdline) (map string-length cmdline)) 50)
    (error 'make-embedding-executable "command line too long"))
  (let* ([sp (find-system-path 'exec-file)]
	 [exe (find-executable-path sp sp)])
    (copy-file exe dest)
    (let ([start (file-size dest)]
	  [in (if (port? file)
		  file
		  (open-input-file file))]
	  [out (open-output-file dest 'append)]
	  [s (make-string 1024)])
      (let loop ()
	(let ([n (read-string-avail! s in)])
	  (unless (eof-object? n)
	    (display (if (= n (string-length s)) s (substring s 0 n))
		     out)
	    (loop))))
      (let ([count (file-position in)])
	(close-input-port in)
	(close-output-port out)
	(let* ([find-cmdline ; Find the magic start
		(lambda ()
		  (define magic (string->list "[Replace me for EXE hack"))
		  (let loop ([pos 0][l magic])
		    (cond
		     [(null? l) (- pos (length magic))]
		     [else (let ([c (read-char)])
			     (when (eof-object? c)
			       (when (file-exists? dest)
				 (delete-file dest))
			       (error 
				'make-embedding-executable
				(format
				 "Couldn't find cmdline position in executable")))
			     (if (eq? c (car l))
				 (loop (add1 pos) (cdr l))
				 (loop (add1 pos) magic)))])))]
	       [cmdpos (with-input-from-file dest find-cmdline)])
	  (let ([out (open-output-file dest 'update)]
		[start-s (number->string start)]
		[end-s (number->string (+ start count))])
	    (file-position out cmdpos)
	    (display "!" out)
	    (for-each
	     (lambda (s)
	       (fprintf out "~c~a~c"
			(integer->char (add1 (string-length s))) s #\000))
	     (list* "-k" start-s end-s cmdline))
	    (display #\000 out)
	    (close-output-port out)))))))

      
      
    