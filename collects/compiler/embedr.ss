
(unit/sig (make-embedding-executable)
  (import)

  ;; Find executable via (find-system-path 'exec-file), then
  ;;  fixup name to be MrEd or MzScheme
  (define (find-exe mred?)
    (let* ([sp (find-system-path 'exec-file)]
	   [exe (find-executable-path sp #f)]
	   [fail
	    (lambda ()
	      (error 'make-embedding-executable
		     "can't find ~a executable"
		     (if mred? "MrEd" "MzScheme")))])
      (unless exe (fail))
      (let-values ([(base name dir?) (split-path exe)])
	(let* ([mr (regexp-match
		    "^(.*)([Mm][Rr][Ee][Dd])(.*)$"
		    name)]
	       [mz (regexp-match
		    "^(.*)([Mm][Zz][Ss][Cc][Hh][Ee][Mm][Ee])(.*)$"
		    name)]
	       [r (or mr mz)])
	  (unless r (fail))
	  (let ([exe
		 (build-path base
			     (string-append (cadr r)
					    (if mred?
						"mred"
						"mzscheme")
					    (cadddr r)))])
	    (unless (file-exists? exe)
	      (fail))
	    exe)))))

  (define (add-files files dest)
    (let ([in (cond
	       [(port? files) files]
	       [(string? files) (open-input-file files)]
	       [(list? files) (begin0
			       (open-input-file (car files))
			       (set! files (cdr files)))])]
	  [out (open-output-file dest 'append)]
	  [s (make-string 1024)])
	(let ([count
	       (let loop ([c 0])
		 (let ([n (read-string-avail! s in)])
		   (if (eof-object? n)
		       (if (pair? files)
			   (begin
			     (close-input-port in)
			     (set! in (open-input-file (car files)))
			     (set! files (cdr files))
			     (loop c))
			   c)
		       (begin
			 (display (if (= n (string-length s)) s (substring s 0 n))
				  out)
			 (loop (+ c n))))))])
	  (close-input-port in)
	  (close-output-port out)
	  count)))

  ;; Find the magic point in the binary:
  (define (find-cmdline)
    (define magic (string->list "[Replace me for EXE hack"))
    (let loop ([pos 0][l magic])
      (cond
       [(null? l) (- pos (length magic))]
       [else (let ([c (read-char)])
	       (when (eof-object? c)
		 (error 
		  'make-embedding-executable
		  (format
		   "can't find cmdline position in executable")))
	       (if (eq? c (car l))
		   (loop (add1 pos) (cdr l))
		   (loop (add1 pos) magic)))])))

  ;; The main function (see doc.txt):
  (define (make-embedding-executable dest mred? files cmdline)
    (unless (< (apply + (length cmdline) (map string-length cmdline)) 50)
      (error 'make-embedding-executable "command line too long"))
    (for-each (lambda (f)
		(unless (file-exists? f)
		  (error 'make-embedding-executable "can't find file: ~s" f)))
	      files)
    (let ([exe (find-exe mred?)])
      (when (file-exists? dest)
	(delete-file dest))
      (copy-file exe dest)
      (with-handlers ([void
		       (lambda (x)
			 (when (file-exists? dest)
			   (delete-file dest))
			 (raise x))])
	(let ([start (file-size dest)]
	      [count (add-files files dest)]
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


      
      
    