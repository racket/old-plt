
(module embed-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "moddep.ss" "syntax"))

  (provide compiler:embed@)

  (define compiler:embed@
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

      (define (get-code filename module-path codes verbose?)
	(when verbose?
	  (fprintf (current-error-port) "Getting ~s~n" filename))
	(let ([normal (normal-case-path (simplify-path (expand-path filename)))])
	  (if (assoc normal (unbox codes))
	      'done ; already got this module
	      (let ([code (get-module-code filename)])
		(set-box! codes
			  (cons (list normal module-path code)
				(unbox codes)))
		(let-values ([(imports fs-imports) (module-compiled-imports code)])
		  (for-each (lambda (i)
			      (unless (symbol? i)
				(get-code (resolve-module-path-index i filename)
					  (collapse-module-path-index i module-path)
					  codes
					  verbose?)))
			    (append imports fs-imports)))))))
      
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
      (define (make-embedding-executable dest mred? verbose? module-paths cmdline)
	(unless ((apply + (length cmdline) (map string-length cmdline)) . < . 50)
	  (error 'make-embedding-executable "command line too long"))
	(let ([files (map
		      (lambda (mp)
			(let ([f (resolve-module-path mp #f)])
			  (unless f
			    (error 'make-embedding-executable "bad module path: ~e" mp))
			  f))
		      module-paths)]
	      [collapsed-mps (map
			      (lambda (mp)
				(collapse-module-path mp "."))
			      module-paths)]
	      ;; As we descend the module tree, we append to the front, so
	      ;; this list will need to be reversed.
	      ;; Each element is (list filename collapsed-module-path code)
	      [codes (box null)])
	  (for-each (lambda (f mp) (get-code f mp codes verbose?)) 
		    files
		    collapsed-mps)
	  (let ([exe (find-exe mred?)])
	    (when verbose?
	      (fprintf (current-error-port) "Copying to ~s~n" dest))
	    (when (file-exists? dest)
	      (delete-file dest))
	    (copy-file exe dest)
	    (with-handlers ([void (lambda (x)
				    (when (file-exists? dest)
				      (delete-file dest))
				    (raise x))])
	      (let ([start (file-size dest)])
		(call-with-output-file* 
		 dest
		 (lambda (o)
		   (write '(current-module-name-prefix '#%embedded:) o)
		   (let ([l (reverse (unbox codes))])
		     (for-each
		      (lambda (nc)
			(fprintf (current-error-port) "Writing ~s~n" (car nc))
			(write (caddr nc) o))
		      l))
		   `(write '(current-module-name-prefix #f) o))
		 'append)
		(let ([end (file-size dest)]
		      [cmdpos (with-input-from-file* dest find-cmdline)])
		  (when verbose?
		    (fprintf (current-error-port) "Setting command line~n"))
		  (let ([out (open-output-file dest 'update)]
			[start-s (number->string start)]
			[end-s (number->string end)])
		    (dynamic-wind
		     void
		     (lambda ()
		       (file-position out cmdpos)
		       (display "!" out)
		       (for-each
			(lambda (s)
			  (fprintf out "~c~a~c"
				   (integer->char (add1 (string-length s))) s #\000))
			(list* "-k" start-s end-s cmdline))
		       (display #\000 out))
		     (lambda ()
		       (close-output-port out)))))))))))))
