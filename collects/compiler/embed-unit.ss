
(module embed-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "list.ss")
	   (lib "thread.ss")
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

      (define (data-fork-size dest)
	(if (eq? (system-type) 'macos)
	    ;; Can't use `file-size', because that includes the data fork.
	    (let ([p (open-input-file dest)]
		  [s (make-string 4096)])
	      (let loop ()
		(if (eof-object? (read-string-avail! s p))
		    (begin0
		     (file-position p)
		     (close-input-port p))
		    (loop))))
	    ;; File has only a "data fork":
	    (file-size dest)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Represent modules with lists starting with the filename, so we
      ;; can use assoc:
      (define (make-mod normal-file-path normal-module-path code name prefix full-name relative-mappings)
	(list normal-file-path normal-module-path code
	      name prefix full-name relative-mappings))

      (define (mod-file m) (car m))
      (define (mod-mod-path m) (cadr m))
      (define (mod-code m) (caddr m))
      (define (mod-name m) (list-ref m 3))
      (define (mod-prefix m) (list-ref m 4))
      (define (mod-full-name m) (list-ref m 5))
      (define (mod-mappings m) (list-ref m 6))
      
      (define re:suffix (regexp "\\..?.?.?$"))

      (define (generate-prefix)
	(format "#%embedded:~a:" (gensym)))
      
      (define (normalize filename)
	(normal-case-path (simplify-path (expand-path filename))))

      ;; Loads module code, using .zo if there, compiling from .scm if not
      (define (get-code filename module-path codes prefixes verbose?)
	(when verbose?
	  (fprintf (current-error-port) "Getting ~s~n" filename))
	(let ([a (assoc filename (unbox codes))])
	  (if a
	      ;; Already have this module. Make sure that library-referenced
	      ;;  modules are consistently referenced through library paths:
	      (let ([found-lib? (and (pair? (mod-mod-path a))
				     (eq? 'lib (car (mod-mod-path a))))]
		    [look-lib? (and (pair? module-path)
				    (eq? 'lib (car module-path)))])
		(cond
		 [(and found-lib? look-lib?)
		  'ok]
		 [(or found-lib? look-lib?)
		  (error 'find-module
			 "module referenced both as a library and through a path: ~a"
			 filename)]
		 [else 'ok]))
	      ;; First use of the module. Get code and then get code for imports.
	      (let ([code (get-module-code filename)])
		(let-values ([(imports fs-imports) (module-compiled-imports code)])
		  (let ([name (let-values ([(base name dir?) (split-path filename)])
				(regexp-replace re:suffix name ""))]
			[prefix (let ([a (assoc filename prefixes)])
				  (if a
				      (cdr a)
				      (generate-prefix)))]
			[all-file-imports (filter (lambda (x) (not (symbol? x)))
						  (append imports fs-imports))])
		    (let ([sub-files (map (lambda (i) (normalize (resolve-module-path-index i filename)))
					  all-file-imports)]
			  [sub-paths (map (lambda (i) (collapse-module-path-index i module-path))
					  all-file-imports)])
		      ;; Get code for imports:
		      (for-each (lambda (sub-filename sub-path)
				  (get-code sub-filename
					    sub-path
					    codes
					    prefixes
					    verbose?))
				sub-files sub-paths)
		      ;; Build up relative module resolutions, relative to this one,
		      ;; that will be requested at run-time.
		      (let ([mappings (map (lambda (sub-i sub-filename)
					     (let-values ([(path base) (module-path-index-split sub-i)])
					       ;; Assert: base should refer to this module:
					       (let-values ([(path2 base2) (module-path-index-split base)])
						 (when (or path2 base2)
						   (error 'embed "unexpected nested module path index")))
					       (let ([m (assoc sub-filename (unbox codes))])
						 (cons path (mod-full-name m)))))
					   all-file-imports sub-files)])
			;; Record the module
			(set-box! codes
				  (cons (make-mod filename module-path code 
						  name prefix (string->symbol
							       (format "~a~a" prefix name))
						  mappings)
					(unbox codes)))))))))))
	    
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (make-module-name-resolver code-l)
	`(let ([orig (current-module-name-resolver)]
	       [ns (current-namespace)]
	       [mapping-table (quote
			       ,(map
				 (lambda (m)
				   `(,(mod-full-name m)
				     ,(mod-mappings m)))
				 code-l))]
	       [library-table (quote
			       ,(filter values
					(map (lambda (m)
					       (let ([path (mod-mod-path m)])
						 (if (and (pair? path)
							  (eq? 'lib (car path)))
						     (cons path (mod-full-name m))
						     #f)))
					     code-l)))])
	   (current-module-name-resolver
	    (lambda (name rel-to stx)
	      (if (or (not name)
		      (not (eq? (current-namespace) ns)))
		  ;; a notification,or wrong namespace
		  (orig name rel-to stx)
		  ;; Have a relative mapping?
		  (let ([a (assoc rel-to mapping-table)])
		    (if a
			(let ([a2 (assoc name (cadr a))])
			  (if a2
			      (cdr a2)
			      (error 'embedding-module-name-resolver
				     "unexpected relative mapping request: ~e in ~e"
				     name rel-to)))
			;; A library mapping that we have? 
			(let ([a3 (and (pair? name)
				       (eq? (car name) 'lib)
				       (ormap (lambda (lib-entry)
						(with-handlers ([not-break-exn? (lambda (x) #f)])
						  ;; To check equality of library references,
						  ;; we have to consider relative paths in the
						  ;; filename part of the name.
						  (let loop ([a (build-path
								 (apply build-path 
									'same
									(cddar lib-entry))
								 (cadar lib-entry))]
							     [b (build-path
								 (apply build-path 
									'same
									(let ([d (cddr name)])
									  (if (null? d)
									      '("mzlib")
									      d)))
								 (cadr name))])
						    (if (equal? a b)
							lib-entry
							(let-values ([(abase aname d?) (split-path a)])
							  (if (eq? aname 'same)
							      (loop abase b)
							      (let-values ([(bbase bname a?) (split-path b)])
								(if (eq? bname 'same)
								    (loop a bbase)
								    (if (equal? aname bname)
									(loop abase bbase)
									#f)))))))))
					      library-table))])
			  (if a3
			      ;; Have it:
			      (cdr a3)
			      ;; Let default handler try:
			      (orig name rel-to stx))))))))))

      ;; The main function (see doc.txt).
      (define (make-embedding-executable dest mred? verbose? 
					 modules 
					 literal-files literal-expression
					 cmdline)
	(unless ((apply + (length cmdline) (map string-length cmdline)) . < . 50)
	  (error 'make-embedding-executable "command line too long"))
	(let* ([module-paths (map cadr modules)]
	       [files (map
		       (lambda (mp)
			 (let ([f (resolve-module-path mp #f)])
			   (unless f
			     (error 'make-embedding-executable "bad module path: ~e" mp))
			   (normalize f)))
		       module-paths)]
	       [collapsed-mps (map
			       (lambda (mp)
				 (collapse-module-path mp "."))
			       module-paths)]
	       [prefix-mapping (map (lambda (f m)
				      (cons f (let ([p (car m)])
						(cond
						 [(symbol? p) (symbol->string p)]
						 [(eq? p #t) (generate-prefix)]
						 [(not p) '||]
						 [else (error
							'make-embedding-executable
							"bad prefix: ~e"
							p)]))))
				    files modules)]
	       ;; Each element is created with `make-mod'.
	       ;; As we descend the module tree, we append to the front after
	       ;; loasing imports, so the list in the right order.
	       [codes (box null)])
	  (for-each (lambda (f mp) (get-code f mp codes prefix-mapping verbose?)) 
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
	      (let ([start (data-fork-size dest)])
		(call-with-output-file* 
		 dest
		 (lambda (o)
		   ;; Install a module name resolver that redirects
		   ;; to the embedded modules
		   (write (make-module-name-resolver (unbox codes)) o)
		   (let ([l (unbox codes)])
		     (for-each
		      (lambda (nc)
			(when verbose?
			  (fprintf (current-error-port) "Writing module from ~s~n" (mod-file nc)))
			(write `(current-module-name-prefix ',(string->symbol (mod-prefix nc))) o)
			(write (mod-code nc) o))
		      l))
		   (write '(current-module-name-prefix #f) o)
		   (newline o)
		   (for-each (lambda (f)
			       (when verbose?
				 (fprintf (current-error-port) "Copying from ~s~n" f))
			       (call-with-input-file*
				f
				(lambda (i)
				  (copy-port i o))))
			     literal-files)
		   (when literal-expression
		     (write literal-expression o)))
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
