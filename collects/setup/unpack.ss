
(module unpack mzscheme

  (require (lib "etc.ss")
	   (lib "inflate.ss")
	   (lib "file.ss")
	   (lib "unit.ss")
	   (lib "base64.ss" "net"))

  ;; Returns a port and a kill thunk
  (define (port64gz->port p64gz)
    ;; Inflate in a thread so the whole input isn't read at once
    (let-values ([(base64-out base64-in) (make-pipe 4096)]
		 [(guz-out guz-in) (make-pipe 4096)])
      (let ([64t
	     (thread (lambda () 
		       (dynamic-wind
			   void
			   (lambda ()
			     (base64-decode-stream p64gz base64-in))
			   (lambda ()
			     (close-output-port base64-in)))))]
	    [gzt
	     (thread (lambda () 
		       (dynamic-wind 
			   void
			   (lambda ()
			     (gunzip-through-ports base64-out guz-in))
			   (lambda ()
			     (close-output-port guz-in)))))])
	(values guz-out
		(lambda ()
		  (kill-thread 64t)
		  (kill-thread gzt))))))

  (define (pretty-name f)
    (with-handlers ([void (lambda (x) f)])
      (let-values ([(base name dir?) (split-path f)])
	(format "~a in ~a" name base))))

  (define (unmztar p filter plthome print-status)
    (let loop ()
      (let ([kind (read p)])
	(unless (eof-object? kind)
	  (case kind
	    [(dir) (let ([s (apply build-path (read p))])
		     (unless (relative-path? s)
		       (error "expected a directory name relative path string, got" s))
		     (when (filter 'dir s plthome)
		       (let ([d (build-path plthome s)])
			 (unless (directory-exists? d)
			   (print-status
			    (format "  making directory ~a" (pretty-name d)))
			   (make-directory* d)))))]
	    [(file file-replace) 
	     (let ([s (apply build-path (read p))])
	       (unless (relative-path? s)
		 (error "expected a file name relative path string, got" s))
	       (let ([len (read p)])
		 (unless (and (number? len) (integer? len))
		   (error "expected a file name size, got" len))
		 (let* ([write? (filter kind s plthome)]
			[path (build-path plthome s)])
		   (let ([out (and write?
				   (if (file-exists? path)
				       (if (eq? kind 'file)
					   #f
					   (open-output-file path 'truncate))
				       (open-output-file path)))])
		     (when (and write? (not out))
		       (print-status (format "  skipping ~a; already exists" (pretty-name path))))
		     (when out
		       (print-status (format "  unpacking ~a" (pretty-name path))))
		     ;; Find starting *
		     (let loop ()
		       (let ([c (read-char p)])
			 (cond
			  [(char=? c #\*) (void)] ; found it
			  [(char-whitespace? c) (loop)]
			  [(eof-object? c) (void)] ; signal the error below
			  [else (error 
				 (format
				  "unexpected character setting up ~a, looking for *"
				  path)
				 c)])))
		     ;; Copy file data
		     (let loop ([n len])
		       (unless (zero? n)
			 (let ([c (read-char p)])
			   (when (eof-object? c)
			     (error (format 
				     "unexpected end-of-file while ~a ~a (at ~a of ~a)"
				     (if out "unpacking" "skipping")
				     path
				     (- len n -1) len)))
			   (when out
			     (write-char c out)))
			 (loop (sub1 n))))
		     (when out
		       (close-output-port out))))))]
	    [else (error "unknown file tag" kind)])
	  (loop)))))

  (define (call-info info flag mk-default test)
    (if info
	(let ([v (info flag mk-default)])
	  (test v)
	  v)
	(mk-default)))

  (define unpack 
    (opt-lambda (archive [plthome (current-directory)] [print-status (lambda (x) (printf "~a~n" x))])
      (let*-values ([(p64gz) (open-input-file archive)]
		    [(p kill) (port64gz->port p64gz)])
	(dynamic-wind
	    void
	    (lambda ()
	      (unless (and (eq? #\P (read-char p))
			   (eq? #\L (read-char p))
			   (eq? #\T (read-char p)))
		(error "not an unpackable distribution archive"))
	      (let* ([n (make-namespace)]
		     [info (let ([orig (current-namespace)])
			     (parameterize ([current-namespace n])
			       (namespace-require '(lib "unit.ss"))
			       (eval (read p))))])
		(unless (and (procedure? info)
			     (procedure-arity-includes? info 2))
		  (error "expected a procedure of arity 2, got" info))
		(let ([name (call-info info 'name (lambda () #f)
				       (lambda (n) 
					 (unless (string? n)
					   (if n
					       (error "couldn't find the package name")
					       (error "expected a string")))))]
		      [unpacker (call-info info 'unpacker (lambda () #f)
					   (lambda (n) 
					     (unless (eq? n 'mzscheme)
					       (error "unpacker isn't mzscheme:" n))))])
		  (unless (and name unpacker)
		    (error "bad name or unpacker"))
		  (print-status
		   (format "Unpacking ~a from ~a" name archive))
		  (let ([u (eval (read p) n)])
		    (unless (eval `(unit? ,u) n)
		      (error "expected a unit, got" u))
		    (let ([plthome plthome]
			  [unmztar (lambda (filter)
				     (unmztar p filter plthome print-status))])
		      (eval `(invoke-unit ,u ,plthome ,unmztar) n))))))
	    (lambda ()
	      (kill)
	      (close-input-port p64gz))))))

  (provide unpack))
