(module archives mzscheme 
  
  (require (lib "inflate.ss")
           (lib "base64.ss" "net")
           (lib "file.ss"))
  
  ;; ============================================================
  ;; ARCHIVE UNPACKING/EXAMINING
  ;; ============================================================
  ;; All this code is ripped out of collects/setup/unpack.ss
  ;; Really that code and this should be refactored
  ;; ------------------------------------------------------------
  
  (define (pretty-name f)
    (with-handlers ([void (lambda (x) f)])
      (let-values ([(base name dir?) (split-path f)])
	(format "~a in ~a" name base))))
  
  (define (unmztar p filter plthome print-status)
    (let loop ()
      (let ([kind (read p)])
	(unless (eof-object? kind)
	  (case kind
	    [(dir) (let ([s (let ([v (read p)])
			      (if (null? v)
				  'same
				  (apply build-path v)))])
		     (unless (or (eq? s 'same) (relative-path? s))
		       (error "expected a directory name relative path string, got" s))
		     (when (or (eq? s 'same) (filter 'dir s plthome))
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
  
  
  ;; decompress : file directory (symbol path directory -> bool) -> void
  ;; unpacks the given planet archive to the given directory, using the 
  ;; given filter to decide what to pull out. (The idea being to allow
  ;; extraction of doc.txt or other similarly interesting files)
  (define (decompress plt-file unpack-dir filter)
    (define-values (p kill) (port64gz->port (open-input-file plt-file)))
    
    (unless (and (eq? (read-char p) #\P)
                 (eq? (read-char p) #\L)
                 (eq? (read-char p) #\T))
        (error "not an unpackable distribution archive"))
      (let ((the-proc (read p))
            (the-unit (read p)))
        (unmztar p filter unpack-dir void)))
  
  (define (extract-files-from-archive archive destination file-pattern)
    (decompress archive 
                destination
                (lambda (a b c) (regexp-match file-pattern b))))
  
  (provide decompress extract-files-from-archive)
  
)