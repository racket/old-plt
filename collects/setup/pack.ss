;; Utilities for creating a .plt package
(module pack mzscheme
  (require (lib "deflate.ss")
           (lib "base64.ss" "net")
           (lib "process.ss")
	   (lib "etc.ss")
	   (lib "getinfo.ss" "setup"))

  (provide pack mztar std-filter pack-collections)
  
  (define pack
    (opt-lambda (dest name paths collections
		      [filter std-filter]
		      [encode? #t]
		      [file-mode 'file]
		      [unpack-unit #f]
		      [plt-relative? #t]
		      [requires null]
		      [conflicts null])
      (let*-values ([(file) (open-output-file dest 'truncate/replace)]
                    [(fileout thd)
                     (if encode?
                         (let-values ([(b64-out b64-in) (make-pipe 4096)]
                                      [(gz-out gz-in) (make-pipe 4096)])
                           (thread
                            (lambda ()
                              (let ([normal (lambda ()
                                              (gzip-through-ports gz-out b64-in #f 0)
                                              (close-output-port b64-in))]
                                    [gzip (find-executable-path "gzip" #f)])
                                (if gzip
                                    (let ([p (process* gzip "-c")])
                                      (if (eq? 'running ((list-ref p 4) 'status))
                                          (begin
					  ;; Use gzip process.
					  ;; Errors to error port:
                                            (thread 
                                             (lambda ()
                                               (dynamic-wind
                                                void
                                                (lambda ()
                                                  (let loop ()
                                                    (let ([r (read-line (cadddr p))])
                                                      (unless (eof-object? r)
                                                        (fprintf (current-error-port)
                                                                 "~a~n" r)
                                                        (loop)))))
                                                (lambda ()
                                                  (close-input-port (cadddr p))))))
					  ;; Copy input to gzip:
                                            (thread
                                             (lambda ()
                                               (let ([s (make-string 4096)])
                                                 (dynamic-wind
                                                  void
                                                  (lambda ()
                                                    (let loop ()
                                                      (let ([n (read-string-avail! s gz-out)])
                                                        (unless (eof-object? n)
                                                          (display (substring s 0 n) (cadr p))
                                                          (loop)))))
                                                  (lambda ()
                                                    (close-input-port gz-out)
                                                    (close-output-port (cadr p)))))))
					  ;; Copy input to b64:
                                            (dynamic-wind
                                             void
                                             (lambda ()
                                               (let ([s (make-string 4096)])
                                                 (let loop ()
                                                   (let ([n (read-string-avail! s (car p))])
                                                     (unless (eof-object? n)
                                                       (display (substring s 0 n) b64-in)
                                                       (loop))))))
                                             (lambda ()
                                               (close-input-port (car p))
                                               (close-output-port b64-in))))
                                          (normal)))
                                    (normal)))))
                           (values
                            gz-in
                            (thread
                             (lambda ()
                               (base64-encode-stream b64-out file)
                               (close-output-port file)))))
                         (values file (thread void)))])
        (fprintf fileout "PLT~n")
        (write
         `(lambda (request failure)
            (case request
              [(name) ,name]
              [(unpacker) 'mzscheme]
	      [(requires) ',requires]
	      [(conflicts) ',conflicts]
	      [(plt-relative?) ,plt-relative?]))
         fileout)
        (newline fileout)
        (write
         (or unpack-unit
             `(unit 
                (import plthome mzuntar)
                (export)
                (mzuntar void)
                (quote ,collections)))
         fileout)
        (newline fileout)
        (for-each
         (lambda (path)
           (mztar path fileout filter file-mode))
         paths)
        (close-output-port fileout)
        (thread-wait thd))))
  
  (define (mztar path output filter file-mode)
    (define (path->list p)
      (if (eq? p 'same)
	  null
	  (let-values ([(base name dir?) (split-path p)])
	    (if (string? base)
		(append (path->list base) (list name))
		(list name)))))
    (define-values (init-dir init-files)
      (if (file-exists? path)
          (let-values ([(base name dir?) (split-path path)])
            (values (if (eq? base 'relative) 'same base) (list name)))
          (values path #f)))
    
    (let loop ([dir init-dir][dpath (path->list init-dir)][files init-files])
      (printf "MzTarring ~a...~n" 
	      (if files
		  (build-path dir (car files))
		  dir))
      (fprintf output "~s~n~s~n" 'dir dpath)
      (for-each
       (lambda (f)
         (let* ([p (build-path dir f)]
                [filter-val (filter p)])
           (when filter-val
             (if (directory-exists? p)
                 (loop p (append dpath (list f)) #f)
                 (let ([len (file-size p)])
                   ; (printf "MzTarring ~a~n" p)
                   (fprintf output "~s~n~s~n~s~n*"
                            (case filter-val
                              [(file) 'file]
                              [(file-replace) 'file-replace]
                              [else file-mode])
                            (append dpath (list f))
                            len)
                   (with-input-from-file p
                     (lambda ()
                       (let ([s (make-string 4096)])
                         (let loop ()
                           (let ([n (read-string-avail! s)])
                             (unless (eof-object? n)
                               (if (= n 4096)
                                   (display s output)
                                   (display (substring s 0 n) output))
                               (loop))))))))))))
       (or files (directory-list dir)))))
  
  (define (std-filter path)
    (not (or (regexp-match "CVS$" path)
             (regexp-match "compiled$" path)
             (regexp-match "~$" path)
             (regexp-match "^#.*#$" path))))

  (define (pack-collections collections output name replace? extra-setup-collections)
    (let-values ([(dir source-files requires conflicts name)
		  (let ([dirs (map (lambda (cp) (apply collection-path cp)) collections)])
		    ;; Figure out the base path:
		    (let* ([base-path #f]
			   [base-path-setter #f]
			   [rel-paths 
			    (map (lambda (dir)
				   (let-values ([(base c-name dir?) (split-path dir)])
				     (let-values ([(base collects-dir-name dir?) (split-path base)])
				       (if base-path
					   (unless (equal? base base-path)
					     (error
					      'mzc
					      "cannot combine collections that live in different directories: \"~a\" and: \"~a\""
					      base-path-setter
					      dir))
					   (begin
					     (set! base-path-setter dir)
					     (set! base-path base)))
				       (build-path 'same collects-dir-name c-name))))
				 dirs)]
			   [infos (map (lambda (cp) (get-info cp))
				       collections)]
			   [coll-list? (lambda (cl)
					 (and (list? cl)
					      (andmap (lambda (c) 
							(and (list? c)
							     (andmap string? c)
							     (andmap relative-path? c)))
						      cl)))]
			   [get-dep-coll (lambda (which)
					   (apply append (map (lambda (i src-cp)
								(let ([rl (if i
									      (i which (lambda () null))
									      null)])
								  (unless (coll-list? rl)
								    (error
								     'mzc
								     "bad ~a specification in info.ss for collection ~s"
								     which
								     src-cp))
								  rl))
							      infos collections)))])
		      (values base-path
			      rel-paths
			      (get-dep-coll 'requires)
			      (append
			       (if replace? null collections)
			       (get-dep-coll 'conflicts))
			      (or name
				  ((or (car infos)
				       (lambda (n f) (caar collections)))
				   'name
				   (lambda () (caar collections)))))))])
      (let ([output (path->complete-path output)])
	(parameterize ([current-directory dir])
	  (pack output name
		(map (lambda (cp) (apply build-path 'same cp)) collections)
		(append
		 extra-setup-collections
		 collections)
		std-filter #t 
		(if replace?
		    'file-replace
		    'file)
		#f
		#t ; plt-relative
		;; For each require, get current version
		(map (lambda (r)
		       (let ([i (get-info r)])
			 (let ([v (and i (i 'version (lambda () #f)))])
			   (if v
			       (begin
				 (unless (and (list? v)
					      (andmap number? v)
					      (andmap exact? v)
					      (andmap integer? v))
				   (error
				    'mzc
				    "bad version specification in info.ss for collection ~s"
				    r))
				 (list r v))
			       (list r null)))))
		     (cons
		      '("mzscheme")
		      requires))
		conflicts))))))
