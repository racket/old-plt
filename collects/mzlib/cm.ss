(module cm mzscheme
  (require (lib "moddep.ss" "syntax")
	   (lib "plthome.ss" "setup"))

  (provide make-compilation-manager-load/use-compiled-handler
	   managed-compile-zo
	   make-caching-managed-compile-zo
	   trust-existing-zos
	   (rename trace manager-trace-handler))
  
  (define trace (make-parameter void))
  (define indent (make-parameter ""))
  (define trust-existing-zos (make-parameter #f))
  
  (define my-max
    (case-lambda
      (() 0)
      (x (apply max x))))
  
  (define (get-deps code path)
    (let-values ([(imports fs-imports) (module-compiled-imports code)])
      (map (lambda (x)
	     (resolve-module-path-index x path))
	   ;; Filter symbols:
	   (let loop ([l (append imports fs-imports)])
	     (cond
	      [(null? l) null]
	      [(symbol? (car l)) (loop (cdr l))]
	      [else (cons (car l) (loop (cdr l)))])))))

  (define (get-compilation-dir+name path)
    (let-values (((base name-suffix must-be-dir?) (split-path path)))
      (let ((name (regexp-replace #rx"\\..?.?.?$" name-suffix "")))
	(values
	 (cond
	  ((eq? 'relative base) (build-path "compiled"))
	  (else (build-path base "compiled")))
	 name))))

  (define (get-compilation-path path)
    (let-values ([(dir name) (get-compilation-dir+name path)])
      (build-path dir name)))

  (define (get-code-dir path)
    (let-values (((base name-suffix must-be-dir?) (split-path path)))
      (cond
        ((eq? 'relative base) (build-path "compiled"))
        (else (build-path base "compiled")))))

  (define (write-deps code path external-deps)
    (let ((dep-path (string-append (get-compilation-path path) ".dep"))
          (deps (get-deps code path)))
      (let ((op (open-output-file dep-path 'replace)))
        (write (cons (version)
		     (append (map plthome-ify deps)
			     (map (lambda (x) (plthome-ify (cons 'ext x)))
                                  external-deps)))
	       op)
        (newline op)
        (close-output-port op))))

  (define (touch path)
    (close-output-port (open-output-file path 'append)))

  (define (compilation-failure path zo-name date-path)
    (with-handlers ((not-break-exn? void))
      (delete-file zo-name))
    (let ([fail-path (string-append (get-compilation-path path) ".fail")])
      (close-output-port (open-output-file fail-path 'truncate/replace)))
    ((trace) (format "~afailure" (indent))))

  (define (compile-zo path)
    ((trace) (format "~acompiling: ~a" (indent) path))
    (parameterize ([indent (string-append "  " (indent))])
      (let ([zo-name (string-append (get-compilation-path path) ".zo")])
        (cond
         [(and (file-exists? zo-name) (trust-existing-zos)) (touch zo-name)]
         [else
          (when (file-exists? zo-name) (delete-file zo-name))
          (with-handlers ([exn:get-module-code?
                           (lambda (ex)
                             (compilation-failure
                              path zo-name (exn:get-module-code-path ex)))])
            (let* ([param
                    ;; Avoid using cm while loading cm-ctime:
                    (parameterize ([use-compiled-file-kinds 'none])
                      (dynamic-require '(lib "cm-ctime.ss" "mzlib" "private")
                                       'current-external-file-registrar))]
                   [external-deps null]
                   [code (parameterize ([param (lambda (ext-file)
                                                 (set! external-deps
                                                       (cons ext-file
                                                             external-deps)))])
                           (get-module-code path))]
                   [code-dir (get-code-dir path)])
              (if (not (directory-exists? code-dir))
                (make-directory code-dir))
              (let ((out (open-output-file zo-name 'replace)))
                (with-handlers ((exn:application:type?
                                 (lambda (ex) (compilation-failure path zo-name #f))))
                  (dynamic-wind
                    void
                    (lambda () (write code out))
                    (lambda () (close-output-port out)))))
              (let ([ss-sec (file-or-directory-modify-seconds path)]
                    [zo-sec (file-or-directory-modify-seconds zo-name)])
                (when (< zo-sec ss-sec)
                  (error 'compile-zo
                         "date for newly created .zo file (~a @ ~a) is before source-file date (~a @ ~a)~a"
                         zo-name
                         (format-date (seconds->date zo-sec))
                         path
                         (format-date (seconds->date ss-sec))
                         (if (> ss-sec (current-seconds))
                           ", which appears to be in the future"
                           ""))))
              (write-deps code path external-deps)))])))
    ((trace) (format "~aend compile: ~a" (indent) path)))

  (define (format-date date)
    (format "~a:~a:~a:~a:~a:~a"
	    (date-year date)
	    (date-month date)
	    (date-day date)
	    (date-hour date)
	    (date-minute date)
	    (date-second date)))
  
  (define (append-object-suffix f)
    (string-append f (case (system-type)
		       [(windows) ".dll"]
		       [else ".so"])))

  (define (get-compiled-time path w/fail?)
    (let-values  ([(dir name) (get-compilation-dir+name path)])
      (first-date
       (lambda () (build-path dir "native" (system-library-subpath) (append-object-suffix "_loader")))
       (lambda () (build-path dir "native" (system-library-subpath) (append-object-suffix name)))
       (lambda () (build-path dir (string-append name ".zo")))
       (and w/fail? (lambda () (build-path dir (string-append name ".zo" ".fail")))))))

  (define first-date
    (case-lambda
     [() -inf.0]
     [(f . l)
      (if f
	  (with-handlers ([exn:i/o:filesystem?
			   (lambda (ex)
			     (apply first-date l))])
	    (file-or-directory-modify-seconds (f)))
	  (apply first-date l))]))
  
  (define (compile-root path up-to-date)
    (let ([path (simplify-path (expand-path path))])
      (let ((stamp (and up-to-date
			(hash-table-get up-to-date path (lambda () #f)))))
	(cond
          (stamp stamp)
          (else
           ((trace) (format "~achecking: ~a" (indent) path))
           (let ((path-zo-time (get-compiled-time path #f))
                 (path-time 
                  (with-handlers ((exn:i/o:filesystem? 
                                   (lambda (ex)
                                     ((trace) (format "~a~a does not exist" (indent) path))
                                     #f)))
                    (file-or-directory-modify-seconds path))))
             (cond
               ((not path-time) path-zo-time)
               (else
                (cond
                  ((> path-time path-zo-time) 
		   ((trace) (format "~anewer src..." (indent)))
		   (compile-zo path))
                  (else
                   (let ((deps (with-handlers ((exn:i/o:filesystem? (lambda (ex) (list (version)))))
                                 (call-with-input-file (string-append (get-compilation-path path) ".dep")
                                   read))))
                     (cond
                       ((or (not (pair? deps))
                            (not (equal? (version) (car deps))))
			((trace) (format "~anewer version..." (indent)))
                        (compile-zo path))
                       ((ormap (lambda (d)
				 ;; str => str is a module file name (check transitive dates)
				 ;; (cons 'ext str) => str is an non-module file (check date)
				 (let ([t (cond
					   [(string? d) (compile-root d up-to-date)]
					   [(and (pair? d) (eq? (car d) 'ext))
					    (with-handlers ((exn:i/o:filesystem?
							     (lambda (ex) +inf.0)))
					      (file-or-directory-modify-seconds (cdr d)))]
					   [else -inf.0])])
				   (when (> t path-zo-time)
				     ((trace) (format "~anewer: ~a (~a > ~a)..." (indent) d t path-zo-time)))
				   (> t path-zo-time)))
			       (map un-plthome-ify (cdr deps)))
                        (compile-zo path))))))
                (let ((stamp (get-compiled-time path #t)))
                  (hash-table-put! up-to-date path stamp)
                  stamp)))))))))
  
  (define (managed-compile-zo zo)
    ((make-caching-managed-compile-zo) zo))
  
  (define (make-caching-managed-compile-zo)
    (let ([cache (make-hash-table 'equal)])
      (lambda (zo)
	(parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler/table cache)])
	  (compile-root (path->complete-path zo) cache)))))

  (define (make-compilation-manager-load/use-compiled-handler)
    (make-compilation-manager-load/use-compiled-handler/table (make-hash-table 'equal)))

  (define (make-compilation-manager-load/use-compiled-handler/table cache)
    (let ([orig-eval (current-eval)]
	  [orig-load (current-load)]
	  [orig-namespace (current-namespace)]
	  [default-handler (current-load/use-compiled)])
      (letrec ([compilation-manager-load-handler
		(lambda (path mod-name)
		  (cond
                    [(not mod-name)
                     ((trace) (format "~askipping:  ~a mod-name ~s" (indent) path mod-name))
                     (default-handler path mod-name)]
                    [(eq? 'none (use-compiled-file-kinds))
                     ((trace) (format "~askipping:  ~a file-kinds ~s" (indent) path (use-compiled-file-kinds)))
                     (default-handler path mod-name)]
                    [(not (eq? compilation-manager-load-handler (current-load/use-compiled)))
                     ((trace) (format "~askipping:  ~a current-load/use-compiled changed ~s"
                                      (indent) path (current-load/use-compiled)))
                     (default-handler path mod-name)]
                    [(not (eq? orig-eval (current-eval)))
                     ((trace) (format "~askipping:  ~a orig-eval ~s current-eval ~s" 
                                      (indent) path orig-eval (current-eval)))
                     (default-handler path mod-name)]
                    [(not (eq? orig-load (current-load)))
                     ((trace) (format "~askipping:  ~a orig-load ~s current-load ~s" 
                                      (indent) path orig-load (current-load)))
                     (default-handler path mod-name)]
                    [(not (eq? orig-namespace (current-namespace)))
                     ((trace) (format "~askipping:  ~a orig-namespace ~s current-namespace ~s" 
                                      (indent) path orig-namespace (current-namespace)))
                     (default-handler path mod-name)]
                    [else 
                     ((trace) (format "~aprocessing: ~a" (indent) path))
                     (compile-root path cache)
                     (default-handler path mod-name)]))])
	compilation-manager-load-handler))))
