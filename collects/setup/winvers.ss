(module winvers mzscheme
  (require (lib "file.ss"))
  
  (define plthome
    (with-handlers ([(lambda (x) #t) (lambda (x) #f)])
      (or (let ([p (getenv "PLTHOME")])
            (and p (normal-case-path (expand-path p))))
          (let ([dir (collection-path "mzlib")])
            (and dir
                 (let-values ([(base name dir?) (split-path dir)])
                   (and (string? base)
                        (let-values ([(base name dir?) (split-path base)])
                          (and (string? base)
                               (complete-path? base)
                               (normal-case-path (expand-path base)))))))))))
  
  (define (make-copy)
    (let ([tmpdir (find-system-path 'temp-dir)])
      (let ([vers (build-path tmpdir "setvers")])
	(unless (directory-exists? vers)
	   (make-directory vers))
        (for-each
         (lambda (p)
           (let ([dest (build-path vers p)])
             (when (file-exists? dest)
               (delete-file dest))
             (copy-file (build-path plthome p) dest)))
         '("mzscheme.exe" "libmzgcxxxxxxx.dll" "libmzschxxxxxxx.dll"))
        (build-path vers "mzscheme.exe"))))

  (define (patch-files)
    (define new-version (substring
			 (regexp-replace*
			  "alpha"
			  (format "~a_000000000" (version))
			  "a")
			 0
			 7))
    (make-directory* (build-path plthome "lib" "msvc"))
    (for-each (lambda (lib)
		(let ([name (format "~axxxxxxx.lib" lib)])
		  (let ([src (build-path plthome "src" "worksp" lib "release" name)]
			[dest (build-path plthome "lib" "msvc" name)])
		    (when (file-exists? src)
		      (when (file-exists? dest)
			(delete-file dest))
		      (copy-file src dest)))))
	      '("libmzgc"
		"libmzsch"))
    (for-each (lambda (f)
                (let ([p (build-path plthome f)])
		  (when (file-exists? p)
		    (let ([i (open-input-file p)]
			  [o (open-output-file p 'append)])
		      (let loop ()
			(file-position i 0)
			(let ([m (regexp-match-positions "xxxxxxx[.]dll" i)])
			  (when m
			    (file-position o (caar m))
			    (display new-version o)
			    (loop))))
		      (close-input-port i)
		      (close-output-port o)))))
              '("mzscheme.exe"
                "mred.exe"
		"mzcom.exe"
                "libmzschxxxxxxx.dll"
                "libmzgcxxxxxxx.dll"
                "libmredxxxxxxx.dll"
		"lib/msvc/libmzgcxxxxxxx.dll"
		"lib/msvc/libmzschxxxxxxx.dll"))
    (for-each (lambda (b suffix)
		(let ([src (build-path plthome (string-append b "xxxxxxx" suffix))]
		      [dest (build-path plthome (string-append b new-version suffix))])
		  (when (file-exists? src)
		    (when (file-exists? dest) 
		      (delete-file dest))
		    (rename-file-or-directory src dest))))
	      '("libmzsch"
		"libmzgc"
		"libmred"
		"lib/msvc/libmzgc"
		"lib/msvc/libmzsch")
	      '(".dll" ".dll" ".dll"
		".lib" ".lib")))
					  
  
  (let ([argv (current-command-line-arguments)])
    (cond
      [(equal? argv #())
       (let ([exe (make-copy)])
         (putenv "PLTHOME" plthome)
         (printf "re-launching first time...~n")
         (subprocess (current-output-port)
                     (current-input-port)                     
                     (current-error-port)
                     exe
                     "-mvqL-"
                     "winvers.ss"
                     "setup"
                     "patch"))]
      [(equal? argv #("patch"))
       (sleep 1) ; time for other process to end
       (patch-files)
       (printf "re-launching last time...~n")
       (subprocess (current-output-port)
                   (current-input-port)                     
                   (current-error-port)
                   (build-path plthome "mzscheme.exe")
                     "-mvqL-"
                     "winvers.ss"
                     "setup"
                   "finish")]
      [(equal? argv #("finish"))
       (sleep 1) ; time for other process to end
       (delete-directory/files (build-path (find-system-path 'temp-dir) 
                                           "setvers"))
       (printf "done!~n")]
      [else
       (error 'winvers "unknown command line: ~e" argv)])))

