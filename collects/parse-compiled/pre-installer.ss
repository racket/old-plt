(module pre-installer mzscheme
	;;; Stolen shamelessly from PLoT
  (require (lib "etc.ss") (lib "file.ss")
	   (lib "file.ss" "dynext") (lib "link.ss" "dynext")
           (lib "compile.ss" "dynext")
           (lib "compiler.ss" "compiler"))

  (define top-dir (this-expression-source-directory))
  (define src-dir (build-path top-dir "src"))
  (define tmp-dir (build-path src-dir "tmp"))
  (define c-files '("parse-compiled.c"))
  (define native-dir
    (build-path top-dir "compiled" "native" (system-library-subpath)))
  (define include-path
    (build-path (let-values ([(base f dir?) (split-path (find-system-path 'exec-file))])
	           base)
		'up "src" "mzscheme" "src"))

  (printf "include path: ~a~n" include-path)
  
  (define (build-library)
      (let* ([libname "parse-compiled"]
             [so-name (build-path top-dir "compiled" "native"
                                  (system-library-subpath)
                                  (append-extension-suffix libname))])
        (printf "parse-compiled: compiling \"~a\"...\n" so-name)
                       ;; we compile a simple .so, not an extension
                       ;[current-standard-link-libraries '()]
                       ;[current-use-mzdyn #f]
          (when (or (not (file-exists? so-name))
                    (let ([so-time (file-or-directory-modify-seconds so-name)])
                      (ormap (lambda (f)
                               (> (file-or-directory-modify-seconds f)
                                  so-time))
                             c-files)))
            (printf "    Compiling \"~a\"\n" libname)
            (make-directory* tmp-dir)
	    (parameterize ([current-extension-compiler-flags
			    (append ((current-make-compile-include-strings) include-path)
				    (list "-g")
				    (current-extension-compiler-flags))])
			  (printf "compile flags: ~a~n"  (current-extension-compiler-flags))
            (compile-c-extension-parts c-files tmp-dir))
            (parameterize ([current-directory tmp-dir])
              (link-extension #f (directory-list tmp-dir) so-name))
            (delete-directory/files tmp-dir))))

  (provide pre-installer)
  (define (pre-installer plthome)
    (unless (directory-exists? src-dir)
      (error 'parse-compiled:pre-installer "Could not find the source directory at ~a"
             src-dir))
    (when (directory-exists? src-dir)
      (unless (directory-exists? native-dir) (make-directory* native-dir))
      (parameterize ([current-directory src-dir])
        (build-library)))))
