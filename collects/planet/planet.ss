(module planet mzscheme
      #|
This module contains code that implements the `planet' command-line tool.
  
PLANNED FEATURES:
  
1. Download and setup a planet package without requiring it
2. Remove a package from the cache
3. Disable a package without removing it (disabling meaning
   that if it's a tool it won't start w/ DrScheme, etc)
4. Examine and alter linkage table
|#
  (require (lib "cmdline.ss")
           (lib "string.ss")
           (lib "file.ss")
           
           "config.ss"
           "resolver.ss") ;; the code I need should be pulled out into a common library
  
  (define plt-files-to-install '())
  (define to-install '())
  (define to-remove '())
  (define install-version (version))
  
  (define (start)

    (make-directory* (PLANET-DIR))
    (make-directory* (CACHE-DIR))
    
    (command-line
     "planet"
     (current-command-line-arguments)
     (once-any
      (("-f" "--file")
       plt-file owner maj min
       "Installs local file <plt-file> as though it had been downloaded from the planet server. The installed package has path (planet (<owner> <plt-file's filename> <maj> <min>))"
       (set! plt-files-to-install (cons (list plt-file owner maj min) plt-files-to-install)))
      (("-i" "--install")
       pkg-spec
       "Downloads and installs the package (require (planet \"file.ss\" <pkg-spec>)) would install"
       (set! to-install (cons pkg-spec to-install)))
      (("-r" "--remove")
       owner pkg maj min
       "Removes the specified package from the local cache"
       (set! to-remove (cons (list owner pkg maj min) to-remove))))
     #;(once-each
        (("-v" "--version")
         version
         "Download and install packages for <version> (default: the current version)"
         (set! install-version version))))

    (for-each download/install to-install)
    (for-each (lambda (argl) (apply install-plt-file argl)) plt-files-to-install)
    
    (unless (null? to-remove)
      (printf "I would remove:\n")
      (for-each (lambda (p) (printf "  ~s\n" p)) to-remove)))
  
  ;; ============================================================
  ;; FEATURE IMPLEMENTATIONS
  
  (define (fail s) (raise (make-exn:fail s (current-continuation-marks))))
  
  (define (download/install pkg-spec-str)
    (let* ([spec (read-from-string pkg-spec-str)]
           [full-pkg-spec (pkg-spec->full-pkg-spec spec #f)])
      (when (get-package-from-cache full-pkg-spec)
        (fail "No package installed (cache already contains a matching package)"))
      (unless (get-package-from-server full-pkg-spec)
        (fail "Could not find matching package"))))
  
  (define (install-plt-file filestr owner majstr minstr)
    (let ((maj (string->number majstr))
          (min (string->number minstr)))
      (unless (and (integer? maj) (integer? min) (> maj 0) (>= min 0))
        (fail "Invalid major/minor version"))
      (unless (file-exists? filestr) (fail (format "File does not exist: ~a" filestr)))
      (let* ([file (normalize-path filestr)]
             [name (let-values ([(base name dir?) (split-path file)]) (path->string name))]
             [spec (list owner name maj min)]
             [fullspec (pkg-spec->full-pkg-spec spec #f)])
        (unless spec (fail "invalid spec: ~a" spec))
        (install-pkg fullspec file maj min))))
  
  
  ;; ============================================================
  ;; start the program
  
  (with-handlers ([exn:fail? 
                   (lambda (e) 
                     (fprintf (current-error-port) "~a\n" (exn-message e))
                     (exit 1))])
    (start)))