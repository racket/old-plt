(module pre-installer mzscheme
  (require (lib "file.ss")
           (lib "make.ss" "make")
           (lib "setup-extension.ss" "make")
           (lib "file.ss" "dynext")
           (lib "compile.ss" "dynext")
           (lib "link.ss" "dynext")
           (lib "compiler.ss" "compiler")
           (lib "option.ss" "compiler")
           (lib "list.ss")
           (lib "etc.ss"))

  ;; (verbose)
  ;;  compile-extension

  (require (rename (lib "plthome.ss" "setup") plthome* plthome))
  (define mz-inc-dir (build-path plthome* "include"))
  (define headers
    (map (lambda (name)
           (build-path mz-inc-dir name))
         '("scheme.h" "schvers.h" "schemef.h" "sconfig.h" "stypes.h")))

  
  (define PLOT-SCHEME-FILE-NAME "plplot-low-level")
  
  (define here (this-expression-source-directory))

  (define dir (build-path here "compiled" "native" (system-library-subpath)))

  (unless (directory-exists? dir) (make-directory* dir))

  (define (get-precompiled-path file.so)
    (printf "~a\n" file.so)
    (let*-values ([(path name _) (split-path file.so)]
                  [(path d1 _)   (split-path path)]
                  [(path d2 _)   (split-path path)]
                  [(path c _)    (split-path path)])
      (build-path (if (eq? 'relative path) 'same path)
                  "precompiled" d2 d1 name)))

  (define (do-copy file.so)
    (let ([pre-compiled (get-precompiled-path file.so)])
      (and (file-exists? pre-compiled)
           (begin (printf "  Copying ~s -> ~s\n" pre-compiled file.so)
                  (when (file-exists? file.so) (delete-file file.so))
                  (copy-file pre-compiled file.so)
                  #t))))

  (define (final-so-file file-name)
    (build-path dir (file-name-from-path (append-extension-suffix file-name))))

  ;; (verbose #t)

  (define (make-ext scheme-file c-file-names src-dir)
    (let* ([c-files  (map (lambda (f) (build-path src-dir f))c-file-names)]
           [final-so (final-so-file scheme-file)]
           [objects  (map append-object-suffix c-files)]
           [scheme-file-with-ext (string-append scheme-file ".ss")])
      (make/proc
       (apply list  ; make lines
              (list final-so   ; target
                    objects    ; depends
                    (lambda () ; link them together
                      (link-extension #f objects final-so)))
              (list (append-c-suffix
                     (build-path src-dir (file-name-from-path scheme-file)))
                    (list scheme-file-with-ext)
                    (lambda ()
                      ((compile-extensions-to-c #f)
                       (list scheme-file-with-ext)
                       src-dir)))
              (list (append-object-suffix scheme-file)
                    (append headers (list append-c-suffix scheme-file)))
              (map (lambda (file)
                     (list (append-object-suffix file)
                           ;(append headers
                           ;        (list (append-c-suffix file)))
                           (list (append-c-suffix file))
                           (lambda ()
                             (compile-c-extension-parts
                              (list (append-c-suffix file))
                              src-dir))))
                   c-files))
       final-so)))

  (provide pre-installer)

  (define (pre-installer plthome)
    (let* ([fit-src-dir     (build-path here "src" "fit")]
           [fit-scheme-file (build-path here "fit-low-level")]
           [fit-c-files     `("fit-low-level" "fit" "matrix")])
      (unless (do-copy (final-so-file fit-scheme-file))
        (make-ext fit-scheme-file fit-c-files fit-src-dir)))
    (let* ([plot-scheme-file (build-path here PLOT-SCHEME-FILE-NAME)]
           [plot-src-dir (build-path here "src" "all")]
           [plot-c-files
            (map (lambda (f) (regexp-replace #rx".c$" f ""))
                 (cons (file-name-from-path plot-scheme-file)
                       (filter (lambda (f)
                                 (and (regexp-match #rx".c$" f)
                                      (not (regexp-match
                                            (file-name-from-path
                                             plot-scheme-file)
                                            f))))
                               (directory-list plot-src-dir))))])
      (unless (do-copy (final-so-file plot-scheme-file))
        (parameterize ([current-extension-compiler-flags
                        (append (current-extension-compiler-flags)
                                (case (system-type)
                                  [(windows) '("/DHAVE_LIBPNG" "/DPLD_png")]
                                  [else '("-DHAVE_LIBPNG" "-DPLD_png")]))])
          (make-ext plot-scheme-file plot-c-files plot-src-dir))))

    ;; copy plot docs from src here
    #;
    (let ((docs-dir
           (build-path (collection-path "doc") "plot")))
      (unless (directory-exists? docs-dir)
        (make-directory* docs-dir))
      (for-each
       (lambda (file)
         (let ((new-file (build-path docs-dir (file-name-from-path file))))
           (if (file-exists? new-file)
             (delete-file new-file))
           (copy-file file new-file)))
       (find-files
        (lambda (file)
          (or (regexp-match #rx"hdindex$" file)
              (and (not (regexp-match #rx".tex" file))
                   (regexp-match #rx"plot-docs" file))))
        (build-path (collection-path "plot") "src" "docs"))))
    ))
