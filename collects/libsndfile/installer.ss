(module installer mzscheme
  (require (lib "make.ss" "make")
           (lib "file.ss" "dynext")
           (lib "compile.ss" "dynext")
           (lib "link.ss" "dynext")
           (lib "file.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "string.ss"))
  
  (provide installer)
  
  (define compiler-include 
    (collection-path "compiler"))
  
  (make-print-reasons #f)
  (make-print-checking #f)
  
  (define (delete/continue x)
    (with-handlers ([not-break-exn? void])
      (delete-file x)))
  
  (define (make-make-spec source-dir target-dir homo-vector-include-dir)
    (let* ([name "mzsndfile"]
           [dot-c (build-path source-dir (append-c-suffix name))]
           [dot-o (build-path source-dir (append-object-suffix name))])
        (list (list (build-path target-dir (append-extension-suffix name)) ; shared-library
                    (list dot-c)
                    (lambda () 
                      (compile-extension #f dot-c dot-o `(,compiler-include ,homo-vector-include-dir ,"/sw/include" ,"/usr/local/include")) ; yuck! what a hack
                      (parameterize ([current-extension-linker-flags
                                      (append (current-extension-linker-flags)
                                              (list (format "-L/sw/lib")
                                                    "-lsndfile"))])
                        (link-extension #f (list dot-o) (build-path target-dir (append-extension-suffix name))))
                      (delete/continue dot-o))))))
  
  (define (installer dir)
    (let* ([target-dir (build-path "compiled" "native" (system-library-subpath))]
           [source-dir (build-path 'same)]
           [homo-vector-include-dir (build-path (collection-path "srfi") "4" "c-generation")])

      ; yuck! what's the right way to do this?
      (parameterize ([current-directory (collection-path "libsndfile")])
        (make-directory* target-dir)
        (make-directory* source-dir)
        
        (make/proc (make-make-spec source-dir target-dir homo-vector-include-dir))))))
  