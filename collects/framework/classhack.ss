(require-library "compile.ss" "compiler")
(define framework-dir (collection-path "framework"))
(define classhack "classhack")
(define classhack.so (build-path framework-dir (string-append classhack ".so")))

(unless (file-exists? classhack.so)
  (let ([classhack.c (build-path framework-dir (string-append classhack ".c"))])
    (require-library "compiler.ss" "compiler")
    (compile-c-extensions (list classhack.c) framework-dir)))

(load-extension classhack.so) ;; now we have interface->names, class->names and interface->super-interfaces

(delete-file classhack.so)
(let ([classhack.o (build-path framework-dir "classhack.o")])
  (when (file-exists? classhack.o)
    (delete-file classhack.o)))