(require-library "compile.ss" "compiler")
(define framework-dir (collection-path "framework"))
(define classhack "classhack")

(define (build-directory/make dir . dirs)
  (let loop ([current dir]
	     [dirs dirs])
    (cond
     [(null? dirs) current]
     [else
      (let ([dir (car dirs)])
	(let ([new (build-path current dir)])
	  (unless (directory-exists? new)
	    (make-directory new))
	  (loop new (cdr dirs))))])))

(define classhack.so-dir
  (build-directory/make framework-dir
			"compiled"
			"native"
			(system-library-subpath)))

(define classhack.so (build-path classhack.so-dir (string-append classhack ".so")))

;(unless (file-exists? classhack.so)
  (let ([classhack.c (build-path framework-dir (string-append classhack ".c"))])
    (require-library "compiler.ss" "compiler")
    (compile-c-extensions (list classhack.c) classhack.so-dir));)

(load-extension classhack.so) ;; now we have interface->names, class->names and interface->super-interfaces

;; don't delete for now
'(delete-file classhack.so)
'(let ([classhack.o (build-path framework-dir "classhack.o")])
  (when (file-exists? classhack.o)
    (delete-file classhack.o)))
