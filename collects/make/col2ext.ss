
(require-library "traceld.ss")

(require-library "collection.ss" "make")
(require-library "functio.ss")

(require-library "options.ss" "compiler")
(require-library "cmdline.ss")

(define argv
  (parse-command-line
   "mzcol2ext"
   argv
   `([once-any
      [("-v" "--verbose")
       ,(lambda (f) (compiler:option:verbose #t))
       ("Verbose compiler mode")]
      [("-d" "--debug")
       ,(lambda (f) (compiler:option:debug #t))
       ("Debug compiler mode")]])
   (lambda (flags) #())
   null))

(define info (load "info.ss"))

(define (scheme-files-of l)
 (let loop ([l l])
   (if (null? l)
       null
       (if (regexp-match "\\.(ss|scm)$" (car l))
	   (cons (car l) (loop (cdr l)))
	   (loop (cdr l))))))

(make-collection-extension 
 (info 'name)

 (list (info 'compile-prefix))

 (remove*
  (info 'compile-omit-files)
  (scheme-files-of (directory-list)))

 argv)

