
(require-library "refer.ss")

(reference "ld.ss")
(require-library "cmdline.ss")

(define linker-prefix "")

(define files
  (parse-command-line
   "mzld"
   argv
   `([once-each
      [("-n" "--name") 
       ,(lambda (f name) name) 
       ("Prefix setup names with <name>" "name")]])
   (lambda (options file . files)
     (when (pair? options) (set! linker-prefix
				 (string-append "_" (car options))))
     (cons file files))
   (list "file")))

(link-multi-file-extension linker-prefix files (current-directory))
