(require 
 (lib "list.ss")
 (lib "process.ss")
 (lib "file.ss")
 (lib "etc.ss"))

; system is either unix (including macosx) or windows;
; not supported on Mac os classic

(define base-system
  (cond [(or 
          (eqv? 'unix (system-type))
          (eqv? 'macosx (system-type)))
         'unix]
        [(eqv? 'windows (system-type))
         'windows]
        [else
         (error "Build is not supported on this platform")]))

; discern between windows and unix os types

(define alt 
  (opt-lambda (winval unval [sys base-system])
    (if (eq? 'windows sys)
        winval
        unval)))

; should where the file is
(define here (this-expression-source-directory) )

; command line flags: / for windows, - for unix
(define flag (alt "/" "-"))

; object code extention
(define o-ext (alt "obj" "o"))

; mzc executable
; can be changed to allow for users not having it in path.. not sure why though..
(define mzc (alt "mzc.exe" "mzc"))


; make directory to hold temporary mess

(define tmp-dir
  (build-path here "src" "tmp"))

(make-directory* tmp-dir)

(define wxdir (simplify-path (build-path here 'up 'up "src" "wxcommon")))

; now copy *everything* into tmpdir

(define (filename file)
  (let-values (((base name must-be-dir?) (split-path file)))
    name))

; find files of a certain type
; file-regexp is the match
; exclude is a list of regexps to exclude
(define find-grep-filter
  (opt-lambda (path file-regexp (excludes empty))
    (filter-all 
     (find-files
      (lambda (f) 
        (regexp-match file-regexp 
                      (filename f))) ; match only on the filename, not the full the string
      
      path)
     excludes)))

; filers the list with all regexps
(define (filter-all f-list r-list)
  (cond
    [(empty? r-list) f-list]
    [else
     (filter-all 
      (filter
       (lambda (file) (not (regexp-match (car r-list) (filename file)))) 
       f-list)
      (cdr r-list))]))
    


  

; copy a list of files to a given directory
(define (copy-files lof dest)
  (for-each
   (lambda (file)      
     ; (if (file-exists? (build-path dest (filename file)))
     ;    null
           (copy-file file (build-path dest (filename file))))
   lof))

(define zlibfiles 
  (find-grep-filter (build-path wxdir "zlib") ".[h|c]$" 
                    '("example.c" "maketree.c" "minigzip.c")))

(define pngfiles
  (find-grep-filter (build-path wxdir "libpng") "\\.[h|c]$" '("example\\.c" "pngtest\\.c")))


(define gdfiles 
  (find-grep-filter (build-path here "src" "gd") "\\.[h|c]$" '("gd_wbmp.c" "gdft\\.c")))

(define plplotfiles 
  (find-grep-filter (build-path here "src" "plplot") "^pl.*\\.c$|\\.h|^mem\\.c|^gd_drv\\.c|pdfutils\\.c"))
                                                             

(copy-files (append 
             zlibfiles 
             pngfiles
             gdfiles
             plplotfiles)                          
            (build-path here "src" "tmp"))

; now fin just all the c files.. a bit redundent, but it's ok..

(define c-files
  (find-grep-filter tmp-dir "\\.c$"))

; now munge it into a string..
(define c-files-string 
  (foldl (lambda (s c) (string-append c " " s)) "" c-files))

c-files-string

; now call mzc on them
(system (string-append mzc " -d " tmp-dir " --cc " c-files-string))

; find all the object files..

(define obj-files
  (find-grep-filter tmp-dir (string-append "\\." o-ext "$")))

(define (ldf-flags files)
  (foldl (lambda (file c) (string-append "++ldf " file " " c)) "" files))

; make native dir

(define 
  ext-dir
  (build-path "compiled" "native" (system-library-subpath)))

(make-directory* ext-dir)

; call mzc

(system (format "~a -d ~a ++ccf ~a ~a plplot-low-level.ss" 
                mzc
                ext-dir 
                (string-append flag "I" (build-path "src" "tmp"))
                (ldf-flags obj-files)))

; clean up the messs

(delete-directory/files (build-path "src" "tmp"))


; now to make the fit module..

(define fit-dir
  (build-path "src" "fit"))

(define fit-files
  (find-grep-filter fit-dir "\\.c$"))

; compile them
(system (format "~a -d ~a --cc ~a"
                mzc
                fit-dir
                (foldl (lambda (s c) (string-append s " " c)) "" fit-files) ))

(define fit-objs
  (find-grep-filter fit-dir (string-append "\\." o-ext "$")))

; link

(system (format "~a -d ~a ~a ++ccf ~a~a~a fit-low-level.ss"
                mzc
                ext-dir
                (ldf-flags fit-objs)
                flag
                "I"
                fit-dir
                ))

; clear the obj-files
(for-each delete-file fit-objs)







  






