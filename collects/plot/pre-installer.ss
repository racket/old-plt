(module pre-installer mzscheme
  (require 
   (lib "list.ss")
   (lib "process.ss")
   (lib "etc.ss")
   (lib "file.ss"))
 
  (define (copy-files-in-dir from to re)
    (for-each 
     (lambda (f)
       (when
         (and 
           (file-exists? (build-path from f))
           (regexp-match re f)
           (not (regexp-match #rx"[.]o$" f))
	   (not (regexp-match #rx"[.]a$" f))
           (not (regexp-match #rx"~$" f))
           (not (regexp-match #rx"[.]plt$" f))
           (not (regexp-match #rx"^#.*#$" f))
           (not (regexp-match #rx"^[.]#" f)))
          (copy-file (build-path from f) (build-path to f))))
     (directory-list from)))
  
  (define (pre-installer plthome)      
    (let ([dir (build-path (collection-path "plot")
                           "precompiled"
                           "native"
                           (system-library-subpath #f))])
	(if (directory-exists? dir) ; just copy things over
            (let ((compiled-dir (build-path
                                 (collection-path "plot")
                                 "compiled"
                                 "native"
                                 (system-library-subpath #f))))
              (unless (directory-exists? compiled-dir)
                (make-directory* compiled-dir))
              (copy-files-in-dir dir compiled-dir #rx"."))
            (build))))
  
  
  ;(provide build)
  
  ; should where the file is
  (define here (this-expression-source-directory) )
  
  (define ext-dir
    (build-path here "compiled" "native" (system-library-subpath)))
  
  (define (build)
    (begin
      (make-directory* ext-dir)
      (build-plot)
      (build-fit)))

  (define base-system
    (cond [(or 
            (eqv? 'unix (system-type))
            (eqv? 'macosx (system-type)))
           'unix]
          [(eqv? 'windows (system-type))
           'windows]
          [else
           (error "Build is not supported on this platform")]))
  
  (define alt 
    (opt-lambda (winval unval [sys base-system])
      (if (eq? 'windows sys)
          winval
          unval)))
  

  
  ; command line flags: / for windows, - for unix
  (define flag (alt "/" "-"))
  
  ; object code extention
  (define o-ext (alt "obj" "o"))
  
  ; mzc executable
  ; can be changed to allow for users not having it in path.. not sure why though..
  (define mzc (alt "mzc.exe" "mzc"))
  
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
  
  (define (ldf-flags files)
    (foldl (lambda (file c) (string-append "++ldf " file " " c)) "" files))
  
  (define (build-plot)
    (let* 
        ([tmp-dir (build-path here "src" "tmp")]
         [wxdir                                         
            (simplify-path 
             (build-path 
              (collection-path "mzlib" )
              'up
              'up
              "src"
              "wxcommon"))]
           [zlibfiles  (let 
                           ((zlib-path 
                             (cond 
                               [(directory-exists? (build-path here "src" "zlib"))   (build-path here "src" "zlib")]
                               [(directory-exists? (build-path wxdir "zlib")) (build-path wxdir "zlib")]
                               [else (error "Could not locate zlib src files")])))
                         (find-grep-filter 
                          zlib-path 
                          "\\.[h|c]$" 
                          '("example.c" "maketree.c" "minigzip.c")))]
           [pngfiles (let
                         ((pngfiles-path
                           (cond
                             [(directory-exists? (build-path here "src" "png"))   (build-path here "src" "png")]
                             [(directory-exists? (build-path wxdir "libpng")) (build-path wxdir "libpng")]
                             [else (error "Could not locate png src files")])))
                       (find-grep-filter pngfiles-path "\\.[h|c]$" '("example\\.c" "pngtest\\.c")))]
           [gdfiles 
            (find-grep-filter (build-path here "src" "gd") "\\.[h|c]$" '("gd_wbmp.c" "gdft\\.c"))]
           [plplotfiles 
            (find-grep-filter (build-path here "src" "plplot") "^pl.*\\.c$|\\.h|^mem\\.c|^gd_drv\\.c|pdfutils\\.c")])      
      (make-directory* tmp-dir)
      (copy-files (append 
                   zlibfiles 
                   pngfiles
                   gdfiles
                   plplotfiles)                          
                  (build-path here "src" "tmp"))      
      (let* 
          ((c-files (find-grep-filter tmp-dir "\\.c$"))
           (c-files-string 
            (foldl (lambda (s c) (string-append c " " s)) "" c-files)))        
        (system 
         (string-append 
          mzc 
          " -d " tmp-dir 
          " "
          "++ccf " flag "DHAVE_LIBPNG " 
          "++ccf " flag "DPLD_png " 
          "++ccf " flag "DPLD_mem "
          "++ccf " flag "I" tmp-dir " "
          "--cc " c-files-string))
        (let
            ((obj-files
              (find-grep-filter tmp-dir (string-append "\\." o-ext "$"))))      
          (system (format "~a -d ~a ++ccf ~a ~a  ~a" 
                          mzc
                          ext-dir 
                          (string-append flag "I" (build-path here "src" "tmp"))
                          (ldf-flags obj-files)
                          (build-path here "plplot-low-level.ss")))        
          (delete-directory/files tmp-dir)))))
  
  
  ; build the FIT module
  (define (build-fit)      
    (let* ((fit-dir (build-path here "src" "fit"))
           (fit-files (find-grep-filter fit-dir "\\.c$")))      
      (system (format "~a -d ~a --cc ~a"
                      mzc
                      fit-dir
                      (foldl (lambda (s c) (string-append s " " c)) "" fit-files) ))
      (let ((fit-objs (find-grep-filter fit-dir (string-append "\\." o-ext "$"))))
        (system (format "~a -d ~a ~a ++ccf ~a~a~a ~a"
                        mzc
                        ext-dir
                        (ldf-flags fit-objs)
                        flag
                        "I"
                        fit-dir
                        (build-path here "fit-low-level.ss")
                        ))
        (for-each delete-file fit-objs))))
                 
  (provide pre-installer))
