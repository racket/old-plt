(module util mzscheme
  
  (require "config.ss"
           "private/planet-shared.ss"
           (lib "pack.ss" "setup"))

  #| The util collection provides a number of useful functions for interacting with the PLaneT system. |#
  
  (provide current-cache-contents
           current-linkage
           make-planet-archive)
    
  ;; current-cache-contents : -> ((string ((string ((nat (nat ...)) ...)) ...)) ...)
  ;; returns the packages installed in the local PLaneT cache
  (define (current-cache-contents)
    (define id (lambda (x) x))
    (cdr
     (tree->list
      (filter-tree-by-pattern
       (directory->tree (cache-dir) (lambda (x) (not (regexp-match ".*/CVS$" x))))
       (list id id id string->number string->number)))))
  
  

  ;; current-linkage : -> ((symbol (package-name nat nat) ...) ...)
  ;; gives the current "linkage table"; a table that links modules to particular versions
  ;; of planet requires that satisfy those linkages
  (define (current-linkage)
    (let* ((links (with-input-from-file (LINKAGE-FILE) read-all))
           (buckets (categorize caar links)))
      (map
       (lambda (x) (cons (car x) (map (lambda (y) (drop-last (cadr y))) (cadr x))))
       buckets)))
       
  ;; make-planet-archive: directory [file] -> file
  ;; Makes a .plt archive file suitable for PLaneT whose contents are
  ;; all files in the given directory and returns that file's name.
  ;; If the optional filename argument is provided, that filename will 
  ;; be used as the output file's name.
  (define make-planet-archive
    (case-lambda
      [(dir) 
       (let-values ([(path name must-be-dir?) (split-path dir)])
         (make-planet-archive dir (string-append name ".plt")))]
      [(dir archive-name)
       (begin
         (parameterize ((current-directory dir))
           (pack archive-name
                 "archive" 
                 '(".") 
                 '()
                 std-filter
                 #t
                 'file
                 #f
                 #f))
         (build-path dir archive-name))])))