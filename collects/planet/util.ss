(module util mzscheme
  
  (require "config.ss"
           (lib "list.ss")
           (lib "pack.ss" "setup"))

  #| The util collection provides a number of useful functions for interacting with the PLaneT system. |#
  
  (provide current-repository-contents
           current-linkage
           make-planet-archive)
  
  
  
  ;; current-repository-contents : -> ((string (string (number (number ...)) ...) ...) ...)
  (define (directory->tree directory valid-dir?)
    (let-values ([(path name _) (split-path directory)])
      (let* ((files (directory-list directory))
             (files (map (lambda (d) (build-path directory d)) files))
             (files (filter (lambda (d) (and (directory-exists? d) (valid-dir? d))) files)))
        (cond
          [(null? files) name]
          [else 
           (list name (map (lambda (d) (directory->tree d valid-dir?)) files))]))))
  
  
  
  ;; current-repository-contents : -> ((string ((string ((nat (nat ...)) ...)) ...)) ...)
  ;; returns the packages installed in the local PLaneT cache
  ;; bug: this code is godawful
  (define (current-repository-contents)
    (let ((tree (cadr (directory->tree (cache-dir) (lambda (x) (not (regexp-match ".*/CVS$" x)))))))
      (map
       (lambda (usr+)
         (list
          (car usr+)
          (map 
           (lambda (pkg+)
             (list
              (car pkg+)
              (map
               (lambda (maj-ver+)
                 (list 
                  (string->number (car maj-ver+))
                  (map
                   (lambda (min-ver) (string->number min-ver))
                   (cadr maj-ver+))))
               (cadr pkg+))))
           (cadr usr+))))
       tree)))
  
  
  ;; current-linkage : -> ((filename (package-name nat nat) ...) ...)
  ;; gives the current "linkage table"; a table that links modules to particular versions
  ;; of planet requires that satisfy those linkages
  (define (current-linkage)
    (void))
  
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
         (build-path dir archive-name))]))
         
      
  
                
  
  
          
          
          
          
          
  )