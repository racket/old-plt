(module make-plt mzscheme
  (require (lib "file.ss")
           (lib "pack.ss" "setup")
           (lib "etc.ss"))
  
  (current-directory (build-path (this-expression-source-directory) 'up 'up 'up))
  
  (define (display-res f)
    (lambda (x)
      (let ([ans (f x)])
        (when ans
          (printf "including ~s~n" x))
        ans)))
  
  (define bad-dirs '("CVS"))
  (define bad-files '(".DS_Store" "reduction-semantics.plt"))
  
  (pack (build-path "collects" "reduction-semantics" "reduction-semantics.plt")
        "Reduction Semantics"
        (list (build-path "collects" "reduction-semantics"))
        '(("reduction-semantics"))
        (display-res
         (lambda (filename)
           (let ([exp (reverse (explode-path (normalize-path filename)))])
             (cond
               [(member (cadr exp) bad-dirs)
                #f]
               [(member (car exp) bad-dirs)
                #f]
               [(member (car exp) bad-files)
                #f]
               [(char=? (string-ref filename (- (string-length filename) 1)) #\~)
                #f]
               [else 
                (std-filter filename)]))))
        #t
        'file-replace))