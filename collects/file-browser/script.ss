(module script "mzrestricted.ss"
  (require (lib "unitsig.ss")
           "sigs.ss"
           "script-param.ss")
  
  (provide (all-defined))
  (define-values/invoke-unit/sig script^ (script-unit-param))
  
  ;; all-files: file -> bool
  (define (all-files f) #t)
  
  ;; unhidden-files: file -> bool
  (define (unhidden-files f)
    (not (char=? (string-ref (file-name f) 0)
                 #\.)))
  
  ;; alphabet: file * file -> bool
  (define (alphabet f1 f2)
    (string<? (file-name f1) (file-name f2)))
  
  ;; reverse-sort: file * file -> bool
  (define (reverse-alphabet f1 f2)
    (string>? (file-name f1) (file-name f2)))
  
  ;; dirs-first: file * file -> bool
  (define (dirs-first f1 f2)
    (cond
      ((and (is-directory? f1) (not (is-directory? f2))) #t)
      ((and (is-directory? f2) (not (is-directory? f1))) #f)
      (else (alphabet f1 f2))))
  
  ;; find: (string U file) * (file ->) -> ()
  (define (find start-dir func)
    (let ((files (directory-list start-dir)))
      (for-each func files)
      (for-each (lambda (file)
                  (if (is-directory? file)
                      (find file func)))
                files)))
  
  )