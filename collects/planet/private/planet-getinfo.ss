(module planet-getinfo mzscheme

  (require (lib "file.ss"))
  (require (lib "match.ss"))

  (provide get-info get-info-from-file)

  (define (get-info coll-path)
    (let* ([dir (apply collection-path coll-path)]
	   [file (build-path dir "info.ss")])
      (get-info-from-file file)))
  
  (define (get-info-from-file file)
    (if (file-exists? file)
        (begin
          (with-input-from-file file
            (lambda ()
              (let ([r (read)])
                (unless (eof-object? (read))
                  (error "info.ss file has multiple expressions in ~a" file))
                (match r
                  [('module 'info '(lib "infotab.ss" "setup")
                     expr ...)
                   'ok]
                  [else (error 
                         'get-info
                         "info file does not contain a module of the right shape: \"~a\""
                         file)]))))
          (dynamic-require `(file ,file) '#%info-lookup))
        #f)))


