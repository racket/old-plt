
(module get-info mzscheme

  (import (lib "file.ss"))
  (import (lib "match.ss"))

  (export get-info)

  (define (get-info coll-path)
    (let ([dir (apply collection-path coll-path)])
      (with-input-from-file* (build-path dir "info.ss")
	(lambda ()
	  (let ([r (read)])
	    (unless (eof-object? (read))
	      (error "info.ss file has multiple expressions in ~a" dir))
	    (match r
	      [('module 'info '(lib "infotab.ss" "setup")
		 expr ...)
	       'ok]
	      [else (error "info.ss file does not contain a module of the right shape")]))))
      (dynamic-import `(lib "info.ss" ,@coll-path) '#%info-lookup))))

