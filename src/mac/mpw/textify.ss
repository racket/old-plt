(require (lib "process.ss")
	 (lib "etc.ss"))

(define re (regexp "[.](c|cc|cxx|cpp|h|inc)$"))
(define re:TEXT (regexp "TEXT"))

(define (go p)
  (cond
   [(directory-exists? p)
    (printf "Checking ~a~n" p)
    (map go
	 (map (lambda (f) (build-path p f))
	      (directory-list p)))]
   [(file-exists? p)
    (when (regexp-match re p)
      (unless (let* ([p (process* "/Developer/Tools/GetFileInfo" "-t" p)])
		(begin0
		 (regexp-match re:TEXT (read-line (car p)))
		 (close-input-port (car p))
		 (close-output-port (cadr p))
		 (close-input-port (cadddr p))))
	(printf "Textifying ~a~n" p)
	'(system (format "/Developer/Tools/Rez /dev/null -t TEXT -o ~a" p))))]))

(go (build-path (this-expression-source-directory) 'up 'up))
