
(unit/sig launcher-maker^
  (import)
  
  (define plthome
    (or (getenv "PLTHOME")
	(let ([dir (collection-path "mzlib")])
	  (and dir
	       (let-values ([(base name dir?) (split-path dir)])
		 (and (string? base)
		      (let-values ([(base name dir?) (split-path base)])
			(and (string? base)
			     (complete-path? base)
			     base))))))))
  
  (define (install-template dest kind mz mr)
    (delete-file dest)
    (unless (copy-file (build-path (collection-path "launcher") 
				   (if (eq? kind 'mzscheme) mz mr))
		       dest)
      (error 'make-launcher "Couldn't copy template to destination: ~a" dest)))
  
  (define (string-append/spaces f flags)
    (if (null? flags)
	""
	(string-append
	 (f (car flags))
	 " "
	 (string-append/spaces f (cdr flags)))))
  
  (define (str-list->sh-str flags)
    (letrec ([trans
	      (lambda (s)
		(cond
		  [(regexp-match "(.*)'(.*)" s)
		   => (lambda (m)
			(string-append (trans (cadr m))
				       "\"'\""
				       (trans (caddr m))))]
		  [else (format "'~a'" s)]))])
      (string-append/spaces trans flags)))
  
  (define (str-list->dos-str flags)
    (letrec ([trans
	      (lambda (s)
		(if (or (regexp-match (string #\space #\newline #\tab #\return) s)
			(regexp-match "\"" s)
			(regexp-match "\\\\" s))
		    (list->string
		     (let loop ([l (string->list s)][wrote-slash 0])
		       (case (car l)
			 [(#\\) (cons #\\ (loop (cdr l) (add1 wrote-slash)))]
			 [(#\") (append
				 (string->list (make-string wrote-slash #\\))
				 `(#\" #\\ #\" #\")
				 (loop (cdr l) 0))]
			 [else (cons (car l) (loop (cdr l) 0))])))
		    s))])
      (string-append/spaces trans flags)))
  
  (define (make-unix-launcher kind flags dest)
    (install-template dest kind "sh" "sh") ; just for something that's executable
    (let* ([newline (string #\newline)]
	   [str (str-list->sh-str flags)]
	   [s (string-append
	       "#!/bin/sh" newline
	       "# This script was created by make-~a-launcher" newline
	       newline
	       "if [ \"$PLTHOME\" = '' ] ; then" newline
	       "  PLTHOME=~a" newline
	       "  export PLTHOME" newline
	       "fi" newline
	       newline
	       "SYS=`${PLTHOME}/bin/archsys`" newline
	       newline
	       "exec ${PLTHOME}/.bin/${SYS}/~a ~a ${1+\"$@\"}" newline)])
      (unless plthome
	(error 'make-unix-launcher 
	       "unable to locate PLTHOME"))
      (let ([p (open-output-file dest 'truncate)])
	(fprintf p s
		 kind
		 plthome
		 kind
		 str)
	(close-output-port p))))
  
  
  (define (make-windows-launcher kind flags dest)
    (install-template dest kind "mzstart.exe" "mrstart.exe")
    (let ([str (str-list->dos-str flags)]
	  [p (open-input-file dest)]
	  [m (string->list "<Command Line: Replace This")])
      ; Find the magic start
      (let* ([pos (let loop ([pos 0][l m])
		    (cond
		      [(null? l) (- pos (length m))]
		      [else (let ([c (read-char p)])
			      (when (eof-object? c)
				(close-input-port p)
				(delete-file dest)
				(error 'make-windows-launcher 
				       "Couldn't find command-line position in template"))
			      (if (eq? c (car l))
				  (loop (add1 pos) (cdr l))
				  (loop (add1 pos) m)))]))]
	     [len (+ (length m)
		     (let loop ([c 1])
		       (let ([v (read-char p)])
			 (if (or (eof-object? v) (eq? v #\>))
			     c
			     (loop (add1 c))))))]
	     [total (+ pos len
		       (let loop ([c 0])
			 (if (eof-object? (read-char p))
			     c
			     (loop (add1 c)))))]
	     [v (make-vector total #\000)])
	(file-position p 0)
	(let loop ([pos 0][c total])
	  (unless (zero? c)
	    (vector-set! v pos (read-char p))
	    (loop (add1 pos) (sub1 c))))
	(close-input-port p)
	(when (> (string-length str) len)
	  (delete-file dest)
	  (error 'make-windows-launcher "collection/file name is too long"))
	(let ([p (open-output-file dest 'truncate)])
	  (display (list->string (vector->list v)) p)
	  (file-position p pos)
	  (display str p)
	  (let loop ([c (- len (string-length str))])
	    (unless (zero? c)
	      (write-char #\space p)
	      (loop (sub1 c))))
	  (close-output-port p)))))
  
  (define (make-macos-launcher kind flags dest)
    (install-template dest kind "MzStarter" "MrStarter")
    (let ([p (open-output-file dest 'truncate)])
      (display (str-list->sh-str flags) p)
      (close-output-port p)))
  
  (define (get-maker)
    (case (system-type)
      [(unix) make-unix-launcher]
      [(windows) make-windows-launcher]
      [(macos) make-macos-launcher]))
  
  (define (make-mred-launcher flags dest)
    ((get-maker) 'mred flags dest))
  
  (define (make-mzscheme-launcher flags dest)
    ((get-maker) 'mzscheme flags dest))
  
  (define (make-mred-program-launcher collection dest)
    (make-mred-launcher (list "-A" collection "--") dest))
  
  (define (make-mzscheme-program-launcher file collection dest)
    (make-mzscheme-launcher (list "-mqvL" file collection "--") dest))
  
  (define l-home (if (eq? (system-type) 'unix)
		     (build-path plthome "bin")
		     plthome))
  (define (sfx file) (case (system-type) 
		       [(unix) (list->string
				(map
				 (lambda (c)
				   (if (char-whitespace? c)
				       #\-
				       (char-downcase c)))
				 (string->list file)))]
		       [(windows) (string-append file ".exe")]
		       [else file]))
  
  (define (install-mred-program-launcher collection name)
    (make-mred-program-launcher collection (build-path l-home (sfx name))))
  
  (define (install-mzscheme-program-launcher file collection name)
    (make-mzscheme-program-launcher file collection (build-path l-home (sfx name)))))
