
(unit/sig
 launcher-maker^
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


 (define (make-unix-launcher kind str dest)
   (install-template dest kind "sh" "sh") ; just for something that's executable
   (let* ([newline (string #\newline)]
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


 (define (make-windows-launcher kind str dest)
   (install-template dest kind "mzstart.exe" "mrstart.exe")
   (let ([p (open-input-file dest)]
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
		   (loop (sub1 c))))))))

 (define (make-macos-launcher kind str dest)
   (install-template dest kind "MzStarter" "MrStarter")
   (let ([p (open-output-file dest 'truncate)])
     (display str p)
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
   (make-mred-launcher (format "-A ~a --" collection) dest))

 (define (make-mzscheme-program-launcher file collection dest)
   (make-mzscheme-launcher (format "-qmL ~a ~a -e '(exit)' --" file collection) dest))

 (define l-home (if (eq? (system-type) 'unix)
		    (build-path plthome "bin")
		    plthome))
 
 (define (install-mred-program-launcher collection name)
   (make-mred-program-launcher collection (build-path l-home name)))

 (define (install-mzscheme-program-launcher file collection name)
   (make-mzscheme-program-launcher file collection (build-path l-home name))))
