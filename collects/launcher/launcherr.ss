
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
    (define src (build-path (collection-path "launcher")
                             (if (eq? kind 'mzscheme) mz mr)))
    (when (file-exists? dest)
      (delete-file dest))
    (copy-file src dest))
  
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

  (define one-arg-x-flags '((xa "-display")
			    (xb "-geometry")
			    (xc "-bg" "-background") 
			    (xd "-fg" "-foregound") 
			    (xe "-font")
			    (xf "-name")
			    (xg "-selectionTimeout")
			    (xh "-title")
			    (xi "-xnllanguage")
			    (xj "-xrm")))
  (define no-arg-x-flags '((xk "-iconic") 
			   (xl "-rv" "-reverse") 
			   (xm "+rv") 
			   (xn "-synchronous")))

  (define (skip-x-flags flags)
    (let ([xfmem (lambda (flag) (lambda (xf) (member flag (cdr xf))))])
      (let loop ([f flags])
	(if (null? f)
	    null
	    (if (ormap (xfmem (car f)) one-arg-x-flags)
		(if (null? (cdr f))
		    null
		    (loop (cddr f)))
		(if (ormap (xfmem (car f)) no-arg-x-flags)
		    (loop (cdr f))
		    f))))))

  (define (output-x-arg-getter exec args)
    (let* ([newline (string #\newline)]
	   [or-flags
	    (lambda (l)
	      (if (null? (cdr l))
		  (car l)
		  (string-append
		   (car l)
		   (apply
		    string-append
		    (map (lambda (s) (string-append " | " s)) (cdr l))))))])
      (apply
       string-append
       (append
	(list "# Find X flags and shift them to the front" newline
	      "findxend()" newline
	      "{" newline
	      " oneargflag=''" newline
	      " case \"$1\" in" newline)
	(map
	 (lambda (f)
	   (format (string-append
		    "  ~a)" newline
		    "     oneargflag=\"$1\"" newline
		    "     ~a=\"$2\"" newline
		    "   ;;" newline)
		   (or-flags (cdr f))
		   (car f)))
	 one-arg-x-flags)
	(map
	 (lambda (f)
	   (format "  ~a)~n    ~a=yes~n  ;;~n" (or-flags (cdr f)) (car f)))
	 no-arg-x-flags)
	(list
	 (format (string-append
		  "  *)~n    ~a~a~a  ;;~n"
		  " esac~n"
		  " shift~n"
		  " if [ \"$oneargflag\" != '' ] ; then~n"
		  "   if [ \"${1+n}\" != 'n' ] ; then echo $0: missing argument for standard X flag $oneargflag ; exit 1 ; fi~n"
		  "   shift~n"
		  " fi~n"
		  " findxend ${1+\"$@\"}~n"
		  "}~nfindxend ${1+\"$@\"}~n")
		 exec 
		 (apply
		  string-append
		  (append
		   (map
		    (lambda (f) (format " ${~a+\"~a\"} ${~a+\"$~a\"}" (car f) (cadr f) (car f) (car f)))
		    one-arg-x-flags)
		   (map
		    (lambda (f) (format " ${~a+\"~a\"}" (car f) (cadr f)))
		    no-arg-x-flags)))
		 args))))))

  (define (make-unix-launcher kind flags dest)
    (install-template dest kind "sh" "sh") ; just for something that's executable
    (let* ([newline (string #\newline)]
	   [post-flags (if (eq? kind 'mred)
			   (skip-x-flags flags)
			   null)]
	   [pre-flags (if (null? post-flags)
			  flags
			  (let loop ([f flags])
			    (if (eq? f post-flags)
				null
				(cons (car f) (loop (cdr f))))))]
	   [pre-str (str-list->sh-str pre-flags)]
	   [post-str (str-list->sh-str post-flags)]
	   [header (format
		    (string-append
		     "#!/bin/sh" newline
		     "# This script was created by make-~a-launcher" newline
		     newline
		     "if [ \"$PLTHOME\" = '' ] ; then" newline
		     "  PLTHOME=~a" newline
		     "  export PLTHOME" newline
		     "fi" newline
		     newline
		     "SYS=`${PLTHOME}/bin/archsys`" newline
		     newline)
		    kind plthome)]
	   [exec (format
		  "exec ${PLTHOME}/.bin/${SYS}/~a ~a"
		  kind pre-str)]
	   [args (format
		  " ~a ${1+\"$@\"}~n"
		  post-str)]
	   [assemble-exec (if (and (eq? kind 'mred)
				   (not (null? post-flags)))
			      output-x-arg-getter
			      string-append)])
      (unless plthome
	(error 'make-unix-launcher 
	       "unable to locate PLTHOME"))
      (let ([p (open-output-file dest 'truncate)])
	(fprintf p "~a~a"
		 header
		 (assemble-exec exec args))
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
				(when (file-exists? dest)
				  (delete-file dest))
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
	  (when (file-exists? dest)
	    (delete-file dest))
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
    (install-template dest kind "GoMz" "GoMr")
    (let ([p (open-output-file dest 'truncate)])
      (display (str-list->sh-str flags) p)
      (close-output-port p)))
  
  (define (get-maker)
    (case (system-type)
      [(unix beos) make-unix-launcher]
      [(windows) make-windows-launcher]
      [(macos) make-macos-launcher]))
  
  (define (make-mred-launcher flags dest)
    ((get-maker) 'mred flags dest))
  
  (define (make-mzscheme-launcher flags dest)
    ((get-maker) 'mzscheme flags dest))
  
  (define (make-mred-program-launcher file collection dest)
    (make-mred-launcher (list "-mqvL" file collection "--") dest))
  
  (define (make-mzscheme-program-launcher file collection dest)
    (make-mzscheme-launcher (list "-mqvL" file collection "--") dest))
  
  (define l-home (if (memq (system-type) '(unix beos))
		     (build-path plthome "bin")
		     plthome))
  (define (sfx file) (case (system-type) 
		       [(unix beos) (list->string
				     (map
				      (lambda (c)
					(if (char-whitespace? c)
					    #\-
					    (char-downcase c)))
				      (string->list file)))]
		       [(windows) (string-append file ".exe")]
		       [else file]))

  (define (mred-program-launcher-path name)
    (build-path l-home (sfx name)))
  
  (define (mzscheme-program-launcher-path name)
    (mred-program-launcher-path name))

  (define (install-mred-program-launcher file collection name)
    (make-mred-program-launcher file collection (mred-program-launcher-path name)))
  
  (define (install-mzscheme-program-launcher file collection name)
    (make-mzscheme-program-launcher file collection (mzscheme-program-launcher-path name))))
