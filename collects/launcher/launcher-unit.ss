
(module launcher-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "string.ss")
	   (lib "etc.ss")

	   (lib "compile-sig.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "embed.ss" "compiler")

	   "launcher-sig.ss")

  (provide launcher@)

  (define launcher@
    (unit/sig launcher^
      (import [c : dynext:compile^]
	      [l : dynext:link^])

      ;; NOTE: evil name dependencies here:
      
      (define mz-app-ppc "MzScheme PPC")
      (define mz-app-68k "MzScheme 68k")
      (define mr-app-ppc "MrEd PPC")
      (define mr-app-68k "MrEd 68k")
      
      ;; END of evil name dependencies
      
      (define plthome
	(with-handlers ([(lambda (x) #t) (lambda (x) #f)])
	  (or (let ([p (getenv "PLTHOME")])
		(and p (normal-case-path (expand-path p))))
	      (let ([dir (collection-path "mzlib")])
		(and dir
		     (let-values ([(base name dir?) (split-path dir)])
		       (and (string? base)
			    (let-values ([(base name dir?) (split-path base)])
			      (and (string? base)
				   (complete-path? base)
				   (normal-case-path (expand-path base)))))))))))
      
      (define current-launcher-variant
	(make-parameter 'normal
			(lambda (v)
			  (unless (memq v '(normal 3m))
			    (raise-type-error
			     'current-launcher-variant
			     "variant symbol"
			     v))
			  v)))

      (define (available-variants kind)
	(if (eq? 'unix (system-type))
	    (append
	     (if (and plthome
		      (file-exists? (build-path plthome "bin" (format "~a3m" kind))))
		 '(3m)
		 null)
	     '(normal))
	    ;; 3m launchers not yet supported for other platforms:
	    '(normal)))

      (define (available-mred-variants)
	(available-variants 'mred))

      (define (available-mzscheme-variants)
	(available-variants 'mzscheme))

      (define (install-template dest kind mz mr)
	(define src (build-path (collection-path "launcher")
				(if (eq? kind 'mzscheme) mz mr)))
	(when (file-exists? dest)
	  (delete-file dest))
	(copy-file src dest))

      (define (variant-suffix variant)
	(case variant
	  [(normal) ""]
	  [(3m) "3m"]))

      (define (add-file-suffix path variant)
	(let ([s (variant-suffix variant)])
	  (if (string=? "" s)
	      path
	      ;; FIXME for windows - need to add before the extension, if any
	      (string-append path s))))
      
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
		    (if (or (regexp-match (string #\[ #\space #\newline #\tab #\return #\vtab #\]) s)
			    (regexp-match "\"" s)
			    (regexp-match "\\\\" s))
			(list->string
			 (let loop ([l (string->list s)][wrote-slash 0])
			   (cond
			    [(null? l) null]
			    [(char-whitespace? (car l))
			     (append
			      (string->list (make-string wrote-slash #\\))
			      (list #\" (car l) #\")
			      (loop (cdr l) 0))]
			    [else
			     (case (car l)
			       [(#\\) (cons #\\ (loop (cdr l) (add1 wrote-slash)))]
			       [(#\") (append
				       (string->list (make-string wrote-slash #\\))
				       `(#\" #\\ #\" #\")
				       (loop (cdr l) 0))]
			       [else (cons (car l) (loop (cdr l) 0))])])))
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

      (define (make-unix-launcher kind variant flags dest aux)
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
			 "  PLTHOME=\"~a\"" newline
			 "  export PLTHOME" newline
			 "fi" newline
			 newline)
			kind (regexp-replace* "\"" plthome "\\\\\""))]
	       [exec (format
		      "exec \"${PLTHOME}/bin/~a~a\" ~a"
		      kind (variant-suffix variant) pre-str)]
	       [args (format
		      " ~a ${1+\"$@\"}~n"
		      post-str)]
	       [assemble-exec (if (and (eq? kind 'mred)
				       (not (null? post-flags)))
				  output-x-arg-getter
				  string-append)])
	  (unless plthome
	    (error 'make-unix-launcher "unable to locate PLTHOME"))
	  (let ([p (open-output-file dest 'truncate)])
	    (fprintf p "~a~a"
		     header
		     (assemble-exec exec args))
	    (close-output-port p))))
      
      (define (make-windows-launcher kind variant flags dest aux)
	(if (not (and (let ([m (assq 'independent? aux)])
			(and m (cdr m)))))
	    ;; Normal launcher: 
	    (make-embedding-executable dest (eq? kind 'mred) #f
				       null null null
				       flags
				       aux
				       #t)
	    ;; Independent launcher (needed for Setup PLT):
	    (begin
	      (install-template dest kind "mzstart.exe" "mrstart.exe")
	      (let ([str (str-list->dos-str flags)]
		    [p (open-input-file dest)]
		    [m "<Command Line: Replace This[^>]*>"]
		    [x "<Executable Directory: Replace This[^>]*>"])
		(let* ([exedir (string-append 
				plthome
				;; null character marks end of executable directory
				(string (latin-1-integer->char 0)))]
		       [find-it ; Find the magic start
			(lambda (magic s)
			  (file-position p 0)
			  (let ([m (regexp-match-positions magic p)])
			    (if m
				(car m)
				(begin
				  (close-input-port p)
				  (when (file-exists? dest)
					(delete-file dest))
				  (error 
				   'make-windows-launcher 
				   (format
				    "Couldn't find ~a position in template" s))))))]
		       [exedir-poslen (find-it x "executable path")]
		       [command-poslen (find-it m "command-line")]
		       [pos-exedir (car exedir-poslen)]
		       [len-exedir (- (cdr exedir-poslen) (car exedir-poslen))]
		       [pos-command (car command-poslen)]
		       [len-command (- (cdr command-poslen) (car command-poslen))]
		       [write-magic
			(lambda (p s pos len)
			  (file-position p pos)
			  (display s p)
			  (display (make-string (- len (string-length s)) #\space) p))]
		       [check-len
			(lambda (len s es)
			  (when (> (string-length s) len)
				(when (file-exists? dest)
				      (delete-file dest))
				(error 
				 (format	
				  "~a exceeds limit of ~a characters with ~a characters: ~a"
				  es len (string-length s) s))))])
		  (close-input-port p)
		  (check-len len-exedir exedir "executable home directory")
		  (check-len len-command str "collection/file name")
		  (let ([p (open-output-file dest 'update)])
		    (write-magic p exedir pos-exedir len-exedir)   
		    (write-magic p str pos-command len-command)   
		    (close-output-port p)))))))
      
      ;; OS X launcher code:
     
      ; make-macosx-launcher : symbol (listof str) pathname ->  
      (define (make-macosx-launcher kind variant flags dest aux)
	(if (eq? kind 'mzscheme) 
	    ;; MzScheme launcher is the same as for Unix
	    (make-unix-launcher kind variant flags dest aux)
	    ;; MrEd "launcher" is a stand-alone executable
	    (make-embedding-executable dest (eq? kind 'mred) #f
				       null null null
				       flags
				       aux
				       #t)))
        
      
      (define (make-macos-launcher kind variant flags dest aux)
	(install-template dest kind "GoMr" "GoMr")
	(let ([p (open-output-file dest 'truncate)])
	  (display (str-list->sh-str flags) p)
	  (close-output-port p)))
      
      (define (get-maker)
	(case (system-type)
	  [(unix) make-unix-launcher]
	  [(windows) make-windows-launcher]
	  [(macos) make-macos-launcher]
	  [(macosx) make-macosx-launcher]))
      
      (define make-mred-launcher
	(opt-lambda (flags dest [aux null])
	  (let ([variant (current-launcher-variant)])
	    ((get-maker) 'mred variant flags dest aux))))
      
      (define make-mzscheme-launcher
	(opt-lambda (flags dest [aux null])
	  (let ([variant (current-launcher-variant)])
	    ((get-maker) 'mzscheme variant flags dest aux))))
      
      (define (strip-suffix s)
	(regexp-replace "[.]..?.?$" s ""))

      (define (build-aux-from-path aux-root)
	(let ([try (lambda (key suffix)
		     (let ([p (string-append aux-root suffix)])
		       (if (file-exists? p)
			   (list (cons key p))
			   null)))])
	  (append
	   (try 'icns ".icns")
	   (try 'ico ".ico")
	   (try 'independent? ".lch"))))

      (define (make-mred-program-launcher file collection dest)
	(make-mred-launcher (list "-mqvL" file collection "--") 
			    dest
			    (build-aux-from-path
			     (build-path (collection-path collection)
					 (strip-suffix file)))))
      
      (define (make-mzscheme-program-launcher file collection dest)
	(make-mzscheme-launcher (list "-mqvL" file collection "--") 
				dest
				(build-aux-from-path
				 (build-path (collection-path collection)
					     (strip-suffix file)))))
      
      (define l-home (if (memq (system-type) '(unix))
			 (build-path plthome "bin")
			 plthome))

      (define l-home-macosx-mzscheme (build-path plthome "bin"))

      (define (unix-sfx file)
	(list->string
	 (map
	  (lambda (c)
	    (if (char-whitespace? c)
		#\-
		(char-downcase c)))
	  (string->list file))))

      (define (sfx file) (case (system-type) 
			   [(unix) (unix-sfx file)]
			   [(windows) (string-append file ".exe")]
			   [else file]))

      (define (mred-program-launcher-path name)
	(string-append
	 (add-file-suffix 
	  (build-path l-home (sfx name))
	  (current-launcher-variant))
	 (if (eq? (system-type) 'macosx) 
	     ".app" 
	     "")))
      
      (define (mzscheme-program-launcher-path name)
	(case (system-type)
	  [(macosx) (add-file-suffix 
		     (build-path l-home-macosx-mzscheme (unix-sfx name))
		     (current-launcher-variant))]
	  [else (mred-program-launcher-path name)]))

      (define mred-launcher-up-to-date?
	(opt-lambda (dest [aux null])
          (mzscheme-launcher-up-to-date? dest aux)))

      (define mzscheme-launcher-up-to-date?
	(opt-lambda (dest [aux null])
           (cond
	    [(eq? 'unix (system-type))
	     (file-exists? dest)]
	    [(eq? 'windows (system-type))
	     (and (let ([m (assq 'independent? aux)])
		    (and m (cdr m)))
		  (file-exists? dest))]
	    [else #f])))

      (define (install-mred-program-launcher file collection name)
	(make-mred-program-launcher file collection (mred-program-launcher-path name)))
      
      (define (install-mzscheme-program-launcher file collection name)
	(make-mzscheme-program-launcher file collection (mzscheme-program-launcher-path name))))))
