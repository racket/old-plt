;; option.ss
;; parses and sets compiler options for the MzScheme Compiler
;; (c) 1996 Sebastian Good

;;------------------------------------------------------------------------
;; OPTIONS
;;

(require-library "functio.ss")
(require-library "file.ss" "mzscheme" "dynext")

(define compiler:option:propagate-constants (make-parameter #t))
(define compiler:option:assume-primitives (make-parameter #f))
(define compiler:option:stupid (make-parameter #f))

   (define vehicles:automatic 'vehicles:automatic)
   (define vehicles:functions 'vehicles:functions)
   (define vehicles:units 'vechicles:units)
   (define vehicles:monolithic 'vehicles:monolithic)
(define compiler:option:vehicles (make-parameter vehicles:automatic))
(define compiler:option:vehicles:monoliths (make-parameter 1))
(define compiler:option:seed (make-parameter 2001))
(define max-monoliths 32)

(define compiler:option:verbose (make-parameter #f))
(define compiler:option:debug (make-parameter #f))
(define compiler:option:test (make-parameter #f))
(define compiler:option:c-only (make-parameter #f))
(define compiler:option:multi-o (make-parameter #f))
(define compiler:option:clean-intermediate-files (make-parameter #t))

(define compiler:option:max-exprs-per-top-level-set (make-parameter 25))

(define compiler:option:setup-prefix (make-parameter ""))

(define (compiler:option:multi-o-constant-pool)
  (compiler:option:multi-o))

; Maybe #f helps for register-poor architectures?
(define compiler:option:unpack-environments (make-parameter #t))

; Returns (values mode files prefixes)
;  where mode is 'compile, 'link, or 'zo
(define (parse-options argv)
  (parse-command-line
   "mzc"
   argv
   `([once-any
      [("-e" "--extention")
       ,(lambda (f) 'compile)
       (,(format "Output ~a file(s) from Scheme source(s) (default)" (append-extension-suffix "")))]
      [("-c" "--c-source")
       ,(lambda (f) (compiler:option:c-only #t) 'compile)
       (,(format "Output only ~a file(s) from Scheme source(s)" (append-c-suffix "")))]
      [("-o" "--object")
       ,(lambda (f) (compiler:option:multi-o #t) 'compile)
       (,(format "Output ~a and ~a files from Scheme source(s) for a multi-file extension" 
		 (append-object-suffix "")
		 (append-constant-pool-suffix "")))]
      [("-l" "--link-extension")
       ,(lambda (f) 'link)
       (,(format "Link multiple ~a files into a ~a file (using ~a files)"
		 (append-object-suffix "")
		 (append-extension-suffix "")
		 (append-constant-pool-suffix "")))]
      [("-z" "--zo")
       ,(lambda (f) 'zo)
       (,(format "Output ~a file(s) from Scheme source(s)" (append-zo-suffix "")))]
      [("--collection-extension")
       ,(lambda (f) 'collection-extension)
       ("Compile specificed collection to extension")]
      [("--collection-zos")
       ,(lambda (f) 'collection-zos)
       (,(format "Compile specified collection to ~a files" (append-zo-suffix "")))]]
     [once-any
      [("-M" "--monoliths") 
       ,(lambda (f v) 
	  (unless (string->number v)
	      (error 'mzc "monolith argument must be a number"))
	  (let ([num (string->number v)])
	    (unless (and (integer? num)
			 (positive? num)
			 (<= num max-monoliths))
		    (error 'mzc:compile "monoliths must be a number between 1 and ~a"
			   max-monoliths))
	    (compiler:option:monoliths num)))
       ("Use n monolithic vehicles during compilation" "n")]
      [("--va")
       ,(lambda (f) (compiler:option:vehicles vehicles:automatic))
       ("Try to optimize function vehicle selection during compilation")]
      [("--vf")
       ,(lambda (f) (compiler:option:vehicles vehicles:function))
       ("Use per-function vehicles during compilation")]
      [("--vu")
       ,(lambda (f) (compiler:option:vehicles vehicles:unit))
       ("Use per-unit vehicles during compilation")]]
     [multi
      [("--ccf-clear") 
       ,(lambda (f) (current-extension-compiler-flags null))
       ("Clear C compiler flags (allowed multiple times)")]
      [("++ccf") 
       ,(lambda (f v) (current-extension-compiler-flags
		       (cons v (current-extension-compiler-flags))))
       ("Add C compiler flag (allowed multiple times)" "flag")]
      [("--ccf") 
       ,(lambda (f v) (current-extension-compiler-flags
		       (remove v (current-extension-compiler-flags))))
       ("Remove C compiler flag (allowed multiple times)" "flag")]
      [("-p" "--prefix") 
       ,(lambda (f v) v)
       ("Add elaboration-time prefix file; i.e., a header file (allowed multiple times)" "file")]]
     [once-each
      [("--cc") 
       ,(lambda (f v) (current-extension-compiler v))
       ("Use <compiler> as C compiler" "compiler")]
      [("-n" "--name") 
       ,(lambda (f name) (compiler:option:setup-prefix name))
       ("Embed <name> as an extra part of public low-level names" "name")]
      [("--seed") 
       ,(lambda (f v) 
	  (unless (string->number v)
		  (error 'mzc "random number seed must be a number"))
	  (let ([num (string->number v)])
	    (unless (and (integer? num)
			 (< (abs num) (expt 2 30)))
		    (error 'mzc "random number seed must be a smallish number"))
	    (compiler:option:seed num)))
       ("Seed monolith randomizer (with -M)" "seed")]
      [("-v") 
       ,(lambda (f) (compiler:option:verbose #t))
       ("Verbose mode")]
#|
      [("--test")
       ,(lambda (f) (compiler:option:test #t))
       ("Test mode")]
|#
      [("--no-prop")
       ,(lambda (f) (compiler:option:propagate-constants #f))
       ("Don't propogate constants")]
      [("--prim")
       ,(lambda (f) (compiler:option:assume-primitives #t))
       ("Assume primitives (e.g., treat `car' as `#%car')")]
      [("--stupid")
       ,(lambda (f) (compiler:option:stupid #t))
       ("Compile despite obvious non-syntactic errors")]
      [("--dirty")
       ,(lambda (f) (compiler:option:clean-intermediate-files #f))
       ("Don't remove intermediate files")]
      [("-D" "--debug")
       ,(lambda (f) (compiler:option:debug #t))
       ("Write debugging output to dump.txt")]])
   (lambda (accum file . files)
     (values 
      (let ([l (filter symbol? accum)])
	(if (null? l)
	    'compile
	    (car l)))
      (cons file files) 
      (filter string? accum)))
   (list "file or collection" "file or collection")))
