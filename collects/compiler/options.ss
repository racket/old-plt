;; option.ss
;; parses and sets compiler options for the MzScheme Compiler
;; (c) 1996 Sebastian Good

;;------------------------------------------------------------------------
;; OPTIONS
;;

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

; Returns (values files prefixes)
(define (parse-options argv)
  (parse-command-line
   "mzc"
   argv
   `([once-any
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
       ("Use n monolithic vehicles" "n")]
      [("--va")
       ,(lambda (f) (compiler:option:vehicles vehicles:automatic))
       ("Try to optimize function vehicle selection")]
      [("--vf")
       ,(lambda (f) (compiler:option:vehicles vehicles:function))
       ("Use per-function vehicles")]
      [("--vu")
       ,(lambda (f) (compiler:option:vehicles vehicles:unit))
       ("Use per-unit vehicles")]]
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
       ("Prefix file (elaboration time)" "file")]]
     [once-any
      [("-c")
       ,(lambda (f) (compiler:option:c-only #t))
       ("Output .c file only")]
      [("-o")
       ,(lambda (f) (compiler:option:multi-o #t))
       ("Output .c/.o file for a multi-file extension")]]
     [once-each
      [("--cc") 
       ,(lambda (f v) (current-extension-compiler v))
       ("C compiler" "compiler")]
      [("--seed") 
       ,(lambda (f v) 
	  (unless (string->number v)
		  (error 'mzc "random number seed must be a number"))
	  (let ([num (string->number v)])
	    (unless (and (integer? num)
			 (< (abs num) (expt 2 30)))
		    (error 'mzc "random number seed must be a smallish number"))
	    (compiler:option:seed num)))
       ("Seed monolith randomizer" "seed")]
      [("-v") 
       ,(lambda (f) (compiler:option:verbose #t))
       ("Verbose mode")]
      [("-D")
       ,(lambda (f) (compiler:option:debug #t))
       ("Write debugging output to dump.txt")]
      [("--test")
       ,(lambda (f) (compiler:option:test #t))
       ("Test mode")]
      [("--no-prop")
       ,(lambda (f) (compiler:option:propagate-constants #f))
       ("Don't propogate constants")]
      [("--prim")
       ,(lambda (f) (compiler:option:assume-primitives #t))
       ("Assume primitives")]
      [("--stupid")
       ,(lambda (f) (compiler:option:stupid #t))
       ("Compile despite obvious errors")]
      [("--dirty")
       ,(lambda (f) (compiler:option:clean-intermediate-files #f))
       ("Don't remove intermediate files")]])
   (lambda (prefixes file . files)
     (values (cons file files) prefixes))
   (list "file" "file")))
