(require-relative-library "ricedefs.ss")
(require-library "sig.ss" "stepper")
(require-library "cores.ss")
(require-library "pconvers.ss")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")
(require-library "coreflats.ss")
(require-relative-library "ricedefs.ss")

(define-signature plt:prims^
  (beginning
   intermediate
   advanced))

;; the extras signatures don't include core-flat anymore --
;; they are now added differently.
(define-signature plt:beginner-extras^
  ((open mzlib:core-flat^)
   (struct posn (x y) -setters)))

(define-signature plt:intermediate-extras^
  plt:beginner-extras^)

;; will be redefined in guserspace's plt:userspace^
;; file if in drscheme
(define-signature plt:userspace^
  ((open mzlib:core-flat^)
   (struct posn (x y))))
(define-signature plt:advanced-extras^
  ((struct posn (x y))
   (open mzlib:core-flat^)))

;; extend structs with a parsing constructor
(define-macro define-struct/parse
  (lambda (str fields)
    (unless (symbol? str)
      (error 'define-struct/parse "no super structs allowed"))
    (let* ([first car]
	   [second cadr]
	   [second-name 'cadr]
	   [third caddr]
	   [defn (expand-defmacro `(#%define-struct ,str ,fields))]
	   [_ (unless (and (pair? defn)
			   (eq? (car defn) '#%define-values))
		(error 'define-struct/parse "expand-defmacro didn't return expected value: ~s~n" defn))]
	   [bindings (second defn)]
	   [exp (third defn)]
	   [make-parse (string->symbol (string-append "make-" (symbol->string str) "/parse"))]
           [unparse (string->symbol (string-append (symbol->string str) "/unparse"))]
	   [maker-name (second bindings)]
           [unparser
            `(lambda (ele)
               (list
                ,@(map (lambda (field-name) `(list ',field-name (,(string->symbol (string-append 
                                                                                   (symbol->string str)
                                                                                   "-"
                                                                                   (symbol->string field-name)))
                                                                 ele)))
                       fields)))]
	   [parser
	    `(lambda (inits)
	       (apply ,maker-name
		      (map (lambda (field)
			     (let ([m (assq field inits)])
			       (unless m
				 (error ',make-parse "no binding for: ~a" field))
			       (unless (= (length m) 2)
				 (error ',make-parse "malformed binding: ~a" m))
			       (,second-name m)))
			   ',fields)))])
      `(define-values ,(list* make-parse unparse bindings)
	 (call-with-values (lambda () ,exp)
			   (lambda bindings (apply values ,parser ,unparser bindings)))))))

(define-signature plt:init-params^
  (initial-line
   initial-offset
   initial-column
   
   initialize-parameters
   settings
   get-default-setting
   get-default-setting-name

   drscheme-load-handler
   
   raw-reader
   zodiac-reader

   zodiac-vocabulary?
   beginner-language?
   intermediate-language?
   advanced-language?
   full-language?
   
   error-display/debug-handler
   current-vocabulary
   current-setting
   intermediate-values-during-load
   bottom-escape-handler

   drscheme-print

   format-source-loc
   
   primitive-eval
   primitive-load
   syntax-checking-primitive-eval

   process/zodiac
   process/no-zodiac

   process-file/zodiac
   process-file/no-zodiac
   process-sexp/zodiac
   process-sexp/no-zodiac

   (struct process-finish (error?))

   setting-name->number
   number->setting
   setting/unparse
   (struct setting (name
		    vocabulary-symbol
		    primitives
		    macro-libraries
		    case-sensitive?
		    allow-set!-on-undefined?
		    unmatched-cond/case-is-error?
		    allow-improper-lists?
		    sharing-printing?
		    abbreviate-cons-as-list?
		    signal-undefined
		    signal-not-boolean
		    eq?-only-compares-symbols?
		    <=-at-least-two-args
                    error-sym/string-only
		    disallow-untagged-inexact-numbers
		    print-tagged-inexact-numbers
		    whole/fractional-exact-numbers
                    print-booleans-as-true/false
		    printing
		    use-pretty-printer?
		    teaching-primitives-and-syntax?))
   make-setting/parse

   teaching-level?
   
   find-setting-named
   add-setting
   copy-setting

   r4rs-style-printing?))

(define-signature plt:init-namespace^
  (teachpack-error-display
   init-namespace
   bad-teachpacks
   teachpack-ok?
   teachpack-changed))

(define-signature plt:basis^
  ((open plt:init-params^)
   (open plt:init-namespace^)))

(define-signature drscheme:interface^ 
  ((open zodiac:interface^)
   (struct exn:zodiac-syntax (link-tag))
   (struct exn:zodiac-read (link-tag))
   set-zodiac-phase))

(define-signature plt:basis-import^
  (in-mzscheme?))
