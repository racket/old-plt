(current-directory "/home/robby/plt/misc")
(load "hierarchy.ss")

;; build the m3 heirarchy
(for-each (lambda (x) (apply add-relation x))
	  '((zodiac #f)
	    (parsed zodiac)

	    (form parsed)
	    (app parsed)

	    (varref parsed)
	    (top-level-varref varref)
	    (bound-varref varref)
	    (lexical-varref bound-varref)
	    (binding parsed)
	    (lexical-binding binding)

	    (top-level-varref/bind top-level-varref)
	    
	    (arglist #f)
	    (sym-arglist arglist)
	    (list-arglist arglist)
	    (ilist-arglist arglist)

	    (paroptarglist #f)
	    (sym-paroptarglist paroptarglist)
	    (list-paroptarglist paroptarglist)
	    (ilist-paroptarglist paroptarglist)

	    (set!-form form)
	    (begin-form form)
	    (begin0-form form)
	    (define-values-form form)
	    (let-values-form form)
	    (letrec*-values-form form)
	    (if-form form)
	    (quote-form form)
	    (case-lambda-form form)
	    (struct-form form)
	    
	    (unit-form form)
	    (compound-unit-form form)
	    (invoke-unit-form form)
	    (invoke-open-unit-form form)
	    
	    (interface-form form)
	    (class*/names-form form)
	    
	    (supervar-binding binding)
	    (superinit-binding binding)
	    (public-binding binding)
	    (private-binding binding)
	    (inherit-binding binding)
	    (rename-binding binding)
	    (supervar-varref bound-varref)
	    (superinit-varref bound-varref)
	    (public-varref bound-varref)
	    (private-varref bound-varref)
	    (inherit-varref bound-varref)
	    (rename-varref bound-varref)

	    (public-clause #f)
	    (private-clause #f)
	    (inherit-clause #f)
	    (rename-clause #f)
	    (sequence-clause #f)))

;; use for testing the container classes
;(define container-frame (build-container-frame))

;; use for testing the heirarchy
(define canvas-frame (build-canvas-frame))

;; build a postscript file
(output-postscript "m3-hierarchy.ps" 1)