
#|

The EoPL language can almost be specified via info.ss fields, but
on-execute needs to install the EoPL exception handler as its 
last action. (The module body can't do that, because a `with-handlers'
wraps the load of the module.)

|#

(module eopl-tool mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "tool.ss" "drscheme"))
  
  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define language-base%
	(class* object% (drscheme:language:simple-module-based-language<%>)
	  (define/public (get-language-numbers)
	    '(-400))
	  (define/public (get-language-position)
	    '("Essentials of Programming Languages (2nd ed.)"))
	  (define/public (get-module)
	    '("eopl.ss" "eopl"))
	  (define/public (get-one-line-summary)
	    "Based on the Friedman, Wand, and Haynes text")
	  (define/public (get-reader)
	    read-syntax)
	  (super-instantiate ())))

      (define language%
	(class (drscheme:language:module-based-language->language-mixin
		(drscheme:language:simple-module-based-language->module-based-language-mixin
		 language-base%))
	  (rename [super-on-execute on-execute])
	  (define/override (on-execute settings run-in-user-thread)
	    (super-on-execute settings run-in-user-thread)
	    (run-in-user-thread
	     (lambda ()
	       (current-exception-handler
		(namespace-variable-value 'eopl-exception-handler)))))
	  (super-instantiate ())))

      (define (phase1) (void))
      (define (phase2)
	(drscheme:language-configuration:add-language 
	 (make-object ((drscheme:language:get-default-mixin) language%)))))))
