
(module eopl-lang mzscheme
  (require (lib "class.ss")
	   (lib "unitsig.ss")
	   (lib "tool.ss" "drscheme"))
  
  (provide tool@)

  (define tool@
    (unit/sig ()
      (import drscheme:tool^)

      (drscheme:language-configuration:add-language
       (make-object
	(drscheme:language:module-based-language->language-mixin
	 (class* object% (drscheme:language:module-based-language<%>)
		 (public*
		  [config-panel (lambda (parent)
				  (case-lambda
				   [() #f]
				   [(ok) (void)]))]
		  [default-settings (lambda () #f)]
		  [default-settings? (lambda (x) #t)]
		  [get-language-position (lambda () (list "EoPL"))]
		  [get-module (lambda () '(lib "eopl.ss" "eopl"))]
		  [get-transformer-module (lambda () 'mzscheme)]
		  [marshall-settings (lambda (x) x)]
		  [on-execute (lambda (s r) (void))]
		  [render-value (lambda (v s port port-write) (write v port))]
		  [render-value/format (lambda (v s port port-write) (write v port) (newline port))]
		  [unmarshall-settings (lambda (x) x)]
		  [use-namespace-require/copy? (lambda () #t)])
		 (super-make-object))))))))

