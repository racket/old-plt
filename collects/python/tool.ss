(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
	   (lib "embed.ss" "compiler")
	   (lib "string-constant.ss" "string-constants")
           (lib "etc.ss")
           "python.ss"
           "primitives.ss")

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) 
;            (drscheme:get/extend:extend-interactions-text
;                  (lambda (super%)
;                    (class super%
;                      (rename [super-display-results display-results])
;                      
;                      (define/override display-results
;                        (opt-lambda (results)
;                          (super-display-results (map (lambda (result)
;                                                        (if (python-node? result)
;                                                            (py-object%->string result)
;                                                            result))
;                                                      results))))
;                      
;                      (super-instantiate ()))))
        (drscheme:language-configuration:add-language
         (make-object (override-mrflow-methods
                       ((drscheme:language:get-default-mixin) 
                        lang%)))))
      
      (define (override-mrflow-methods %)
        %) ;; TODO: MRFLOW PRIMITIVES


      (define lang%
        (class* object% (drscheme:language:language<%>)
          (define/public (config-panel parent)
            (case-lambda
              [() null]
              [(x) (void)]))
          (define/public (default-settings) null)
          (define/public (default-settings? x) #t)
          (define/public (front-end input settings)
            (let-values ([(port name)
                          (if (string? input)
                              (values (open-input-file input) (path->complete-path input))
                              (let ([text (drscheme:language:text/pos-text input)])
                                (values
                                 (open-input-string
                                  (send text
                                        get-text
                                        (drscheme:language:text/pos-start input)
                                        (drscheme:language:text/pos-end input)))
                                 text)))])

              (let ([ast-list (parse-python-port port name)])
                (lambda ()
                  (if (null? ast-list)
                      eof
                      (begin0 (compile-python-ast (car ast-list))
                              (set! ast-list (cdr ast-list))))))))
          
          (define/public (get-style-delta) #f)
          (define/public (get-language-position) (list "Python"))
          (define/public (get-language-name) "Python")
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers) (list 10))
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)
          (define/public (on-execute settings run-in-user-thread)



            (dynamic-require '(lib "base.ss" "python") #f)
            (let ([path ((current-module-name-resolver) '(lib "base.ss" "python") #f #f)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
		 (current-eval 
		  (drscheme:debug:make-debug-eval-handler (current-eval)))
                 
                 
                 (with-handlers ([void (lambda (x)
                                         (printf "~a~n"
                                                 (exn-message x)))])
                   (namespace-attach-module n path)
                   (namespace-require path))))))
          (define/public (render-value value settings port port-write)
            (let ([to-render (if (python-node? value)
                                (py-object%->string value)
                                value)])
              (if port-write
                  (port-write to-render)
                  (write to-render port))))
          (define/public (render-value/format value settings port port-write width)
            (render-value value settings port port-write))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable settings parent src-file)
	    (let ([dst-file (drscheme:language:put-executable
			     parent src-file #f #f
			     (string-constant save-a-mzscheme-stand-alone-executable))])
	      (when dst-file
		(let ([code (compile-python
                             (parse-python-file src-file))]);(compile-simplified (simplify (parse-a60-file src-file)
			;				  base-importing-stx)
				;		base-importing-stx)])
		  (make-embedding-executable dst-file
					     #f #f
					     '((#f (lib "base.ss" "python")))
					     null
					     (compile
					      `(module m (lib "base.ss" "python")
						 ,code))
					     (list "-mvqe" "(require m)"))))))
	  (define/public (get-one-line-summary) "The Python language (www.python.org)")
          
          (super-instantiate ()))))))
