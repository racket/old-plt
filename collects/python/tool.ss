#cs(module tool mzscheme
  (require  (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "stacktrace.ss" "errortrace")
           (lib "unitsig.ss")
           (lib "class.ss")
	   (lib "embed.ss" "compiler")
	   (lib "string-constant.ss" "string-constants")
           (lib "etc.ss")
           "python.ss"
           "compile-python.ss"
           ;"base.ss"
           "get-base.ss"
           "runtime-support.ss"
           ;"primitives.ss"
           ;"c-bindings.ss"
           "python-import.ss")

  (provide tool@)
     
;  (my-dynamic-require (current-namespace) '(lib "base.ss" "python"))
;  (my-dynamic-require (current-namespace) '(lib "runtime-support.ss" "python"))
;  (my-dynamic-require (current-namespace) '(lib "python-import.ss" "python"))
;  (my-dynamic-require (current-namespace) '(lib "primitives.ss" "python"))
;  (toggle-python-use-cache-namespace! #f)

  (define (create-python-cache-namespace)
    (print "creating the python cache namespace!")
    ;(set-python-cache-namespace! (current-namespace))
    (make-python-namespace)
    (print "done creating the python cache namespace"))

  (define cpcn create-python-cache-namespace)

  (define outer-namespace (current-namespace))
  (define pn (make-python-namespace))

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


      (define-struct python-settings (test-coverage?))


      (define lang%
        (class* object% (drscheme:language:language<%>)

          (define/public (config-panel parent)

            (let* ([prefs-panel (make-object vertical-panel% parent)]
                   [features-groupbox (make-object group-box-panel% "Features" prefs-panel)]
                   [test-coverage?-checkbox (make-object check-box%
                                              "Enable test coverage highlighting?"
                                              features-groupbox
                                              (lambda (cbox event)
                                                (when (eq? (send event get-event-type) 'check)
                                                  (send cbox set-value (not (send cbox get-value))))))])
              ;(send prefs-panel show #t))
              (case-lambda
                [() (make-python-settings (send test-coverage?-checkbox get-value))]
                [(s) (send test-coverage?-checkbox set-value (python-settings-test-coverage? s))])))

          (define/public (default-settings)
            (make-python-settings #f))

          (define/public (default-settings? s)
            (let ([def (default-settings)])
              (eq? (python-settings-test-coverage? s)
                   (python-settings-test-coverage? def))))

          (define/public (marshall-settings s)
            (list (python-settings-test-coverage? s)))

          (define/public (unmarshall-settings s)
            (if (and (list? s)
                     (= 1 (length s))
                     (boolean? (car s)))
                (make-python-settings (car s))
                #f))


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
                  (on-execute settings (lambda (thunk) (thunk)))
                  (if (null? ast-list)
                      eof
                      (begin0 (parameterize ([current-runtime-support-context #'here]
                                             [current-toplevel-context ;base-importing-stx])
                                                                       #f])
                                                                       ;my-toplevel-context])
                                (syntax-as-top (compile-python-ast (car ast-list))))
                              (set! ast-list (cdr ast-list))))))))

          (define/public (front-end/complete-program input settings teachpack-cache)
            (front-end input settings))

          (define/public (front-end/interaction input settings teachpack-cache)
            (front-end input settings))

          (define/private (syntax-as-top s)
            (if (syntax? s)
                (namespace-syntax-introduce s)
                s))

          (define/public (get-style-delta) #f)
          (define/public (get-language-position) (list (string-constant experimental-languages) "Python"))
          (define/public (get-language-name) "Python")
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers)
            (list 1000 10))
          (define/public (get-teachpack-names) null)

          (define my-toplevel-context 'foo)

          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require '(lib "base.ss" "python") #f)
            (let ([base-path ((current-module-name-resolver) '(lib "base.ss" "python") #f #f)]
                  [outer-namespace (current-namespace)])
              ;(with-handlers ([void (lambda (e)
               ;                       (printf "on-execute: outer-namespace: ~a~n"
               ;                               (exn-message e)))])
                ;(load-c-spy outer-namespace)
            ;(dynamic-require '(lib "base.ss" "python") #f)
               ; (namespace-attach-module outer-namespace path)
               ; (namespace-transformer-require path)
               ; (namespace-require path))
              (run-in-user-thread
               (lambda ()
                 (set! my-toplevel-context #'here)
                 (error-display-handler
                  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
                 (current-eval
                  (let ([e (drscheme:debug:make-debug-eval-handler (current-eval))])
                    (if (python-settings-test-coverage? settings)
                        (add-annotation e)
                        e)))
                 (drscheme:debug:test-coverage-enabled (python-settings-test-coverage? settings))
                 (with-handlers ([void (lambda (e)
                                         (printf (string-append "on-execute: in user thread: "
                                                                (exn-message e)
                                                                "~n")))])
                   (let ();[path ((current-module-name-resolver) '(lib "base.ss" "python") #f #f)])
                   ;(load-c-spy (current-namespace))
                 ;(dynamic-require '(lib "base.ss" "python") #f)
                   (namespace-attach-module outer-namespace base-path)
                     ;(namespace-require 'mzscheme)
                   ;(namespace-transformer-require base-path)
                   (namespace-require base-path)
;                   (load-extension (build-path (this-expression-source-directory)
;                                               "c" "stringobject.so"))
                   ))
                 ))))
          
          (define (render value port)
            (let ([to-render (if (python-node? value)
                                 (get-py-string (py-object->py-string value))
                                 value)])
              (display to-render port)))
            
          (define/public (render-value value settings port port-write)
            (render value port))
            ;(render-python-value value port port-write))
          (define/public (render-value/format value settings port port-write width)
            (render value port))
            ;(render-python-value/format value port port-write))

          ;; default implementation provided by Robby
          (define/public (order-manuals x) (values x #t))
          
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

          (super-instantiate ())))



      ;; cm-key : symbol
      ;; the key used to put information on the continuation
      (define cm-key (gensym 'python-continuation-mark-key))


      ;; with-mark : syntax (any -> syntax) syntax -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with cm-key are
      ;; members of the debug-source type
      (define (with-mark source-stx make-st-mark expr)
        (let ([source (syntax-source source-stx)]
              [start-position (syntax-position source-stx)]
              [span (syntax-span source-stx)])
          (if (and (is-a? source text:basic<%>)
                   (number? start-position)
                   (number? span))
              (with-syntax ([expr expr]
                            [mark (make-st-mark `(,source ,(- start-position 1) . ,span))]
                            [cm-key cm-key])
                #`(with-continuation-mark 'cm-key mark expr))
              expr)))



      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  profiling infrastructure. Not used.
      ;;

      (define profile-key (gensym))
      (define (profiling-enabled) #f)
      (define (initialize-profile-point . x) (void))
      (define (register-profile-start . x) #f)
      (define (register-profile-done . x) (void))



      ;;
      ;;  test coverage
      ;;

      (define test-coverage-enabled (make-parameter #t))
      (define current-test-coverage-info (make-parameter #f))

      (define (initialize-test-coverage-point key expr)
        (unless (current-test-coverage-info)
	  (let ([ht (make-hash-table)])
	    (current-test-coverage-info ht)
            (when (drscheme:rep:current-rep)
              (send (drscheme:rep:current-rep) set-test-coverage-info
                    ht
                    (let ([s (make-object style-delta%)])
                      (send s set-delta-foreground "black")
                      s)
                    (let ([s (make-object style-delta%)])
                      (send s set-delta-foreground "firebrick")
                      s)
                    #f))))
        (hash-table-put! (current-test-coverage-info) key (list #f expr)))

      (define (test-covered key)
        (let ([v (hash-table-get (current-test-coverage-info) key)])
          (set-car! v #t)))

      (define-values/invoke-unit/sig stacktrace^ stacktrace@ #f stacktrace-imports^)

      ;; add-annotation : (sexp -> value) -> sexp -> value
      ;; adds debugging and test coverage information to `sexp' and calls `oe'
      (define (add-annotation oe)
        (let ([teaching-language-eval-handler
               (lambda (exp)
                 (let ([annotated
                        (if (compiled-expression?
                             (if (syntax? exp) (syntax-e exp) exp))
                            exp
                            (annotate-top (expand exp) #f))])
                   (oe annotated)))])
          teaching-language-eval-handler))


      )))