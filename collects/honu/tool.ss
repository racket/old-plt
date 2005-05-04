(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
           "parsers/parse.ss"
           "ast.ss"
           "tenv.ss"
           "private/typechecker/honu-type-utils.ss"
           "compile.ss"
           "honu-compile-context.ss"
	   (lib "embed.ss" "compiler")
	   (lib "string-constant.ss" "string-constants"))

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) 
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) (honu-lang-mixin 'single))))
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) (honu-lang-mixin 'group)))))
      
      (define (honu-lang-mixin level)
        (class* object% (drscheme:language:language<%>)
          (define/public (config-panel parent)
            (case-lambda
              [() null]
              [(x) (void)]))
          (define/public (get-comment-character) (values "//" #\*))
          (define/public (default-settings) null)
          (define/public (default-settings? x) #t)
          (define tenv (empty-tenv))
          (define env (empty-env))
          (define level-parser
            (case level
              [(single) parse-port]
              [(group)  parse-group]))
          (define/public (front-end/complete-program port settings teachpack-cache)
            (set! tenv (empty-tenv))
            (let ([name (object-name port)])
              (lambda ()
                (if (eof-object? (peek-char-or-special port))
                    eof
                    (let* ([parsed (level-parser port name)]
                           [compiled-defns (compile/complete-program tenv parsed)])
                      (set! env (get-initial-env tenv))
                      (datum->syntax-object #f `(run-honu-full-program ,compiled-defns) #f))))))
          (define/public (front-end/interaction port settings teachpack-cache)
            (let ([name (object-name port)])
              (lambda ()
                (if (eof-object? (peek-char-or-special port))
                    eof
                    (let ([parsed (parse-interaction port name)])
                      (let-values ([(compiled-expr new-env)
                                    (compile/interaction tenv env parsed)])
                        (begin (set! env new-env)
                               (datum->syntax-object #f `(run-honu-interaction ,compiled-expr) #f))))))))
          (define/public (get-style-delta) #f)
          (define/public (get-language-position)
	    (list (string-constant experimental-languages)
		  "Honu"
                  (case level
                    [(single) "Single File"]
                    [(group)  "Group File"])))
          (define/public (order-manuals x) 
            (values 
             (list #"drscheme" #"tour" #"help")
             #f))
          (define/public (get-language-name)
            (case level
              [(single) "Honu (single)"]
              [(group)  "Honu (group)"]))
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers)
            (case level
              [(single) (list 1000 10 1)]
              [(group)  (list 1000 10 2)]))
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)
          (define/private (syntax-as-top s)
            (if (syntax? s) (namespace-syntax-introduce s) s))
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require '(lib "base.ss" "honu") #f)
            (let ([path ((current-module-name-resolver) '(lib "base.ss" "honu") #f #f)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
		 (let ([old-current-eval (drscheme:debug:make-debug-eval-handler (current-eval))])
                   (current-eval
                    (with-handlers ([(lambda (x) #t) (lambda (x) (printf "~a~n" (exn-message x)))])
                      (lambda (exp)
                      (syntax-case exp (run-honu-full-program run-honu-interaction)
                        [(run-honu-full-program defns)
                         (let loop ([defns (syntax->list #'defns)])
                           (if (null? defns)
                               (void)
                               (begin (old-current-eval (syntax-as-top (car defns)))
                                      (loop (cdr defns)))))]
                        [(run-honu-interaction ex)
                         (old-current-eval (syntax-as-top #'ex))]
                        [(_) (old-current-eval exp)])))))
                 (with-handlers ([(lambda (x) #t) (lambda (x) (printf "~a~n" (exn-message x)))])
                   (namespace-attach-module n path)
                   (namespace-require path))))))
          (define/public (render-value value settings port) (write value port))
          (define/public (render-value/format value settings port width) (write value port))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable settings parent src-file teachpacks)
	    (message-box "Unsupported"
			 "Sorry - executables are not supported for Honu at this time"
			 parent))
	  (define/public (get-one-line-summary)
            (case level
              [(single) "Honu (also not Scheme at all!)"]
              [(group)  "List of Honu files to run together"]))
          
          (super-instantiate ()))))))
