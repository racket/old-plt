#cs
(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
	   (lib "string-constant.ss" "string-constants")
           "compile.ss"
           "parameters.ss")

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      (define (phase2) 
        (drscheme:language-configuration:add-language
         (make-object (override-render-value-set
                       ((drscheme:language:get-default-mixin) 
                        full-lang%))))
        (drscheme:language-configuration:add-language
         (make-object (override-render-value-set
                       ((drscheme:language:get-default-mixin) 
                        advanced-lang%))))
        (drscheme:language-configuration:add-language
         (make-object (override-render-value-set
                       ((drscheme:language:get-default-mixin) 
                        intermediate-lang%))))
        (drscheme:language-configuration:add-language
         (make-object (override-render-value-set
                       ((drscheme:language:get-default-mixin) 
                        beginner-lang%)))))
      
      (define (override-render-value-set %)
        (class %
          (inherit [super-render-value-set render-value-set])
          (define/override (render-value-set . x)
            ;; needs to be filled in!
            (super-render-value-set . x))
          (super-instantiate ())))
      
      (define (java-lang-mixin level name position numbers one-line)
        (class* object% (drscheme:language:language<%>)
          
          (define/public (config-panel parent)
            (case-lambda
              [() null]
              [(x) (void)]))
          
          (define/public (default-settings) null)
          (define/public (default-settings? x) #t)

          (define execute-types (create-type-record))
          
          (define/public (front-end/complete-program input settings)
            (set! execute-types (create-type-record))
            (let-values ([(port name)
                          (let ([text (drscheme:language:text/pos-text input)])
                            (values
                             (open-input-string
                              (send text
                                    get-text
                                    (drscheme:language:text/pos-start input)
                                    (drscheme:language:text/pos-end input)))
                             text))])              
              (let ((main-mod #f)
                    (require? #f)
                    (name-to-require #f)
                    (modules null))
                (lambda ()
                  (let ((end? (eof-object? (peek-char port))))
                    (cond
                      ((and end? (not require?) (null? modules)) eof)
                      ((and end? require?) 
                       (set! require? #f)
                       (with-syntax ([name name-to-require])
                         (syntax (require name))))
                      (end?
                       (set! require? #t)
                       (let-values (((name syn) (if (eq? main-mod (car modules))
                                                    (add-main-call (expand (car modules)))
                                                    (get-module-name (expand (car modules))))))
                         (set! name-to-require name)
                         (set! modules (cdr modules))
                         syn))
                      (else
                       (execution? #t)
                       (let ((mods (compile-java 'port 'out level #f port name execute-types)))
                         (set! main-mod (find-main-module mods))
                         (set! mods (order mods))
                         (if (null? mods) eof
                             (begin
                               (set! require? #t)
                               (let-values (((name syn) (if (eq? main-mod (car mods))
                                                            (add-main-call (expand (car mods)))
                                                            (get-module-name (expand (car mods))))))
                                 (set! name-to-require name)
                                 (set! modules (cdr mods))
                                 syn)))))))))))
          
          (define/public (front-end/interaction input settings)
            (let-values ([(port name)
                          (let ([text (drscheme:language:text/pos-text input)])
                            (values
                             (open-input-string
                              (send text
                                    get-text
                                    (drscheme:language:text/pos-start input)
                                    (drscheme:language:text/pos-end input)))
                             text))])
              (interactions-offset (drscheme:language:text/pos-start input))
              (lambda ()
                (if (eof-object? (peek-char port))
                    eof
                    (compile-interactions port name execute-types level)))))

          
          ;find-main-module: (list compilation-unit) -> (U syntax #f)
          (define (find-main-module mod-lists)
            (if (null? mod-lists)
                #f
                (let ((names (compilation-unit-contains (car mod-lists)))
                      (syntaxes (compilation-unit-code (car mod-lists))))
                  (if (member (cadr (main)) names)
                      (if (= (length syntaxes) 1)
                          (list-ref syntaxes 0)
                          (list-ref syntaxes (find-position names 1)))
                      (find-main-module (cdr mod-lists))))))
        
          ;find-position: (list string) number-> number
          (define (find-position l p)
            (when (null? l)
              (error 'find-position "Internal Error: member incorrectly chose an element as a member"))
            (if (equal? (cadr (main)) (car l))
                p
                (find-position (cdr l) (add1 p))))
          
          ;order: (list compilation-unit) -> (list syntax)
          (define order
            (lambda (mod-lists)
              (if (null? mod-lists)
                  null
                  (append (compilation-unit-code (car mod-lists))
                          (order (cdr mod-lists))))))
              
          (define/public (get-style-delta) #f)
          (define/public (get-language-position) (cons (string-constant experimental-languages) position))
          (define/public (get-language-numbers) numbers)
          (define/public (get-language-name) name)
          (define/public (get-language-url) #f)
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)

          (define/public (on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
               (error-display-handler 
                (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
               (current-eval 
                (drscheme:debug:make-debug-eval-handler (current-eval)))
               (with-handlers ([void (lambda (x)  (printf "~a~n" (exn-message x)))])
                 (namespace-require 'mzscheme)
                 (namespace-require '(lib "class.ss"))
                 (namespace-require '(prefix javaRuntime: (lib "runtime.scm" "profj" "libs" "java")))))))
          
          (define/public (render-value value settings port port-write) (write value port))
          (define/public (render-value/format value settings port port-write width) (write value port))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable fn parent . args)
	    (message-box "Unsupported"
			 "Sorry - executables are not supported for Java"
			 parent))
	  (define/public (get-one-line-summary) one-line)
          
          (super-instantiate ())))


      
      (define full-lang% 
        (java-lang-mixin 'full "Java" (list "ProfessorJ" "Full Java") (list 1000 10 4) "Java 1.0 (some 1.1)"))
      (define advanced-lang% 
        (java-lang-mixin 'advanced "Advanced Java" 
                         (list "ProfessorJ" "Advanced") (list 1000 10 3) "Java Advanced teaching language"))
      (define intermediate-lang% 
        (java-lang-mixin 'intermediate "Intermediate Java" 
                         (list "ProfessorJ" "Intermediate") (list 1000 10 2) "Java Intermediate teaching language"))
      (define beginner-lang% (java-lang-mixin 'beginner "Beginner Java" (list "ProfessorJ" "Beginner")
                                              (list 1000 10 1) "Java Beginner teaching language"))
      ))
      
  
  (define (get-module-name stx)
    (syntax-case stx (module #%plain-module-begin)
      [(module name lang (#%plain-module-begin bodies ...))
       (values (syntax name)
               (syntax (module name lang
                         (#%plain-module-begin bodies ...))))]
      [else 
       (raise-syntax-error 'Java 
                           "Internal Syntax error in getting module name"
                           stx)]))
  
  (define (add-main-call stx)
    (syntax-case stx (module #%plain-module-begin)
      [(module name lang (#%plain-module-begin bodies ...))
       (let ([execute-body (if (car (main))
                               `(lambda (x) 
                                  (display "executing main - ")
                                  (display (,(string->symbol (string-append (cadr (main)) "-main_java.lang.String1")) x)))
                               'void)])
         (with-syntax ([main (datum->syntax-object #f execute-body #f)]) 
           (values (syntax name)
                   (syntax (module name lang 
                             (#%plain-module-begin 
                              (begin bodies ...)
                              (main "temporary")))))))]
      [else
       (raise-syntax-error 'Java
                           "Internal Syntax error in compiling Java Program"
                           stx)])))

      
