#|

not to forget: teachpakcs

;; we don't use the built in debugging, use our own
;; version here that has no bug icon and only
;; annotates code that comes from editors.

|#

(module htdp-langs mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           (lib "stacktrace.ss" "errortrace")
           (lib "pretty.ss")
           (prefix pc: (lib "pconvert.ss"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred"))
  
  (provide tool@)
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      
      (define htdp-language<%>
        (interface ()
          get-module
          get-language-position
          get-sharing-printing
          get-abbreviate-cons-as-list
          get-allow-sharing?
          get-use-function-output-syntax?
          get-accept-quasiquote?
          get-read-accept-dot))
      
      ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
      ;;                                   -> (implements drscheme:language:module-based-language<%>)
      ;; changes the default settings and sets a few more paramters during `on-execute'
      (define (module-based-language-extension super%)
        (class* super% ()
          (rename [super-on-execute on-execute]
                  [super-render-value/format render-value/format]
                  [super-render-value render-value])
          (inherit get-sharing-printing get-abbreviate-cons-as-list)
          
          (define/override (default-settings)
            (drscheme:language:make-simple-settings 
             #t
             'constructor
             'repeating-decimal
             (get-sharing-printing)
             #t
             'none))
          
          (rename [super-config-panel config-panel])
          (inherit get-allow-sharing? get-use-function-output-syntax? 
                   get-accept-quasiquote? get-read-accept-dot)
          (define/override (config-panel parent)
            (sharing/not-config-panel (get-allow-sharing?) parent))
          
          (define/override (on-execute settings run-in-user-thread)
            (let ([drs-namespace (current-namespace)])
              (run-in-user-thread
               (lambda ()
                 (read-accept-quasiquote (get-accept-quasiquote?))
                 (namespace-attach-module drs-namespace 'drscheme-secrets)
                 (error-display-handler teaching-languages-error-display-handler)
                 (current-eval (add-annotation (current-eval)))
                 (error-print-source-location #f)
                 (read-decimal-as-inexact #f)
                 (read-accept-dot (get-read-accept-dot)))))
            (super-on-execute settings run-in-user-thread))

	  ;; set-printing-parameters : settings ( -> TST) -> TST
	  ;; is implicitly exposed to the stepper.  watch out!  --  john
          (define/public (set-printing-parameters settings thunk)
            (parameterize ([pc:booleans-as-true/false #t]
                           [pc:abbreviate-cons-as-list (get-abbreviate-cons-as-list)]
                           [pretty-print-show-inexactness #t]
                           [pretty-print-.-symbol-without-bars #t]
                           [pretty-print-exact-as-decimal #t]
                           [pc:use-named/undefined-handler
                            (lambda (x)
                              (and (get-use-function-output-syntax?)
                                   (procedure? x)
                                   (object-name x)))]
                           [pc:named/undefined-handler
                            (lambda (x)
                              (string->symbol
                               (format "function:~a" (object-name x))))])
              (thunk)))
          
          (define/override (render-value/format value settings port put-snip width)
            (set-printing-parameters
             settings
             (lambda ()
               (super-render-value/format value settings port put-snip width))))
          
          (define/override (render-value value settings port put-snip)
            (set-printing-parameters
             settings
             (lambda ()
               (super-render-value value settings port put-snip))))
          
          (super-instantiate ())))
      
      ;; sharing/not-config-panel :  boolean parent -> (case-> (-> settings) (settings -> void))
      ;; constructs the config-panel for a language without a sharing option.
      (define (sharing/not-config-panel allow-sharing-config? _parent)
        (let* ([parent (make-object vertical-panel% _parent)]
               
               [input-panel (instantiate group-box-panel% ()
                              (parent parent)
                              (label (string-constant input-syntax))
                              (alignment '(left center)))]
               
               [output-panel (instantiate group-box-panel% ()
                               (parent parent)
                               (label (string-constant output-syntax))
                               (alignment '(left center)))]
               
               [case-sensitive (make-object check-box%
                                 (string-constant case-sensitive-label)
                                 input-panel
                                 void)]
               [output-style (make-object radio-box%
                               (string-constant output-style-label)
                               (list (string-constant constructor-printing-style)
                                     (string-constant quasiquote-printing-style)
                                     (string-constant write-printing-style))
                               output-panel
                               void)]
               [fraction-style
                (make-object radio-box% (string-constant fraction-style)
                  (list (string-constant use-mixed-fractions)
                        (string-constant use-repeating-decimals))
                  output-panel
                  void)]
               [show-sharing #f]
               [insert-newlines (make-object check-box%
                                  (string-constant use-pretty-printer-label)
                                  output-panel
                                  void)])
          
          (when allow-sharing-config?
            (set! show-sharing
                  (instantiate check-box% ()
                    (parent output-panel)
                    (label (string-constant sharing-printing-label))
                    (callback void))))
          
          ;; set the characteristics of the GUI
          (send _parent set-alignment 'center 'center)
          (send parent stretchable-height #f)
          (send parent stretchable-width #f)
          (send parent set-alignment 'center 'center)
          
          (case-lambda
            [()
             (drscheme:language:make-simple-settings
              (send case-sensitive get-value)
              (case (send output-style get-selection)
                [(0) 'constructor]
                [(1) 'quasiquote]
                [(2) 'write])
              (case (send fraction-style get-selection)
                [(0) 'mixed-fraction]
                [(1) 'repeating-decimal])
              (and allow-sharing-config? (send show-sharing get-value))
              (send insert-newlines get-value)
              'none)]
            [(settings)
             (send case-sensitive set-value (drscheme:language:simple-settings-case-sensitive settings))
             (send output-style set-selection
                   (case (drscheme:language:simple-settings-printing-style settings)
                     [(constructor) 0]
                     [(quasiquote) 1]
                     [(write) 2]
                     [(print) 2]))
             (send fraction-style set-selection
                   (case (drscheme:language:simple-settings-fraction-style settings)
                     [(mixed-fraction) 0]
                     [(repeating-decimal) 1]))
             (when allow-sharing-config?
               (send show-sharing set-value (drscheme:language:simple-settings-show-sharing settings)))
             (send insert-newlines set-value 
                   (drscheme:language:simple-settings-insert-newlines settings))])))
      

      (define simple-htdp-language%
        (class* drscheme:language:simple-module-based-language% (htdp-language<%>)
          (init-field sharing-printing
                      abbreviate-cons-as-list
                      allow-sharing?
                      (use-function-output-syntax? #f)
                      (accept-quasiquote? #t)
                      (read-accept-dot #f)
                      (style-delta #f))
          (define/public (get-sharing-printing) sharing-printing)
          (define/public (get-abbreviate-cons-as-list) abbreviate-cons-as-list)
          (define/public (get-allow-sharing?) allow-sharing?)
          (define/public (get-use-function-output-syntax?) use-function-output-syntax?)
          (define/public (get-accept-quasiquote?) accept-quasiquote?)
          (define/public (get-read-accept-dot) read-accept-dot)
          ;(define/override (get-one-line-summary) one-line-summary)
          (define/public (get-htdp-style-delta) style-delta)
          
          (super-instantiate ()
            (language-url "http://www.htdp.org/"))))
      
      (define (language-extension %)
        (class %
          (inherit get-htdp-style-delta)
          
          (inherit get-module get-transformer-module get-init-code
                   use-namespace-require/copy?)
          (define/override (create-executable setting parent program-filename)
            (let ([executable-filename
		   (drscheme:language:put-executable
		    parent program-filename
		    #f 
		    #t
		    (string-constant save-a-mred-stand-alone-executable))])
              (when executable-filename
                (drscheme:language:create-module-based-stand-alone-executable
                 program-filename
                 executable-filename
                 (get-module)
                 (get-transformer-module)
                 (get-init-code setting)
                 #t
                 (use-namespace-require/copy?)))))
          
          (define/override (get-style-delta)
            (get-htdp-style-delta))
          
          (inherit get-reader set-printing-parameters)
          (define/override (front-end/complete-program input settings)
            (let-values ([(port source offset line col) (drscheme:language:open-program-for-reading input)])
              (let ([state 'init]
                    [reader (get-reader)])
                (lambda ()
                  (case state
                    [(init)
                     (with-syntax ([(body-exp ...) 
                                    (let loop ()
                                      (let ([result (reader source port (list line col offset))])
                                        (if (eof-object? result)
                                            null
                                            (cons result (loop)))))]
                                   [language-module (get-module)]
                                   [(require-specs ...) 
                                    (begin
                                      '(teachpack-cache-require-specs 
                                        (send rep get-user-teachpack-cache))
                                      '())])
                       (set! state 'require)
                       (let ([mod (expand (syntax (module #%htdp language-module 
                                                    (require require-specs ...)
                                                    body-exp ...)))])
                         (rewrite-module mod)))]
                    [(require) 
                     (set! state 'done)
                     (syntax (require #%htdp))]
                    [(done) eof])))))

          (super-instantiate ())))

      ;; rewrite-module : syntax -> syntax
      ;; rewrites te module to provide all definitions and 
      ;; print out all results.
      (define (rewrite-module stx)
        (syntax-case stx (module #%plain-module-begin)
          [(module name lang (#%plain-module-begin bodies ...))
           (with-syntax ([(rewritten-bodies ...) 
                          (rewrite-bodies (syntax->list (syntax (bodies ...))))])
             (syntax (module name lang
                       (#%plain-module-begin 
                        rewritten-bodies ...))))]
          [else
           (raise-syntax-error 'htdp-languages "internal error .1")]))
      
      ;; rewrite-bodies : (listof syntax) -> syntax
      (define (rewrite-bodies bodies)
        (let loop ([bodies bodies]
                   [ids null])
          (cond
            [(null? bodies) 
             (list
              (with-syntax ([(ids ...) ids])
                (syntax (provide ids ...))))]
            [else
             (let ([body (car bodies)])
               (syntax-case body (define-values define-syntaxes require require-for-syntax provide)
                 [(define-values (new-vars ...) e)
                  (cons body (loop (cdr bodies)
                                   (append
                                    ids 
                                    (filter-ids (syntax (new-vars ...))))))]
                 [(define-syntaxes (new-vars ...) e)
                  (cons body (loop (cdr bodies)
                                   (append
                                    ids 
                                    (filter-ids (syntax (new-vars ...))))))]
                 [(require specs ...)
                  (loop (cdr bodies) ids)]
                 [(require-for-syntax specs ...)
                  (cons body (loop (cdr bodies) ids))]
                 [(provide specs ...)
                  (loop (cdr bodies) ids)]
                 [else 
                  (let ([new-exp
                         (with-syntax ([body body]
                                       [print-results
                                        (lambda (results)
                                          (let ([rep (drscheme:rep:current-rep)])
                                            (when rep
                                              (send rep display-results results))))])
                           (syntax 
                            (let ([already-exited? #f])
                              (dynamic-wind
                               void
                               (lambda ()
                                 (call-with-values
                                  (lambda () body)
                                  (lambda results
                                    (unless already-exited?
                                      (print-results results)))))
                               (lambda ()
                                 (set! already-exited? #t))))))])
                    (cons new-exp (loop (cdr bodies) ids)))]))])))
      
      ;; filter-ids : syntax[list] -> listof syntax
      (define (filter-ids ids)
        ;; When a `define-values' or `define-syntax' declaration
        ;; is macro-generated, if the defined name also originates
        ;; from a macro, then the name is hidden to anything
        ;; that wasn't generated by the same macro invocation. This
        ;; hiding relies on renaming at the symbol level, and it's
        ;; exposed by the fact that `syntax-e' of the identifier 
        ;; returns a different name than `identifier-binding'.
        (filter
         (lambda (id)
           (let ([ib (identifier-binding id)])
             ;; ib should always be a 4-elem list, but
             ;; check, just in case:
             (or (not (pair? ib)) 
                 (eq? (syntax-e id)
                      (cadr ib)))))
         (syntax->list ids)))
                 

;                                                                                              
;                                                                                              
;                                                                                              
;   ;                                                             ;                     ;      
;   ;                                                             ;                     ;      
;   ;                                                             ;                     ;      
;   ; ;;    ;;;    ; ;;     ;; ;   ; ;;  ;;    ;;;    ; ;;        ; ;;    ;;;     ;;;   ;   ;  
;   ;;  ;  ;   ;   ;;  ;   ;  ;;   ;;  ;;  ;  ;   ;   ;;  ;       ;;  ;  ;   ;   ;   ;  ;  ;   
;   ;   ;      ;   ;   ;  ;    ;   ;   ;   ;      ;   ;   ;       ;   ;      ;  ;       ; ;    
;   ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;   ;;;;   ;   ;       ;   ;   ;;;;  ;       ;;;    
;   ;   ;  ;   ;   ;   ;  ;    ;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;  ;       ;  ;   
;   ;   ;  ;   ;   ;   ;   ;  ;;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;   ;   ;  ;   ;  
;   ;   ;   ;;;;;  ;   ;    ;; ;   ;   ;   ;   ;;;;;  ;   ;       ;   ;   ;;;;;   ;;;   ;    ; 
;                              ;                                                               
;                         ;    ;                                                               
;                          ;;;;                                                                
                 
                 
      ;; this inspector should be powerful enough to see
      ;; any structure defined in the user's namespace
      (define drscheme-inspector (current-inspector))
      
      (eval `(module drscheme-secrets mzscheme
               (provide drscheme-inspector)
               (define drscheme-inspector ,drscheme-inspector)))
      (namespace-require 'drscheme-secrets)
      
      
      
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                 ;                             
      ;    ;;;   ; ;  ; ;   ;;;    ; ; ;;;;  ; ;  ;;;     ;;;    ;;;  
      ;   ;   ;  ;;   ;;   ;   ;   ;;   ;    ;;  ;   ;   ;   ;  ;   ; 
      ;  ;    ;  ;    ;   ;     ;  ;    ;    ;       ;  ;      ;    ; 
      ;  ;;;;;;  ;    ;   ;     ;  ;    ;    ;    ;;;;  ;      ;;;;;; 
      ;  ;       ;    ;   ;     ;  ;    ;    ;   ;   ;  ;      ;      
      ;   ;      ;    ;    ;   ;   ;    ;    ;   ;   ;   ;   ;  ;     
      ;    ;;;;  ;    ;     ;;;    ;     ;;  ;    ;;;;;   ;;;    ;;;; 
      ;                                                               
      ;                                                               
      ;                                                               
      
      
      
      
      ;; cm-key : symbol
      ;; the key used to put information on the continuation
      (define cm-key (gensym 'teaching-languages-continuation-mark-key))
      
      ;; teaching-languages-error-display-handler : 
      ;;    (string (union TST exn) -> void) -> string exn -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (teaching-languages-error-display-handler msg exn)
        (let ([rep (drscheme:rep:current-rep)])
          
          (let ([src 
                 (cond
                   [(exn:syntax? exn) (syntax-source (exn:syntax-expr exn))]
                   [(exn:read? exn) (exn:read-source exn)]
                   [(exn? exn) 
                    (let ([cms (continuation-mark-set->list (exn-continuation-marks exn) cm-key)])
                      (when (and cms (not (null? cms)))
                        (let* ([first-cms (st-mark-source (car cms))]
                               [src (car first-cms)])
                          src)))]
                   [else #f])])
            (when (string? src)
              (display src (current-error-port))
              (display ": " (current-error-port))))
          
          (if (exn? exn)
              (display (exn-message exn) (current-error-port))
              (fprintf (current-error-port) "uncaught exception: ~e" exn))
          (fprintf (current-error-port) "\n")
          (send rep wait-for-io-to-complete/user)
          (cond
            [(exn:syntax? exn) 
             (let ([obj (exn:syntax-expr exn)])
               (when (syntax? obj)
                 (let ([src (syntax-source obj)]
                       [pos (syntax-position obj)]
                       [span (syntax-span obj)])
                   (when (and (is-a? src text:basic<%>)
                              (number? pos)
                              (number? span))
                     (send rep highlight-error src (- pos 1) (+ pos -1 span))))))]
            [(exn:read? exn) 
             (let ([src (exn:read-source exn)]
                   [pos (exn:read-position exn)]
                   [span (exn:read-span exn)])
               (when (and (is-a? src text:basic<%>)
                          (number? pos)
                          (number? span))
                 (send rep highlight-error src (- pos 1) (+ pos -1 span))))]
            [(drscheme:rep:exn:locs? exn)
             (let ([locs (drscheme:rep:exn:locs-locs exn)])
               (send rep highlight-errors locs))]
            [(exn? exn) 
             (let ([cms (continuation-mark-set->list (exn-continuation-marks exn) cm-key)])
               (when (and cms (not (null? cms)))
                 (let* ([first-cms (st-mark-source (car cms))]
                        [src (car first-cms)]
                        [start-position (cadr first-cms)]
                        [end-position (+ start-position (cddr first-cms))])
                   (send rep highlight-error src start-position end-position))))]
            [else (void)])))
      
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
                #`(with-continuation-mark
                      'cm-key
                    mark
                    expr))
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
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  test coverage
      ;;
      
      (define test-coverage-enabled (make-parameter #t))
      (define current-test-coverage-info (make-parameter #f))
      
      (define (initialize-test-coverage-point key expr)
        (unless (current-test-coverage-info)
          (let ([ht (make-hash-table)])
            (current-test-coverage-info ht)
            (let ([rep (drscheme:rep:current-rep)])
              (when rep
                (send rep set-test-coverage-info
                      ht
                      (let ([s (make-object style-delta%)])
                        (send s set-delta-foreground "black")
                        s)
                      (let ([s (make-object style-delta%)])
                        (send s set-delta-foreground "firebrick")
                        s)
                      #f)))))
        (hash-table-put! (current-test-coverage-info)
                         key
                         (list #f expr)))
      
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
      
      
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                            ;                   ;   ;                                        ;                  
;                                                ;   ;                                        ;                  
;                   ;            ;               ;   ;       ;                           ;    ;                  
;   ; ;;    ;   ;  ;;;;      ;  ;;;;      ;;;    ;   ;      ;;;;   ;;;     ;; ;    ;;;  ;;;;  ; ;;     ;;;   ; ; 
;   ;;  ;   ;   ;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;   ;  ;    ;;  ;   ;   ;  ;;  
;   ;    ;  ;   ;   ;        ;   ;           ;   ;   ;       ;   ;     ; ;    ;  ;    ;  ;    ;   ;  ;    ;  ;   
;   ;    ;  ;   ;   ;        ;   ;        ;;;;   ;   ;       ;   ;     ; ;    ;  ;;;;;;  ;    ;   ;  ;;;;;;  ;   
;   ;    ;  ;   ;   ;        ;   ;       ;   ;   ;   ;       ;   ;     ; ;    ;  ;       ;    ;   ;  ;       ;   
;   ;;  ;   ;  ;;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;      ;    ;   ;   ;      ;   
;   ; ;;     ;; ;    ;;      ;    ;;      ;;;;;  ;   ;        ;;   ;;;     ;; ;    ;;;;   ;;  ;   ;    ;;;;  ;   
;   ;                                                                         ;                                  
;   ;                                                                    ;    ;                                  
;   ;                                                                     ;;;;                                   
      
      
      ;; add-htdp-language : (instanceof htdp-language<%>) -> void
      (define (add-htdp-language o)
        (drscheme:language-configuration:add-language o))
      
      ;; phase2 : -> void
      (define (phase2)
        (define htdp-language%
          ((drscheme:language:get-default-mixin)
           (language-extension
            (drscheme:language:module-based-language->language-mixin
             (module-based-language-extension
              (drscheme:language:simple-module-based-language->module-based-language-mixin
               simple-htdp-language%))))))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant advanced-one-line-summary))
           (module '(lib "htdp-advanced.ss" "lang"))
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant advanced-student)))
           (language-numbers '(-500 -500 5))
           (sharing-printing #t)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate/lambda-one-line-summary))
           (module '(lib "htdp-intermediate-lambda.ss" "lang"))
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student/lambda)))
           (style-delta (let ([match (regexp-match-positions
                                      "lambda"
                                      (string-constant intermediate-student/lambda))])
                          (if match
                              (let ([pos (car match)])
                                (list (list (make-object style-delta% 'change-family 'modern)
                                            (car pos)
                                            (cdr pos))))
                              #f)))
           (language-numbers '(-500 -500 4))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate-one-line-summary))
           (module '(lib "htdp-intermediate.ss" "lang"))
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student)))
           (language-numbers '(-500 -500 3))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (use-function-output-syntax? #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning/abbrev-one-line-summary))
           (module '(lib "htdp-beginner-abbr.ss" "lang"))
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student/abbrev)))
           (language-numbers '(-500 -500 2))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning-one-line-summary))
           (module '(lib "htdp-beginner.ss" "lang"))
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student)))
           (language-numbers '(-500 -500 1))
           (sharing-printing #f)
           (abbreviate-cons-as-list #f)
           (allow-sharing? #f)
           (accept-quasiquote? #f)))))))
