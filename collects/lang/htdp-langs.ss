#|

WARNING: printf is rebound in the mosdule to always print
to the original stdout of DrScheme.

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
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred"))
  
  (provide tool@)
  
  (define o (current-output-port))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      
      (define (printf . args)
        (apply fprintf o args))
      
      
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
                 (current-eval (add-debugging (current-eval)))
                 (error-print-source-location #f)
                 (read-decimal-as-inexact #f)
                 (read-accept-dot (get-read-accept-dot)))))
            (super-on-execute settings run-in-user-thread))
          
          (define (set-printing-parameters settings thunk)
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
               
               [input-msg (make-object message% (string-constant input-syntax) parent)]
               [input-panel (instantiate vertical-panel% ()
                              (parent parent)
                              (style '(border))
                              (alignment '(left center)))]
               
               [output-msg (make-object message% (string-constant output-syntax) parent)]
               [output-panel (instantiate vertical-panel% ()
                               (parent parent)
                               (style '(border))
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
                     [(write) 2]))
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
          
          (super-instantiate ())))
      
      (define (language-extension %)
        (class %
          (inherit get-htdp-style-delta)
          
          (inherit get-module get-transformer-module get-init-code
                   use-namespace-require/copy?)
          (define/override (create-executable setting parent program-filename executable-filename)
            (drscheme:language:create-module-based-stand-alone-executable
             program-filename
             executable-filename
             (get-module)
             (get-transformer-module)
             (get-init-code setting)
             #t
             (use-namespace-require/copy?)))
          
          (define/override (get-style-delta)
            (get-htdp-style-delta))
          (super-instantiate ())))
                                                                                          

                                                                                    
;;                                                      ;;                   ;;     
 ;                                                       ;                    ;     
 ;                                                       ;                    ;     
 ; ;;   ;;;;  ; ;;;    ;;; ;;;; ;   ;;;;  ; ;;;          ; ;;   ;;;;    ;;;   ;  ;; 
 ;;  ;      ;  ;;  ;  ;   ;  ; ; ;      ;  ;;  ;         ;;  ;      ;  ;   ;  ; ;   
 ;   ;   ;;;;  ;   ;  ;   ;  ; ; ;   ;;;;  ;   ;         ;   ;   ;;;;  ;      ;;    
 ;   ;  ;   ;  ;   ;  ;   ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;   ;  ;      ; ;   
 ;   ;  ;   ;  ;   ;  ;   ;  ; ; ;  ;   ;  ;   ;         ;   ;  ;   ;  ;   ;  ;  ;  
;;; ;;;  ;;; ;;;;  ;;  ;;;; ;; ; ;;  ;;; ;;;;  ;;       ;;; ;;;  ;;; ;  ;;;  ;;   ;;
                          ;                                                         
                          ;                                                         
                       ;;;                                                          
      


      ;; this inspector should be powerful enough to see
      ;; any structure defined in the user's namespace
      (define drscheme-inspector (current-inspector))
      
      (eval `(module drscheme-secrets mzscheme
               (provide drscheme-inspector)
               (define drscheme-inspector ,drscheme-inspector)))
      (namespace-require 'drscheme-secrets)
      

      
      
      
                                          
  ;;;    ;;;  ;;  ;;  ; ;;;   ;;;    ;;;  
 ;   ;  ;   ;  ;   ;   ;     ;   ;  ;   ; 
  ;;;   ;   ;  ;   ;   ;     ;      ;;;;; 
     ;  ;   ;  ;   ;   ;     ;      ;     
 ;   ;  ;   ;  ;   ;   ;     ;   ;  ;   ; 
  ;;;    ;;;    ;;; ; ;;;;    ;;;    ;;;  
                                          
                                          
                                          

                                                                             
                                    ;;;                    ;                 
                                      ;            ;                         
                                      ;            ;                         
  ;;;    ;;;   ; ;;;  ; ;;;   ;;;     ;    ;;;;   ;;;;;  ;;;     ;;;  ; ;;;  
 ;   ;  ;   ;   ;      ;     ;   ;    ;        ;   ;       ;    ;   ;  ;;  ; 
 ;      ;   ;   ;      ;     ;;;;;    ;     ;;;;   ;       ;    ;   ;  ;   ; 
 ;      ;   ;   ;      ;     ;        ;    ;   ;   ;       ;    ;   ;  ;   ; 
 ;   ;  ;   ;   ;      ;     ;   ;    ;    ;   ;   ;   ;   ;    ;   ;  ;   ; 
  ;;;    ;;;   ;;;;   ;;;;    ;;;   ;;;;;;  ;;; ;   ;;;  ;;;;;   ;;;  ;;;  ;;
                                                                             
                                                                             
                                                                             
      
      
      ;; cm-key : symbol
      ;; the key used to put information on the continuation
      (define cm-key (gensym 'teaching-languages-continuation-mark-key))
      
      ;; add-error-display : (string (union TST exn) -> void) -> string exn -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (teaching-languages-error-display-handler msg exn)
        (let ([rep (drscheme:rep:current-rep)])
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
            [(exn? exn) 
             (let ([cms (continuation-mark-set->list (exn-continuation-marks exn) cm-key)])
               (when (and cms (not (null? cms)))
                 (let* ([first-cms (car cms)]
                        [src (car first-cms)]
                        [start-position (cadr first-cms)]
                        [end-position (+ start-position (cddr first-cms))])
                   (send rep highlight-error src start-position end-position))))]
            [else (void)])))
      
      ;; wrap : syntax syntax -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with cm-key are
      ;; members of the debug-source type
      (define (with-mark mark expr)
        (let ([source (syntax-source mark)]
              [start-position (syntax-position mark)]
              [span (syntax-span mark)])
          (if (and (is-a? source text:basic<%>)
                   (number? start-position)
                   (number? span))
              (with-syntax ([expr expr]
                            [source source]
                            [offset (- start-position 1)]
                            [span span]
                            [cm-key cm-key])
                (syntax
                 (with-continuation-mark
                  'cm-key
                  '(source offset . span)
                  expr)))
              expr)))
      
      ;; profiling infrastructure. Not used.
      (define profile-key (gensym))
      (define (profiling-enabled) #f)
      (define (initialize-profile-point . x) (void))
      (define (register-profile-start . x) #f)
      (define (register-profile-done . x) (void))
      
      (define-values/invoke-unit/sig stacktrace^ stacktrace@ #f stacktrace-imports^)
      
      ;; add-debugging : (sexp -> value) -> sexp -> value
      ;; adds debugging information to `sexp' and calls `oe'
      (define (add-debugging oe)
        (let ([teaching-language-eval-handler
               (lambda (exp)
                 (let ([annotated
                        (if (compiled-expression? 
                             (if (syntax? exp) (syntax-e exp) exp))
                            exp
                            (annotate-top (expand exp) #f))])
                   (oe annotated)))])
          teaching-language-eval-handler))
      


                                                                      
                               ;                         ;;;    ;;;   
                ;                    ;                     ;      ;   
                ;                    ;                     ;      ;   
; ;;;  ;;  ;;  ;;;;;         ;;;    ;;;;;         ;;;;     ;      ;   
 ;   ;  ;   ;   ;              ;     ;                ;    ;      ;   
 ;   ;  ;   ;   ;              ;     ;             ;;;;    ;      ;   
 ;   ;  ;   ;   ;              ;     ;            ;   ;    ;      ;   
 ;   ;  ;   ;   ;   ;          ;     ;   ;        ;   ;    ;      ;   
 ;;;;    ;;; ;   ;;;         ;;;;;    ;;;          ;;; ; ;;;;;; ;;;;;;
 ;                                                                    
 ;                                                                    
;;;                                                                   

      
                                                        
                                   ;;                   
  ;                           ;     ;                   
  ;                           ;     ;                   
 ;;;;;   ;;;    ;;; ;  ;;;   ;;;;;  ; ;;    ;;;   ; ;;; 
  ;     ;   ;  ;   ;  ;   ;   ;     ;;  ;  ;   ;   ;    
  ;     ;   ;  ;   ;  ;;;;;   ;     ;   ;  ;;;;;   ;    
  ;     ;   ;  ;   ;  ;       ;     ;   ;  ;       ;    
  ;   ; ;   ;  ;   ;  ;   ;   ;   ; ;   ;  ;   ;   ;    
   ;;;   ;;;    ;;;;   ;;;     ;;; ;;; ;;;  ;;;   ;;;;  
                   ;                                    
                   ;                                    
                ;;;                                     

      
      
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
      
	;; technically this should move somewhere else,
	;; but we leave it here for now.
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant pretty-big-scheme-one-line-summary))
           (sharing-printing #t)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #t)
           (language-numbers '(-500 6))
           (module '(lib "htdp-full-graphics.ss" "lang"))
           (language-position
            (list (string-constant plt)
                  (string-constant pretty-big-scheme)))
           (read-accept-dot #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant advanced-one-line-summary))
           (module '(lib "htdp-advanced.ss" "lang"))
           (language-position
            (list (string-constant how-to-design-programs)
                  (string-constant advanced-student)))
           (language-numbers '(-500 5))
           (sharing-printing #t)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate/lambda-one-line-summary))
           (module '(lib "htdp-intermediate-lambda.ss" "lang"))
           (language-position
            (list (string-constant how-to-design-programs)
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
           (language-numbers '(-500 4))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate-one-line-summary))
           (module '(lib "htdp-intermediate.ss" "lang"))
           (language-position
            (list (string-constant how-to-design-programs)
                  (string-constant intermediate-student)))
           (language-numbers '(-500 3))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (use-function-output-syntax? #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning/abbrev-one-line-summary))
           (module '(lib "htdp-beginner-abbr.ss" "lang"))
           (language-position
            (list (string-constant how-to-design-programs)
                  (string-constant beginning-student/abbrev)))
           (language-numbers '(-500 2))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning-one-line-summary))
           (module '(lib "htdp-beginner.ss" "lang"))
           (language-position
            (list (string-constant how-to-design-programs)
                  (string-constant beginning-student)))
           (language-numbers '(-500 1))
           (sharing-printing #f)
           (abbreviate-cons-as-list #f)
           (allow-sharing? #f)
           (accept-quasiquote? #f)))))))
