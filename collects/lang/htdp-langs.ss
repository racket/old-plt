(module htdp-langs mzscheme
  (provide)

  (define htdp-based-language<%>
    (interface ()
      get-module
      get-language-position
      
      default-case-sensitive
      default-whole/fractional-exact-numbers
      default-printing
      default-use-pretty-printer
      default-sharing-printing
      read-decimal-as-exact
      disallow-untagged-inexact-numbers
      abbreviate-cons-as-list
      print-tagged-inexact-numbers
      print-booleans-as-true/false
      print-exact-as-decimal
      print-.-symbols-without-bars
      print-whole/part-fractions))
  
  
      ;; settings structure
      (define-struct/parse 
       setting
       (language-defining-module
        
        read-decimal-as-exact
        case-sensitive
        disallow-untagged-inexact-numbers
        
        whole/fractional-exact-numbers
        
        printing
        use-pretty-printer
        sharing-printing
        abbreviate-cons-as-list
        print-tagged-inexact-numbers
        print-booleans-as-true/false
        print-exact-as-decimal
        print-.-symbols-without-bars
        print-whole/part-fractions))
  
  ;; build-simple-module-based-language-settings : ((instanceof panel<%>) -> (-> setting))
  ;; constrcts the standard settings panel
  (define (build-simple-module-based-language-settings parent language)
    (let* ([make-sub-panel
            (lambda (name panel)
              (let* ([p (make-object vertical-pane% panel)]
                     [message (make-object message% name p)])
                (make-object vertical-panel% parent '(border))))]
           [input-syntax-panel (make-sub-panel (string-constant input-syntax) parent)]
           [output-syntax-panel (make-sub-panel (string-constant output-syntax) parent)]
           [right-align
            (opt-lambda (mo panel)
              (let* ([hp (make-object horizontal-pane% panel)])
                (begin0
                  (mo hp)
                  (make-object horizontal-pane% hp))))]
           [make-check-box
            (lambda (name panel)
              (right-align
               (lambda (hp)
                 (make-object check-box% name hp void))
               panel))]
           [case-sensitive-cb (make-check-box (string-constant case-sensitive-label)
                                              input-syntax-panel)]
           
           
           [symbol->printer-number
            (lambda (printing-setting)
              (case printing-setting
                [(constructor-style) 0]
                [(quasi-style) 1]
                [(quasi-read-style) 1]
                [(r4rs-style) 2]
                [else (error 'drscheme:language:update-to "got: ~a as printing style"
                             printing-setting)]))]
           [printer-number->symbol
            (lambda (which)
              (case which
                [(0) 'constructor-style]
                [(1) 'quasi-style]
                [(2) 'r4rs-style]
                [else 'constructor-style]))]
           
           [printing-rb
            (right-align
             (lambda (main)
               (make-object radio-box%
                 (string-constant output-style-label)
                 (list (string-constant constructor-printing-style)
                       (string-constant quasiquote-printing-style)
                       (string-constant write-printing-style))
                 main
                 void))
             output-syntax-panel)]
           
           [sharing-printing-cb
            (make-check-box (string-constant sharing-printing-label) output-syntax-panel)]
           [whole/fractional-exact-numbers-cb
            (make-check-box (string-constant whole/fractional-exact-numbers-label) output-syntax-panel)]
           [booleans-as-true/false-cb
            (make-check-box (string-constant booleans-as-true/false-label) output-syntax-panel)]
           [use-pretty-printer-cb
            (make-check-box (string-constant use-pretty-printer-label) output-syntax-panel)])
      
      (send printing-rb set-value (symbol->printer-number (setting-use-pretty-printer setting)))
      (send case-sensitive-cb set-value (setting-case-sensitive setting))
      (send whole/fractional-exact-numbers-cb set-value (setting-whole/fractional-exact-numbers-cb setting))
      (send sharing-printing-cb set-value (setting-sharing-printing setting))
      (send booleans-as-true/false-cb set-value (setting-booleans-as-true/false setting))
      (send use-pretty-printer-cb set-value (setting-use-pretty-printer-cb setting))
      
      (lambda ()
        (make-settings/parse 
         `((case-sensitive ,(send case-sensitive-cb get-value))
           (whole/fractional-exact-numbers ,(send whole/fractional-exact-numbers-cb get-value))
           (printing ,(printer-number->symbol (send printing-rb get-value)))
           (use-pretty-printer ,(send use-pretty-printer-cb get-value))
           (sharing-printing ,(send sharing-printing-cb get-value))
           (read-decimal-as-exact #t)
           (disallow-untagged-inexact-numbers #f)
           (abbreviate-cons-as-list #t)
           (print-tagged-inexact-numbers #t)
           (print-booleans-as-true/false #t)
           (print-exact-as-decimal #t)
           (print-.-symbols-without-bars #f)
           (print-whole/part-fractions #t)))))))