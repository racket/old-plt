(module language-tower mzscheme
  (require "drsig.ss"
           "string-constant.ss"
           (lib "etc.ss")
	   (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide language-tower@)

  (define language-tower@
    (unit/sig drscheme:language-tower^
      (import)
      
      (define language<%>
	(interface ()
	  front-end
	  config-panel
	  on-execute
          get-language-position))
      
      (define module-based-language<%>
	(interface ()
	  get-module
	  config-panel
	  on-execute
          get-language-position))
      
      (define simple-module-based-language<%>
	(interface ()
	  get-module
          get-language-position))
      
      (define simple-module-based-language%
        (class* object% (simple-module-based-language<%>)
          (init-field module language-position)
          (public get-module get-language-position)
          (define (get-module) module)
	  (define (get-language-position) language-position)
	  (super-instantiate ())))
      
      ;; simple-module-based-language->module-based-language : module-based-language<%>
      ;; transforms a simple-module-based-language into a module-based-language<%>
      (define simple-module-based-language->module-based-language%
	(class* object% (module-based-language<%>)
	  (init-field simple-module-based-language)
	  (public get-module config-panel on-execute get-language-position)
          (define (get-module)
            (send simple-module-based-language get-module))
          (define (config-panel parent)
            (build-simple-module-based-language-settings parent))
          (define (on-execute setting run-in-user-thread)
            (initialize-module-based-language setting (get-module) run-in-user-thread))
	  (define (get-language-position)
	    (send simple-module-based-language get-language-position))
	  (super-instantiate ())))
      
      ;; module-based-language->language : language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language%
	(class* object% (language<%>)
	  (init-field module-based-language)
	  (public front-end config-panel on-execute get-language-position)
          (define (front-end input settings)
            (lambda ()
              '...))
          (define (config-panel panel)
            (send module-based-language config-panel panel))
          (define (on-execute settings run-in-user-thread)
            (send module-based-language on-execute settings run-in-user-thread))
	  (define (get-language-position)
	    (send module-based-language get-language-position))
	  (super-instantiate ())))
      
      ;; build-simple-module-based-language-settings : ((instanceof panel<%>) -> (-> setting))
      ;; constrcts the standard settings panel
      (define (build-simple-module-based-language-settings parent)
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
               [case-sensitive? (make-check-box (string-constant case-sensitive?-label)
						input-syntax-panel)]
               
               [printer-number->symbol
                (lambda (which)
                  (case which
                    [(0) 'constructor-style]
                    [(1) 'quasi-style]
                    [(2) 'r4rs-style]
                    [else 'constructor-style]))]
               
               [printing
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
               
               [sharing-printing?
                (make-check-box (string-constant sharing-printing?-label) output-syntax-panel)]
               [whole/fractional-exact-numbers
                (make-check-box (string-constant whole/fractional-exact-numbers-label) output-syntax-panel)]
               [booleans-as-true/false
                (make-check-box (string-constant booleans-as-true/false-label) output-syntax-panel)]
               [use-pretty-printer?
                (make-check-box (string-constant use-pretty-printer?-label) output-syntax-panel)])
        
        (lambda ()
          'dummy-settings)))
      
      (define (initialize-module-based-language setting module-spec run-in-user-thread)
      ;; must call the resolver before setting the namespace
        (let ([namespace (make-namespace 'empty)])
          (let ([lang-module-spec module-spec])
            (dynamic-require lang-module-spec #f)
            (let ([orig-namespace (current-namespace)]
                  [lang-name ((current-module-name-resolver)
                              lang-module-spec #f #f)])
              (current-namespace namespace)
              (namespace-attach-module orig-namespace lang-name)
              (namespace-require lang-name))))))))
