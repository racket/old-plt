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
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?
          
          front-end
	  config-panel
	  on-execute
          render-value
          
          get-language-position))
      
      (define module-based-language<%>
	(interface ()
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?

          get-module
	  config-panel
	  on-execute
          render-value
          
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
	  (public get-module config-panel on-execute get-language-position 
                  render-value
                  default-settings? default-settings marshall-settings unmarshall-settings)
          (define (marshall-settings settings) settings)
          (define (unmarshall-settings printable) printable)
          (define (default-settings) 'no-settings)
          (define (default-settings? x) (equal? x (default-settings)))
          (define (get-module)
            (send simple-module-based-language get-module))
          (define (config-panel parent)
            (lambda ()
              (default-settings)))
          (define (on-execute setting run-in-user-thread)
            (initialize-module-based-language setting (get-module) run-in-user-thread))
	  (define (get-language-position)
	    (send simple-module-based-language get-language-position))
          (define (render-value value port put-snip)
            (write value port))
	  (super-instantiate ())))
      
      ;; module-based-language->language : language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language%
	(class* object% (language<%>)
	  (init-field module-based-language)
	  (public front-end config-panel on-execute get-language-position 
                  render-value
                  default-settings? default-settings marshall-settings unmarshall-settings)
          (define (marshall-settings settings)
            (send module-based-language marshall-settings settings))
          (define (unmarshall-settings printable)
            (send module-based-language unmarshall-settings printable))
          (define (default-settings)
            (send module-based-language default-settings))
          (define (default-settings? x)
            (send module-based-language default-settings? x))
          (define (front-end input settings)
            (lambda ()
              '...))
          (define (config-panel panel)
            (send module-based-language config-panel panel))
          (define (on-execute settings run-in-user-thread)
            (send module-based-language on-execute settings run-in-user-thread))
	  (define (get-language-position)
	    (send module-based-language get-language-position))
          (define (render-value value port put-snip)
            (send module-based-language render-value value port put-snip))
	  (super-instantiate ())))
      
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
