(module language-tower mzscheme
  (require "drsig.ss"
           (lib "hierlist.ss" "hierlist")
	   (lib "unitsig.ss")
	   (lib "class.ss"))

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
      
      (define simple-module-based-language
        (class* object% (simple-module-based-language<%>)
          (init-field module language-position)
          (public get-module get-language-position)
          (define (get-module) module)
          (define (get-language-position) language-position)))
      
      ;; simple-module-based-language->module-based-language : module-based-language<%>
      ;; transforms a simple-module-based-language into a module-based-language<%>
      (define simple-module-based-language->module-based-language
	(class* object% (module-based-language<%>)
	  (init-field simple-module-based-language)
	  (public get-module config-panel on-execute)
          (define (get-module)
            (send simple-module-based-language get-module))
          (define (config-panel parent)
            (build-simple-module-based-language-settings parent))
          (define (on-execute setting run-in-user-thread)
            (initialize-module-based-language setting (get-module) run-in-user-thread))))
      
      ;; module-based-language->language : language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language
	(class* object% (language<%>)
	  (init-field module-based-language)
	  (public front-end config-panel on-execute)
          (define (front-end input settings)
            (lambda ()
              '...))
          (define (config-panel panel)
            (lambda (panel)
              (send module-based-language config-panel panel)))
          (define (on-execute settings run-in-user-thread)
            (send module-based-language on-execute settings run-in-user-thread))))
      
      ;; build-simple-module-based-language-settings : ((instanceof panel<%>) -> (-> setting))
      ;; constrcts the standard settings panel
      (define (build-simple-module-based-language-settings parent)
        ;; construct dialog
        (lambda ()
          'dummy-settings))
      
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