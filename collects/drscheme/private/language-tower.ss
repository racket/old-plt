(module language-tower mzscheme
  (require "drsig.ss"
	   (lib "unitsig.ss")
	   (lib "class.ss"))

  (provide languages@)

  (define languages@
    (unit/sig drscheme:languages^
      (import)

      (define language<%>
	(interface ()
	  front-end
	  config-panel
	  on-execute
	  editor-mixin))

      (define module-based-language<%>
	(interface ()
	  get-module
	  config-panel
	  on-execute))

      (define simple-module-based-language<%>
	(interface ()
	  get-module))

      (define simple-module-based-language->module-based-language
	(class* object% (module-based-language<%>)
	  (init-field simple-module-based-language)
	  (public
	    [get-module
	     (lambda ()
	       (send simple-module-based-language get-module))]
	    [config-panel
	     (lambda (parent)
	       (build-simple-module-based-language-settings parent))]
	    [on-execute
	     (lambda (setting)
	       (initialize-module-based-language setting (get-module)))])))

      (define module-based-language->language
	(class* object% (language<%>)
	  (init-field module-based-language)
	  (public
	    [front-end
	     (lambda (input settings)
	       (lambda ()
		 ...))]
	    [config-panel
	     (lambda (panel)
	       (send module-based-language config-panel panel))]
	    [on-execute
	     (lambda (settings)
	       (send module-based-language on-execute settings))]
	    [editor-mixin
	     (lambda (super%)
	       ...]))))

      (define (initialize-module-based-language setting module)



	..))))
