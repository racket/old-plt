(module language-tower mzscheme
  (require "drsig.ss"
           "string-constant.ss"
	   (lib "macro.ss" "userspce")
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
	  (define the-default-settings (make-simple-settings #f 'write #f #t))
          (define (marshall-settings settings)
	    (simple-settings->vector settings))
          (define (unmarshall-settings printable)
	    (and (vector? printable)
		 (= (vector-length printable)
		    (+ (procedure-arity make-simple-settings) 1))
		 (apply make-simple-settings (cdr (vector->list printable)))))
          (define (default-settings) the-default-settings)
          (define (default-settings? x) (equal? x the-default-settings))
          (define (get-module)
            (send simple-module-based-language get-module))
          (define (config-panel _parent settings)
	    (simple-module-based-language-config-panel _parent settings))
	  (define (on-execute setting run-in-user-thread)
	    (initialize-module-based-language setting (get-module) run-in-user-thread))
	  (define (get-language-position)
	    (send simple-module-based-language get-language-position))
          (define (render-value value port put-snip)
            (write value port))
	  (super-instantiate ())))

      ;; settings for a simple module based language
      ;;  case-sensitive  : boolean
      ;;  printing-style  : (union 'write 'constructor 'quasiquote)
      ;;  show-sharing    : boolean
      ;;  insert-newlines : boolean
      (define-struct/parse simple-settings
	(case-sensitive printing-style show-sharing insert-newlines))
      

      ;; simple-module-based-language-config-panel : parent settings -> (-> settings)
      (define (simple-module-based-language-config-panel _parent settings)
	(let* ([parent (make-object vertical-panel% _parent)]
	       [_gap1 (make-object vertical-panel% parent)]
	       [input-parent-panel (make-object vertical-panel% parent)]
	       [_gap2 (make-object vertical-panel% parent)]
	       [output-parent-panel (make-object vertical-panel% parent)]
	       [_gap3 (make-object vertical-panel% parent)]

	       [input-msg (make-object message% (string-constant input-syntax)
				       input-parent-panel)]
	       [input-panel (make-object vertical-panel% input-parent-panel '(border))]
	       
	       [output-msg (make-object message% (string-constant output-syntax)
					output-parent-panel)]
	       [output-panel (make-object vertical-panel% output-parent-panel '(border))]

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
	       [show-sharing (make-object check-box%
			       (string-constant sharing-printing-label)
			       output-panel
			       void)]
	       [insert-newlines (make-object check-box%
				  (string-constant use-pretty-printer-label)
				  output-panel
				  void)])
	  
	  ;; set the characteristics of the GUI
	  (send input-parent-panel stretchable-height #f)
	  (send input-panel stretchable-width #f)
	  (send output-parent-panel stretchable-height #f)
	  (send output-panel stretchable-width #f)
	  (send output-panel set-alignment 'right 'center)

	  (let ([w (max (send input-panel min-width)
			(send output-panel min-width))])
	    (send input-panel min-width w)
	    (send output-panel min-width w))


	  ;; update the GUI to the input settings
	  (send case-sensitive set-value (simple-settings-case-sensitive settings))
	  (send output-style set-selection
		(case (simple-settings-printing-style settings)
		  [(constructor) 0]
		  [(quasiquote) 1]
		  [(write) 2]))
	  (send show-sharing set-value (simple-settings-show-sharing settings))
	  (send insert-newlines set-value (simple-settings-insert-newlines settings))

	  ;; procedure to extract the settings from the GUI
	  (lambda ()
	    (make-simple-settings
	     (send case-sensitive get-value)
	     (case (send output-style get-value)
	       [(0) 'constructor]
	       [(1) 'quasiquote]
	       [(2) 'write])
	     (send show-sharing get-value)
	     (send insert-newlines get-value)))))

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
          (define (config-panel panel settings)
            (send module-based-language config-panel panel settings))
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
