(module language-tower mzscheme
  (require "drsig.ss"
           "string-constant.ss"
           (lib "pconvert.ss")
           (lib "pretty.ss")
	   (lib "macro.ss" "userspce")
           (lib "etc.ss")
	   (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide language-tower@)

  (define language-tower@
    (unit/sig drscheme:language-tower^
      (import [drscheme:rep : drscheme:rep^])
      
      (define language<%>
	(interface ()
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?
          
          front-end
	  config-panel
	  on-execute
          render-value/format
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
          render-value/format
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
                  render-value/format render-value
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
          (define (render-value/format value settings port put-snip)
            (simple-module-based-language-render-value/format value settings port put-snip))
          (define (render-value value settings port put-snip)
            (simple-module-based-language-render-value value settings port put-snip))
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
	       
	       [input-msg (make-object message% (string-constant input-syntax) parent)]
	       [input-panel (make-object vertical-panel% parent '(border))]
	       
	       [output-msg (make-object message% (string-constant output-syntax) parent)]
	       [output-panel (make-object vertical-panel% parent '(border))]

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
	  (send parent stretchable-height #f)
	  (send parent stretchable-width #f)
	  (send output-panel stretchable-width #f)
	  (send output-panel set-alignment 'left 'center)
          
          (send parent set-alignment 'center 'center)

          (send parent reflow-container)
	  (let*-values ([(iw) (send input-panel get-width)]
                        [(ow) (send output-panel get-width)]
                        [(w) (max iw ow)])
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
	     (case (send output-style get-selection)
	       [(0) 'constructor]
	       [(1) 'quasiquote]
	       [(2) 'write])
	     (send show-sharing get-value)
	     (send insert-newlines get-value)))))

      ;; simple-module-based-language-render-value/format : TST settings port (union #f (snip% -> void)) -> void
      (define (simple-module-based-language-render-value/format value settings port put-snip)
        (let ([converted-value
               (simple-module-based-language-convert-value value settings)])
          (cond
            [(simple-settings-insert-newlines settings)
             (pretty-print converted-value port)]
            [else
             (write converted-value port)])))
      
      ;; simple-module-based-language-render-value : TST settings port (union #f (snip% -> void)) -> void
      (define (simple-module-based-language-render-value value settings port put-snip)
        (write (simple-module-based-language-convert-value value settings) port))

      ;; simple-module-based-language-convert-value : TST settings -> TST
    (define (simple-module-based-language-convert-value value settings)
      (case (simple-settings-printing-style settings)
        [(write) value]
        [(constructor)
         (parameterize ([constructor-style-printing #t])
           (print-convert value))]
        [(quasiquote)
         (parameterize ([constructor-style-printing #f])
           (print-convert value))]))
        
      ;; initialize-simple-module-based-language : setting module-spec ((-> void) -> void)
      (define (initialize-module-based-language settings module-spec run-in-user-thread)
        ;; must call the resolver before setting the namespace
        (dynamic-require module-spec #f)
        (let ([orig-namespace (current-namespace)]
              [lang-name (if (symbol? module-spec)
			     module-spec
			     ((current-module-name-resolver) module-spec #f #f))])
          (run-in-user-thread
           (lambda ()
             (namespace-attach-module orig-namespace lang-name)
             (namespace-require lang-name)
             (read-case-sensitive (simple-settings-case-sensitive settings))))))
      
      ;; module-based-language->language : language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language%
	(class* object% (language<%>)
	  (init-field module-based-language)
	  (public front-end config-panel on-execute get-language-position 
                  render-value/format render-value
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
            (module-based-language-front-end input settings))
          (define (config-panel panel settings)
            (send module-based-language config-panel panel settings))
          (define (on-execute settings run-in-user-thread)
            (send module-based-language on-execute settings run-in-user-thread))
	  (define (get-language-position)
	    (send module-based-language get-language-position))
          (define (render-value/format value settings port put-snip)
            (send module-based-language render-value/format value settings port put-snip))
          (define (render-value value settings port put-snip)
            (send module-based-language render-value value settings port put-snip))
	  (super-instantiate ())))
      
      ;; module-based-language-front-end : (input settings -> (-> (union sexp syntax eof)))
      (define (module-based-language-front-end input settings)
        (let ([port (cond
                      [(string? input) (open-input-file input)]
                      [else (open-input-text (drscheme:rep:text/pos-text input)
                                             (drscheme:rep:text/pos-start input)
                                             (drscheme:rep:text/pos-end input))])])
          (lambda ()
            (read-syntax input port))))
      
      ;; open-input-text : (instanceof text%) num num -> input-port
      ;; creates a user port whose input is taken from the text%,
      ;; starting at position `start' and ending at position `end'
      (define (open-input-text text start end)
        (send text split-snip start)
        (send text split-snip end)
        (let* ([snip (send text find-snip start 'after-or-none)]
               [str (and (object? snip) (send snip get-text 0 (send snip get-count)))]
               [pos 0]
               [next-snip
                (lambda ()
                  (set! snip (send snip next))
                  (set! str (and (object? snip) (send snip get-text 0 (send snip get-count))))
                  (set! pos 0))]
               [read-char (lambda () 
                            (when (and str
                                       ((string-length str) . <= . pos))
                              (next-snip))
                            (cond
                              [(not str) eof]
                              [else
                               (begin0
                                 (string-ref str pos)
                                 (set! pos (+ pos 1)))]))]
               [char-ready? (lambda () #t)]
               [close (lambda () (void))]
               [peek-char #f])
          (make-input-port read-char char-ready? close peek-char))))))
