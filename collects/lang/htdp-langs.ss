(module htdp-langs mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "pretty.ss")
           (prefix pc: (lib "pconvert.ss"))
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "tool.ss" "drscheme")
           (lib "macro.ss" "userspce")
           (lib "mred.ss" "mred"))
  
  (provide tool@)
  
  (define tool@
    (unit/sig () 
      (import drscheme:tool^)
      
      (define htdp-language<%>
        (interface ()
          get-module
          get-language-position
          get-teachpack-names
          sharing-printing
          abbreviate-cons-as-list
          allow-sharing?))
            
      ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
      ;;                                   -> (implements drscheme:language:module-based-language<%>)
      ;; changes the default settings and sets a few more paramters during `on-execute'
      (define (module-based-language-extension super%)
        (class* super% ()
          (override default-settings on-execute render-value/format render-value)
          (rename [super-on-execute on-execute]
                  [super-render-value/format render-value/format]
                  [super-render-value render-value])
          (inherit sharing-printing abbreviate-cons-as-list)
          
          (define (default-settings)
            (drscheme:language:make-simple-settings/parse
             `((case-sensitive #t)
               (printing-style constructor)
               (show-sharing ,(sharing-printing))
               (insert-newlines #t))))
          
          (override config-panel)
          (rename [super-config-panel config-panel])
          (inherit allow-sharing?)
          (define (config-panel parent)
            (if (allow-sharing?)
                (super-config-panel parent)
                (no-sharing-config-panel parent)))
          
          (define (on-execute settings run-in-user-thread)
            (run-in-user-thread
             (lambda ()
	       (error-print-source-location #f)
               (read-decimal-as-inexact #f)
               (read-dot-as-symbol #t)))
            (super-on-execute settings run-in-user-thread))

          (define (set-printing-parameters thunk)
            (parameterize ([pc:booleans-as-true/false #t]
                           [pc:abbreviate-cons-as-list (abbreviate-cons-as-list)]
                           [pretty-print-show-inexactness #t]
                           [pretty-print-.-symbol-without-bars #t]
                           [drscheme:rep:use-number-snip htdp-lang-use-number-snip]
                           [pretty-print-exact-as-decimal #t])
              (thunk)))
          
          ;; htdp-lang-use-number-snip : TST -> boolean
          ;; returns #t for the same numbers as the default
          ;; of this parameter, except those that have finite
          ;; decimal expansions. Those numbers are not printed
          ;; as snips.
          (define (htdp-lang-use-number-snip x)
            (if (and (number? x)
                     (exact? x)
                     (real? x)
                     (not (integer? x)))
                (not (or (zero? (modulo (denominator x) 2))
                         (zero? (modulo (denominator x) 5))))
                #f))
          
          (define (render-value/format value settings port put-snip)
            (set-printing-parameters
             (lambda ()
               (super-render-value/format value settings port put-snip))))
          
          (define (render-value value settings port put-snip)
            (set-printing-parameters
             (lambda ()
               (super-render-value value settings port put-snip))))
          
          (super-instantiate ())))

      ;; no-sharing-config-panel :  parent -> (case-> (-> settings) (settings -> void))
      ;; constructs the config-panel for a language without a sharing option.
      (define (no-sharing-config-panel _parent)
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
	       [insert-newlines (make-object check-box%
				  (string-constant use-pretty-printer-label)
				  output-panel
				  void)])
	  
	  ;; set the characteristics of the GUI
          (send _parent set-alignment 'center 'center)
	  (send parent stretchable-height #f)
	  (send parent stretchable-width #f)
          (send parent set-alignment 'center 'center)
	  (send output-panel stretchable-width #f)
	  (send output-panel set-alignment 'left 'center)
          
          (send parent reflow-container)
	  (let*-values ([(iw) (send input-panel get-width)]
                        [(ow) (send output-panel get-width)]
                        [(w) (max iw ow)])
            (send input-panel min-width w)
            (send output-panel min-width w))
          
	  (case-lambda
           [()
	    (drscheme:language:make-simple-settings
	     (send case-sensitive get-value)
	     (case (send output-style get-selection)
	       [(0) 'constructor]
	       [(1) 'quasiquote]
	       [(2) 'write])
             #f
	     (send insert-newlines get-value))]
           [(settings)
            (send case-sensitive set-value (drscheme:language:simple-settings-case-sensitive settings))
            (send output-style set-selection
                  (case (drscheme:language:simple-settings-printing-style settings)
                    [(constructor) 0]
                    [(quasiquote) 1]
                    [(write) 2]))
            (send insert-newlines set-value 
                  (drscheme:language:simple-settings-insert-newlines settings))])))
      
      ;; add-htdp-language : (implements htdp-language<%>) -> void
      (define (add-htdp-language class%)
        (let ([% (drscheme:language:module-based-language->language-mixin
                  (module-based-language-extension
                   (drscheme:language:simple-module-based-language->module-based-language-mixin
                    class%)))])
          (drscheme:language-configuration:add-language
           (make-object %)
           #t)))

      (add-htdp-language
       (class* object% (htdp-language<%> drscheme:language:simple-module-based-language<%>) 
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list
                 get-teachpack-names
                 allow-sharing?)
         (define (get-module) '(lib "advanced.ss" "lang"))
         (define (get-language-position)
           (list (string-constant how-to-design-programs)
                 (string-constant advanced-student)))
         (define (sharing-printing) #t)
         (define (abbreviate-cons-as-list) #t)
         (define (get-teachpack-names) '(make-posn posn-x posn-y posn? set-posn-x! set-posn-y!))
         (define (allow-sharing?) #t)
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%> drscheme:language:simple-module-based-language<%>) 
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list
                 get-teachpack-names
                 allow-sharing?)
         (define (get-module) '(lib "intermediate.ss" "lang"))
         (define (get-language-position)
           (list (string-constant how-to-design-programs)
                 (string-constant intermediate-student)))
         (define (sharing-printing) #f)
         (define (abbreviate-cons-as-list) #t)
         (define (get-teachpack-names) '(make-posn posn-x posn-y posn?))
         (define (allow-sharing?) #f)
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%> drscheme:language:simple-module-based-language<%>)
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list
                 get-teachpack-names
                 allow-sharing?)
         (define (get-module) '(lib "beginner-abbr.ss" "lang"))
         (define (get-language-position)
           (list (string-constant how-to-design-programs)
                 (string-constant beginning-student/abbrev)))
         (define (sharing-printing) #f)
         (define (abbreviate-cons-as-list) #t)
         (define (get-teachpack-names) '(make-posn posn-x posn-y posn?))
         (define (allow-sharing?) #f)
         (super-instantiate ())))
      
      (add-htdp-language
       (class* object% (htdp-language<%> drscheme:language:simple-module-based-language<%>)
         (public get-module
                 get-language-position
                 sharing-printing
                 abbreviate-cons-as-list
                 get-teachpack-names
                 allow-sharing?)
         (define (get-module) '(lib "beginner.ss" "lang"))
         (define (get-language-position) (list (string-constant how-to-design-programs)
                                               (string-constant beginning-student)))
         (define (sharing-printing) #f)
         (define (abbreviate-cons-as-list) #f)
         (define (get-teachpack-names) '(make-posn posn-x posn-y posn?))
         (define (allow-sharing?) #f)
         (super-instantiate ()))))))
