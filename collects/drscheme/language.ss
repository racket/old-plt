
(unit/sig drscheme:language^
  (import [mred : mred-interfaces^]
	  [fw : framework^]
	  [drscheme:unit : drscheme:unit^]
	  [aries : plt:aries^]
	  [zodiac : drscheme:zodiac^]
	  [basis : userspace:basis^]
	  mzlib:function^
	  mzlib:file^
	  mzlib:print-convert^)

  (define (get-printer-style-number printing-setting)
    (case printing-setting
      [(constructor-style) 0]
      [(quasi-style) 1]
      [(quasi-read-style) 1]
      [(r4rs-style) 2]
      [else (error 'drscheme:language:update-to "got: ~a as printing style"
		   printing-setting)]))

  (define (language-dialog)
    (letrec
	([dialog% (class mred:dialog% (name)
		    (override
		      [on-close
		       (lambda ()
			 (when (procedure? unregister-callback)
			   (unregister-callback)))])
		    (sequence (super-init name)))]
	 [language-levels (map basis:setting-name basis:settings)]
	 [f (make-object dialog% "Language")]
	 [main (make-object mred:vertical-pane% f)]
	 [language-panel (make-object mred:horizontal-panel% main '(border))]
	 [customization-panel (make-object mred:horizontal-panel% main)]
	 [customization-left-panel (make-object mred:vertical-pane% customization-panel)]
	 [customization-right-panel (make-object mred:vertical-pane% customization-panel)]
	 [when-message (make-object mred:message% "Language changes effective after next execution" main)]
	 [make-sub-panel
	  (lambda (name panel)
	    (let* ([p (make-object mred:vertical-pane% panel)]
		   [message (make-object mred:message% name p)])
	      (make-object mred:vertical-panel% p '(border))))]
	 [input-syntax-panel (make-sub-panel "Input Syntax" customization-left-panel)]
	 [dynamic-panel (make-sub-panel "Safety Properties" customization-left-panel)]
	 [output-syntax-panel (make-sub-panel "Output Syntax" customization-right-panel)]
	 
	 [specifics-shown? #f]
	 [show-specifics
	  (lambda (bool)
	    (set! specifics-shown? bool)
	    (send ok-panel change-children
		  (lambda (l)
		    (cons (if bool hide-button show-button)
			  (remq hide-button
				(remq show-button l)))))
	    (send main change-children
		  (lambda (l)
		    (if bool
			(list language-panel customization-panel when-message ok-panel)
			(list language-panel when-message ok-panel)))))]
	 [language-choice (make-object mred:choice%
			    "Language"
			    language-levels
			    language-panel
			    (lambda (choice evt)
			      (fw:preferences:set
			       'drscheme:settings
			       (basis:copy-setting
				(basis:number->setting
				 (send choice get-selection))))))]
	 [custom-message (make-object mred:message% "Custom" language-panel)]
	 [right-align
	  (opt-lambda (mo panel)
	    (let* ([hp (make-object mred:horizontal-pane% panel)])
	      (begin0
	       (mo hp)
	       (make-object mred:horizontal-pane% hp))))]
	 [make-check-box
	  (lambda (set-setting! setting name panel)
	    (right-align
	     (lambda (hp)
	       (make-object mred:check-box%
		 name
		 hp
		 (lambda (check-box evt)
		   (let ([i (send check-box get-value)]
			 [s (fw:preferences:get 'drscheme:settings)])
		     (set-setting! s i)
		     (fw:preferences:set 'drscheme:settings s)))))
	     panel))]
	 
	 [case-sensitive? (make-check-box basis:set-setting-case-sensitive?!
					  basis:setting-case-sensitive?
					  "Case sensitive"
					  input-syntax-panel)]
	 [unmatched-cond/case-is-error?
	  (make-check-box basis:set-setting-unmatched-cond/case-is-error?!
			  basis:setting-unmatched-cond/case-is-error?
			  "Unmatched cond/case is an error"
			  dynamic-panel)]
	 [signal-undefined
	  (make-check-box basis:set-setting-signal-undefined!
			  basis:setting-signal-undefined
			  "Signal undefined variables when first referenced"
			  dynamic-panel)]
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
	     (make-object mred:radio-box%
	       "Output Style"
	       (list "Constructor"
		     "Quasiquote"
		     "R4RS")
	       main
	       (lambda (box evt)
		 (let* ([which (send box get-selection)]
			[setting (fw:preferences:get 'drscheme:settings)]
			[symbol-which (printer-number->symbol which)])
		   (basis:set-setting-printing! setting symbol-which)
		   (fw:preferences:set 'drscheme:settings setting)))))
	   output-syntax-panel)]
	 [sharing-printing?
	  (make-check-box basis:set-setting-sharing-printing?!
			  basis:setting-sharing-printing?
			  "Show sharing in values"
			  output-syntax-panel)]
	 [whole/fractional-exact-numbers
	  (make-check-box basis:set-setting-whole/fractional-exact-numbers!
			  basis:setting-whole/fractional-exact-numbers
			  "Print rationals in whole/part notation"
			  output-syntax-panel)]
	 [ok-panel (make-object mred:horizontal-pane% main)]
	 [hide-button (make-object mred:button%
			"Hide Details"
			ok-panel
			(lambda (button evt) (show-specifics #f)))]
	 [show-button (make-object mred:button%
			"Show Details"
			ok-panel
			(lambda (button evt) (show-specifics #t)))]
	 [_3 (make-object mred:horizontal-pane% ok-panel)]
	 [cancel-button (make-object mred:button%
			  "Cancel"
			  ok-panel
			  (lambda (button evt) 
			    (fw:preferences:read)
			    (send f show #f)
			    (when (procedure? unregister-callback)
			      (unregister-callback))))]
	 [ok-button (make-object mred:button%
		      "OK"
		      ok-panel
		      (lambda (button evt) 
			(send f show #f)
			(when (procedure? unregister-callback)
			  (unregister-callback)))
		      '(border))]
	 [spacer (make-object mred:grow-box-spacer-pane% ok-panel)]
	 [compare-setting-to-gui
	  (lambda (setting)
	    (let* ([compare-check-box
		    (lambda (check-box selector)
		      (let* ([cbv (send check-box get-value)]
			     [ss (selector setting)])
			(equal? (not cbv)
				(not ss))))])
	      (and (compare-check-box case-sensitive? basis:setting-case-sensitive?)
		   (compare-check-box unmatched-cond/case-is-error? basis:setting-unmatched-cond/case-is-error?)
		   (compare-check-box signal-undefined basis:setting-signal-undefined)
		   (compare-check-box sharing-printing? basis:setting-sharing-printing?)
		   (compare-check-box whole/fractional-exact-numbers basis:setting-whole/fractional-exact-numbers)
		   (eq? (printer-number->symbol (send printing get-selection))
			(basis:setting-printing setting)))))]
	 [reset-choice
	  (lambda ()
	    (send language-panel
		  change-children
		  (lambda (l)
		    (let ([not-custom?
			   (compare-setting-to-gui
			    (basis:number->setting
			     (send language-choice get-selection)))])
		      (if not-custom?
			  (list language-choice)
			  (list language-choice custom-message))))))]
	 [update-to
	  (lambda (v)
	    (let ([zodiac? (basis:zodiac-vocabulary? v)])

	      (send language-choice set-string-selection (basis:setting-name v))
	      
	      (send printing set-selection
		    (get-printer-style-number (basis:setting-printing v)))
	      (let ([r4rs-style? (eq? 'r4rs-style (basis:setting-printing v))])
		(send whole/fractional-exact-numbers enable (not r4rs-style?))
		(when r4rs-style?
		  (basis:set-setting-whole/fractional-exact-numbers! v #f)))

	      (for-each
	       (lambda (get check-box) (send check-box set-value (get v)))
	       (list basis:setting-case-sensitive?
		     basis:setting-sharing-printing?
		     basis:setting-whole/fractional-exact-numbers
		     basis:setting-unmatched-cond/case-is-error?
		     basis:setting-signal-undefined)
	       (list case-sensitive? 
		     sharing-printing?
		     whole/fractional-exact-numbers
		     unmatched-cond/case-is-error?
		     signal-undefined))

	      (send printing enable 1
		    (not (eq? (basis:setting-vocabulary-symbol v) 'beginner)))
	      (send signal-undefined enable zodiac?)

	      (reset-choice)))]
	 [unregister-callback
	  (fw:preferences:add-callback 'drscheme:settings 
				       (lambda (p v) 
					 (send (fw:group:get-the-frame-group)
					       for-each-frame 
					       (lambda (frame)
						 (when (is-a? frame drscheme:unit:frame%)
						   (let ([edit (ivar frame definitions-edit)])
						     (when (is-a? edit mred:editor<%>)
						       (send edit language-changed))))))
					 (update-to v)))])
      (send f stretchable-width #f)
      (send f stretchable-height #f)
      (send language-choice stretchable-width #f)
      (send printing stretchable-width #f)
      (update-to (fw:preferences:get 'drscheme:settings))
      (show-specifics (not (ormap compare-setting-to-gui basis:settings)))
      (for-each (lambda (x) (send x stretchable-height #f))
		(list language-panel ok-panel main))
      (send language-panel set-alignment 'center 'center)
      (send ok-button min-width (send cancel-button get-width))
      (fw:preferences:save)
      (send f center 'both)
      (send f show #t)
      f))
  
  
  ; object to remember last library directory
  
  (define library-directory 
    (let ([lib-dir (build-path 
		    (collection-path "mzlib")
		    'up 'up "lib")])
      (if (directory-exists? lib-dir)
	  lib-dir
	  null)))

  (define (fill-language-menu language-menu)
    (make-object mred:menu-item%
      "Configure Language..."
      language-menu
      (lambda (_1 _2) (language-dialog))
      #\l)
    (make-object mred:separator-menu-item% language-menu)
    (make-object mred:menu-item%
      "Set Library To..."
      language-menu
      (lambda (_1 _2)
	(let ([lib-file (fw:finder:get-file 
			 library-directory
			 "Select a library" 
			 ".*\\.ss$")])
	  (when lib-file
	    (fw:preferences:set
	     'drscheme:library-file lib-file)
	    (set! library-directory (path-only lib-file))))))
    (make-object mred:menu-item%
      "Clear Library"
      language-menu
      (lambda (_1 _2) (fw:preferences:set 'drscheme:library-file #f)))))

