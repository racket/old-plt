(unit/sig drscheme:language^
  (import [mred : mred^]
	  [drscheme:basis : drscheme:basis^]
	  [aries : plt:aries^]
	  [zodiac : drscheme:zodiac^]
	  mzlib:function^
	  mzlib:print-convert^)
  
  (mred:debug:printf 'invoke "drscheme:language@")
  
  (define-struct setting (use-zodiac?
			  vocabulary-symbol
			  case-sensitive?
			  allow-set!-on-undefined?
			  unmatched-cond/case-is-error?
			  allow-improper-lists?
			  sharing-printing?
			  abbreviate-cons-as-list?
			  signal-undefined
			  signal-not-boolean
			  eq?-only-compares-symbols?
			  read-exact-numbers
			  printing))
  
  (define settings
    (list (list 'Beginner (make-setting #t 'core #t #f #t #f #f #f #t #t #t #t
					'constructor-style))
	  (list 'Intermediate (make-setting #t 'structured
					    #t #f #t #f #f #t #t #t #t #t
					    'constructor-style))
	  (list 'Advanced (make-setting #t 'side-effecting
					#t #f #t #f #t #t #t #f #t #f
					'constructor-style))
	  (list 'Quasi-R4RS (make-setting #t 'advanced
					  #t #t #t #t #f #t #t #f #f #f
					  'r4rs-style))))
  
  (define (setting-name setting)
    (or (ormap (lambda (x)
		 (if (equal? (second x) setting)
		     (first x)
		     #f))
	       settings)
	'Custom))
  
  (define copy-setting
    (lambda (x)
      (apply make-setting (cdr (vector->list (struct->vector x))))))
  
  (define (get-default)
    (copy-setting (second (car (reverse settings)))))
  (mred:set-preference-default 'drscheme:settings
			       (get-default)
			       setting?)
  (mred:set-preference-un/marshall 'drscheme:settings
				   (compose cdr vector->list struct->vector)
				   (lambda (x) 
				     (if (and (list? x)
					      (= (arity make-setting) (length x)))
					 (apply make-setting x)
					 (get-default))))
  
  (define get-printer-style-number
    (lambda (printing-setting)
      (case printing-setting
	[(constructor-style) 0]
	[(quasi-style) 1]
	[(quasi-read-style) 2]
	[(r4rs-style) 3]
	[else (error 'drscheme:language:update-to "got: ~a as printing style"
		     printing-setting)])))
  
  (define eq?-only-compares-symbols (make-parameter #f))
  (define r4rs-style-printing (make-parameter #f))
  (define use-zodiac (make-parameter #t))
  
  (define install-language
    (lambda (parameterization)
      (let ([pref (mred:get-preference 'drscheme:settings)])
	(use-zodiac (setting-use-zodiac? pref))
	(zodiac:current-vocabulary (setting-vocabulary-symbol pref))
	
	((in-parameterization parameterization read-case-sensitive)
	 (setting-case-sensitive? pref))
	;; this sets both the zodiac and the cons procedure settings,
	;; via a dynamic link in basis.ss
	(zodiac:allow-improper-lists (or (not (setting-use-zodiac? pref))
					 (setting-allow-improper-lists? pref)))
	
	(eq?-only-compares-symbols (setting-eq?-only-compares-symbols? pref))
	
	(zodiac:read-exact-numbers (setting-read-exact-numbers pref))

	(aries:signal-undefined (setting-signal-undefined pref))
	(aries:signal-not-boolean (setting-signal-not-boolean pref))
	
	(compile-allow-set!-undefined (setting-allow-set!-on-undefined? pref))
	(compile-allow-cond-fallthrough (not (setting-unmatched-cond/case-is-error? pref)))
	
	;; need to introduce parameter for constructor-style vs r4 style
	(case (setting-printing pref)
	  [(constructor-style)
	   (r4rs-style-printing #f)
	   (constructor-style-printing #t)]
	  [(quasi-style)
	   (r4rs-style-printing #f)
	   (constructor-style-printing #f)
	   (quasi-read-style-printing #f)]
	  [(quasi-read-style)
	   (r4rs-style-printing #f)
	   (constructor-style-printing #f)
	   (quasi-read-style-printing #t)]
	  [(r4rs-style) (r4rs-style-printing #t)]
	  [else (error 'install-language "found bad setting-printing: ~a~n" 
		       (setting-printing pref))])
	(show-sharing (setting-sharing-printing? pref))
	(print-graph (setting-sharing-printing? pref))
	(abbreviate-cons-as-list (setting-abbreviate-cons-as-list? pref)))))
  
  (define language-dialog
    (lambda ()
      (letrec*
	  ([language-levels (map (compose symbol->string first) settings)]
	   [f (make-object mred:dialog-box% '() "Language" #t)]
	   [main (make-object mred:vertical-panel% f)]
	   [language-panel (make-object mred:horizontal-panel% main -1 -1 -1 -1 wx:const-border)]
	   [customization-panel (make-object mred:horizontal-panel% main)]
	   [customization-left-panel (make-object mred:vertical-panel% customization-panel)]
	   [customization-right-panel (make-object mred:vertical-panel% customization-panel)]
	   [when-message (make-object mred:message% main "Language changes effective after next execution")]
	   [make-sub-panel
	    (lambda (name panel)
	      (let* ([p (make-object mred:vertical-panel% panel)]
		     [message (make-object mred:message% p name)])
		(make-object mred:vertical-panel% p -1 -1 -1 -1 wx:const-border)))]
	   [input-syntax-panel (make-sub-panel "Input Syntax" customization-right-panel)]
	   [dynamic-panel (make-sub-panel "Safety Properties" customization-left-panel)]
	   [output-syntax-panel (make-sub-panel "Output Syntax" customization-right-panel)]
	   
	   [_1 (make-object mred:horizontal-panel% language-panel)]
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
	   [language-choice (make-object mred:choice% language-panel
					 (lambda (choice evt)
					   (let ([which (send evt get-command-int)]
						 [len (length settings)])
					     (when (= which len)
					       (show-specifics #t))
					     (when (< which len)
					       (mred:set-preference
						'drscheme:settings
						(copy-setting (second (list-ref settings which)))))))
					 "Language"
					 -1 -1 -1 -1
					 (append language-levels (list "Custom")))]
	   [_2 (make-object mred:horizontal-panel% language-panel)]
	   [right-align
	    (opt-lambda (mo panel)
	      (let* ([hp (make-object mred:horizontal-panel% panel)])
		(begin0
		  (mo hp)
		  (make-object mred:horizontal-panel% hp))))]
	   [make-check-box
	    (lambda (set-setting! setting name panel)
	      (right-align
	       (lambda (hp)
		 (make-object mred:check-box% hp
			      (lambda (check-box evt)
				(let ([i (send evt checked?)]
				      [s (mred:get-preference 'drscheme:settings)])
				  (set-setting! s i)
				  (mred:set-preference 'drscheme:settings
						       s)))
			      name))
	       panel))]
	   [vocab (right-align 
		   (lambda (hp)
		     (make-object mred:choice% hp
				  (lambda (vocab evt)
				    (let ([pos (send evt get-selection)]
					  [s (mred:get-preference 'drscheme:settings)])
				      (if (= pos (length drscheme:basis:level-symbols))
					  (set-setting-use-zodiac?! s #f)
					  (begin 
					    (set-setting-use-zodiac?! s #t)
					    (set-setting-vocabulary-symbol!
					     s (list-ref drscheme:basis:level-symbols pos))))
				      (mred:set-preference 'drscheme:settings s)))
				  "Vocabulary"
				  -1 -1 -1 -1
				  (append language-levels (list "MrEd"))))
		   input-syntax-panel)]
	   [case-sensitive? (make-check-box set-setting-case-sensitive?!
					    setting-case-sensitive?
					    "Case sensitive?"
					    input-syntax-panel)]
	   [allow-improper-lists?
	    (make-check-box set-setting-allow-improper-lists?!
			    setting-allow-improper-lists?
			    "Allow improper lists?"
			    dynamic-panel)]
	   [allow-set!-on-undefined?
	    (make-check-box set-setting-allow-set!-on-undefined?!
			    setting-allow-set!-on-undefined?
			    "Allow set! on undefined identifiers?"
			    dynamic-panel)]
	   [unmatched-cond/case-is-error?
	    (make-check-box set-setting-unmatched-cond/case-is-error?!
			    setting-unmatched-cond/case-is-error?
			    "Unmatched cond/case is an error?"
			    dynamic-panel)]
	   [signal-undefined
	    (make-check-box set-setting-signal-undefined!
			    setting-signal-undefined
			    "Signal undefined variables when first referenced?"
			    dynamic-panel)]
	   [signal-not-boolean
	    (make-check-box set-setting-signal-not-boolean!
			    setting-signal-not-boolean
			    "Conditionals must evaluate to either #t or #f"
			    dynamic-panel)]
	   [eq?-only-compares-symbols?
	    (make-check-box set-setting-eq?-only-compares-symbols?!
			    setting-eq?-only-compares-symbols?
			    "Eq? only compares symbols"
			    dynamic-panel)]
	   [read-exact-numbers
	    (make-check-box set-setting-read-exact-numbers!
			    setting-read-exact-numbers
			    "Decimal numbers are read as exact numbers"
			    dynamic-panel)]
	   [printer-number->symbol
	    (lambda (which)
	      (case which
		[(0) 'constructor-style]
		[(1) 'quasi-style]
		[(2) 'quasi-read-style]
		[(3) 'r4rs-style]
		[else (error 'printing-callback "got: ~a~n" which)]))]
	   [printing
	    (right-align
	     (lambda (main)
	       (make-object mred:choice% main
			    (lambda (box evt)
			      (let* ([which (send evt get-command-int)]
				     [settings (mred:get-preference 'drscheme:settings)]
				     [symbol-which (printer-number->symbol which)])
				(set-setting-printing! settings symbol-which)
				(mred:set-preference 'drscheme:settings settings)))
			    "Output Style" -1 -1 -1 -1
			    (list "Constructor"
				  "Quasiquote (lists only)"
				  "Quasiquote (read syntax)"
				  "R4RS")))
	     output-syntax-panel)]
	   [abbreviate-cons-as-list?
	    (make-check-box set-setting-abbreviate-cons-as-list?!
			    abbreviate-cons-as-list?
			    "Abbreviate multiples cons's with list?"
			    output-syntax-panel)]
	   [sharing-printing?
	    (make-check-box set-setting-sharing-printing?!
			    setting-sharing-printing?
			    "Show sharing in values?"
			    output-syntax-panel)]
	   [ok-panel (make-object mred:horizontal-panel% main)]
	   [hide-button (make-object mred:button% ok-panel
				     (lambda (button evt) (show-specifics #f))
				     "Hide Details")]
	   [show-button (make-object mred:button% ok-panel
				     (lambda (button evt) (show-specifics #t))
				     "Show Details")]
	   [_3 (make-object mred:horizontal-panel% ok-panel)]
	   [cancel-button (make-object mred:button% ok-panel (lambda (button evt) 
							       (mred:read-user-preferences)
							       (send f show #f))
				       "Cancel")]
	   [ok-button (make-object mred:button% ok-panel (lambda (button evt) (send f show #f)) "OK")]
	   [compare-setting-to-gui
	    (lambda (setting)
	      (let ([compare-check-box
		     (lambda (check-box selector)
		       (let ([cbv (send check-box get-value)]
			     [ss (selector setting)])
			 (equal? (not cbv)
				 (not ss))))])
		(and (compare-check-box case-sensitive? setting-case-sensitive?)
		     (compare-check-box allow-set!-on-undefined? setting-allow-set!-on-undefined?)
		     (compare-check-box unmatched-cond/case-is-error? setting-unmatched-cond/case-is-error?)
		     (compare-check-box signal-undefined setting-signal-undefined)
		     (compare-check-box signal-not-boolean setting-signal-not-boolean)
		     (compare-check-box eq?-only-compares-symbols? setting-eq?-only-compares-symbols?)
		     (compare-check-box read-exact-numbers setting-read-exact-numbers)
		     (compare-check-box allow-improper-lists? setting-allow-improper-lists?)
		     (compare-check-box sharing-printing? setting-sharing-printing?)
		     (compare-check-box abbreviate-cons-as-list? setting-abbreviate-cons-as-list?)
		     (eq? (printer-number->symbol (send printing get-selection))
			  (setting-printing setting))
		     (if (= (send vocab get-selection) 
			    (length drscheme:basis:level-symbols))
			 (not (setting-use-zodiac? setting))
			 (= (drscheme:basis:level->number (setting-vocabulary-symbol setting))
			    (send vocab get-selection))))))]
	   [reset-choice
	    (lambda ()
	      (when (andmap (lambda (setting-name)
			      (let ([setting (second setting-name)]
				    [name (first setting-name)])
				(if (compare-setting-to-gui setting)
				    (begin
				      (send language-choice set-selection
					    (drscheme:basis:level->number
					     (setting-vocabulary-symbol setting)))
				      #f)
				    #t)))
			    settings)
		(send language-choice set-selection
		      (length drscheme:basis:level-symbols))))]
	   [update-to
	    (lambda (v)
	      (let ([zodiac? (setting-use-zodiac? v)])
		(send vocab set-selection (if zodiac?
					      (drscheme:basis:level->number (setting-vocabulary-symbol v))
					      (length drscheme:basis:level-symbols)))
		(for-each (lambda (control) (send control enable zodiac?))
			  (list allow-improper-lists?
				signal-undefined
				signal-not-boolean)))
	      (send printing set-selection
		    (get-printer-style-number (setting-printing v)))
	      (send abbreviate-cons-as-list? enable (not (eq? 'r4rs-style (setting-printing v))))
	      (map (lambda (get check-box) (send check-box set-value (get v)))
		   (list setting-case-sensitive?
			 setting-allow-set!-on-undefined?
			 setting-unmatched-cond/case-is-error?
			 setting-signal-undefined
			 setting-signal-not-boolean
			 setting-read-exact-numbers
			 setting-eq?-only-compares-symbols?
			 setting-allow-improper-lists?
			 setting-sharing-printing?
			 setting-abbreviate-cons-as-list?)
		   (list case-sensitive? 
			 allow-set!-on-undefined? 
			 unmatched-cond/case-is-error?
			 signal-undefined
			 signal-not-boolean
			 read-exact-numbers
			 eq?-only-compares-symbols?
			 allow-improper-lists?
			 sharing-printing?
			 abbreviate-cons-as-list?))
	      (reset-choice))])
	(send language-choice stretchable-in-x #f)
	(send printing stretchable-in-x #f)
	(send vocab stretchable-in-x #f)
	(update-to (mred:get-preference 'drscheme:settings))
	(show-specifics (not (ormap (compose compare-setting-to-gui second) settings)))
	(mred:add-preference-callback 'drscheme:settings (lambda (p v) (update-to v)))
	(for-each (lambda (x) (send x stretchable-in-y #f))
		  (list language-panel ok-panel main))
	(send ok-button user-min-width (send cancel-button get-width))
	(mred:save-user-preferences)
	(send f center wx:const-both)
	(send f show #t)
	f)))
  
  (define fill-language-menu
    (lambda (language-menu)
      (send* language-menu 
	(append-item "Configure Language..." language-dialog)
	(append-separator)
	(append-item "Select Library..."
		     (lambda ()
		       (let ([lib-file (mred:get-file 
					() 
					"Select a Library" 
					".*\\.ss$")])
			 (when lib-file
			   (mred:set-preference
			    'drscheme:library-file lib-file)))))
	(append-item "Clear Library"
		     (lambda ()
		       (mred:set-preference 'drscheme:library-file #f)))))))
