
(unit/sig drscheme:language^
  (import [wx : wx^]
	  [mred : mred^]
	  [drscheme:unit : drscheme:unit^]
	  [aries : plt:aries^]
	  [zodiac : drscheme:zodiac^]
	  [basis : userspace:basis^]
	  mzlib:function^
	  mzlib:file^
	  mzlib:print-convert^)
  
  (mred:debug:printf 'invoke "drscheme:language@")
  
  (mred:set-preference-default 'drscheme:settings
			       (basis:get-default-setting)
			       basis:setting?)
  (mred:set-preference-un/marshall 'drscheme:settings
				   (compose cdr vector->list struct->vector)
				   (lambda (x) 
				     (if (and (list? x)
					      (equal? (arity basis:make-setting) (length x)))
					 (apply basis:make-setting x)
					 (basis:get-default-setting))))

  (define (get-printer-style-number printing-setting)
    (case printing-setting
      [(constructor-style) 0]
      [(quasi-style) 1]
      [(quasi-read-style) 2]
      [(r4rs-style) 3]
      [else (error 'drscheme:language:update-to "got: ~a as printing style"
		   printing-setting)]))
  
  (define (language-dialog)
    (letrec*
	([language-levels (map (lambda (x) (symbol->string (vector-ref x 0))) basis:settings)]
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
					       [len (length basis:settings)])
					   (when (= which len)
					     (show-specifics #t))
					   (when (< which len)
					     (mred:set-preference
					      'drscheme:settings
					      (basis:copy-setting (vector-ref (list-ref basis:settings which) 1))))))
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
				    (basis:set-setting-vocabulary-symbol! s (list-ref basis:level-symbols pos))
				    (basis:set-setting-use-zodiac?! s (not (= pos (- (length basis:level-symbols) 1))))
				    (mred:set-preference 'drscheme:settings s)))
				"Vocabulary"
				-1 -1 -1 -1
				language-levels))
		 input-syntax-panel)]
	 [case-sensitive? (make-check-box basis:set-setting-case-sensitive?!
					  basis:setting-case-sensitive?
					  "Case sensitive"
					  input-syntax-panel)]
	 [allow-improper-lists?
	  (make-check-box basis:set-setting-allow-improper-lists?!
			  basis:setting-allow-improper-lists?
			  "Allow improper lists"
			  dynamic-panel)]
	 [allow-set!-on-undefined?
	  (make-check-box basis:set-setting-allow-set!-on-undefined?!
			  basis:setting-allow-set!-on-undefined?
			  "Allow set! on undefined identifiers"
			  dynamic-panel)]
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
	 [signal-not-boolean
	  (make-check-box basis:set-setting-signal-not-boolean!
			  basis:setting-signal-not-boolean
			  "Conditionals must evaluate to either #t or #f"
			  dynamic-panel)]
	 [eq?-only-compares-symbols?
	  (make-check-box basis:set-setting-eq?-only-compares-symbols?!
			  basis:setting-eq?-only-compares-symbols?
			  "Eq? only compares symbols"
			  dynamic-panel)]
#|
	 [disallow-untagged-inexact-numbers
	  (make-check-box basis:set-setting-disallow-untagged-inexact-numbers!
			  basis:setting-disallow-untagged-inexact-numbers
			  "Inexact numbers require #i"
			  input-syntax-panel)]
|#
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
				   [setting (mred:get-preference 'drscheme:settings)]
				   [symbol-which (printer-number->symbol which)])
			      (basis:set-setting-printing! setting symbol-which)
			      (mred:set-preference 'drscheme:settings setting)))
			  "Output Style" -1 -1 -1 -1
			  (list "Constructor"
				"Quasiquote (lists only)"
				"Quasiquote (read syntax)"
				"R4RS")))
	   output-syntax-panel)]
	 [abbreviate-cons-as-list?
	  (make-check-box basis:set-setting-abbreviate-cons-as-list?!
			  abbreviate-cons-as-list?
			  "Abbreviate multiples cons's with list"
			  output-syntax-panel)]
	 [sharing-printing?
	  (make-check-box basis:set-setting-sharing-printing?!
			  basis:setting-sharing-printing?
			  "Show sharing in values"
			  output-syntax-panel)]
	 [print-tagged-inexact-numbers
	  (make-check-box basis:set-setting-print-tagged-inexact-numbers!
			  basis:setting-print-tagged-inexact-numbers
			  "Print inexact numbers with #i"
			  output-syntax-panel)]
	 [whole/fractional-exact-numbers
	  (make-check-box basis:set-setting-whole/fractional-exact-numbers!
			  basis:setting-whole/fractional-exact-numbers
			  "Print rationals in whole/part notation"
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
	      (and (compare-check-box case-sensitive? basis:setting-case-sensitive?)
		   (compare-check-box allow-set!-on-undefined? basis:setting-allow-set!-on-undefined?)
		   (compare-check-box unmatched-cond/case-is-error? basis:setting-unmatched-cond/case-is-error?)
		   (compare-check-box signal-undefined basis:setting-signal-undefined)
		   (compare-check-box signal-not-boolean basis:setting-signal-not-boolean)
		   (compare-check-box eq?-only-compares-symbols? basis:setting-eq?-only-compares-symbols?)
		   ; (compare-check-box disallow-untagged-inexact-numbers basis:setting-disallow-untagged-inexact-numbers)
		   (compare-check-box allow-improper-lists? basis:setting-allow-improper-lists?)
		   (compare-check-box sharing-printing? basis:setting-sharing-printing?)
		   (compare-check-box print-tagged-inexact-numbers basis:setting-print-tagged-inexact-numbers)
		   (compare-check-box whole/fractional-exact-numbers basis:setting-whole/fractional-exact-numbers)
		   (compare-check-box abbreviate-cons-as-list? basis:setting-abbreviate-cons-as-list?)
		   (eq? (printer-number->symbol (send printing get-selection))
			(basis:setting-printing setting))
		   (if (= (send vocab get-selection)
			  (length basis:level-symbols))
		       (not (basis:setting-use-zodiac? setting))
		       (= (basis:level->number (basis:setting-vocabulary-symbol setting))
			  (send vocab get-selection))))))]
	 [reset-choice
	  (lambda ()
	    (when (andmap (lambda (name-setting)
			    (let ([name (vector-ref name-setting 0)]
				  [setting (vector-ref name-setting 1)])
			      (if (compare-setting-to-gui setting)
				  (begin
				    (send language-choice set-selection
					  (basis:level->number
					   (basis:setting-vocabulary-symbol setting)))
				    #f)
				  #t)))
			  basis:settings)
	      (send language-choice set-selection
		    (length basis:level-symbols))))]
	 [update-to
	  (lambda (v)
	    (let ([zodiac? (basis:setting-use-zodiac? v)])
	      (send vocab set-selection (basis:level->number (basis:setting-vocabulary-symbol v)))
	      (for-each (lambda (control) (send control enable zodiac?))
			(list allow-improper-lists?
			      signal-undefined
			      signal-not-boolean)))
	    (send printing set-selection
		  (get-printer-style-number (basis:setting-printing v)))
	    (for-each (lambda (x) (send x enable (not (eq? 'r4rs-style (basis:setting-printing v)))))
		      (list abbreviate-cons-as-list?
			    print-tagged-inexact-numbers
			    whole/fractional-exact-numbers))
	    (map (lambda (get check-box) (send check-box set-value (get v)))
		 (list basis:setting-case-sensitive?
		       basis:setting-allow-set!-on-undefined?
		       basis:setting-unmatched-cond/case-is-error?
		       basis:setting-signal-undefined
		       basis:setting-signal-not-boolean
		       ; basis:setting-disallow-untagged-inexact-numbers
		       basis:setting-eq?-only-compares-symbols?
		       basis:setting-allow-improper-lists?
		       basis:setting-sharing-printing?
		       basis:setting-print-tagged-inexact-numbers
		       basis:setting-whole/fractional-exact-numbers
		       basis:setting-abbreviate-cons-as-list?)
		 (list case-sensitive? 
		       allow-set!-on-undefined? 
		       unmatched-cond/case-is-error?
		       signal-undefined
		       signal-not-boolean
		       ; disallow-untagged-inexact-numbers
		       eq?-only-compares-symbols?
		       allow-improper-lists?
		       sharing-printing?
		       print-tagged-inexact-numbers
		       whole/fractional-exact-numbers
		       abbreviate-cons-as-list?))
	    (reset-choice))])
      (send language-choice stretchable-in-x #f)
      (send printing stretchable-in-x #f)
      (send vocab stretchable-in-x #f)
      (update-to (mred:get-preference 'drscheme:settings))
      (show-specifics (not (ormap (lambda (x) (compare-setting-to-gui (vector-ref x 1))) basis:settings)))
      (mred:add-preference-callback 'drscheme:settings 
				    (lambda (p v) 
				      (send mred:the-frame-group
					    for-each-frame 
					    (lambda (x)
					      (when (is-a? x drscheme:unit:frame%)
						(send (ivar x definitions-edit)
						      language-changed))))
				      (update-to v)))
      (for-each (lambda (x) (send x stretchable-in-y #f))
		(list language-panel ok-panel main))
      (send ok-button user-min-width (send cancel-button get-width))
      (mred:save-user-preferences)
      (send f center wx:const-both)
      (send f show #t)
      f))
  
  (define library-directory%
    (class null ()
	   (private [the-dir #f])
	   (public
	    [set! (lambda (s)
		    (set! the-dir s))]
	    [get (lambda () the-dir)]
	    [set-to-default 
	     (lambda ()
	       (let ([home-dir (getenv "PLTHOME")])
		 (if home-dir
		     (set! the-dir
			   (build-path (getenv "PLTHOME") "lib"))
		     (set! the-dir
			   ()))))])
	   (sequence
	     (set-to-default))))

  ; object to remember last library directory
  
  (define *library-directory*
    (make-object 
     (class null ()

	    (private 
	     [the-dir #f]
	     [set-to-default 
	      (lambda ()
		(set! the-dir
		      (build-path (getenv "PLTHOME") "lib")))])
	    (public
	     [get (lambda () the-dir)]
	     [set-from-file!
	      (lambda (file) 
		(set! the-dir (path-only file)))])

	    (sequence
	      (set-to-default)))))

  (define (fill-language-menu language-menu)
    (send* language-menu 
	   (append-item "Configure Language..." language-dialog)
	   (append-separator)
	   (append-item "Set Library To..."
			(lambda ()
			  (let ([lib-file (mred:get-file 
					   (send *library-directory* get) 
					   "Select a library" 
					   ".*\\.ss$")])
			    (when lib-file
			      (mred:set-preference
			       'drscheme:library-file lib-file)
			      (send *library-directory*
				    set-from-file! lib-file)))))
	   (append-item "Clear Library"
			(lambda ()
			  (mred:set-preference 'drscheme:library-file #f))))))













