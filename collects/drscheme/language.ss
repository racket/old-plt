(define drscheme:language@
  (unit/sig drscheme:language^
    (import [mred : mred^]
	    [drscheme:basis : drscheme:basis^])

    (mred:debug:printf 'invoke "drscheme:language@")

    (define-struct setting (name constructor-printing
				 case-sensitive?
				 allow-set!-on-undefined?
				 unmatched-cond/case-is-error?
				 allow-improper-lists?
				 vocabulary-symbol))
    (define settings
      (list (make-setting 'Beginner     #t #t #f #t #f 'core)
	    (make-setting 'Intermediate #t #t #f #t #f 'structured)
	    (make-setting 'Advanced     #t #t #f #t #f 'side-effects)
	    (make-setting 'Quasi-R4RS   #t #t #t #t #t 'advanced)))

    (define apply-setting-to-f
      (lambda (f)
	(lambda (setting)
	  (f 'drscheme:constructor-printing (setting-constructor-printing setting))
	  (f 'drscheme:case-sensitive? (setting-case-sensitive? setting))
	  (f 'drscheme:allow-set!-on-undefined? (setting-allow-set!-on-undefined? setting))
	  (f 'drscheme:unmatched-cond/case-is-error? (setting-unmatched-cond/case-is-error? setting))
	  (f 'drscheme:allow-improper-lists? (setting-allow-improper-lists? setting)))))

    '((apply-setting mred:set-preference-default) (car (reverse settings)))
    (define apply-setting (apply-setting-to-f mred:set-preference))

    (define language-dialog
      (lambda (edit)
	(let* ([f (make-object mred:dialog-box% '() "Language" #t)]
	       [main (make-object mred:vertical-panel% f)]
	       [language-panel (make-object mred:horizontal-panel% main)]
	       [_ (make-object mred:horizontal-panel% language-panel)]
	       [language-choice (make-object mred:choice% language-panel language-panel)]
	       [_ (make-object mred:horizontal-panel% language-panel)])
	  f)))

    (define fill-language-menu
      (lambda (language-menu)
	(send* language-menu
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
			      (mred:set-preference 'drscheme:library-file #f)))
	       (append-separator)
	       (append-check-set
		(map cons drscheme:basis:level-strings
		     drscheme:basis:level-symbols)
		(let ([state #t])
		  (lambda (s)
		    (mred:set-preference 'drscheme:scheme-level s)
		    (when state
		      (set! state #f)
		      (unless (mred:get-choice
			       "Changes to the language level will not take effect until DrScheme is restarted"
			       "Continue Working"
			       "Exit")
			(mred:exit)))))
		(drscheme:basis:level->number
		 (mred:get-preference 'drscheme:scheme-level))))))))