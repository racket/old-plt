(unit/sig ()
  (import mred^
	  [fw : framework^]
	  [pretty-print : mzlib:pretty-print^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:get/extend : drscheme:get/extend^]
	  [drscheme:language : drscheme:language^]
	  [basis : plt:basis^]
	  mzlib:function^
          mzlib:file^
	  setup:plt-installer^)


  (fw:finder:default-extension "scm")

  ;; add the graphical settings
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named
				 "Textual Full Scheme without Debugging (MzScheme)"))])
     (basis:set-setting-name! s "Graphical Full Scheme without Debugging (MrEd)")
     (basis:set-setting-vocabulary-symbol! s 'mred)
     s)
   3)
  (basis:add-setting 
   (let ([s (basis:copy-setting (basis:find-setting-named
				 "Textual Full Scheme (MzScheme)"))])
     (basis:set-setting-name! s "Graphical Full Scheme (MrEd)")
     (basis:set-setting-vocabulary-symbol! s 'mred-debug)
     s)
   3)

  (fw:application:current-app-name "DrScheme")
  (fw:version:add-spec 'd 103)

  (define (valid-setting? setting)
    (ormap (lambda (x)
	     (equal? (basis:setting-name setting)
		     (basis:setting-name x)))
           basis:settings))

  (fw:preferences:set-default 'drscheme:unit-window-size-percentage 1/2 
                              (lambda (x) (and (number? x) (<= 0 x 1))))
  
  (let ([frame-width 600]
	[frame-height 650]
	[window-trimming-upper-bound-width 20]
	[window-trimming-upper-bound-height 50])
    (let-values ([(w h) (get-display-size)])
      (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
      (set! frame-height (min frame-height (- h window-trimming-upper-bound-height))))
    (fw:preferences:set-default 'drscheme:unit-window-width frame-width number?)
    (fw:preferences:set-default 'drscheme:unit-window-height frame-height number?))


  (fw:preferences:set-default 'drscheme:backtrace-window-width 400 number?)
  (fw:preferences:set-default 'drscheme:backtrace-window-height 300 number?)

  (fw:preferences:set-default 
   'drscheme:keybindings-window-size
   (cons 200 400)
   (lambda (x) (and (pair? x)
                    (number? (car x))
                    (number? (cdr x)))))

  ;; if the unmarshaller returns #f, that will fail the
  ;; test for this preference, reverting back to the default.
  ;; In that case, the default is specified in the pref.ss file
  ;; of the default collection and may not be the default
  ;; specified below.
  (fw:preferences:set-un/marshall
   drscheme:language:settings-preferences-symbol
   (lambda (x) (cdr (vector->list (struct->vector x))))
   (lambda (x) 
     (if (and (list? x)
	      (equal? (arity basis:make-setting)
		      (length x)))
	 (let ([setting (apply basis:make-setting x)])
	   (if (valid-setting? setting)
	       setting
	       #f))
	 #f)))

  (fw:preferences:set-default
   drscheme:language:settings-preferences-symbol
   (basis:get-default-setting)
   basis:setting?)


  (fw:preferences:set-default
   'drscheme:enable-backtrace-in-teaching-levels
   #f
   boolean?)

  (fw:preferences:set-default
   'drscheme:execute-warning-once
   #f
   (lambda (x)
     (or (eq? x #t)
	 (not x))))

  (include "various-programs.ss")

  (define get-fixed-faces
    (cond
      [(eq? (system-type) 'unix) 
       (lambda () (get-face-list))]
      [else
       (lambda ()
         (let* ([canvas (make-object canvas% (make-object frame% "bogus"))]
                [dc (send canvas get-dc)]
                [ans
                 (let loop ([faces (get-face-list)])
                   (cond
                     [(null? faces) null]
                     [else (let* ([face (car faces)]
                                  [font (make-object font% 12 face 'default 'normal 'normal #f)])
                             (let*-values ([(wi _1 _2 _3) (send dc get-text-extent "i" font)]
                                           [(ww _1 _2 _3) (send dc get-text-extent "w" font)])
                               (if (= ww wi)
                                   (cons face (loop (cdr faces)))
                                   (loop (cdr faces)))))]))])
           (set! get-fixed-faces (lambda () ans))
           ans))]))

  (define default-font-name (get-family-builtin-face 'modern))
  
  (fw:preferences:set-default
   'drscheme:font-name
   default-font-name
   string?)

  (fw:preferences:set-default
   'drscheme:font-size
   (send (send (send (make-object text%) 
		     get-style-list)
	       basic-style)
	 get-size)
   (lambda (x) (and (number? x) (exact? x) (= x (floor x)))))

  (define (set-font-size size)
    (let* ([scheme-standard (send (fw:scheme:get-style-list)
				  find-named-style "Standard")]
	   [scheme-delta (make-object style-delta%)])
      (send scheme-standard get-delta scheme-delta)
      (send scheme-delta set-size-mult 0)
      (send scheme-delta set-size-add size)
      (send scheme-standard set-delta scheme-delta)))

  (define (set-font-name name)
    (let* ([scheme-standard (send (fw:scheme:get-style-list)
				  find-named-style "Standard")]
	   [scheme-delta (make-object style-delta%)])
      (send scheme-standard get-delta scheme-delta)
      (send scheme-delta set-delta-face name)
      (send scheme-delta set-family 'modern)
      (send scheme-standard set-delta scheme-delta)))

  (set-font-size (fw:preferences:get 'drscheme:font-size))
  (set-font-name (fw:preferences:get 'drscheme:font-name))

  (fw:preferences:add-callback
   'drscheme:font-size
   (lambda (p v)
     (set-font-size v)))
  
  (fw:preferences:add-callback
   'drscheme:font-name
   (lambda (p v)
     (set-font-name v)))
  
  (unless (member (fw:preferences:get 'drscheme:font-name)
                  (get-fixed-faces))
    (fw:preferences:set 'drscheme:font-name default-font-name))
  
  (fw:preferences:add-panel
   "Font"
   (lambda (panel)
     (let* ([main (make-object vertical-panel% panel)]
	    [options-panel (make-object horizontal-panel% main)]
	    [size (make-object slider% "Font Size" 1 72 options-panel
			       (lambda (size evt)
                                 (fw:preferences:set 'drscheme:font-size (send size get-value)))
			       (fw:preferences:get 'drscheme:font-size))]

	    [font-name-control
	     (case (system-type)
	       [(windows macos)
		(let ([choice
                       (make-object choice% "Font Name"
                                    (get-fixed-faces)
                                    options-panel
                                    (lambda (font-name evt)
                                      (fw:preferences:set 
                                       'drscheme:font-name
                                       (send font-name get-string-selection))))])
		  (send choice set-string-selection
			(fw:preferences:get 'drscheme:font-name))
		  choice)]
	       [else
		(make-object button%
		  "Set Font..."
		  options-panel
		  (lambda xxx
		    (let ([choice (get-choices-from-user
				   "Select Font Name"
				   "Select Font Name"
				   (get-fixed-faces))])
		      (when choice
			(fw:preferences:set 
                         'drscheme:font-name 
                         (list-ref (get-fixed-faces) (car choice)))))))])]
				      
	    [text (make-object text%)]
	    [ex-panel (make-object horizontal-panel% main)]
	    [msg (make-object message% "Example Text:" ex-panel)]
	    [canvas (make-object editor-canvas% main text)]
	    [update-text
	     (lambda (setting)
	       (let ([language (basis:setting-name setting)])
		 (send text begin-edit-sequence)
		 (send text lock #f)
		 (send text erase)
		 (send text insert
		       (cond
			[(basis:beginner-language? setting) beginner-program]
			[(basis:intermediate-language? setting) intermediate-program]
			[(basis:advanced-language? setting) advanced-program]
			[(regexp-match "MrEd" language) mred-program]
			[(regexp-match "MzScheme" language) mzscheme-program]
                        [else
                         (format "unknown language: ~a" language)]))
		 (send text set-position 0 0)
		 (send text lock #t)
		 (send text end-edit-sequence)))])
		 
       (fw:preferences:add-callback
	drscheme:language:settings-preferences-symbol
	(lambda (p v)
	  (update-text v)))
       (update-text (fw:preferences:get drscheme:language:settings-preferences-symbol))
       (send ex-panel set-alignment 'left 'center)
       (send ex-panel stretchable-height #f)
       (send canvas allow-tab-exit #t)
       (send options-panel stretchable-height #f)
       (send options-panel set-alignment 'center 'top)
       (send text set-style-list (fw:scheme:get-style-list))
       (send text lock #t)
       main)))

  (fw:scheme:add-preferences-panel)
  (fw:preferences:add-general-panel)

  (fw:preferences:add-panel
   "General II"
   (lambda (panel)
     (let* ([main (make-object vertical-panel% panel)]
	    [right-align-in-main
	     (lambda (f)
	       (let ([hp (make-object horizontal-panel% main)])
		 (send hp stretchable-height #f)
		 (begin0 (f hp)
			 (make-object horizontal-panel% hp))))]
	    [make-check-box
	     (lambda (pref-sym string)
	       (right-align-in-main
		(lambda (p)
		  (let ([q (make-object check-box%
			     string
			     p
			     (lambda (checkbox evt)
			       (fw:preferences:set 
				pref-sym 
				(send checkbox get-value))))])
		    (send q set-value (fw:preferences:get pref-sym))))))])
       (make-check-box 'drscheme:execute-warning-once
		       "Only warn once when executions and interactions are not synchronized")
       (make-check-box 'drscheme:enable-backtrace-in-teaching-levels
		       "Enable backtrace bug icon in teaching languages")
       (make-object vertical-panel% main)
       main)))

  ;; add a handler to open .plt files.
  (fw:handler:insert-format-handler 
   "Projects"
   (lambda (filename)
     (and (equal? "plt" (filename-extension filename))
	  (fw:gui-utils:get-choice (format "Install ~a or open for editing?" filename)
				   "Install" "Edit")))
   (lambda (filename)
     (run-installer filename))))
  
