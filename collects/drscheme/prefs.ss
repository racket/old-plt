(unit/sig drscheme:prefs^
  (import mred^
	  [framework : framework^]
	  [basis : userspace:basis^])

  (framework:preferences:set-default 'drscheme:execute-warning-once
				#f
				(lambda (x)
				  (or (eq? x #t)
				      (not x))))

  (include "various-programs.ss")

  (define (get-fixed-faces)
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
      ans))

  (framework:preferences:set-default
   'drscheme:font-name
   (or (send the-font-name-directory get-face-name
	     (send the-font-name-directory
		   find-family-default-font-id
		   'modern))
       (let ([fixed-faces (get-fixed-faces)])
	 (if (null? fixed-faces)
	     #f
	     (car fixed-faces))))
   (lambda (x) (or (string? x) (not x))))

  (framework:preferences:set-default
   'drscheme:font-size
   (let ([b (box 0)])
     (if (get-resource "mred" "defaultFontSize" b)
	 (unbox b)
	 12))
   (lambda (x) (and (number? x) (exact? x) (= x (floor x)))))

  (define (set-font-size size)
    (let* ([scheme-standard (send (framework:scheme:get-style-list)
				  find-named-style "Standard")]
	   [scheme-delta (make-object style-delta%)])
      (framework:preferences:set 'drscheme:font-size size)
      (send scheme-standard get-delta scheme-delta)
      (send scheme-delta set-size-mult 0)
      (send scheme-delta set-size-add size)
      (send scheme-standard set-delta scheme-delta)))

  (define (set-font-name name)
    (let* ([scheme-standard (send (framework:scheme:get-style-list)
				  find-named-style "Standard")]
	   [scheme-delta (make-object style-delta%)])
      (framework:preferences:set 'drscheme:font-name name)
      (send scheme-standard get-delta scheme-delta)
      (send scheme-delta set-delta-face name)
      (send scheme-standard set-delta scheme-delta)))

  (set-font-size (framework:preferences:get 'drscheme:font-size))
  (set-font-name (framework:preferences:get 'drscheme:font-name))

  (framework:preferences:add-panel
   "Font"
   (lambda (panel)
     (let* ([main (make-object vertical-panel% panel)]
	    [options-panel (make-object horizontal-panel% main)]
	    [size (make-object slider% "Font Size" 1 72 options-panel
			       (lambda (size evt)
				 (set-font-size (send size get-value)))
			       (framework:preferences:get 'drscheme:font-size))]

	    [font-name-control
	     (case (system-type)
	       [(windows macos)
		(make-object choice% "Font Name"
			     (get-fixed-faces)
			     options-panel
			     (lambda (font-name evt)
			       (set-font-name
				(send font-name get-string-selection))))]
	       [else
		(make-object button%
		  "Set Font"
		  options-panel
		  (lambda xxx
		    (let ([choice (get-choices-from-user
				   "Select Font Name"
				   "Select Font Name"
				   (get-fixed-faces))])
		      (when choice
			(set-font-name (list-ref (get-fixed-faces) (car choice)))))))])]
				      
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
			[(string=? language "Beginner") beginner-program]
			[(string=? language "Intermediate") intermediate-program]
			[(string=? language "Advanced") advanced-program]
			[(or (string=? language "MrEd")
			     (string=? language "MrEd Debug"))
			 mred-program]
			[(or (string=? language "MzScheme")
			     (string=? language "MzScheme Debug"))
			 mzscheme-program]))
		 (send text set-position 0 0)
		 (send text lock #t)
		 (send text end-edit-sequence)))])
		 
       (framework:preferences:add-callback
	'drscheme:settings
	(lambda (p v)
	  (update-text v)))
       (update-text (framework:preferences:get 'drscheme:settings))
       (send ex-panel set-alignment 'left 'center)
       (send ex-panel stretchable-height #f)
       (send options-panel stretchable-height #f)
       (send options-panel set-alignment 'center 'top)
       (send text set-style-list (framework:scheme:get-style-list))
       (send text lock #t)
       main)))

  (framework:scheme:add-preferences-panel)
  (framework:preferences:add-general-panel)

  (framework:preferences:add-panel
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
			       (framework:preferences:set 
				pref-sym 
				(send checkbox get-value))))])
		    (send q set-value (framework:preferences:get pref-sym))))))])
       (make-check-box 'drscheme:execute-warning-once
		       "Only warn once when executions and interactions are not synchronized")
       (make-object vertical-panel% main)
       main))))
