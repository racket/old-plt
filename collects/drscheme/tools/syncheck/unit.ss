  (unit/sig ()
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [print-convert : mzlib:print-convert^]
	    drscheme:export^
	    [zodiac : zodiac:system^]
            [params : plt:parameters^])
    
    (define add/mult-set
      (lambda (m v)
	(send m set (car v) (cadr v) (caddr v))))
    
    (define add/mult-get
      (lambda (m)
	(let ([b1 (box 0)]
	      [b2 (box 0)]
	      [b3 (box 0)])
	  (send m get b1 b2 b3)
	  (map unbox (list b1 b2 b3)))))

    (define style-delta-get/set
      (list (cons (lambda (x) (send x get-alignment-off))
		  (lambda (x v) (send x set-alignment-off v)))
	    (cons (lambda (x) (send x get-alignment-on))
		  (lambda (x v) (send x set-alignment-on v)))
	    (cons (lambda (x) (add/mult-get (send x get-background-add)))
		  (lambda (x v) (add/mult-set (send x get-background-add) v)))
	    (cons (lambda (x) (add/mult-get (send x get-background-mult)))
		  (lambda (x v) (add/mult-set (send x get-background-mult) v)))
	    (cons (lambda (x) (send x get-face))
		  (lambda (x v) (send x set-face v)))
	    (cons (lambda (x) (send x get-family))
		  (lambda (x v) (send x set-family v)))
	    (cons (lambda (x) (add/mult-get (send x get-foreground-add)))
		  (lambda (x v) (add/mult-set (send x get-foreground-add) v)))
	    (cons (lambda (x) (add/mult-get (send x get-foreground-mult)))
		  (lambda (x v) (add/mult-set (send x get-foreground-mult) v)))
	    (cons (lambda (x) (send x get-size-add))
		  (lambda (x v) (send x set-size-add v)))
	    (cons (lambda (x) (send x get-size-mult))
		  (lambda (x v) (send x set-size-mult v)))
	    (cons (lambda (x) (send x get-style-off))
		  (lambda (x v) (send x set-style-off v)))
	    (cons (lambda (x) (send x get-style-on))
		  (lambda (x v) (send x set-style-on v)))
	    (cons (lambda (x) (send x get-underlined-off))
		  (lambda (x v) (send x set-underlined-off v)))
	    (cons (lambda (x) (send x get-underlined-on))
		  (lambda (x v) (send x set-underlined-on v)))
	    (cons (lambda (x) (send x get-weight-off))
		  (lambda (x v) (send x set-weight-off v)))
	    (cons (lambda (x) (send x get-weight-on))
		  (lambda (x v) (send x set-weight-on v)))))
    
    (define marshall-style
      (lambda (style)
	(map (lambda (fs) ((car fs) style)) style-delta-get/set)))

    (define unmarshall-style
      (lambda (info)
	(let ([style (make-object wx:style-delta%)])
	  (for-each (lambda (fs v) ((cdr fs) style v)) style-delta-get/set info)
	  style)))
    
    
    (define delta-symbols (list 'mzprizm:syntax
				'mzprizm:primitive
				'mzprizm:constant
				'|mzprizm:bound variable|
				'|mzprizm:unbound variable|))

    (for-each (lambda (s) (mred:set-preference-un/marshall
			   s
			   marshall-style
			   unmarshall-style))
	      delta-symbols)

    (mred:set-preference-default 'mzprizm:primitive
				 (let ([s (make-object wx:style-delta%
						       wx:const-change-bold 1)])
				   (send s set-delta-foreground "FIREBRICK")
				   s))
    (mred:set-preference-default 'mzprizm:syntax
				 (let ([s (make-object wx:style-delta%
						       wx:const-change-bold 1)])
				   (send s set-delta-foreground "BLACK")
				   s))
    (mred:set-preference-default 'mzprizm:constant
				 (let ([s (make-object wx:style-delta%
						       wx:const-change-bold 1)])
				   (send s set-delta-foreground "BLUE")
				   s))
    (mred:set-preference-default '|mzprizm:bound variable|
				 (let ([s (make-object wx:style-delta%)])
				   (if (< (wx:display-depth) 8)
				       (send s set-delta wx:const-change-underline 1)
				       (begin (send s set-delta-foreground "DARK GREEN")
					      (send s set-delta wx:const-change-bold 1)))
				   s))
    (mred:set-preference-default '|mzprizm:unbound variable|
				 (let ([s (make-object wx:style-delta%
					    wx:const-change-style wx:const-slant)])
				   (send s set-delta-foreground "RED")
				   s))

    ; takes and edit to set the style in
    ; a symbol naming the style  and a delta to set it to
    (define set-slatex-style
      (lambda (sym delta)
	(let* ([name (symbol->string sym)]
	       [style (send mred:scheme-mode-style-list find-named-style name)])
	  (if (null? style)
	      (send mred:scheme-mode-style-list new-named-style name
		    (send mred:scheme-mode-style-list find-or-create-style
			  (send mred:scheme-mode-style-list
				find-named-style "Standard")
			  delta))
	      (send style set-delta delta)))))

    (for-each set-slatex-style delta-symbols (map mred:get-preference delta-symbols))
	      

    (define wx:const-base -1)

    (define others-string "Other...")

    (define short-colors '("BLACK" "RED" "BLUE" "NAVY" "GREEN" "DARK OLIVE GREEN"
			   "PURPLE" "MAROON" "BROWN" "ORANGE"))

    (define other-colors '("AQUAMARINE" "BLUE VIOLET"
			   "CADET BLUE" "CORAL" "CORNFLOWER BLUE" "CYAN" "DARK GREY"
			   "DARK GREEN" "DARK ORCHID" "DARK SLATE BLUE"
			   "DARK SLATE GREY" "DARK TURQUOISE" "DIM GREY" "FIREBRICK" "FOREST GREEN"
			   "GOLD" "GOLDENROD" "GREY" "GREEN YELLOW" "INDIAN RED"
			   "KHAKI" "LIGHT BLUE" "LIGHT GREY" "LIGHT STEEL BLUE" "LIME GREEN"
			   "MAGENTA" "MEDIUM AQUAMARINE" "MEDIUM BLUE"
			   "MEDIUM FOREST GREEN" "MEDIUM GOLDENROD" "MEDIUM ORCHID"
			   "MEDIUM SEA GREEN" "MEDIUM SLATE BLUE" "MEDIUM SPRING GREEN"
			   "MEDIUM TURQUOISE" "MEDIUM VIOLET RED" "MIDNIGHT BLUE"
			   "ORANGE RED" "ORCHID" "PALE GREEN" "PINK" "PLUM" "RED"
			   "SALMON" "SEA GREEN" "SIENNA" "SKY BLUE" "SLATE BLUE"
			   "SPRING GREEN" "STEEL BLUE" "TAN" "THISTLE" "TURQUOISE" "VIOLET"
			   "VIOLET RED" "WHEAT" "WHITE" "YELLOW" "YELLOW GREEN"))

    (for-each (lambda (x) (send wx:the-colour-database find-colour x))
	      (append short-colors other-colors))

    ;; used for quicker debugging of the preference panel
    '(define test-preference-panel
      (lambda (name f)
	(let ([frame (make-object mred:frame% '() name)])
	  (f frame)
	  (send frame show #t))))

    (mred:add-preference-panel
     "Check Syntax"
     (let ([delta-panel
	    (lambda (sym parent)
	      (let* ([delta (mred:get-preference sym)]
		     [style-name (symbol->string sym)]
		     [h (make-object mred:horizontal-panel% parent
				     -1 -1 -1 -1 wx:const-border)]
		     [c (make-object mred:one-line-canvas% h 30 20 -1 -1 ""
				     (+ wx:const-mcanvas-hide-h-scroll
					wx:const-mcanvas-hide-v-scroll))]
		     [e (make-object (class-asi mred:scheme-mode-edit%
				       (inherit change-style get-style-list)
				       (rename [super-after-insert after-insert])
				       (public
					[after-insert
					 (lambda (pos offset)
					   (super-after-insert pos offset)
					   (let ([style (send (get-style-list) find-named-style style-name)])
					     (change-style style pos (+ pos offset))))])))]
		     [_ (mred:add-preference-callback sym
						      (lambda (sym v)
							(set-slatex-style sym v)
							#t))]
		     [_ (set-slatex-style sym delta)]
		     [make-check
		      (lambda (name on off)
			(let* ([c (lambda (check command)
				    (if (send command checked?)
					(on)
					(off))
				    (mred:set-preference sym delta))]
			       [check (make-object mred:check-box% h c name)])
			  check))]
		     [_ (send c set-media e)]
		     [_ (send* e
			       (insert (substring style-name
						  (string-length "mzprizm:")
						  (string-length style-name)))
			       (set-position 0))]
		     [slant-check
		      (make-check "Slant"
				  (lambda ()
				    (send delta set-style-on wx:const-slant)
				    (send delta set-style-off wx:const-base))
				  (lambda ()
				    (send delta set-style-on wx:const-base)
				    (send delta set-style-off wx:const-slant)))]
		     [bold-check
		      (make-check "Bold"
				  (lambda ()
				    (send delta set-weight-on wx:const-bold)
				    (send delta set-weight-off wx:const-base))
				  (lambda ()
				    (send delta set-weight-on wx:const-base)
				    (send delta set-weight-off wx:const-bold)))]
		     [underline-check
		      (make-check "Underline"
				  (lambda ()
				    (send delta set-underlined-on #t)
				    (send delta set-underlined-off #f))
				  (lambda ()
				    (send delta set-underlined-off #t)
				    (send delta set-underlined-on #f)))]
		     [change-color
		      (lambda (choice command)
			(let ([string (send choice get-string (send command get-selection))])
			  (when (string=? string others-string)
			    (set! string (wx:get-single-choice "Choose a color" "Colors" other-colors)))
			  (unless (null? string)
			    (send delta set-delta-foreground string)
			    (mred:set-preference sym delta))))]
		     [color-choice (if (< (wx:display-depth) 8)
				       #f
				       (make-object mred:choice% h change-color
						    "Color" -1 -1 -1 -1
						    (if (eq? wx:platform 'unix)
							(append short-colors
								(list others-string))
							(append short-colors
								other-colors))))]
		     [style (send (send e get-style-list) find-named-style style-name)])
		(when color-choice
		  (let ([color (send wx:the-colour-database find-name (send style get-foreground))])
		    (cond
		     [(null? color)
		      (send delta set-delta-foreground "BLACK")
		      (mred:set-preference sym delta)
		      (send color-choice set-selection
			    (send color-choice find-string "BLACK"))]
		     [else
		      (let ([c (send color-choice find-string color)])
			(if (= -1 c)
			    (send color-choice set-selection (length short-colors))
			    (send color-choice set-selection c)))])))
		(send slant-check set-value (eq? (send style get-style) wx:const-slant))
		(send bold-check set-value (eq? (send style get-weight) wx:const-bold))
		(send underline-check set-value (send style get-underlined))))])
     (lambda (parent)
       (let ([v (make-object mred:vertical-panel% parent)])
	 (for-each (lambda (sym) (delta-panel sym v))
		   delta-symbols)
	 v))))

    (define improper-for-each
      (lambda (f lis)
	(cond
	  ((null? lis) '())
	  ((pair? lis) (begin (f (car lis))
			      (improper-for-each f (cdr lis))))
	  (else (f lis)))))
    
    (define remove-callbacks null)

    (define color-syntax
      (lambda (reader edit frame)
	(letrec* ([user-param (ivar (ivar frame interactions-edit) param)]
		  [find-string (ivar edit find-string)]
		  [change-style (lambda (s x y)
				  ((ivar edit change-style) s x y))]
		  [get-char (ivar edit get-character)]
		  [find-next-whitespace
		   (lambda (start)
		     (let* ([find (lambda (s)
				    (let ([ans (find-string s 1 start)])
				      (if (= -1 ans)
					  #f
					  ans)))]
			    [mymin
			     (lambda (args)
			       (let loop ([a args]
					  [min #f])
				 (cond
				  [(null? a) min]
				  [else (if (or (not min)
						(and (car a) (< (car a) min)))
					    (loop (cdr a) (car a))
					    (loop (cdr a) min))])))])
		       (mymin (map find (list " " "(" "[" "{"
					      (string #\newline)
					      (string #\tab))))))]
		  [find-next-non-whitespace
		   (lambda (start)
		     (let ([char (get-char start)])
		       (if (member char (list #\newline #\tab #\space))
			   (find-next-non-whitespace (add1 start))
			   start)))]
		  [top-level-varrefs null]
		  [defineds (make-hash-table)]
		  [style-list (send edit get-style-list)]
		  [bound-style (send style-list find-named-style "mzprizm:bound variable")]
		  [unbound-style (send style-list find-named-style "mzprizm:unbound variable")]
		  [primitive-style (send style-list find-named-style "mzprizm:primitive")]
		  [syntax-style (send style-list find-named-style "mzprizm:syntax")]
		  [const-style (send style-list find-named-style "mzprizm:constant")]
		  [color-loop
		   (lambda (zodiac-ast)
		     '(begin (mzlib:pretty-print@:pretty-print zodiac-ast)
			     (newline))
		     (let* ([source-object?
			     (eq? (zodiac:origin-who
				   (zodiac:zodiac-origin zodiac-ast))
				  'source)]
			    [z:start (zodiac:location-offset
				      (zodiac:zodiac-start zodiac-ast))]
			    [z:finish (+ 1
					 (zodiac:location-offset
					  (zodiac:zodiac-finish zodiac-ast)))]
			    [color-syntax
			     (lambda ()
			       (when source-object?
				 (let* ([start (find-next-non-whitespace (add1 z:start))]
					[finish (find-next-whitespace start)])
				   (when (and finish start)
				     (change-style syntax-style start finish)))))]
			    [color
			     (lambda (delta)
			       (when source-object?
				 (let* ([start z:start]
					[finish z:finish])
				   (when (and finish start)
				     (change-style delta start finish)))))]

			    [color-argss
			     (lambda (argss)
			       (for-each (lambda (x) (for-each color-syntax x))
					 argss))])
		       (cond
			[(zodiac:quote-form? zodiac-ast)
			 (color const-style)]
			
			[(zodiac:binding? zodiac-ast) (color bound-style)]
			[(zodiac:bound-varref? zodiac-ast) (color bound-style)]
			
			[(zodiac:top-level-varref? zodiac-ast)
			 (print-struct #t)
			 (when source-object?
			   (set! top-level-varrefs (cons zodiac-ast top-level-varrefs)))]
			
			[(or (zodiac:list? zodiac-ast)
			     (zodiac:improper-list? zodiac-ast)
			     (zodiac:vector? zodiac-ast))
			 (improper-for-each (lambda (x) (color-loop x))
					    (zodiac:read-object zodiac-ast))]
			
			[(zodiac:if-form? zodiac-ast)
			 (color-syntax)
			 (color-loop (zodiac:if-form-test zodiac-ast))
			 (color-loop (zodiac:if-form-then zodiac-ast))
			 (color-loop (zodiac:if-form-else zodiac-ast))]
			
			[(zodiac:set!-form? zodiac-ast)
			 (color-syntax)
			 (color-loop (zodiac:set!-form-var zodiac-ast))
			 (color-loop (zodiac:set!-form-val zodiac-ast))]
			
			[(zodiac:define-values-form? zodiac-ast)
			 (color-syntax)
			 (when source-object?
			   (for-each 
			    (lambda (var)
			      (hash-table-put! defineds (zodiac:varref-var var) #t))
			    (zodiac:define-values-form-vars zodiac-ast)))
			 (for-each color-loop (zodiac:define-values-form-vars zodiac-ast))
			 (color-loop (zodiac:define-values-form-val zodiac-ast))]
			
			[(zodiac:begin-form? zodiac-ast)
			 (color-syntax)
			 (for-each color-loop (zodiac:begin-form-bodies zodiac-ast))]
			[(zodiac:begin0-form? zodiac-ast)
			 (color-syntax)
			 (for-each color-loop (zodiac:begin0-form-bodies zodiac-ast))]
			
			[(zodiac:case-lambda-form? zodiac-ast)
			 (color-syntax)
			 (for-each (lambda (x) (for-each color-loop (zodiac:arglist-vars x)))
				   (zodiac:case-lambda-form-args zodiac-ast))
			 (for-each color-loop (zodiac:case-lambda-form-bodies zodiac-ast))]

			[(zodiac:letrec*-values-form? zodiac-ast)
			 (color-syntax)
			 (for-each (lambda (x) (for-each color-loop x))
				   (zodiac:letrec*-values-form-vars zodiac-ast))
			 (for-each color-loop
				   (zodiac:letrec*-values-form-vals zodiac-ast))
			 (color-loop (zodiac:letrec*-values-form-body zodiac-ast))]

			[(zodiac:let-values-form? zodiac-ast)
			 (color-syntax)
			 (for-each (lambda (x) (for-each color-loop x))
				   (zodiac:let-values-form-vars zodiac-ast))
			 (for-each color-loop
				   (zodiac:let-values-form-vals zodiac-ast))
			 (color-loop (zodiac:let-values-form-body zodiac-ast))]
			
			[(zodiac:app? zodiac-ast)
			 (color-loop (zodiac:app-fun zodiac-ast))
			 (for-each color-loop
				   (zodiac:app-args zodiac-ast))]
			
			;; little grossness hear to make life easier.
			[(zodiac:symbol? zodiac-ast) (color bound-style)]

			[(zodiac:struct-form? zodiac-ast)
			 (color-syntax)
			 (color-loop (zodiac:struct-form-type zodiac-ast))
			 (when (zodiac:struct-form-super zodiac-ast)
			   (color-loop (zodiac:struct-form-super zodiac-ast)))
			 (for-each color-loop
				   (zodiac:struct-form-fields zodiac-ast))]
			
			[else
			 (begin
			   '(display zodiac-ast) 
			   '(newline)
			   '(mred:message-box (string-append 
					       (format "unrecognized syntax ~a"
						       zodiac-ast))
					      "Unrecognized Syntax")
			   (void))])))])
	  
	  ; reset all of the buffer to the default style
	  (let* ([list (send edit get-style-list)]
		 [style (send list find-named-style "Standard")])
	    (if (null? style)
		(printf "Warning: couldn't find Standard style~n")
		(change-style style 0 (send edit last-position))))

	  ; read expand and color
	  (let read-loop ()
	    '(printf "reading   ")
	    (let* ([time (lambda (x) x)]
		   [expr (time (reader))])
	      (unless (zodiac:eof? expr)
		'(printf "expanding ")
		(let ([expanded (time (zodiac:scheme-expand expr user-param))])
		  '(printf "coloring  ")
		  (time (color-loop expanded))
		  (read-loop)))))
	  
	  ; color the top-level varrefs
	  (let ([user-defined?
		 (lambda (s)
		   (with-parameterization (ivar (ivar frame
						      interactions-edit)
						param)
		     (lambda ()
		       (eval `(defined? ',s)))))])
	    (for-each (lambda (var)
			(let ([id (zodiac:varref-var var)])
			  (change-style
			   (cond
			     [(hash-table-get defineds id (lambda () #f))
			      bound-style]
			     [(user-defined? id) primitive-style]
			     [else unbound-style])
			   (zodiac:location-offset (zodiac:zodiac-start var))
			   (add1 (zodiac:location-offset (zodiac:zodiac-finish var))))))
		      top-level-varrefs)))))

    (define transform
      (lambda (frame edit)
	(let ([console-edit (ivar frame interactions-edit)])
	  (let/ec k
	    (with-handlers
		([void ; should be: zodiac:interface:zodiac-exn?
		  (lambda (exn)
		    (send console-edit insert-prompt)
		    (k (void)))])
	      (let* ([thunk (mred:read-snips/chars-from-buffer edit)]
		     [reader (zodiac:read thunk (zodiac:make-location 1 1 0 edit))])
		(color-syntax reader edit frame)))))))
    
    (define button-callback
      (lambda (frame)
	(let ([edit (ivar frame definitions-edit)])
	  (dynamic-wind
	   (lambda ()
	     (wx:begin-busy-cursor)
	     (send edit set-styles-fixed #f)
	     (send edit begin-edit-sequence #f))
	   (lambda ()
	     (transform frame edit))
	   (lambda ()
	     (send edit end-edit-sequence)
	     (send edit set-styles-fixed #t)
	     (wx:end-busy-cursor))))))

    (define to-each-frame 
      (lambda (frame)
	  ;; install a button in each unit frame
	(when (is-a? frame drscheme:unit:frame%)
	  (let* ([panel (ivar frame button-panel)]
		 [button (make-object mred:button% panel
				      (lambda (button evt)
					(button-callback frame))
				      (drscheme:unit:make-bitmap
				       (build-path mred:plt-home-directory
						   "icons"
						   "syncheck.gif")
				       (mred:debug:if 'mrslatex
						      "CS"
						      "Check Syntax")))])
	    
	    (send (ivar frame definitions-edit) set-styles-fixed #t)
	    (send panel change-children
		  (lambda (l)
		    (cons button
			  (mzlib:function@:remove button l))))))))
    
    (send drscheme:frame:group for-each-frame to-each-frame)
    button-callback)

