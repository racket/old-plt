
;; use some kind of signature predicate to get the primitive
;; color right. 
;; Ie allow turtle primitives to be primitive color, but if the
;; user defines something in the bottom window, and then does
;; syntax checker, that should be unbound.

  (unit/sig ()
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [print-convert : mzlib:print-convert^]
	    [drscheme : drscheme:export^]
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
		  (send color-choice stretchable-in-x #f)
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
    
    (define-structure (graphic pos* locs->thunks draw-fn click-fn))
    (define-structure (arrow start-pos-left start-pos-right end-pos-left end-pos-right
			     start-x start-y end-x end-y
			     tacked? painted?))

    (define TACKED-BRUSH (send wx:the-brush-list find-or-create-brush "BLUE" wx:const-solid))
    (define UNTACKED-BRUSH (send wx:the-brush-list find-or-create-brush "WHITE" wx:const-solid))
    (define PEN (send wx:the-pen-list find-or-create-pen (make-object wx:colour% "BLUE") 1 wx:const-solid))

    (class null args (sequence (apply super-init args)))

    (define graphics:media-edit%
      (let* ([pi (* 2 (asin 1))]
	     [arrow-head-angle (/ pi 8)]
	     [cos-angle (cos arrow-head-angle)]
	     [sin-angle (sin arrow-head-angle)]
	     [arrow-head-size 10]
	     [arrow-root-radius 3.5]
	     [cursor-arrow (make-object wx:cursor% wx:const-cursor-arrow)])
	(class-asi (drscheme:parameters:current-definitions-edit%)
	  (inherit set-cursor get-admin invalidate-bitmap-cache set-position
		   position-location
		   get-canvas get-frame last-position dc-location-to-buffer-location
		   find-position begin-edit-sequence end-edit-sequence)
	  (rename
	   [super-after-insert after-insert]
	   [super-after-delete after-delete]
	   [super-on-paint on-paint]
	   [super-on-event on-event])
	  (private
	    [arrow-vector #f]
	    [cursor-location #f]
	    [find-poss
	     (lambda (left-pos right-pos)
	       (let ([xlb (box 0)]
		     [ylb (box 0)]
		     [xrb (box 0)]
		     [yrb (box 0)])
		 (position-location left-pos xlb ylb #t)
		 (position-location right-pos xrb yrb #f)
		 (values (/ (+ (unbox xlb) (unbox xrb)) 2)
			 (/ (+ (unbox ylb) (unbox yrb)) 2))))]
	    [update-poss
	     (lambda (arrow)
	       (let-values ([(start-x start-y) (find-poss (arrow-start-pos-left arrow)
							  (arrow-start-pos-right arrow))]
			    [(end-x end-y) (find-poss (arrow-end-pos-left arrow)
						      (arrow-end-pos-right arrow))])
		 (set-arrow-start-x! arrow start-x)
		 (set-arrow-start-y! arrow start-y)
		 (set-arrow-end-x! arrow end-x)
		 (set-arrow-end-y! arrow end-y)))])

	  (public
	    [syncheck:init-arrows
	     (lambda ()
	       (set! arrow-vector
		     (make-vector (add1 (last-position)) null)))]
	    [syncheck:clear-arrows
	     (lambda () 
	       (set! arrow-vector #f)
	       (set! cursor-location #f)
	       (invalidate-bitmap-cache))]
	    [syncheck:add-arrow
	     (lambda (end-pos-left end-pos-right
				   start-pos-left start-pos-right id-name rename)
	       (let ([arrow (make-arrow start-pos-left start-pos-right
					end-pos-left end-pos-right
					0 0 0 0 #f #f)])
		 (let loop ([p start-pos-left])
		   (when (<= p start-pos-right)
		     (vector-set! arrow-vector p
				  (cons arrow (vector-ref arrow-vector p)))
		     (loop (add1 p))))))])
	  (public
	    [after-delete
	     (lambda (start len)
	       (super-after-delete start len)
	       (syncheck:clear-arrows))]
	    
	    [after-insert
	     (lambda (start len)
	       (super-after-insert start len)
	       (syncheck:clear-arrows))]
	    
	    [on-paint
	     (lambda (before dc left top right bottom dx dy draw-caret)
	       (super-on-paint before dc left top right bottom dx dy draw-caret)
	       (when (and arrow-vector
			  cursor-location
			  (not before))
		 (time
		  (let ([draw-arrow
			 (lambda (arrow)
			   (let* ([start-x (arrow-start-x arrow)]
				  [start-y (arrow-start-y arrow)]
				  [end-x   (arrow-end-x arrow)]
				  [end-y   (arrow-end-y arrow)]
				  [delta   0]
				  [ofs-x   (- start-x end-x)]
				  [ofs-y   (- start-y end-y)]
				  [len     (sqrt (+ (* ofs-x ofs-x) (* ofs-y ofs-y)))]
				  [ofs-x   (/ ofs-x len)]
				  [ofs-y   (/ ofs-y len)]
				  [head-x  (* ofs-x arrow-head-size)]
				  [head-y  (* ofs-y arrow-head-size)]
				  [end-x   (+ end-x (* ofs-x delta))]
				  [end-y   (+ end-y (* ofs-y delta))]
				  [pt1     (make-object wx:point% end-x end-y)]
				  [pt2     (make-object 
					       wx:point%
					     (+ end-x (* cos-angle head-x) 
						(* sin-angle head-y))
					     (+ end-y (- (* sin-angle head-x))
						(* cos-angle head-y)))]
				  [pt3     (make-object 
					       wx:point%
					     (+ end-x (* cos-angle head-x)
						(- (* sin-angle head-y)))
					     (+ end-y (* sin-angle head-x)
						(* cos-angle head-y)))]
				  [pts (list pt1 pt2 pt3)])
			     (send dc set-brush TACKED-BRUSH)
			     ;; (send dc set-logical-function wx:const-or)
			     (send dc draw-line
				   (+ start-x dx) (+ start-y dy)
				   (+ end-x dx) (+ end-y dy))
			     (send dc draw-polygon pts dx dy)
			     (send dc draw-ellipse 
				   (- (+ start-x dx) arrow-root-radius)
				   (- (+ start-y dy) arrow-root-radius)
				   (* 2 arrow-root-radius)
				   (* 2 arrow-root-radius))))])
		    (let ([old-brush (send dc get-brush)]
			  [old-pen   (send dc get-pen)]
			  [old-logfn (send dc get-logical-function)])
		      (send dc set-pen PEN)
		      (for-each draw-arrow (vector-ref arrow-vector cursor-location))
		      (send dc set-brush old-brush)
		      (send dc set-pen old-pen)                    
		      (send dc set-logical-function old-logfn))))
		 (printf "on-paint~n")))]
	    [on-event
	     (lambda (event)
	       (when arrow-vector
		 (time
		  (cond
		   [(send event moving?)
		    (let*-values ([(event-x event-y)
				   (values (send event get-x)
					   (send event get-y))]
				  [(x y) (dc-location-to-buffer-location
					  event-x event-y)]
				  [(pos) (find-position x y)])
		      (unless (and cursor-location
				   (= pos cursor-location))
			(set! cursor-location pos)
			(for-each update-poss (vector-ref arrow-vector cursor-location))
			(time (invalidate-bitmap-cache))
			(printf "invalidate bitmap cache~n"))
		      #f)]
		   [(and #f (send event button-down? 3))
		    '(and this-time
			  (let* ([canvas (get-canvas)]
				 [JUMP-ID 1]
				 [STICK-ID 2]
				 [RENAME-ID 3]
				 [callback (lambda (menu evt)
					     (let ([id (send evt get-command-int)])
					       (cond
						[(= id STICK-ID)
						 (set! perm-arrow-on? (not perm-arrow-on?))
						 (set! tmp-arrow-on? #f)
						 (send brush set-colour
						       (if perm-arrow-on?
							   color-background
							   color-highlight))
						 (invalidate-bitmap-cache)]
						[(= id JUMP-ID)
						 (set-position end-pos-left end-pos-right)]
						[(= id RENAME-ID)
						 (set! tmp-arrow-on? #f)
						 (rename (mred:get-text-from-user
							  (format "Rename ~a to:" id-name)
							  "Rename Identifier"
							  (format "~a" id-name)))
						 (invalidate-bitmap-cache)])))]
				 [menu (make-object wx:menu% null callback)])
			    (send menu append STICK-ID (if perm-arrow-on?
							   "Untack Arrow"
							   "Tack Arrow"))
			    (send menu append JUMP-ID "Jump")
			    (send menu append RENAME-ID "Rename")
			    (send canvas popup-menu menu
				  (send event get-x)
				  (send event get-y))))]
		   [else #f]))
		 (printf "on-event~n")))]))))

    (define new%
      (class (drscheme:parameters:current-frame%) args
	(inherit button-panel definitions-edit interactions-edit)
	(sequence (apply super-init args))
	(private
	  [button-callback
	   (lambda ()
	     (letrec* ([user-param (ivar interactions-edit param)]
		       [add-arrow (ivar definitions-edit syncheck:add-arrow)]
		       [find-string (ivar definitions-edit find-string)]
		       [change-style (lambda (s x y)
				       ((ivar definitions-edit change-style) s x y))]
		       [get-char (ivar definitions-edit get-character)]
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
		       [local-bindings (make-hash-table)]
		       [style-list (send definitions-edit get-style-list)]
		       [bound-style (send style-list find-named-style "mzprizm:bound variable")]
		       [unbound-style (send style-list find-named-style "mzprizm:unbound variable")]
		       [primitive-style (send style-list find-named-style "mzprizm:primitive")]
		       [syntax-style (send style-list find-named-style "mzprizm:syntax")]
		       [const-style (send style-list find-named-style "mzprizm:constant")]
		       [rename-bindings
			(lambda (occurrances new-name)
			  (send definitions-edit begin-edit-sequence)
			  (let ([sorted (mzlib:function@:quicksort
					 occurrances
					 (lambda (x y)
					   (<= (zodiac:location-offset (zodiac:zodiac-start y))
					       (zodiac:location-offset (zodiac:zodiac-start x)))))]
				[rename-one
				 (lambda (z)
				   (send definitions-edit insert new-name
					 (zodiac:location-offset (zodiac:zodiac-start z))
					 (add1 (zodiac:location-offset (zodiac:zodiac-finish z)))))])
			    (for-each rename-one sorted))
			  (button-callback)
			  (send definitions-edit end-edit-sequence))]
		       [color-loop
			(lambda (zodiac-ast)
			  (let* ([source-object?
				  (eq? (zodiac:origin-who
					(zodiac:zodiac-origin zodiac-ast))
				       'source)]
				 [z:start (zodiac:location-offset (zodiac:zodiac-start zodiac-ast))]
				 [z:finish (+ 1
					      (zodiac:location-offset
					       (zodiac:zodiac-finish zodiac-ast)))]
				 [search-for-orig-syntax
				  (lambda ()
				    (let loop ([zobj zodiac-ast])
				      (let* ([origin (zodiac:zodiac-origin zobj)]
					     [who (zodiac:origin-who origin)])
					(cond
					 [(eq? who 'macro) (loop (zodiac:origin-how origin))]
					 [(and (eq? who 'source) (zodiac:symbol? zobj))
					  (change-style syntax-style
							(zodiac:location-offset (zodiac:zodiac-start zobj))
							(add1 (zodiac:location-offset 
							       (zodiac:zodiac-finish zobj))))]
					 [else (void)]))))]
				 [color-syntax
				  (lambda ()
				    (if source-object?
					(let* ([start (find-next-non-whitespace (add1 z:start))]
					       [finish (find-next-whitespace start)])
					  (when (and finish start)
					    (change-style syntax-style start finish)))
					(search-for-orig-syntax)))]
				 
				 [color
				  (lambda (delta)
				    (when (and source-object? z:finish z:start)
				      (change-style delta z:start z:finish)))]
				 
				 [color-argss
				  (lambda (argss)
				    (for-each (lambda (x) (for-each color-syntax x))
					      argss))])
			    (cond
			      [(zodiac:quote-form? zodiac-ast)
			       (search-for-orig-syntax)
			       (color const-style)]			      
			      [(zodiac:binding? zodiac-ast) (color bound-style)]
			      [(zodiac:bound-varref? zodiac-ast)
			       (when source-object?
				 (let* ([binding (zodiac:bound-varref-binding zodiac-ast)]
					[user-name (zodiac:binding-orig-name binding)]
					[gen-name (zodiac:varref-var zodiac-ast)]
					[start (zodiac:location-offset (zodiac:zodiac-start binding))]
					[finish (add1 (zodiac:location-offset (zodiac:zodiac-finish binding)))]
					[rename (lambda (new-name)
						  (when new-name
						    (rename-bindings
						     (cons binding
							   (hash-table-get local-bindings
									   gen-name (lambda () null)))
						     (format "~a" (string->symbol new-name)))))])
				   (hash-table-put!
				    local-bindings
				    gen-name
				    (cons zodiac-ast
					  (hash-table-get local-bindings
							  gen-name (lambda () null))))
				   (add-arrow z:start z:finish start finish user-name rename)
				   (add-arrow start finish z:start z:finish user-name rename))
				 (color bound-style))]
			      
			      [(zodiac:top-level-varref? zodiac-ast)
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
			       (for-each 
				(lambda (var) (hash-table-put! defineds (zodiac:varref-var var) var))
				(zodiac:define-values-form-vars zodiac-ast))
			       (for-each (lambda (var)
					   (when (eq? 'source (zodiac:origin-who (zodiac:zodiac-origin var)))
					     (change-style bound-style 
							   (zodiac:location-offset (zodiac:zodiac-start var))
							   (add1 (zodiac:location-offset (zodiac:zodiac-finish var))))))
					 (zodiac:define-values-form-vars zodiac-ast))
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
			       (search-for-orig-syntax)
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
	       (dynamic-wind
		(lambda ()
		  (wx:begin-busy-cursor)
		  (send definitions-edit set-styles-fixed #f)
		  (send definitions-edit begin-edit-sequence #f))
		(lambda ()
		  ; reset all of the buffer to the default style
		  ; and clear out arrows
		  (let* ([list (send definitions-edit get-style-list)]
			 [style (send list find-named-style "Standard")])
		    (send* definitions-edit
			   (syncheck:clear-arrows)
			   (syncheck:init-arrows))
		    (if (null? style)
			(printf "Warning: couldn't find Standard style~n")
			(change-style style 0 (send definitions-edit last-position))))

		  ;; color each exp
		  (send interactions-edit process-edit/zodiac
			definitions-edit
			(lambda (expr recur)
			  (cond
			    [(eq? expr #t) (send interactions-edit insert-prompt)]
			    [(not expr) (void)]
			    [else
			     (color-loop expr)
			     (recur)]))
			0
			(send definitions-edit last-position)
			#f)
		  ; color the top-level varrefs
		  (let ([built-in?
			 (lambda (s)
			   (with-parameterization (ivar interactions-edit param)
			     (lambda ()
			       (built-in-name s))))])
		    (for-each (lambda (var)
				(let ([id (zodiac:varref-var var)])
				  (change-style
				   (cond
				     [(hash-table-get defineds id (lambda () #f)) bound-style]
				     [(built-in? id) primitive-style]
				     [else unbound-style])
				   (zodiac:location-offset (zodiac:zodiac-start var))
				   (add1 (zodiac:location-offset (zodiac:zodiac-finish var))))))
			      top-level-varrefs)))
	       (lambda ()
		 (send definitions-edit end-edit-sequence)
		 (send definitions-edit set-styles-fixed #t)
		 (wx:end-busy-cursor)))))]
	  [button (make-object mred:button% button-panel
			       (lambda (button evt) (button-callback))
			       (drscheme:unit:make-bitmap
				(build-path mred:constants:plt-home-directory
					    "icons"
					    "syncheck.bmp")
				(mred:debug:if 'mrslatex
					       "CS"
					       "Check Syntax")))])
	(sequence
	  (send definitions-edit set-styles-fixed #t)
	  (send button-panel change-children
		(lambda (l)
		  (cons button
			(mzlib:function@:remove button l)))))))

    (drscheme:parameters:current-definitions-edit% graphics:media-edit%)
    (drscheme:parameters:current-frame% new%))

