
(unit/sig ()
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [print-convert : mzlib:print-convert^]
	  [drscheme : drscheme:export^]
	  [zodiac : zodiac:system^]
          [prims : drscheme:syncheck:prims^]
	  [drscheme:arrow : drscheme:draw-arrow^])
  
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
      (let ([style (make-object mred:style-delta%)])
	(for-each (lambda (fs v) ((cdr fs) style v)) style-delta-get/set info)
	style)))
  
  (define default-style-assoc-list 
    (map (lambda (x) 
           (cons
            (string->symbol (string-append "drscheme:check-syntax:" (symbol->string (car x))))
            (cdr x)))
         (require-library "default-style.ss" "drscheme" "tools" "syncheck")))
  
  (define delta-symbols (map car default-style-assoc-list))
  
  (for-each 
   (lambda (s) 
     (fw:preferences:set-un/marshall s marshall-style unmarshall-style))
   delta-symbols)
  
  (let ([set-default
         (lambda (default)
           (let ([sym (car default)]
                 [color (cadr default)]
                 [style-settings (cddr default)])
             (fw:preferences:set-default
              sym
              (let ([s (make-object mred:style-delta%)])
                (send s set-delta-foreground (if (string? color)
                                                 color
                                                 (make-object mred:color%
                                                   (car color)
                                                   (cadr color)
                                                   (caddr color))))
                (for-each (lambda (sym)
                            (case sym
                              [(bold) (send s set-delta 'change-bold)]
                              [(underline) (send s set-delta 'change-underline)]
                              [(italic) (send s set-delta 'change-slant)]))
                          style-settings)
                s)
              (lambda (x)
                (is-a? x mred:style-delta%)))))])
    (for-each set-default default-style-assoc-list))
  
  ; takes and edit to set the style in
  ; a symbol naming the style  and a delta to set it to
  (define set-slatex-style
    (lambda (sym delta)
      (let* ([style-list (fw:scheme:get-style-list)]
	     [name (symbol->string sym)]
	     [style (send style-list find-named-style name)])
	(if style
	    (send style set-delta delta)
	    (send style-list new-named-style name
		  (send style-list find-or-create-style
			(send style-list
			      find-named-style "Standard")
			delta))))))
  
  (for-each set-slatex-style delta-symbols (map fw:preferences:get delta-symbols))
  
  ;; used for quicker debugging of the preference panel
  '(define test-preference-panel
     (lambda (name f)
       (let ([frame (make-object mred:frame% name)])
	 (f frame)
	 (send frame show #t))))
  
  (define simple-scheme-text% (fw:scheme:text-mixin fw:text:basic%))
  
  (fw:preferences:add-panel
   "Check Syntax"
   (let ([delta-panel
	  (lambda (sym parent)
	    (let* ([delta (fw:preferences:get sym)]
		   [style-name (symbol->string sym)]
		   [h (make-object mred:horizontal-panel% parent '(border))]
		   [c (make-object mred:editor-canvas% h
				   #f
				   (list 'hide-hscroll
					 'hide-vscroll))]
		   [_ (send c set-line-count 1)]
		   [_ (send c allow-tab-exit #t)]
		   [e (make-object (class100 simple-scheme-text% ()
				     (inherit change-style get-style-list)
				     (rename [super-after-insert after-insert])
				     (override
				      [after-insert
				       (lambda (pos offset)
					 (super-after-insert pos offset)
					 (let ([style (send (get-style-list) find-named-style style-name)])
					   (change-style style pos (+ pos offset))))])
                                     (sequence (super-init))))]
		   [_ (fw:preferences:add-callback sym
						   (lambda (sym v)
						     (set-slatex-style sym v)
						     #t))]
		   [_ (set-slatex-style sym delta)]
		   [make-check
		    (lambda (name on off)
		      (let* ([c (lambda (check command)
				  (if (send check get-value)
				      (on)
				      (off))
				  (fw:preferences:set sym delta))]
			     [check (make-object mred:check-box% name h c)])
			check))]
		   [_ (send c set-editor e)]
		   [short-style-name (substring style-name
						(string-length "drscheme:check-syntax:")
						(string-length style-name))]
		   [_ (send* e
			(insert short-style-name)
			(set-position 0))]
		   [slant-check
		    (make-check "Slant"
				(lambda ()
				  (send delta set-style-on 'slant)
				  (send delta set-style-off 'base))
				(lambda ()
				  (send delta set-style-on 'base)
				  (send delta set-style-off 'slant)))]
		   [bold-check
		    (make-check "Bold"
				(lambda ()
				  (send delta set-weight-on 'bold)
				  (send delta set-weight-off 'base))
				(lambda ()
				  (send delta set-weight-on 'base)
				  (send delta set-weight-off 'bold)))]
		   [underline-check
		    (make-check "Underline"
				(lambda ()
				  (send delta set-underlined-on #t)
				  (send delta set-underlined-off #f))
				(lambda ()
				  (send delta set-underlined-off #t)
				  (send delta set-underlined-on #f)))]
		   [color-button
		    (and (>= (mred:get-display-depth) 8)
			 (make-object mred:button%
				      "Change Color"
				      h
				      (lambda (color-button evt)
					(let* ([add (send delta get-foreground-add)]
					       [color (make-object mred:color%
							(send add get-r)
							(send add get-g)
							(send add get-b))]
					       [users-choice
						(mred:get-color-from-user
						 (format "Choose a color for ~a~a"
							 short-style-name
							 (if (string=? "syntax" short-style-name)
							     ""
							     "s"))
						 (send color-button get-top-level-window)
						 color)])
					  (when users-choice
					    (send delta set-delta-foreground users-choice)
					    (fw:preferences:set sym delta))))))]
		   [style (send (send e get-style-list) find-named-style style-name)])
	      (send slant-check set-value (eq? (send style get-style) 'slant))
	      (send bold-check set-value (eq? (send style get-weight) 'bold))
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
  
  (define-struct graphic (pos* locs->thunks draw-fn click-fn))
  (define-struct arrow (start-pos-left start-pos-right end-pos-left end-pos-right
				       start-x start-y end-x end-y
				       id-name rename))
  
  (define tacked-brush (send mred:the-brush-list find-or-create-brush "BLUE" 'solid))
  (define untacked-brush (send mred:the-brush-list find-or-create-brush "WHITE" 'solid))
  (define the-pen (send mred:the-pen-list find-or-create-pen "BLUE" 1 'solid))
  
  (define make-graphics-text%
    (lambda (super%)
      (let* ([cursor-arrow (make-object mred:cursor% 'arrow)])
	(class100 super% args
	  (inherit set-cursor get-admin invalidate-bitmap-cache set-position
		   position-location
		   get-canvas last-position dc-location-to-editor-location
		   find-position begin-edit-sequence end-edit-sequence)
	  
	  (rename
	   [super-after-insert after-insert]
	   [super-after-delete after-delete]
	   [super-on-paint on-paint]
	   [super-on-local-event on-local-event])
	  (private

            ;; (union #f (vector (union (-> (union #f menu)) (listof arrow))))
	    [arrow-vector #f]

	    [tacked-hash-table (make-hash-table)]
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
	       (set! tacked-hash-table (make-hash-table))
	       (set! arrow-vector (make-vector (add1 (last-position)) null)))]
	    [syncheck:clear-arrows
	     (lambda () 
	       (when (or arrow-vector cursor-location)
		 (set! arrow-vector #f)
		 (set! cursor-location #f)
		 (invalidate-bitmap-cache)))]
	    [syncheck:add-menu
             (lambda (start-pos end-pos make-menu)
               (when (and (<= 0 start-pos end-pos (last-position)))
                 (let loop ([p start-pos])
                   (when (<= p end-pos)
                     (vector-set! arrow-vector p make-menu)
                     (loop (+ p 1))))))]
            [syncheck:add-arrow
	     (lambda (start-pos-left start-pos-right end-pos-left end-pos-right id-name rename)
	       (let* ([arrow (make-arrow start-pos-left start-pos-right
					 end-pos-left end-pos-right
					 0 0 0 0
					 id-name rename)]
		      [add-to-range
		       (lambda (start end)
			 (let loop ([p start])
			   (when (<= p end)
                             (let ([r (vector-ref arrow-vector p)])
                               (vector-set! arrow-vector p (cons arrow (if (list? r) r null))))
			     (loop (add1 p)))))])
		 (add-to-range start-pos-left start-pos-right)
		 (add-to-range end-pos-left end-pos-right)))])
	  (inherit get-top-level-window)
	  (override
	   [after-delete
	    (lambda (start len)
	      (super-after-delete start len)
	      (syncheck:clear-arrows))]
	   
	   [after-insert
	    (lambda (start len)
	      (super-after-insert start len)
	      (syncheck:clear-arrows))]

	   [on-change
	    (lambda ()
	      (when arrow-vector
		(let loop ([n (vector-length arrow-vector)])
		  (unless (zero? n)
                    (let ([ele (vector-ref arrow-vector (- n 1))])
                      (when (pair? ele)
                        (for-each update-poss ele)))
		    (loop (- n 1))))
		(invalidate-bitmap-cache)))]

	   [on-paint
	    (lambda (before dc left top right bottom dx dy draw-caret)
	      (super-on-paint before dc left top right bottom dx dy draw-caret)
	      (when (and arrow-vector
			 cursor-location
			 (not before))
		(let ([draw-arrow2
		       (lambda (arrow)
			 (let ([start-x (arrow-start-x arrow)]
			       [start-y (arrow-start-y arrow)]
			       [end-x   (arrow-end-x arrow)]
			       [end-y   (arrow-end-y arrow)])
			   (drscheme:arrow:draw-arrow dc start-x start-y end-x end-y dx dy)))]
		      [old-brush (send dc get-brush)]
		      [old-pen   (send dc get-pen)])
		  (send dc set-pen the-pen)
		  (send dc set-brush tacked-brush)
		  (hash-table-for-each tacked-hash-table
				       (lambda (arrow v) 
					 (when v 
					   (draw-arrow2 arrow))))
		  (send dc set-brush untacked-brush)
		  (let ([ele (vector-ref arrow-vector cursor-location)])
		    (when (pair? ele)
		      (for-each draw-arrow2 ele)))
		  (send dc set-brush old-brush)
		  (send dc set-pen old-pen))))]

	   [on-local-event
	    (let ([get-pos
		   (lambda (event)
		     (let*-values ([(event-x event-y)
				    (values (send event get-x)
					    (send event get-y))]
				   [(x y) (dc-location-to-editor-location
					   event-x event-y)])
		       (find-position x y)))])
	      (lambda (event)
		(if arrow-vector
		    (cond
		     [(send event moving?)
		      (let ([pos (get-pos event)])
                        (unless (and cursor-location
                                     (= pos cursor-location))
                          (let ([old-pos pos])
                            (set! cursor-location pos)
                            (let ([ele (vector-ref arrow-vector cursor-location)])
                              (when (pair? ele)
                                (for-each update-poss ele)))
                            (invalidate-bitmap-cache))))]
		     [(send event button-down? 'right)
		      (let* ([pos (get-pos event)]
			     [arrows (vector-ref arrow-vector pos)])
			(cond
                          [(null? arrows)
                           (super-on-local-event event)]
                          [else
                           (let ([menu 
                                  (cond
                                    [(procedure? arrows) (arrows)]
                                    [else
                                     (let* ([menu (make-object mred:popup-menu% #f)]
                                            [stick-item
                                             (make-object mred:menu-item%
                                               "Tack/Untack Arrow"
                                               menu
                                               (lambda (item evt)
                                                 (for-each 
                                                  (lambda (arrow)
                                                    (hash-table-put! tacked-hash-table 
                                                                     arrow 
                                                                     (not (hash-table-get
                                                                           tacked-hash-table
                                                                           arrow
                                                                           (lambda () #f)))))
                                                  arrows)
                                                 (invalidate-bitmap-cache)))]
                                            [jump-item
                                             (make-object mred:menu-item%
                                               "Jump"
                                               menu
                                               (lambda (item evt)
                                                 (unless (null? arrows)
                                                   (let* ([arrow (car arrows)]
                                                          [start-pos-left (arrow-start-pos-left arrow)]
                                                          [start-pos-right (arrow-start-pos-right arrow)]
                                                          [end-pos-left (arrow-end-pos-left arrow)]
                                                          [end-pos-right (arrow-end-pos-right arrow)])
                                                     (if (<= start-pos-left pos start-pos-right)
                                                         (set-position end-pos-left end-pos-right)
                                                         (set-position start-pos-left start-pos-right))))))]
                                            [rename-item
                                             (make-object mred:menu-item%
                                               "Rename"
                                               menu
                                               (lambda (item evt)
                                                 (unless (null? arrows)
                                                   (let* ([arrow (car arrows)]
                                                          [id-name (arrow-id-name arrow)]
                                                          [new-id 
                                                           (fw:keymap:call/text-keymap-initializer
                                                            (lambda ()
                                                              (mred:get-text-from-user
                                                               "Rename Identifier"
                                                               (format "Rename ~a to:" id-name)
                                                               #f
                                                               (format "~a" id-name))))])
                                                     ((arrow-rename arrow) new-id))
                                                   (invalidate-bitmap-cache)
                                                   (send (get-top-level-window)
                                                         syncheck:button-callback))))])
                                       menu)])])
                             (if menu
                                 (send (get-canvas) popup-menu menu
                                       (+ 1 (inexact->exact (floor (send event get-x))))
                                       (+ 1 (inexact->exact (floor (send event get-y)))))
                                 (super-on-local-event event)))]))]
                     [else (super-on-local-event event)])
		    (super-on-local-event event))))]))
        (sequence (apply super-init args)))))
  
  (define (make-clear-text super%)
    (class super% args
      (sequence
	(apply super-init args))))
	
  
  (define syncheck-bitmap
    (drscheme:unit:make-bitmap
     "Check Syntax"
     (build-path (collection-path "icons") "syncheck.bmp")))
  
  (define (make-new-unit-frame% super%)
    (class super% args
      (inherit button-panel definitions-canvas definitions-text interactions-text
	       get-directory)
      
      (rename [super-disable-evaluation disable-evaluation]
              [super-enable-evaluation enable-evaluation])
      (private
        [button-visible? #t])
      (override
        [enable-evaluation
         (lambda ()
           (send check-syntax-button enable #t)
           (super-enable-evaluation))]
        [disable-evaluation
         (lambda ()
           (send check-syntax-button enable #f)
           (super-disable-evaluation))])
      (private
        [clear-highlighting
         (lambda ()
           (send definitions-text syncheck:clear-arrows)
           (let* ([list (send definitions-text get-style-list)]
                  [style (send list find-named-style "Standard")])
             (if style
                 (send definitions-text
                       change-style style 0 (send definitions-text last-position))
                 (begin
                   (set! clear-highlighting (lambda () (send definitions-text syncheck:clear-arrows)))
                   (mred:message-box "DrScheme: Syntax Check" "Warning: couldn't find Standard style")))))])
      (public
        [syncheck:clear-highlighting
         (lambda ()
           (hide-error-report-window)
           (clear-highlighting))]
        [syncheck:enable-checking
         (lambda (on?)
           (set! button-visible? on?)
           (when (object? check-syntax-button)
             (send check-syntax-button show on?)))])
      
      (rename [super-make-root-area-container make-root-area-container])
      (private
	[rest-panel 'uninitialized-root]
	[super-root 'uninitialized-super-root]
        [docs-panel 'uninitialized-docs-panel]
        [docs-messages 'uninitialized-docs-lines])
      (override
	[make-root-area-container
	 (lambda (% parent)
	   (let* ([s-root (super-make-root-area-container
			   mred:vertical-panel%
			   parent)]
		  [r-root (make-object % s-root)])
	     (set! super-root s-root)
	     (set! rest-panel r-root)
             (set! docs-panel (make-object mred:vertical-panel% super-root))
	     (set! docs-messages null)
             (send docs-panel set-label-font
                   (send mred:the-font-list find-or-create-font 
                         (send (send docs-panel get-label-font) get-point-size)
                         'modern 'normal 'normal #f))
             (send docs-panel stretchable-height #f)
             (send super-root change-children (lambda (l) (list rest-panel)))
	     r-root))])
      (private
        [docs-messages-shown? #f]
        [hide-docs-messages
         (lambda ()
           (when docs-messages-shown?
             (set! docs-messages-shown? #f)
             (send super-root change-children
                   (lambda (l)
                     (list rest-panel)))))]
        [set-docs-messages
         (let ([docs-lines-shown? #f])
           (lambda (lines)
	     (when (< (length docs-messages) (length lines))
	       (set! docs-messages
		     (append
		      docs-messages
		      (let loop ([n (- (length lines) (length docs-messages))])
			(cond
			 [(zero? n) null]
			 [else
			  (let ([m (make-object mred:message% "" docs-panel)])
			    (send m stretchable-width #t)
			    (cons m (loop (- n 1))))])))))
	     (let ([to-be-shown
		    (let loop ([lines lines]
			       [docs-messages docs-messages])
		      (cond
		       [(null? lines) null]
		       [else
			(send (car docs-messages) set-label (car lines))
			(cons (car docs-messages)
			      (loop (cdr lines)
				    (cdr docs-messages)))]))])
	       (unless (= (length to-be-shown) (length (send docs-panel get-children)))
		 (send docs-panel change-children (lambda (l) to-be-shown)))
	       (unless docs-messages-shown?
		 (set! docs-messages-shown? #t)
		 (send super-root change-children
		       (lambda (l)
			 (list rest-panel docs-panel)))))))])
      
      (private
        [report-error-frame (make-object mred:frame% "Check Syntax Error" #f 400 10)]
        [report-error-text (make-object mred:text%)]
        [report-error-canvas (make-object mred:editor-canvas% report-error-frame report-error-text
                               '(hide-hscroll hide-vscroll))]
        [hide-error-report-window (lambda () (send report-error-frame show #f))]
        [report-error
         (lambda (message)
           (send* report-error-text
             (begin-edit-sequence)
             (lock #f)
             (erase)
             (insert message)
             (lock #t)
             (end-edit-sequence))
           (send report-error-frame show #t))])
      (sequence
        (send report-error-text hide-caret #t)
        (send report-error-canvas set-line-count 1))
      
      (public
        [syncheck:principal?
         (lambda (zodiac-ast)
           (let ([who (zodiac:origin-who
                       (zodiac:zodiac-origin zodiac-ast))])
             (or (eq? who 'source)
                 (eq? who 'reader)
                 (and (or (eq? who 'micro)
                          (eq? who 'macro))
                      (syncheck:principal? 
                       (zodiac:origin-how (zodiac:zodiac-origin zodiac-ast)))))))]
        [syncheck:button-callback
         (lambda ()
           (let* ([project-window
		   (and
		    (ivar-in-interface? 'project:get-project-window (object-interface this))
		    (send this project:get-project-window))]
		  [setting
		   (if project-window
		       (send project-window get-language-setting)
		       (fw:preferences:get drscheme:language:settings-preferences-symbol))])
             (if (drscheme:basis:zodiac-vocabulary? setting)
                 (letrec ([run-in-user-thread
                           (let* ([custodian (make-custodian)]
                                  [eventspace (parameterize ([current-custodian custodian])
                                                (mred:make-eventspace))]
                                  [user-thread #f])
                             (let ([s (make-semaphore 0)])
                               (parameterize ([mred:current-eventspace eventspace])
                                 (mred:queue-callback
                                  (lambda ()
                                    (set! user-thread (current-thread))
                                    (semaphore-post s)
                                    (drscheme:basis:initialize-parameters custodian setting)
				    (let ([dir (get-directory)])
				      (when dir
					(current-directory dir)
					(current-load-relative-directory dir)))))
                                 (semaphore-wait s)))
                             (lambda (thnk)
                               (letrec ([done-sema (make-semaphore 0)]
                                        [just-one-sema (make-semaphore 1)]
                                        [continue-sema (make-semaphore 0)]
                                        [thd-alive
                                         (thread
                                          (lambda ()
                                            (semaphore-wait done-sema)
                                            (semaphore-wait just-one-sema)
                                            (kill-thread thd-kill)
                                            (semaphore-post continue-sema)))]
                                        [thd-kill
                                         (thread 
                                          (lambda () 
                                            (thread-wait user-thread)
                                            (semaphore-wait just-one-sema)
                                            (kill-thread thd-alive)
                                            (semaphore-post continue-sema)))])
                                 (parameterize ([mred:current-eventspace eventspace])
                                   (mred:queue-callback 
                                    (lambda ()
                                      (thnk)
                                      (semaphore-post done-sema))))
                                 (semaphore-wait continue-sema))))]
                          [built-in?
                           (let* ([syms null])
			     (let ([s (make-semaphore 0)])
			       (run-in-user-thread
				(lambda ()
				  (set! syms (map car (make-global-value-list)))
				  (semaphore-post s)))
			       (semaphore-wait s))
                             (lambda (s)
			       (member s syms)))]
                          [add-arrow (ivar definitions-text syncheck:add-arrow)]
                          [find-string (ivar definitions-text find-string)]
                          [change-style (lambda (s x y)
                                          ((ivar definitions-text change-style) s x y))]
                          [get-char (ivar definitions-text get-character)]
                          [find-next-whitespace
                           (lambda (start)
                             (let* ([find (lambda (s)
                                            (let ([ans (find-string s 'forward start)])
                                              ans))]
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
                          [style-list (send definitions-text get-style-list)]
                          [bound-style (send style-list find-named-style "drscheme:check-syntax:bound-variable")]
                          [unbound-style (send style-list find-named-style "drscheme:check-syntax:unbound-variable")]
                          [primitive-style (send style-list find-named-style "drscheme:check-syntax:primitive")]
                          [keyword-style (send style-list find-named-style "drscheme:check-syntax:keyword")]
                          [const-style (send style-list find-named-style "drscheme:check-syntax:constant")]
                          [base-style (send style-list find-named-style "drscheme:check-syntax:base")]
                          [rename-bindings
                           (lambda (occurrances input-name)
                             (send definitions-text begin-edit-sequence)
                             (let* ([new-name (format "~a" (string->symbol input-name))]
                                    [sorted (mzlib:function:quicksort
                                             occurrances
                                             (lambda (x y)
                                               (<= (zodiac:location-offset
                                                    (zodiac:zodiac-start y))
                                                   (zodiac:location-offset
                                                    (zodiac:zodiac-start x)))))]
                                    [rename-one
                                     (lambda (z)
                                       (send definitions-text insert new-name
                                             (zodiac:location-offset (zodiac:zodiac-start z))
                                             (add1 (zodiac:location-offset
                                                    (zodiac:zodiac-finish z)))))])
                               (for-each rename-one sorted))
                             (send definitions-text end-edit-sequence))]
                          [color-loop
                           (lambda (zodiac-ast)
                             (let* ([z:start (zodiac:location-offset (zodiac:zodiac-start zodiac-ast))]
                                    [z:finish (+ 1
                                                 (zodiac:location-offset
                                                  (zodiac:zodiac-finish zodiac-ast)))]
                                    [z:text (zodiac:location-file (zodiac:zodiac-start zodiac-ast))]
                                    [search-for-orig-syntax
                                     (lambda (zobj)
                                       (let loop ([zobj zobj])
                                         (or (syncheck:principal? zobj)
                                             (let* ([origin (zodiac:zodiac-origin zobj)]
                                                    [who (zodiac:origin-who origin)])
                                               (cond
                                                 [(or (eq? who 'macro) 
                                                      (eq? who 'micro))
                                                  (loop (zodiac:origin-how origin))]
                                                 [else #f])))))]
                                    [color-syntax
                                     (lambda ()
                                       (when (search-for-orig-syntax zodiac-ast)
                                         (let* ([start (find-next-non-whitespace (add1 z:start))]
                                                [finish (find-next-whitespace start)])
                                           (when (and finish start)
                                             (if (is-a? z:text mred:text%)
                                                 (send z:text change-style keyword-style start finish)
                                                 (change-style keyword-style start finish))))))]
                                    
                                    [color
                                     (lambda (delta)
                                       (when (and (syncheck:principal? zodiac-ast)
                                                  z:finish z:start)
                                         (if (is-a? z:text mred:text%)
                                             (send z:text change-style delta z:start z:finish)
                                             (change-style delta z:start z:finish))))])
                               ; No matter what this expression is,
                               ; if it's not direct from the
                               ; source, it might be a macro or micro expansion.
                               (unless (syncheck:principal? zodiac-ast)
                                 (color-syntax))
                               
                               (cond
                                 [(zodiac:quote-form? zodiac-ast)
                                  (color const-style)]
                                 [(zodiac:binding? zodiac-ast) (color bound-style)]
                                 [(zodiac:bound-varref? zodiac-ast)
                                  (when (syncheck:principal? zodiac-ast)
                                    (let* ([binding (zodiac:bound-varref-binding zodiac-ast)])
                                      (when (syncheck:principal? binding)
                                        (let* ([user-name (zodiac:binding-orig-name binding)]
                                               [gen-name (zodiac:varref-var zodiac-ast)]
                                               [start (zodiac:location-offset
                                                       (zodiac:zodiac-start binding))]
                                               [finish (add1 (zodiac:location-offset
                                                              (zodiac:zodiac-finish binding)))]
                                               [rename (lambda (new-name)
                                                         (when new-name
                                                           (rename-bindings
                                                            (cons binding
                                                                  (hash-table-get
                                                                   local-bindings
                                                                   gen-name (lambda () null)))
                                                            new-name)))])
                                          (hash-table-put!
                                           local-bindings
                                           gen-name
                                           (cons zodiac-ast
                                                 (hash-table-get local-bindings
                                                                 gen-name (lambda () null))))
                                          (add-arrow z:start z:finish start finish user-name rename))))
                                    (color bound-style))]
                                 
                                 [(zodiac:top-level-varref? zodiac-ast)
                                  (when (syncheck:principal? zodiac-ast)
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
                                   (lambda (var) 
                                     (hash-table-put! 
                                      defineds (zodiac:varref-var var)
                                      (cons var
                                            (hash-table-get defineds 
                                                            (zodiac:varref-var var)
                                                            (lambda () null)))))
                                   (zodiac:define-values-form-vars zodiac-ast))
                                  (for-each (lambda (var)
                                              (when (syncheck:principal? var)
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
                                 
                                 [(zodiac:letrec-values-form? zodiac-ast)
                                  (color-syntax)
                                  (for-each (lambda (x) (for-each color-loop x))
                                            (zodiac:letrec-values-form-vars zodiac-ast))
                                  (for-each color-loop
                                            (zodiac:letrec-values-form-vals zodiac-ast))
                                  (color-loop (zodiac:letrec-values-form-body zodiac-ast))]
                                 
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
                                 
				;; little grossness here to make life easier.
                                 [(zodiac:symbol? zodiac-ast) (color bound-style)]
                                 
                                 [(zodiac:unit-form? zodiac-ast)
                                  (color-syntax)
                                  (for-each color-loop (zodiac:unit-form-imports zodiac-ast))
                                  (for-each color-loop (map car (zodiac:unit-form-exports zodiac-ast)))
                                  (for-each color-loop (zodiac:unit-form-clauses zodiac-ast))]
                                 [(zodiac:compound-unit-form? zodiac-ast)
                                  (color-syntax)
                                  (for-each color-loop (map cadr (zodiac:compound-unit-form-links zodiac-ast)))]
                                 [(zodiac:invoke-unit-form? zodiac-ast)
                                  (color-syntax)
                                  (color-loop (zodiac:invoke-unit-form-unit zodiac-ast))
                                  (for-each color-loop (zodiac:invoke-unit-form-variables zodiac-ast))]
                                 
                                 [(zodiac:interface-form? zodiac-ast)
                                  (color-syntax)
                                  (for-each color-loop (zodiac:interface-form-super-exprs zodiac-ast))]
                                 [(zodiac:class*/names-form? zodiac-ast)
                                  (color-syntax)
                                  (color-loop (zodiac:class*/names-form-this zodiac-ast))
                                  (color-loop (zodiac:class*/names-form-super-init zodiac-ast))
                                  (color-loop (zodiac:class*/names-form-super-expr zodiac-ast))
                                  (for-each color-loop (zodiac:class*/names-form-interfaces zodiac-ast))
                                  (for-each color-loop
                                            (zodiac:paroptarglist-vars (zodiac:class*/names-form-init-vars zodiac-ast)))
                                  (for-each
                                   (lambda (clause)
                                     (cond
                                       ((zodiac:public-clause? clause)
                                        (for-each color-loop (zodiac:public-clause-internals clause))
                                        (for-each color-loop (zodiac:public-clause-exprs clause)))
                                       ((zodiac:override-clause? clause)
                                        (for-each color-loop (zodiac:override-clause-internals clause))
                                        (for-each color-loop (zodiac:override-clause-exprs clause)))
                                       ((zodiac:private-clause? clause)
                                        (for-each color-loop (zodiac:private-clause-internals clause))
                                        (for-each color-loop (zodiac:private-clause-exprs clause)))
                                       ((zodiac:inherit-clause? clause)
                                        (for-each color-loop (zodiac:inherit-clause-internals clause)))
                                       ((zodiac:rename-clause? clause)
                                        (for-each color-loop (zodiac:rename-clause-internals clause)))
                                       ((zodiac:sequence-clause? clause)
                                        (for-each color-loop (zodiac:sequence-clause-exprs clause)))))
                                   (zodiac:class*/names-form-inst-clauses zodiac-ast))]
                                 
                                 [(zodiac:struct-form? zodiac-ast)
                                  (color-syntax)
                                  (color-loop (zodiac:struct-form-type zodiac-ast))
                                  (when (zodiac:struct-form-super zodiac-ast)
                                    (color-loop (zodiac:struct-form-super zodiac-ast)))
                                  (for-each color-loop
                                            (zodiac:struct-form-fields zodiac-ast))]
                                 
                                 [else (void)])))])
                   (let ([mod-flag (void)]) ; buffer modified before check-syntax run
                     (dynamic-wind
                      (lambda ()
                        (mred:begin-busy-cursor)
                        (set! mod-flag
                              (send definitions-text is-modified?))
                        (send definitions-text set-styles-fixed #f)
                        (send definitions-text begin-edit-sequence #f))
                      (lambda ()
                        (send definitions-text clear-annotations)
                        (send definitions-text syncheck:init-arrows)

                        (send definitions-text change-style base-style 0 (send definitions-text last-position))
                        
			;; color each exp
                        (let ([output-port (current-output-port)]
                              [error-raised? #f]
                              [error #f]
                              [debug #f]
                              [msg #f])
                          (run-in-user-thread
                           (lambda ()
                             (let/ec k
                               (parameterize ([current-output-port output-port]
                                              [drscheme:basis:error-display/debug-handler
                                               (lambda (m d x)
                                                 (set! msg m)
                                                 (set! debug (if (null? d) #f (car d)))
                                                 (set! error x)
                                                 (set! error-raised? #t))]
                                              [error-escape-handler k])

				 (when project-window
				   (for-each
				    (lambda (file)
				      (eval
				       (case (car file)
					 [(build-path)
					  (load-relative
					   (let ([p (apply build-path (cdr file))])
					     (if (relative-path? p)
						 (build-path
						  (send project-window get-project-directory)
						  p)
						 p)))]
					 [(require-library)
					  (apply require-library/proc (cdr file))])))
				    (send project-window get-elaboration-files)))

                                 (drscheme:load-handler:process-text/zodiac
                                  definitions-text
                                  (lambda (expr recur)
                                    (cond
                                      [(drscheme:basis:process-finish? expr) 
                                       (void)]
                                      [(not (zodiac:zodiac? expr))
                                       (recur)]
                                      [else
                                       (color-loop expr)
                                       (recur)]))
                                  0
                                  (send definitions-text last-position)
                                  #f
                                  #t)))))
                          (if error-raised?
                              (begin (send interactions-text highlight-error
                                           (zodiac:location-file (zodiac:zodiac-start debug))
                                           (zodiac:location-offset (zodiac:zodiac-start debug))
                                           (+ (zodiac:location-offset (zodiac:zodiac-finish debug)) 1))
                                     (report-error msg))
                              (hide-error-report-window)))
                        
                        ; color the top-level varrefs
                        (for-each
                         (lambda (var)
                           (let* ([id (zodiac:varref-var var)]
                                  [text (zodiac:location-file (zodiac:zodiac-start var))]
                                  [change-style (if (is-a? text mred:text%)
                                                    (ivar text change-style)
                                                    change-style)]
                                  [is-built-in? (built-in? id)]
                                  [start (zodiac:location-offset (zodiac:zodiac-start var))]
                                  [finish (add1 (zodiac:location-offset (zodiac:zodiac-finish var)))])
                             (when (and is-built-in? (is-a? text mred:text%))
                               (send text syncheck:add-menu start finish 
                                     (lambda ()
                                       (prims:initialize-tables)
                                       (let ([ht (cond
                                                   [(drscheme:basis:beginner-language? setting) prims:beginning]
                                                   [(drscheme:basis:intermediate-language? setting) prims:intermediate]
                                                   [(drscheme:basis:advanced-language? setting) prims:advanced]
                                                   [else #f])])
                                         (cond
                                           [(and ht (hash-table-get ht id (lambda () #f)))
                                            =>
                                            (lambda (txt)
                                              (let ([m (make-object mred:popup-menu%)])
                                                (make-object mred:menu-item% (format "Show summary of ~a" id)
                                                  m
                                                  (lambda x
                                                    (set-docs-messages txt)))
                                                (send 
                                                 (make-object mred:menu-item% "Hide primitive summary pane"
                                                   m
                                                   (lambda x
                                                     (hide-docs-messages)))
                                                 enable
                                                 docs-messages-shown?)
                                                m))]
                                           [(drscheme:basis:full-language? setting)
                                            (let ([m (make-object mred:popup-menu%)])
                                              (make-object mred:menu-item% 
                                                (format "Show documentation for ~a" id)
                                                m
                                                (lambda x
                                                  (drscheme:help-desk:help-desk
						   (symbol->string id)
						   #t
						   'keyword
						   'exact)))
                                              m)]
                                           [else #f])))))
                             (change-style
                              (cond
                                [(hash-table-get defineds id (lambda () #f))
                                 => 
                                 (lambda (defn-vars)
                                   (when (syncheck:principal? (car defn-vars))
                                     (let* ([defn-var (car defn-vars)]
                                            [end-pos-left (zodiac:location-offset (zodiac:zodiac-start defn-var))]
                                            [end-pos-right (add1 (zodiac:location-offset (zodiac:zodiac-finish defn-var)))]
                                            [start-pos-left (zodiac:location-offset (zodiac:zodiac-start var))]
                                            [start-pos-right (add1 (zodiac:location-offset (zodiac:zodiac-finish var)))]
                                            [rename (lambda (new-name)
                                                      (when new-name
                                                        (rename-bindings
                                                         (mzlib:function:foldl
                                                          (lambda (test-var l)
                                                            (if (eq? (zodiac:varref-var test-var)
                                                                     (zodiac:varref-var defn-var))
                                                                (cons test-var l)
                                                                l))
                                                          defn-vars
                                                          top-level-varrefs)
                                                         new-name)))])
                                       (add-arrow start-pos-left start-pos-right end-pos-left end-pos-right
                                                  (zodiac:varref-var defn-var) rename)))
                                   bound-style)]
                                [is-built-in? primitive-style]
                                [else unbound-style])
                              start finish)))
                         top-level-varrefs))
                      (lambda () ; post part of dynamic wind
                        (send definitions-text end-edit-sequence)
                        (unless mod-flag
                          (send definitions-text set-modified #f))
                        (send definitions-text set-styles-fixed #t)
                        (send definitions-canvas focus)
                        (mred:end-busy-cursor)))))
                 (mred:message-box "DrScheme Check Syntax"
                                   "In order to check the syntax, the language must be a teaching language or a debug language"))))])
      (sequence (apply super-init args))
      
      (public
        [check-syntax-button
         (make-object mred:button%
           (syncheck-bitmap this)
           button-panel
           (lambda (button evt) (syncheck:button-callback)))])
      (sequence
        (send definitions-text set-styles-fixed #t)
        (send check-syntax-button show button-visible?)
        (send button-panel change-children
              (lambda (l)
                (cons check-syntax-button
                      (mzlib:function:remove check-syntax-button l)))))))
  
  (define (make-syncheck-definitions-text% super%)
    (class100 super% args
      (rename [super-clear-annotations clear-annotations])
      (inherit get-top-level-window)
      (override
        [clear-annotations
         (lambda ()
           (super-clear-annotations)
           (when (get-top-level-window)
             (send (get-top-level-window) syncheck:clear-highlighting)))])
      (sequence (apply super-init args))))
  
  
  (drscheme:get/extend:extend-definitions-text make-graphics-text%)
  (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f)
  (drscheme:get/extend:extend-definitions-text make-syncheck-definitions-text%))
