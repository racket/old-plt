; Dan Grossman
; Hyper Frame
; July 17, 1995

(define mred:hyper-frame@
  (unit/sig mred:hyper-frame^
    (import ([unit mred:debug : mred:debug^]
	     [unit mred:hyper-edit : mred:hyper-edit^]
	     [unit mred:hyper-dialog : mred:hyper-dialog^]
	     [unit mred:frame : mred:frame^]
	     [unit mred:canvas : mred:canvas^]
	     [unit mred:group : mred:group^]
	     [unit mred:handler : mred:handler^]))
	    
    (mred:debug:printf 'invoke "mred:hyper-frame@")

    (define hyper-frame-group (make-object mred:group:frame-group%))

    (define make-hyper-canvas%
      (lambda (super%)
	(class-asi super%
		   (inherit get-media get-client-size set-focus on-paint on-size)
		   (rename [super-set-media set-media])
		   (public
		    [history-stack ()]
		    [forward-stack ()]
		    [home #f] 
		    [locked? #t]
		    [set-locking  (lambda (val) (set! locked? val))]
		    [set-history 
		     (lambda (stack)
		       (set! history-stack stack))]
		    [set-forward 
		     (lambda (stack)
		       (set! forward-stack stack))]
		    [set-home 
		     (lambda (tag)
		       (set! home (cons (get-media) tag)))]
		    [adjust-stacks-on-follow 
		     (lambda (tag)
		       (set-history  (cons (cons (get-media) tag)
					   history-stack))
		       (set-forward  ()))]
		    [position-to-top
		     (lambda ()
		       (let* ([edit (get-media)]
			      [bt (box 0)][bb (box 0)]
			      [this-line (send edit position-line 
					       (send edit get-start-position))])
			 (send edit get-visible-line-range bt bb)
			 (send edit scroll-to-position 
			       (send edit line-start-position this-line) #t
			       (send edit line-start-position
				     (+ this-line (- (unbox bb)(unbox bt)))))))]
		    [do-tag
		     (lambda ()
		       (let ([tag the-tag]
			     [edit (get-media)])
			 (unless (null? edit)
			   (dynamic-wind
			    (lambda ()
			      (send edit begin-edit-sequence))
			    (lambda ()
			      (if tag
				  (let tag-loop ([tags-left (ivar edit hypertags-list)])
				    (cond
				     [(null? tags-left)
				      (send edit show-message
					    "Unable to find destination position."
					    "Error")]
				     [(string=? tag (mred:hyper-edit:hypertag-name (car tags-left)))
				      (send edit set-position 
					    (mred:hyper-edit:hypertag-position (car tags-left)))]
				     [else (tag-loop (cdr tags-left))])))
			      (position-to-top))
			    (lambda ()
			      (send edit end-edit-sequence))))
			 (on-paint)
			 (adjust-stacks-on-follow tag)
			 (set-the-tag #f)))]
		    [the-tag #f]
		    [set-the-tag (lambda (tag) (set! the-tag tag))]
		    [set-media  
		     (opt-lambda (edit [display #t])
		       (super-set-media edit (null? edit))
		       (do-tag))]))))

    (define hyper-canvas% 
      (make-hyper-canvas% mred:canvas:simple-frame-canvas%))

    ; a simple frame w/ a file close and always locked.
    (define make-hyper-basic-frame%
      (lambda (super%)
	(class super% ([file-name #f] [group #f] [keep-locked? #t]
				      [tag "top"] [relative? #f])
	       (inherit 
		active-canvas edit active-edit 
		TOP-MARGIN LEFT-MARGIN
		make-menu menu-bar%
		on-size)
	       (rename [super-open-file open-file])
	       (public
		[edit% mred:hyper-edit:hyper-edit%]
		[canvas% hyper-canvas%]
		[button% wx:button%]
		[panel% wx:panel%])
	       (private
		[do-backward
		 (lambda (button event)
		   (let* ([canvas (active-canvas)]
			  [backs (ivar canvas history-stack)]
			  [fors (ivar canvas forward-stack)])
		     (when (not (or (null? backs)(null? (cdr backs))))
		       (send canvas set-the-tag (cdadr backs))
		       (send canvas set-media (caadr backs))
		       (send canvas set-history (cdr backs))
		       (send canvas set-forward (cons (car backs) fors)))
		     (send canvas set-focus )))]
		[do-forward
		 (lambda (button event)
		   (let* ([canvas (active-canvas)]
			  [backs (ivar canvas history-stack)]
			  [fors (ivar canvas forward-stack)])
		     (when (not (null? fors))
		       (send* canvas
			      (set-the-tag (cdar fors))
			      (set-media (caar fors))
			      (set-history (cons (car fors) backs))
			      (set-forward (cdr fors))))
		     (send canvas set-focus)))]
		[do-home
		 (lambda (button event)
		   (let* ([canvas (active-canvas)]
			  [backs (ivar canvas history-stack)]
			  [home (ivar canvas home)])
		     (when home
		       (send* canvas
			      (set-the-tag (cdr home))
			      (set-media (car home))
			      (set-history (cons home backs))
			      (set-forward ())))
		     (send canvas set-focus)))])
	       (public
		[open-file 
		 (opt-lambda (filename [tag #f])
		   (send (active-canvas) set-the-tag tag)
		   (super-open-file filename))])
	       (sequence
		 (super-init file-name #t (if group group hyper-frame-group)))

	       (private
		(button-panel (make-object panel% this))
		(backward-button
		 (make-object button% 
		   button-panel do-backward "Back"))
		(forward-button
		 (make-object button%
		   button-panel do-forward "Forward"))
		(home-button
		 (make-object button%
		   button-panel do-home "Home")))
	       (sequence
		 (send button-panel new-line)
		 (send button-panel fit)
		 (let ([w (box 0)][h (box 0)])
		   (send button-panel get-size w h)
		   (send button-panel set-size LEFT-MARGIN TOP-MARGIN 1000 (unbox h))
		   (set! TOP-MARGIN (+ TOP-MARGIN (unbox h))))
		 (on-size 0 0)
		 (send (active-canvas) set-focus)
		 (send (active-canvas) set-home tag)
		 (send (active-canvas) set-history ())
		 (send (active-canvas) adjust-stacks-on-follow tag)))))
    (define hyper-basic-frame% (make-hyper-basic-frame% mred:frame:editor-frame%))

    (define make-hyper-view-frame%
      (lambda (super%)
	(class-asi super%
		   (inherit menu-bar% make-menu show)
		   (public
		    [make-menu-bar
		     (lambda ()
		       (let ([mb (make-object menu-bar%)]
			     [file-menu (make-menu)])
			 (send file-menu append-item "Close" (lambda () (show #f)))
			 (send mb append file-menu "File")
			 mb))]))))
    (define hyper-view-frame% 
      (make-hyper-view-frame% hyper-basic-frame%))

    
    (define make-hyper-make-frame%
      (lambda (super%)
	(class super% ([file-name #f] [group #f])
	       (inherit active-edit active-canvas get-menu-bar make-menu edit%)
	       (rename [super-make-menu-bar make-menu-bar])
	       (private
		follow-item 
		view-item 
		nothing-item)
	       (public
		[link-click-mode 'follow]
		[set-link-click
		 (lambda (mode)
		   (unless (eq? mode link-click-mode)
		     (set! link-click-mode mode)
		     (let ([edit (active-edit)]
			   [menu-bar (get-menu-bar)])
		       (if (eq? mode 'nothing)
			   (send edit uninstall-clickbacks)
			   (send edit install-clickbacks))
		       (send edit set-follow-on-click 
			     (case mode
			       (follow #t)
			       (view #f)
			       (nothing ())))
		       (send menu-bar check follow-item (eq? mode 'follow))
		       (send menu-bar check view-item (eq? mode 'view))
		       (send menu-bar check nothing-item (eq? mode 'nothing)))))]
		[make-edit
		 (lambda ()
		   (make-object edit% #f))]
		[make-menu-bar
		 (lambda ()
		   (let ([menu-bar (super-make-menu-bar)]
			 [hyper-menu (make-menu)])
		     (send hyper-menu append-item "New Link..."
			   (lambda ()
			     (let ([edit (active-edit)])
			       (send edit make-link
				     (send edit get-start-position)
				     (send edit get-end-position)))))
		     (send hyper-menu append-item "Change Link..."
			   (lambda ()
			     (let ([edit (active-edit)])
			       (send edit change-link 
				     (send edit get-start-position)))))
		     (send hyper-menu append-item "Remove Link"
			   (lambda ()
			     (let ([edit (active-edit)])
			       (send edit remove-link 
				     (send edit get-start-position)))))
		     
		     (send hyper-menu append-separator)
		     (set! follow-item
			   (send hyper-menu append-item "Follow Links" 
				 (lambda () (set-link-click 'follow))
				 () #t))
		     (set! view-item
			   (send hyper-menu append-item "Display Links" 
				 (lambda () (set-link-click 'view))
				 () #t))
		     (set! nothing-item
			   (send hyper-menu append-item "Ignore Links"
				 (lambda () (set-link-click 'nothing))
				 () #t))
		     (send hyper-menu append-separator)
		     (send hyper-menu append-item "Make Tag"
			   (lambda ()
			     (let ([edit (active-edit)])
			       (send edit make-tag 
				     (send edit get-start-position)))))
		     (send hyper-menu append-item "Remove Tag"
			   (lambda ()
			     (let* ([edit (active-edit)]
				    [tag-name (send (make-object 
							mred:hyper-dialog:hyper-tag-dialog% 
						      (reverse
						       (map mred:hyper-edit:hypertag-name 
							    (ivar edit 
								  hypertags-list))))
						    get-answer)])
			       (if tag-name (send edit remove-tag  tag-name)))))
		     (send hyper-menu append-item "Go to Tag"
			   (lambda ()
			     (let* ([edit (active-edit)]
				    [tag-name (send (make-object 
							mred:hyper-dialog:hyper-tag-dialog% 
						      (reverse
						       (map mred:hyper-edit:hypertag-name 
							    (ivar edit 
								  hypertags-list))))
						    get-answer)])
			       (if tag-name (send edit show-tag tag-name)))))
		     (send menu-bar append hyper-menu "Hyper-Text")
		     (send menu-bar check follow-item #t)
		     menu-bar))])
	       (sequence
		 (super-init file-name group #f)
		 (wx:yield)
		 (send (active-canvas) set-focus)
		 (send (active-canvas) set-locking #f)))))
    (define hyper-make-frame% 
      (make-hyper-make-frame% hyper-basic-frame%))

    ; For Autoloading:

    (define open-hyper-view
      (opt-lambda ([file-name #f][group #f][keep-locked? #t][tag "top"][relative? #f])
	(make-object hyper-view-frame% file-name group keep-locked? tag relative?)))

    (define open-hyper-make
      (opt-lambda ([file-name #f][group #f])
	(make-object hyper-make-frame% file-name group)))

    (define hyper-text-require (lambda () (void)))

    (mred:handler:insert-format-handler  "Hyper-Text" "htx" 
					  (lambda (filename group)
					    (open-hyper-make filename group)))))
