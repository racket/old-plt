
  (unit/sig drscheme:frame^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:basis : drscheme:basis^]
	    [drscheme:setup : drscheme:setup^]
	    [drscheme:unit : drscheme:unit^]
	    [drscheme:compound-unit : drscheme:compound-unit^]
	    [zodiac : zodiac:system^])
    
    (mred:debug:printf 'invoke "drscheme:frame@")

    (define group (make-object mred:frame-group%))
    (send group set-empty-callback (lambda () (mred:exit) #f))
    (mred:current-frames group)

    (define frame%
      (class mred:simple-menu-frame% (name snip arg-group)
	(rename [super-make-root-panel make-root-panel]
		[super-on-close on-close]
		[super-make-menu-bar make-menu-bar])
	(inherit panel get-edit save-as)
	(public
	  [on-close
	   (lambda ()
	     (let* ([user-allowed-or-not-modified
		     (let ([edit (get-edit)])
		       (or (not (send edit modified?))
			   (case (mred:unsaved-warning (let ([fn (send edit get-filename)])
							 (if (string? fn)
							     fn
							     "Untitled"))
						       "Close"
						       #t)
			     [(continue) #t]
			     [(save) (send edit save-file)]
			     [else #f])))]
		    [super (and user-allowed-or-not-modified (super-on-close))]
		    [grp (and super (send group remove-frame this))])
	       grp))])

	(public
	  [root-panel #f]
	  [make-root-panel
	   (lambda (% parent)
	     (let* ([s-root (super-make-root-panel mred:vertical-panel% parent)]
		    [root (make-object % s-root)])
	       (set! root-panel s-root)
	       root))])

	(inherit make-menu)
	(public
	  [show-menu #f]
	  [imports-id #f]
	  
	  [update-shown
	   (lambda ()
	     (send root-panel change-children
		   (lambda (l)
		     (let ([removed (mzlib:function@:remq imports-panel l)])
		       (if (and imports-id (send show-menu checked? imports-id))
			   (cons imports-panel removed)
			   removed)))))]
	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)])
	       (set! show-menu (make-menu))
	       (send mb append show-menu "S&how")
	       (set! imports-id 
		     (begin '(send show-menu append-item
				   "&Imports"
				   (lambda () (update-shown))
				   "Show the imports to this unit"
				   #t)
			    #f))
	       '(send show-menu check imports-id snip)
	       mb))])

	(public
	  [file-menu:new-stringg "Unit"]
	  [file-menu:between-save-and-print
	   (lambda (file-menu)
	     (send* file-menu 
	       (append-item "Save as Text..." (lambda () (save-as wx:const-media-ff-text)))
	       (append-separator)))]
	  [file-menu:new
	   (lambda ()
	     (make-object drscheme:unit:frame% #f #f group))]
	  [file-menu:between-new-and-open
	   (lambda (file-menu)
	     '(send file-menu append-item "Get Program Text"
		   (lambda ()
		     (mred:message-box
		      (format "~s" (zodiac:parsed->raw
				    (send group get-whole-program))))))
	     '(send file-menu append-item "New Compound Unit"
		   (lambda ()
		     (make-object drscheme:compound-unit:frame% #f #f group))))]

	  [group arg-group]
	  [file-menu:open (lambda () (mred:open-file group))])

	(sequence 
	  (mred:debug:printf 'super-init "before drscheme:frame%")
	  (super-init name)
	  (mred:debug:printf 'super-init "after drscheme:frame%"))
	(public
	  [imports-panel (make-object mred:horizontal-panel% root-panel)]
	  [imports-message
	   (make-object mred:message% imports-panel "imports")]
	  [update-imports
	   (lambda ()
	     (let ([make-button
		    (lambda (snip)
		      (make-object mred:button% imports-panel
				   (lambda x (send snip open))
				   (ivar snip name)))])
	       (send imports-panel change-children
		     (lambda (l) (list imports-message)))
               (when snip
	         (for-each make-button (ivar snip parents)))))]
	  [on-change-imports (lambda ()
			       (update-imports))])
	(sequence
	  (update-imports)
	  (send imports-panel stretchable-in-y #f)))))