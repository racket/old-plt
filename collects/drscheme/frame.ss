(unit/sig drscheme:frame^
  (import [wx : wx^]
	  [mred : mred^]
	  [mzlib : mzlib:core^]
	  [drscheme:basis : drscheme:basis^]
	  [drscheme:setup : drscheme:setup^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:app : drscheme:app^]
	  [zodiac : drscheme:zodiac^])
  
  (mred:debug:printf 'invoke "drscheme:frame@")
  
  (define (make-frame% super%)
    (rec drscheme:frame:frame%
	 (class super% (unit)
	   (rename [super-make-root-panel make-root-panel]
		   [super-make-menu-bar make-menu-bar])
	   (inherit panel get-edit save-as info-panel)
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
	     
	     [update-shown (lambda () (void))]
	     [make-menu-bar
	      (lambda ()
		(let ([mb (super-make-menu-bar)])
		  (set! show-menu (make-menu))
		  (send mb append show-menu "&View")
		  mb))])
	   
	   (public
	     [running
	      (lambda ()
		(send running-message set-label "running"))]
	     [not-running
	      (lambda ()
		(send running-message set-label "not running"))])
	   
	   (public
	     ;[file-menu:new-string "Unit"]
	     [file-menu:new
	      (lambda ()
		(send (drscheme:unit:make-unit #f) create-frame))]
	     [file-menu:between-new-and-open
	      (lambda (file-menu)
		'(send file-menu append-item "New Compound Unit"
		       (lambda ()
			 (send (drscheme:compound-unit:make-compound-unit #f)
			       create-frame))))]
	     [file-menu:open (lambda () (mred:open-file) #t)]
	     [help-menu:about (lambda () (drscheme:app:about-drscheme))])
	   
	   (sequence 
	     (mred:debug:printf 'super-init "before drscheme:frame%")
	     (super-init (send unit get-name))
	     (mred:debug:printf 'super-init "after drscheme:frame%"))
	   
	   (private
	     [running-message
	      (make-object wx:message% info-panel "not running")])))))
