(unit/sig drscheme:frame^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [drscheme:basis : drscheme:basis^]
	  [drscheme:setup : drscheme:setup^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:app : drscheme:app^]
	  [zodiac : drscheme:zodiac^])
  
  (mred:debug:printf 'invoke "drscheme:frame@")
  
  (define (make-frame% super%)
    (class super% (unit)
      (rename [super-make-root-panel make-root-panel]
	      [super-can-close? can-close?]
	      [super-make-menu-bar make-menu-bar])
      (inherit panel get-edit save-as)
      (public
	[can-close?
	 (lambda ()
	   (let* ([edit (get-edit)]
		  [user-allowed-or-not-modified
		   (or (not (send edit modified?))
		       (case (mred:unsaved-warning
			      (let ([fn (send edit get-filename)])
				(if (string? fn)
				    fn
				    "Untitled"))
			      "Close"
			      #t)
			 [(continue) #t]
			 [(save) (begin (send edit save-file) #t)]
			 [else #f]))])
	     (and user-allowed-or-not-modified
		  (super-can-close?))))])
      
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
	     (send mb append show-menu "S&how")
	     mb))])
      
      (public
	[file-menu:new-string "Unit"]
	[file-menu:between-save-and-print
	 (lambda (file-menu)
	   (send* file-menu 
	     (append-item "Save as Text..." (lambda () (save-as wx:const-media-ff-text)))
	     (append-separator)))]
	[file-menu:new
	 (lambda ()
	   (send (drscheme:unit:make-unit #f) create-frame))]
	[file-menu:between-new-and-open
	 (lambda (file-menu)
	   (send file-menu append-item "New Compound Unit"
		 (lambda ()
		   (send (drscheme:compound-unit:make-compound-unit #f)
			 create-frame))))]
	[file-menu:open (lambda () (mred:open-file) #t)]
	[help-menu:about (lambda () (drscheme:app:about-drscheme))])
      
      (sequence 
	(mred:debug:printf 'super-init "before drscheme:frame%")
	(super-init (send unit get-name))
	(mred:debug:printf 'super-init "after drscheme:frame%")))))