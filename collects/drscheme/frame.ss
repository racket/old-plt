(unit/sig drscheme:frame^
  (import [mred : mred-interfaces^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:app : drscheme:app^]
	  [help : help:start-help-desk^]
	  [zodiac : drscheme:zodiac^])
  
  (rename [-mixin mixin])

  (define help-desk
    (let ([help-desk-frame #f])
      (case-lambda
       [()
	(mred:begin-busy-cursor)
	(set! help-desk-frame (help:start-help-desk))
	(mred:end-busy-cursor)]
       [(key)
	(let ([turn-cursor-off? (not help-desk-frame)])
	  (if help-desk-frame
	      (send help-desk-frame show #t)
	      (begin (mred:begin-busy-cursor)
		     (help-desk)))
	  (send help-desk-frame search-for-help key 'keyword+index 'exact)
	  (when turn-cursor-off?
	    (mred:end-busy-cursor)))])))

  (define basics<%> (interface (fw:frame:standard-menus<%>)))

  (define basics-mixin
    (mixin (fw:frame:standard-menus<%>) (basics<%>) args
      (override
       [help-menu:after-about
	(lambda (help-menu)
	  (make-object mred:menu-item%
	    "Help Desk"
	    help-menu
	    (lambda (item evt)
	      (help-desk))))]
      ;[file-menu:new-string (lambda () "Unit")]
       [file-menu:new
	(lambda (item evt)
	  (send (drscheme:unit:make-unit #f) create-frame))]
       [file-menu:between-new-and-open
	(lambda (file-menu)
	  '(send file-menu append-item "New Compound Unit"
		 (lambda ()
		   (send (drscheme:compound-unit:make-compound-unit #f)
			 create-frame))))]
       [file-menu:open (lambda (item evt) (fw:handler:open-file) #t)]
       [help-menu:about (lambda (item evt) (drscheme:app:about-drscheme))]
       [help-menu:about-string (lambda () "DrScheme")])
      
      (sequence 
	(apply super-init args))))

  (define <%> (interface (fw:frame:info<%> basics<%>)))

  (define -mixin
    (mixin (fw:frame:info<%> basics<%>) (<%>) (unit)
      (rename [super-make-root-area-container make-root-area-container])
      (inherit get-info-panel)
      (public
       [root-panel #f])
      (override
       [make-root-area-container
	(lambda (% parent)
	  (let* ([s-root (super-make-root-area-container mred:vertical-panel% parent)]
		 [root (make-object % s-root)])
	    (set! root-panel s-root)
	    root))])
      
      (public
	[show-menu #f]
	
	[update-shown (lambda () (void))])
      
      (private
	[get-bitmap/string
	 (lambda (icon string)
	   (let ([p (build-path (collection-path "icons") icon)])
	     (if (file-exists? p)
		 (make-object mred:bitmap% p 'gif)
		 string)))]
	[currently-running? #f]
	[sleepy-bitmap (get-bitmap/string "snoopy-sleepy.gif" "not running")]
	[active-bitmap (get-bitmap/string "snoopy-active.gif" "running")])
      (public
	[running
	 (lambda ()
	   (unless currently-running?
	     (set! currently-running? #t)
	     (send running-message set-label active-bitmap)))]
	[not-running
	 (lambda ()
	   (when currently-running?
	     (set! currently-running? #f)
	     (send running-message set-label sleepy-bitmap)))])
      
      (inherit get-menu% get-menu-bar)
      (sequence 
	(super-init (send unit get-name))
	(set! show-menu (make-object (get-menu%) "Show" (get-menu-bar))))
      
      (private
	[running-message
	 (make-object mred:message% sleepy-bitmap (get-info-panel))]))))

