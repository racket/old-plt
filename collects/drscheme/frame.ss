(unit/sig drscheme:frame^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:intro : drscheme:intro^]
	  [drscheme:text : drscheme:text^]
	  [help : help:drscheme-interface^]
	  [zodiac : zodiac:system^])
  
  (rename [-mixin mixin])

  (define basics<%> (interface (fw:frame:standard-menus<%>)))

  (define basics-mixin
    (mixin (fw:frame:standard-menus<%>) (basics<%>) args
      (override
       [help-menu:before-about
	(lambda (help-menu)
	  (make-object mred:menu-item%
	    "Introduction to DrScheme"
	    help-menu
	    (lambda (item evt)
	      (drscheme:intro:show-introduction)))
	  (make-object mred:menu-item%
	    "Help Desk"
	    help-menu
	    (lambda (item evt)
	      (help:help-desk))))]

       [help-menu:about (lambda (item evt) (drscheme:app:about-drscheme))]
       [help-menu:about-string (lambda () "DrScheme")]


       [file-menu:new-string (lambda () "")]
       [file-menu:new
	(lambda (item evt)
	  (drscheme:unit:open-drscheme-window))]
       [file-menu:open (lambda (item evt) (fw:handler:open-file) #t)]
       [file-menu:open-string (lambda () "")]
       [file-menu:between-open-and-revert
        (lambda (file-menu) 
          (make-object mred:menu-item% 
                       "Open URL..."
                       file-menu
                       (lambda (item evt)
                         (help:open-users-url this))))])
      
      (sequence 
	(apply super-init args))))

  (define <%> (interface (fw:frame:info<%> basics<%>)))

  (define -mixin
    (mixin (fw:frame:info<%> basics<%>) (<%>) (name)


;      (inherit get-editor)
;      (rename [super-file-menu:print file-menu:print])
;      (override
;	[file-menu:print (lambda (item control)
;			   (parameterize ([mred:current-ps-setup
;					   (let ([ps (make-object mred:ps-setup%)])
;					     (send ps copy-from (mred:current-ps-setup))
;					     (send ps set-mode 'file)
;					     ps)])
;			     (let ([t (get-editor)])
;			       (when (is-a? t drscheme:text:text<%>)
;				 (send t printing-on))
;			       (super-file-menu:print item control)
;			       (when (is-a? t drscheme:text:text<%>)
;				 (send t printing-off))))
;			   #t)])

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
	(super-init name)
	(set! show-menu (make-object (get-menu%) "&Show" (get-menu-bar))))
      
      (private
	[running-message
	 (make-object mred:message% sleepy-bitmap (get-info-panel))]))))

