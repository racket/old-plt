(unit/sig drscheme:frame^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [mzlib:date : mzlib:date^]
	  [fw : framework^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:app : drscheme:app^]
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


      (inherit get-editor)
      (rename [super-file-menu:print file-menu:print])
      (override
	[file-menu:print (lambda (item control)
			   (let* ([text (get-editor)]
				  [str (string-append
					(mzlib:date:date->string (seconds->date (current-seconds)))
					" "
					(if (string? (send text get-filename))
					    (send text get-filename)
					    "Untitled"))]
				  [modified? (send text is-modified?)])
			     (send text begin-edit-sequence)
			     (send text insert (string #\newline) 0 0 #f)
			     (send text insert str 0 0 #f)
			     (send text change-style
				   (make-object mred:style-delta% 'change-bold)
				   0
				   (string-length str))
			     (super-file-menu:print item control)
			     (send text delete 0 (+ (string-length str) 1) #f)
			     (send text set-modified modified?)
			     (send text end-edit-sequence))
			   #t)])

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

