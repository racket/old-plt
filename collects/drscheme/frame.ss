(unit/sig drscheme:frame^
  (import [mred : mred-interfaces^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:compound-unit : drscheme:compound-unit^]
	  [drscheme:app : drscheme:app^]
	  [zodiac : drscheme:zodiac^])
  
  (define (make-frame% super%)
    (rec drscheme:frame:frame%
	 (class super% (unit)
	   (rename [super-make-root-panel make-root-panel]
		   [super-make-menu-bar make-menu-bar])
	   (inherit panel get-edit save-as info-panel)
	   (override

	     [help-menu:after-about
	      (lambda (help-menu)
		(make-object mred:menu-item%
		  "Help Desk"
		  help-menu
		  (lambda (item evt)
		    (parameterize ([current-namespace (make-namespace)]
				   [mred:current-eventspace (mred:make-eventspace)]
				   [current-custodian (make-custodian)])
		      (let/ec k
			(for-each
			 (lambda (filename) (require-library/proc filename "help"))
			 ((require-library "info.ss" "help")
			  'mred-libraries
			  (lambda ()
			    (mred:message-box "Help Desk"
					      "Cannot load help desk. info.ss format changed")))))))))]
			    
			

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
	     [file-menu:open (lambda () (fw:handler:open-file) #t)]
	     [help-menu:about (lambda () (drscheme:app:about-drscheme))])
	   
	   (sequence 
	     (super-init (send unit get-name)))
	   
	   (private
	     [running-message
	      (make-object mred:message% info-panel sleepy-bitmap)])))))

