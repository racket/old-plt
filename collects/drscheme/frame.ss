(unit/sig drscheme:frame^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
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
	      (let ([help-menu:insert-items
		     (lambda (items)
		       (for-each (lambda (x) (apply (ivar (ivar this help-menu) append-item) x))
				 items))]
		    [reg (regexp "<TITLE>(.*)</TITLE>")])
		(lambda (help-menu)
		  (let* ([dir (with-handlers ([void (lambda (x) #f)]) (collection-path "doc"))])
		    (if (and dir (directory-exists? dir))
			(let* ([dirs (directory-list dir)]
			       [find-title
				(lambda (name)
				  (lambda (port)
				    (let loop ([l (read-line port)])
				      (if (eof-object? l)
					  name
					  (let ([match (regexp-match reg l)])
					    (if match
						(cadr match)
						(loop (read-line port))))))))]
			       [build-item
				(lambda (local-dir output)
				  (let* ([f (build-path dir local-dir "index.htm")])
				    (if (file-exists? f)
					(let ([title (call-with-input-file f (find-title local-dir))])
					  (cons 
					   (list title
						 (lambda ()
						   (let* ([f (make-object mred:hyper-frame:hyper-view-frame%
							       (string-append "file:" f))])
						     (send f set-title-prefix title)
						     f)))
					   output))
					output)))]
			       [item-pairs 
				(mzlib:function:quicksort
				 (mzlib:function:foldl build-item null dirs)
				 (lambda (x y) (string-ci<? (car x) (car y))))])
			  (unless (null? item-pairs)
			    (send help-menu append-separator))
			  (help-menu:insert-items item-pairs))))))]

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
		      (make-object wx:bitmap% p wx:const-bitmap-type-gif)
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
	     [file-menu:open (lambda () (mred:open-file) #t)]
	     [help-menu:about (lambda () (drscheme:app:about-drscheme))])
	   
	   (sequence 
	     (super-init (send unit get-name)))
	   
	   (private
	     [running-message
	      (make-object mred:message% info-panel sleepy-bitmap)])))))

