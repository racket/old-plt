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
      (inherit get-edit-target-object get-menu-bar)
      (private
	[get-menu-bindings
	 (lambda ()
	   (let ([name-ht (make-hash-table)]
		 [fun-ht (make-hash-table)])
	     (let loop ([menu-container (get-menu-bar)])
	       (for-each
		(lambda (item)
		  (when (is-a? item mred:selectable-menu-item<%>)
		    (let ([short-cut (send item get-shortcut)])
		      (when short-cut
			(let ([keyname
			       (fw:keymap:canonicalize-keybinding-string
				(string-append
				 (case (system-type)
				   [(windows) "c:"]
				   [(macos) "d:"]
				   [(unix)
				    (case (send item get-x-shortcut-prefix)
				      [(meta) "m:"]
				      [(alt) "a:"]
				      [(ctl) "c:"]
				      [(ctl-m) "c:m;"])]
				   [else ""])
				 (string short-cut)))])
			  (hash-table-put! name-ht keyname (send item get-plain-label))
			  (hash-table-put! fun-ht keyname
					   (lambda ()
					     (let ([evt (make-object mred:control-event% 'menu)])
					       (send evt set-time-stamp (current-milliseconds))
					       (send item command evt))))))))
		  (when (is-a? item mred:menu-item-container<%>)
		    (loop item)))
		(send menu-container get-items)))
	     (values name-ht fun-ht)))]
		
	[copy-hash-table
	 (lambda (ht)
	   (let ([res (make-hash-table)])
	     (hash-table-for-each
	      ht
	      (lambda (x y) (hash-table-put! res x y)))
	     res))]

        [show-keybindings
         (lambda ()
           (let ([edit-object (get-edit-target-object)])
             (if (and edit-object
                      (is-a? edit-object mred:editor<%>))
                 (let ([keymap (send edit-object get-keymap)])
                   (when (is-a? keymap fw:keymap:aug-keymap<%>)
                     (let*-values ([(menu-names menu-funs) (get-menu-bindings)])
		       (let* ([table (send keymap get-map-function-table/ht
					   (copy-hash-table menu-names))]
			      [structured-list
			       (mzlib:function:quicksort
				(hash-table-map table list)
				(lambda (x y) (string-ci<=? (cadr x) (cadr y))))]
			      [choice
			       (mred:get-choices-from-user
				"Key Bindings" "Choose binding"
				(map
				 (lambda (x) (format "~a (~a)" (cadr x) (car x)))
				 structured-list))])
			 (when choice
			   (let* ([choice-item (list-ref structured-list (car choice))]
				  [choice-key (car choice-item)]
				  [choice-name (cadr choice-item)])
			     (cond
			      [(hash-table-get menu-funs choice-key (lambda () #f))
			       =>
			       (lambda (f) (f))]
			      [else
			       (let ([ke (make-object mred:key-event%)])
				 ;; should set all of ke here, but that is a pain!
				 ;; to do this properly, need to change split out the
				 ;; parser from the normalizer in
				 ;; collects/framework/keymap.ss.
				 (send keymap call-function choice-name edit-object ke #t))])))))))
				     
                 (mred:bell))))])
      
      (override
       [help-menu:before-about
	(lambda (help-menu)
	  (make-object mred:menu-item%
	    "Help Desk"
	    help-menu
	    (lambda (item evt)
	      (help:help-desk)))
	  (make-object mred:menu-item%
	    "Welcome to DrScheme"
	    help-menu
	    (lambda (item evt)
	      (drscheme:app:invite-tour))))]

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
                         (help:open-users-url this))))]
       
       [edit-menu:between-find-and-preferences
        (lambda (menu)
          (make-object mred:separator-menu-item% menu)
          (make-object mred:menu-item% "Keybindings" menu
		       (lambda x (show-keybindings))
		       (and (fw:preferences:get 'framework:menu-bindings)
			    #\h)
		       "Show the currently active keybindings")
          (make-object mred:separator-menu-item% menu))])
      
      (sequence 
	(apply super-init args))))

  (define <%> (interface (fw:frame:editor<%> basics<%> fw:frame:text-info<%>)))

  (define -mixin
    (mixin (fw:frame:editor<%> fw:frame:text-info<%> basics<%>) (<%>) (name)


      (inherit get-editor)
      (rename [super-file-menu:print file-menu:print])
      (override
       [file-menu:print
	(lambda (item control)
	  (let ([ps-setup (make-object mred:ps-setup%)])
	    (send ps-setup copy-from (mred:current-ps-setup))
	    (parameterize ([mred:current-ps-setup ps-setup])
	      (send (get-editor) print))))])

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

