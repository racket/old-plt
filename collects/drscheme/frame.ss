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

  (define (show-keybindings-to-user bindings)
    (letrec ([f (make-object mred:dialog% "Keybindings" #f #f #f #f #f '(resize-border))]
	     [bp (make-object mred:horizontal-panel% f)]
	     [b-name (make-object mred:button% "Sort by Name" bp (lambda x (update-bindings #f)))]
	     [b-key (make-object mred:button% "Sort by Key" bp (lambda x (update-bindings #t)))]
	     [lb
	      (make-object mred:list-box% #f null f void)]
	     [bp2 (make-object mred:horizontal-panel% f)]
	     [cancel (make-object mred:button% "OK" bp2 (lambda x (send f show #f)))]
             [space (make-object mred:grow-box-spacer-pane% bp2)]
	     [update-bindings
	      (lambda (by-key?)
		(let ([format-binding/name
		       (lambda (b) (format "~a (~a)" (cadr b) (car b)))]
		      [format-binding/key
		       (lambda (b) (format "~a (~a)" (car b) (cadr b)))]
		      [predicate/key
		       (lambda (a b) (string-ci<=? (format "~a" (car a))
						   (format "~a" (car b))))]
		      [predicate/name
		       (lambda (a b) (string-ci<=? (cadr a) (cadr b)))])
		  (send lb set
			(if by-key?
			    (map format-binding/key (mzlib:function:quicksort bindings predicate/key))
			    (map format-binding/name (mzlib:function:quicksort bindings predicate/name))))))])
      (send bp stretchable-height #f)
      (send bp set-alignment 'center 'center)
      (send bp2 stretchable-height #f)
      (send bp2 set-alignment 'right 'center)
      (update-bindings #f)
      (send f show #t)))

  (define basics-mixin
    (mixin (fw:frame:standard-menus<%>) (basics<%>) args
      (inherit get-edit-target-window get-edit-target-object get-menu-bar)
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
	[can-show-keybindings?
	 (lambda ()
           (let ([edit-object (get-edit-target-object)])
	     (and edit-object
		  (is-a? edit-object mred:editor<%>)
		  (let ([keymap (send edit-object get-keymap)])
		    (is-a? keymap fw:keymap:aug-keymap<%>)))))]

        [show-keybindings
         (lambda ()
             (if (can-show-keybindings?)
		 (let ([edit-object (get-edit-target-object)])
		   (let ([keymap (send edit-object get-keymap)])
		     (let*-values ([(menu-names menu-funs) (get-menu-bindings)])
		       (let* ([table (send keymap get-map-function-table/ht
					   (copy-hash-table menu-names))]
			      [structured-list
			       (mzlib:function:quicksort
				(hash-table-map table list)
				(lambda (x y) (string-ci<=? (cadr x) (cadr y))))])
			 (show-keybindings-to-user structured-list)))))
                 (mred:bell)))])
      
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
	  (let ([keybindings-menu-item%
		 (class mred:menu-item% args
		   (inherit enable)
		   (override
		    [on-demand
		     (lambda ()
		       (let ([last-edit-object
			      (get-edit-target-window)])
			 (enable (can-show-keybindings?))))])
		   (sequence (apply super-init args)))])
	    (make-object keybindings-menu-item% "Keybindings" menu
			 (lambda x (show-keybindings))
			 #f
			 "Show the currently active keybindings"))
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

