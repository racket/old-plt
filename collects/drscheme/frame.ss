
(define drscheme:frame@
  (unit/sig drscheme:frame^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [drscheme:rep : drscheme:rep^]
	    [drscheme:basis : drscheme:basis^]
	    [drscheme:setup : drscheme:setup^]
	    [drscheme:tool : drscheme:tool^]
	    [drscheme:compound-unit : drscheme:compound-unit^])
    
    (mred:debug:printf 'invoke "drscheme:frame@")

    (define do-help
      (lambda ()
	(mred:open-hyper-view (build-path mred:plt-home-directory
					  "doc"
					  "drscheme"
					  "index.htm"))))
    
    (define frame%
      (class mred:simple-menu-frame% (name snip)
	(rename [super-make-root-panel make-root-panel]
		[super-make-menu-bar make-menu-bar])
	(inherit panel)
	(public
	  [root-panel #f]
	  [make-root-panel
	   (lambda (% parent)
	     (let* ([s-root (super-make-root-panel mred:vertical-panel% parent)]
		    [root (make-object % s-root)])
	       (send* s-root (border 0) (spacing 0))
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
		       (if (send show-menu checked? imports-id)
			   (cons imports-panel removed)
			   removed)))))]
	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)])
	       (set! show-menu (make-menu))
	       (send mb append show-menu "S&how")
	       (set! imports-id 
		     (send show-menu append-item
			   "&Imports"
			   (lambda () (update-shown))
			   "Show the imports to this unit"
			   #t))
	       (send show-menu check imports-id snip)
	       mb))])

	(public
	  [file-menu:new-string "Compound Unit"]
	  [file-menu:new
	   (lambda ()
	     (make-object drscheme:compound-unit:frame% #f #f #f))]
	  [file-menu:between-new-and-open
	   (lambda (file-menu)
	     (send file-menu append-item "New Unit"
		   (lambda ()
		     (make-object unit-frame% #f #f #f))))])

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
	  (send imports-panel stretchable-in-y #f))))
	

    (define unit-frame%
      (class (mred:make-searchable-frame% frame%) (filename frameset snip [show? #t])
	(inherit canvas edit imports-panel
		 set-title-prefix show-menu
		 show menu-bar% make-menu
		 active-edit active-canvas panel 
		 file-menu file-menu:open-id file-menu:new-id file-menu:save-id 
		 file-menu:save-as-id file-menu:revert-id file-menu:print-id)
	(rename [super-make-menu-bar make-menu-bar]
		[super-update-shown update-shown]
		[super-on-close on-close])
	(public
	  [definitions-id #f]
	  [interactions-id #f]

	  [name-message #f]
	  [save-button #f]
	  [save-init-shown? #f])

	(public
	  [canvas-show-mode #f]
	  [frames frameset]
	  [allow-split? #f]
	  [forced-quit? #f])

	(public
	  [get-canvas% 
	   (let ([%
		  (class mred:simple-frame-canvas% args
		    (inherit get-media)
		    (rename [super-edit-modified edit-modified])
		    (public
		      [edit-renamed
		       (lambda (name)
			 (when save-button
			   (let ([msg (make-object 
				       mred:message% top-panel
				       (if (null? name)
					   "Untitled" 
					   (or (mzlib:file@:file-name-from-path name)
					       "Untitlesd")))])
			     (set! name-message msg)
			     (send top-panel change-children
				   (lambda (l) (build-top-panel-children))))))]
		      [edit-modified
		       (lambda (mod?)
			 (if save-button
			     (send save-button show mod?)
			     (set! save-init-shown? mod?))
			 (super-edit-modified mod?))])
		    (sequence
		      (mred:debug:printf 'super-init "before drscheme:frame::get-canvas%")
		      (apply super-init args)
		      (mred:debug:printf 'super-init "after drscheme:frame::get-canvas%")
		      (let ([m (get-media)])
			(set! save-init-shown? (and (not (null? m)) (send m modified?))))))])
	     (lambda ()
	       %))]
	  [ensure-interactions-shown
	   (lambda ()
	     (unless (send show-menu checked? interactions-id)
	       (send show-menu check interactions-id #t)
	       (update-shown)))])
	
	(public
	  [get-edit%
	   (lambda ()
	     (class mred:scheme-mode-edit% args
	       (public
		 [auto-set-wrap? (mred:get-preference 'drscheme:wrap-program?)])
	       (sequence
		 (mred:debug:printf 'super-init "drscheme frame::get-edit% before.5")
		 (apply super-init args)
		 (mred:debug:printf 'super-init
				    "drscheme frame::get-edit% after.5"))))]
	  
	  [change-to-file
	   (lambda (name)
	     (cond
	       [(and name (file-exists? name))
		(send definitions-edit load-file name)]
	       [name
		(send definitions-edit set-filename name)]
	       [else (send definitions-edit clear)])
	     (send definitions-canvas set-focus))])
	
	(public
	  [file-menu:print-string " Definitions"]
	  [file-menu:print-transcript-id #f]
	  [file-menu:between-print-and-close
	   (lambda (file-menu)
	     (set! file-menu:print-transcript-id
		   (send file-menu append-item "Print Interactions..."
			 (lambda () (send interactions-edit print '()))))
	     (send file-menu append-separator))]

	  [id->child
	   (lambda (id)
	     (cond
	       [(= id interactions-id) interactions-canvas]
	       [(= id definitions-id) definitions-canvas]
	       [else imports-panel]))]
	  [update-shown
	   (lambda ()
	     (super-update-shown)
	     (send panel change-children
		   (lambda (l)
		     (cons (if (send show-menu checked? definitions-id)
			       top-panel
			       scheme-only-panel)
			   (mzlib:function@:foldl
			    (lambda (id sofar)
			      (if (send show-menu checked? id)
				  (cons (id->child id) sofar)
				  sofar))
			    null
			    (list interactions-id definitions-id)))))

	     (send interactions-edit scroll-to-position 
		   (send interactions-edit get-end-position)
		   #f
		   (send interactions-edit get-start-position)
		   1)
	     (send definitions-edit scroll-to-position 
		   (send definitions-edit get-end-position)
		   #f
		   (send definitions-edit get-start-position)	
		   1)
	     (map 
	      (lambda (id)
		(send file-menu enable id (send show-menu checked? definitions-id)))
	      (list file-menu:open-id
		    file-menu:new-id
		    file-menu:save-id
		    file-menu:save-as-id 
		    file-menu:revert-id
		    file-menu:print-id))
	     (send file-menu enable file-menu:print-transcript-id 
		   (send show-menu checked? interactions-id)))]
	  [make-menu-bar
	   (lambda ()
	     (let ([mb (super-make-menu-bar)]
		   [scheme-menu (make-menu)]
		   [tools-menu (make-menu)]
		   [language-menu (make-menu)])
	       
	       (send* mb
		      (append scheme-menu "S&cheme")
		      (append tools-menu "&Tools")
		      (append language-menu "&Language"))

	       (send* language-menu
		      (append-item "Select Library..."
				   (let ([lib-dir "/home/comp210"]
					 [save-dir ""])
				     (lambda ()
				       (set! save-dir 
					     (mred:current-find-file-directory))
				       (mred:current-find-file-directory lib-dir)
				       (let ([lib-file (mred:get-file 
							() 
							"Select a Library" 
                                                        ".*\\.ss$")])
					 (when lib-file
					   (mred:set-preference
					    'drscheme:library-file lib-file)))
				       (set! lib-dir
					     (mred:current-find-file-directory))
				       (mred:current-find-file-directory save-dir))))
		      (append-item "Clear Library"
				   (lambda ()
				     (mred:set-preference 'drscheme:library-file #f)))
		      (append-separator)
		      (append-check-set
		       (map cons drscheme:basis:level-strings
			    drscheme:basis:level-symbols)
		       (let ([state #t])
			 (lambda (s)
			   (mred:set-preference 'drscheme:scheme-level s)
			   (when state
			     (set! state #f)
			     (unless (mred:get-choice
				      "Changes to the language level will not take effect until DrScheme is restarted"
				      "Continue Working"
				      "Exit")
			       (mred:exit)))))
		       (drscheme:basis:level->number
			(mred:get-preference 'drscheme:scheme-level))))

		 

	       
	       (for-each 
		(lambda (x)
		  (let* ([id #f]
			 [callback
			  (lambda ()
			    (unless mred:debug:on?
			      (send tools-menu enable id #f))
			    (drscheme:tool:load/invoke-tool x))])
		    (set! id (send tools-menu append-item (drscheme:tool:tool-name x) callback))))
		drscheme:tool:tools)

	       (send* scheme-menu
		      (append-item "&Indent" 
				   (lambda () 
				     (send (ivar (active-edit) mode) 
					   tabify-selection (active-edit))))
		      (append-item "Indent &All"
				   (lambda ()
				     (send (ivar (active-edit) mode) tabify-all (active-edit)))
				   "" #f "i")
		      (append-item "&Comment Out"
				   (lambda ()
				     (send (ivar (active-edit) mode) 
					   comment-out-selection (active-edit))))
		      (append-item "&Uncomment"
				   (lambda ()
				     (send (ivar (active-edit) mode)
					   uncomment-selection (active-edit)))))
	       (when (mred:get-preference 'drscheme:use-setup?)
		 (send* scheme-menu
			(append-separator)
			(append-item "Setup H&omework..." 
				     (lambda () (drscheme:setup:do-setup "hw")))
			(append-item "Setup &Lab..."
				     (lambda () (drscheme:setup:do-setup "lab")))))

	       (set! definitions-id
		     (send show-menu append-item "&Definitions"
			   (lambda () (update-shown))
			   "Show the definitions in this unit"
			   #t))
	       (set! interactions-id
		     (send show-menu append-item "&Interactions"
			   (lambda () (update-shown))
			   "Show the interactions with this unit"
			   #t))
	       mb))]
	  
	  [on-close
	   (lambda ()
             (when snip
	       (send snip on-close-frame (send definitions-edit get-filename)))
	     (super-on-close))]
	  
	  [running? #t]; is this necessary?
	  
	  [on-forced-quit
	   (lambda ()
	     (set! forced-quit? #t)
	     (let loop ()
	       (if (not (on-close))
		   (loop)))
	     (show #f))]
	  [on-quit
	   (lambda ()
	     (if (on-close)
		 (show #f)))]
	  [execute-callback
	   (lambda (button evt)
	     (let* ([definitions-edit definitions-edit]
		    [interactions-edit interactions-edit])
	       (send interactions-edit reset-console)
	       (send interactions-edit do-many-aries-evals
		     definitions-edit
		     0 (send definitions-edit last-position)
		     (lambda () (void))
		     (lambda ()
		       (send (send interactions-edit get-canvas) set-focus)
		       (send interactions-edit insert-prompt)))))])
	
	(sequence
	  (mred:debug:printf 'super-init "before drscheme:unit-frame%: frameset:~a" frameset)
	  (super-init (cond 
			[snip (ivar snip name)]
			[filename filename]
			[else "Untitled"])
		      snip)
	  (mred:debug:printf 'super-init "after drscheme:unit-frame%"))
	
	(private
	  [top-panel (make-object mred:horizontal-panel% panel)])
	
	(public
	  [definitions-canvas canvas]
	  [definitions-edit edit]
	  [interactions-canvas (make-object mred:console-canvas% panel)]
	  [interactions-edit (make-object drscheme:rep:edit%)])
	
	(sequence
	  (send definitions-edit set-mode (make-object mred:scheme-mode%))
	  (send* interactions-canvas (set-media interactions-edit) (set-frame this))
	  (send interactions-edit set-auto-set-wrap #t)
	  (change-to-file filename))

	(sequence
	  (set! name-message
		(make-object mred:message% top-panel
			     (let ([fn (send definitions-edit get-filename)])
			       (cond
				 [(null? fn) "Untitled"]
				 [(mzlib:file@:file-name-from-path fn)]
				 [else "Untitled"]))))

	  (set! save-button
		(make-object mred:button% 
			     top-panel
			     (lambda args
			       (let* ([edit definitions-edit])
				 (unless (or (null? edit) (not edit))
				   (send edit save-file))))
			     "Save")))
	(private 
	  [make-library-name-msg
	   (lambda (panel n)
	     (make-object mred:message% panel 
			  (if n
			      (let-values ([(base name must-be-dir) (split-path n)])
				name)
			      "")))]
	  [space1 (make-object mred:horizontal-panel% top-panel)]
	  [library-msg (make-library-name-msg
			top-panel
			(mred:get-preference 'drscheme:library-file))]
	  [space2 (make-object mred:horizontal-panel% top-panel)])
	
	(public
	  [stop-execute-button (void)]
	  [execute-button (void)]
	  [button-panel (make-object mred:horizontal-panel% top-panel)]
	  [scheme-only-panel (make-object mred:horizontal-panel% panel)])
	
	(private
	 [scheme-only-library-msg
	  (make-library-name-msg scheme-only-panel
				 (mred:get-preference 'drscheme:library-file))]
	 [scheme-only-space
	  (make-object mred:vertical-panel% scheme-only-panel)]
	 [scheme-only-stop-executing
	  (make-object mred:button% scheme-only-panel
		       (lambda args (send interactions-edit break))
		       "Stop Executing")]
	 [scheme-only-help
	  (make-object mred:button% scheme-only-panel
		       (lambda args (do-help))
		       "Help")])
	 (sequence
	  (send panel delete-child scheme-only-panel)

	  (send save-button show save-init-shown?))
	
	 (sequence
	  (set! execute-button
	    (make-object mred:button% button-panel
			 execute-callback
			 "Execute"))
	  (set! stop-execute-button
		(make-object mred:button% button-panel 
			     (lambda args (send interactions-edit break))
			     "Stop Executing"))
	 (make-object mred:button% button-panel
		       (lambda args (do-help))
		       "Help")
	  
	  (send scheme-only-panel border 1)
	  (send button-panel border 1)
	  (send top-panel border 1)
	  (send top-panel spacing 10)
	  (send scheme-only-panel stretchable-in-y #f)
	  (send button-panel stretchable-in-y #f)
	  (send button-panel stretchable-in-x #f) 
	  (send top-panel stretchable-in-y #f)
	  
	  (mred:add-preference-callback
	   'drscheme:library-file
	   (lambda (p v)
	     (set! scheme-only-library-msg
		   (make-library-name-msg scheme-only-panel v))
	     (set! library-msg (make-library-name-msg top-panel v))
	     (send scheme-only-panel change-children
		   (lambda (l) (list scheme-only-library-msg
				     scheme-only-space
				     scheme-only-stop-executing
				     scheme-only-help)))
	     (send top-panel change-children 
		   (lambda (l) (build-top-panel-children))))))
		     
	(private
	  [build-top-panel-children
	   (lambda ()
	     (list name-message save-button space1 library-msg space2 button-panel))])
	
	(sequence
	  (send show-menu check definitions-id #t)
	  (send show-menu check interactions-id #t)
	  (update-shown)

	  (send interactions-edit initialize-console)
	  (send interactions-edit enable-autoprompt)
	  (send interactions-edit insert-prompt)
	  (send interactions-edit clear-undos)
	  (set-title-prefix "DrScheme")

	  (send definitions-canvas set-focus)
	  (when show?
	    (show #t))
	  (mred:debug:printf 'super-init "drscheme:frame% finished ivars~n"))))

  (mred:insert-format-handler "Units"
                              (list "ss")
				(opt-lambda ([name null] [group #f])
				  (make-object unit-frame% name group #f)))))