
 (unit/sig mred:frame^
  (import [wx : wx^]
	  [mred:constants : mred:constants^]
	  [mred:console : mred:console^]
	  [mred:preferences : mred:preferences^]
	  [mred:edit : mred:edit^]
	  [mred:container : mred:container^]
	  [mred:canvas : mred:canvas^]
	  [mred:icon : mred:icon^]
	  [mred:menu : mred:menu^] 
	  [mred:group : mred:group^]
	  [mred:finder : mred:finder^]
	  [mred:find-string : mred:find-string^]
	  [mred:hyper-frame : mred:hyper-frame^]
	  [mred:handler : mred:handler^]
	  [mred:keymap : mred:keymap^]
	  [mred:exit : mred:exit^]
	  [mred:autosave : mred:autosave^]
	  [mred:panel : mred:panel^]
	  [mred:gui-utils : mred:gui-utils^]
	  [mred:application : mred:application^]
	  [mzlib:function : mzlib:function^]
	  [mzlib:file : mzlib:file^]
	  [mzlib:date : mzlib:date^])
  
  (mred:debug:printf 'invoke "mred:frame@")
  
  (define frame-width 600)
  (define frame-height 650)
  (let ([w (box 0)]
	[h (box 0)])
    (wx:display-size w h)
    (when (< (unbox w) frame-width)
      (set! frame-width (- (unbox w) 65)))
    (when (< (unbox h) frame-height)
      (set! frame-height (- (unbox h) 65))))
  
  (define make-empty-frame%
    (lambda (super%)
      (rec mred:empty-frame%
	   (class super% args
	     (rename [super-on-frame-active on-frame-active]
		     [super-pre-on-char pre-on-char]
		     [super-on-close on-close]
		     [super-pre-on-event pre-on-event])
	     (sequence (mred:debug:printf 'creation "creating a frame"))
	     (inherit active-canvas)
	     (rename [super-show show])
	     (public
	       [get-panel% (lambda () mred:container:vertical-panel%)]
	       [show
		(lambda (on?)
		  (when on?
			(unless (member this (send mred:group:the-frame-group 
						   get-frames))
			    (send mred:group:the-frame-group 
				  insert-frame this)))
		  (super-show on?))]
	       [can-close? (lambda () (send mred:group:the-frame-group
					    can-remove-frame?
					    this))]
	       [do-close (lambda () 
			   (send mred:group:the-frame-group
				 remove-frame
				 this))]
	       [on-close
		(lambda ()
		  (if (and (super-on-close) 
			   (can-close?))

		      (begin 

			; exit if this is the only frame

			(let ([frames 
			       (send mred:group:the-frame-group 
				     get-frames)])
			  (when (eq? (length frames) 1)
				(mred:exit:exit)))

			(do-close) 
			
			#t)

		      #f))])

	     (sequence 
	       (mred:debug:printf 'super-init "before empty-frame%")
	       (apply super-init args)
	       (mred:debug:printf 'super-init "after empty-frame%"))
	     (public
	       [on-frame-active
		(lambda ()
		  (super-on-frame-active)
		  (send mred:group:the-frame-group set-active-frame this))]
	       [keymap (make-object wx:keymap%)]
	       [make-root-panel
		(lambda (% parent)
		  (make-object % parent))]
	       [panel (make-root-panel (get-panel%) this)])
	     (public
	       [pre-on-char
		(lambda (receiver event)
		  (let* ([canvas (active-canvas)]
			 [edit-should-handle?
			  (and canvas (send canvas is-focus-on?))]
			 [handled? (if edit-should-handle?
				       #f
				       (send keymap handle-key-event this event))])
		    (or handled?
			(super-pre-on-char receiver event))))]
	       [pre-on-event
		(lambda (receiver event)
		  (let* ([canvas (active-canvas)]
			 [canvas-should-handle?
			  (and canvas (send canvas is-focus-on?))]
			 [handled? (if canvas-should-handle?
				       #f
				       (send keymap handle-mouse-event this event))])
		    (or handled?
			(super-pre-on-event receiver event))))])))))
  
  (define make-menu-frame%
    (lambda (super%)
      (rec mred:menu-frame%
	   (class super% args
	     (inherit set-menu-bar get-menu-bar)
	     (public
	       [menu% mred:menu:menu%]
	       [menu-bar% mred:menu:menu-bar%])
	     (public
	       [get-menu-bar% (lambda () menu-bar%)]
	       [get-menu% (lambda () menu%)]
	       [next-menu-id (lambda () 0)]
	       [make-menu-bar
		(lambda ()
		  (make-object (get-menu-bar%)))]
	       [make-menu
		(lambda ()
		  (make-object (get-menu%)))]
	       [on-menu-command
		(lambda (op)
		  (send (get-menu-bar) dispatch op))]
	       
	       [move-help-menu-right
		(lambda ()
		  (let* ([menu-bar (get-menu-bar)]
			 [move-right
			  (lambda (name)
			    (let ([menu (send menu-bar get-menu-named name)])
			      (and menu
				   (begin 
				     (send menu-bar delete menu -1)
				     (send menu-bar append menu name)
				     #t))))])
		    (or (move-right "Help")
			(move-right "&Help"))))])
	     (sequence
	       (mred:debug:printf 'super-init "before mred:menu-frame%")
	       (apply super-init args)
	       (mred:debug:printf 'super-init "after mred:menu-frame%")
	       ; Build and install the menu bar:
	       (let ([menu-bar (make-menu-bar)])
		 (set-menu-bar menu-bar)
		 (send menu-bar set-frame this)
		 (move-help-menu-right)))))))
  
  (define tab (string #\tab))
  
  (mred:preferences:set-preference-default
   'mred:menu-bindings #t 
   (lambda (x) 
     (or (eq? x #t)
	 (eq? x #f))))
  
  (define mred:standard-menus-frameI
    (interface () 
	       edit-menu
	       edit-menu:after-standard-items
	       edit-menu:between-clear-and-select-all
	       edit-menu:between-copy-and-paste
	       edit-menu:between-cut-and-copy
	       edit-menu:between-paste-and-clear
	       edit-menu:between-redo-and-cut
	       edit-menu:between-replace-and-preferences
	       edit-menu:between-select-all-and-find
	       edit-menu:clear
	       edit-menu:clear-help-string
	       edit-menu:clear-id
	       edit-menu:clear-string
	       edit-menu:copy
	       edit-menu:copy-help-string
	       edit-menu:copy-id
	       edit-menu:copy-string
	       edit-menu:cut
	       edit-menu:cut-help-string
	       edit-menu:cut-id
	       edit-menu:cut-string
	       edit-menu:find
	       edit-menu:find-help-string
	       edit-menu:find-id
	       edit-menu:find-string
	       edit-menu:paste
	       edit-menu:paste-help-string
	       edit-menu:paste-id
	       edit-menu:paste-string
	       edit-menu:preferences
	       edit-menu:preferences-help-string
	       edit-menu:preferences-id
	       edit-menu:redo
	       edit-menu:redo-help-string
	       edit-menu:redo-id
	       edit-menu:redo-string
	       edit-menu:replace
	       edit-menu:replace-help-string
	       edit-menu:replace-id
	       edit-menu:replace-string
	       edit-menu:select-all
	       edit-menu:select-all-help-string
	       edit-menu:select-all-id
	       edit-menu:select-all-string
	       edit-menu:undo
	       edit-menu:undo-help-string
	       edit-menu:undo-id
	       edit-menu:undo-string
	       file-menu
	       file-menu:after-quit
	       file-menu:between-close-and-quit
	       file-menu:between-new-and-open
	       file-menu:between-open-and-save
	       file-menu:between-print-and-close
	       file-menu:between-save-and-print
	       file-menu:close
	       file-menu:close-help-string
	       file-menu:close-id
	       file-menu:close-string
	       file-menu:new
	       file-menu:new-help-string
	       file-menu:new-id
	       file-menu:new-string
	       file-menu:open
	       file-menu:open-help-string
	       file-menu:open-id
	       file-menu:open-string
	       file-menu:open-url
	       file-menu:open-url-help-string
	       file-menu:open-url-id
	       file-menu:open-url-string
	       file-menu:print
	       file-menu:print-help-string
	       file-menu:print-id
	       file-menu:print-string
	       file-menu:quit
	       file-menu:quit-help-string
	       file-menu:quit-id
	       file-menu:quit-string
	       file-menu:revert
	       file-menu:revert-help-string
	       file-menu:revert-id
	       file-menu:revert-string
	       file-menu:save
	       file-menu:save-as
	       file-menu:save-as-help-string
	       file-menu:save-as-id
	       file-menu:save-as-string
	       file-menu:save-help-string
	       file-menu:save-id
	       file-menu:save-string
	       help-menu
	       help-menu:about
	       help-menu:about-help-string
	       help-menu:about-id
	       help-menu:about-string
	       help-menu:after-about
	       windows-menu)) 

  (define make-standard-menus-frame%
    (lambda (super%)
      (let ([join (opt-lambda (base special [suffix ""])
		    (if (string=? special "")
			(string-append base suffix)
			(string-append base " " special suffix)))])
	;(begin-elaboration-time (reference-library "macro.ss"))
	(begin-elaboration-time
	 (let-struct between (menu name separator)
	   (let-struct an-item (name help-string proc key menu-string-before menu-string-after)
	     (letrec* ([wx:platform
			;; mrspidey hack
			(if (defined? 'wx:platform)
			    (global-defined-value 'wx:platform)
			    (if (= 0 (modulo 43 13))
				'unix
				(if (= 1 (modulo 43 13))
				    'windows
				    'macintosh)))]
		       [build-id
			(lambda (name post)
			  (let* ([name-string (symbol->string name)]
				 [answer (string->symbol (string-append name-string post))])
			    answer))]
		       [build-public-clause
			(opt-lambda (item)
			  (let ([name (an-item-name item)]
				[help-string (an-item-help-string item)]
				[proc (an-item-proc item)])
			    `(public 
			       [,name ,proc]
			       [,(build-id name "-id") #f]
			       [,(build-id name "-string") ""]
			       [,(build-id name "-help-string") ,help-string])))]
		       [build-proc-clause
			(lambda (item)
			  (let* ([name (an-item-name item)]
				 [name-string (symbol->string name)]
				 [menu-before-string (an-item-menu-string-before item)]
				 [menu-after-string (an-item-menu-string-after item)]
				 [key (an-item-key item)]
				 [file-menu? (string=? (substring name-string 0 9) "file-menu")]
				 [edit-menu? (string=? (substring name-string 0 9) "edit-menu")]
				 [windows-menu? (string=? (substring name-string 0 9) "windows-m")]
				 [help-menu? (string=? (substring name-string 0 9) "help-menu")])
			    `(begin
			       (when ,name
				 (set! ,(build-id name "-id")
				       (send ,(cond
						[file-menu? 'file-menu]
						[edit-menu? 'edit-menu]
						[windows-menu? 'windows-menu]
						[help-menu? 'help-menu]
						[else '(begin (printf "WARNING: defaulting item to help-menu ~s~n" name-string)
							      help-menu)])
					     append-item 
					     (join ,menu-before-string 
						   ,(build-id name "-string")
						   ,menu-after-string)
					     ,name ,(build-id name "-help-string")
					     #f 
					     ,key))))))]
		       [build-between-ivar
			(lambda (between)
			  (string->symbol 
			   (string-append (symbol->string (between-menu between))
					  ":"
					  (symbol->string (between-name between)))))]
		       [build-between-proc-clause
			(lambda (between)
			  `(,(build-between-ivar between) ,(between-menu between)))]
		       [build-between-public-clause
			(lambda (between)
			  `(public
			     [,(build-between-ivar between) 
			      ,(if (between-separator between)
				   `(lambda (m) (send m append-separator))
				   `(lambda (x) (void)))]))]
		       [build-make-menu-bar
			(lambda (items)
			  (let ([mb (gensym "mb")])
			    `(lambda ()
			       (let ([,mb (super-make-menu-bar)])
				 (set! file-menu (make-menu))
				 (set! edit-menu (make-menu))
				 (when windows-menu
				   (set! windows-menu (make-menu)))
				 (set! help-menu (make-menu))

				 (send ,mb append file-menu
				       (if (eq? wx:platform 'windows)
					   "&File" "F&ile"))
				 (send ,mb append edit-menu "&Edit")
				 (when windows-menu
				   (send ,mb append windows-menu "&Windows"))
				 (send ,mb append help-menu "&Help")

				 ,@(map (lambda (x)
					  (if (between? x)
					      (build-between-proc-clause x)
					      (build-proc-clause x)))
					items)
				 ,mb))))]
		       [items
			(list (make-an-item 'file-menu:new "Open a new file"
					    '(lambda () (mred:handler:edit-file #f) #t)
					    "n" "&New" "")
			      (make-between 'file-menu 'between-new-and-open #f)
			      (make-an-item 'file-menu:open "Open a file from disk"
					    '(lambda () (mred:handler:open-file) #t)
					    "o" "&Open" "...")
			      (make-an-item 'file-menu:open-url "Open a Uniform Resource Locater"
					    '(lambda () (mred:handler:open-url) #t)
					    #f "Open &URL" "...")
			      (make-an-item 'file-menu:revert 
					    "Revert this file to the copy on disk"
					    #f #f "&Revert" "")
			      (make-between 'file-menu 'between-open-and-save #f)
			      (make-an-item 'file-menu:save "" #f "s" "&Save" "")
			      (make-an-item 'file-menu:save-as "" #f #f "Save" " &As...")
			      (make-between 'file-menu 'between-save-and-print #t)
			      (make-an-item 'file-menu:print "" #f "p" "&Print" "...")
			      (make-between 'file-menu 'between-print-and-close #t)
			      (make-an-item 'file-menu:close "" 
					    '(lambda () (when (on-close) (show #f)) #t)
					    "w" "&Close" "")
			      (make-between 'file-menu 'between-close-and-quit #f)
			      (make-an-item 'file-menu:quit "" '(lambda () (mred:exit:exit))
					    "q"
					    '(if (eq? wx:platform 'windows) "E&xit" "Quit")
					    "")
			      (make-between 'file-menu 'after-quit #f)
			      
			      (make-an-item 'edit-menu:undo "" #f "z" "&Undo" "")
			      (make-an-item 'edit-menu:redo "" #f "y" "&Redo" "")
			      (make-between 'edit-menu 'between-redo-and-cut #t)
			      (make-an-item 'edit-menu:cut "" #f "x" "Cu&t" "")
			      (make-between 'edit-menu 'between-cut-and-copy #f)
			      (make-an-item 'edit-menu:copy "" #f "c" "&Copy" "")
			      (make-between 'edit-menu 'between-copy-and-paste #f)
			      (make-an-item 'edit-menu:paste "" #f "v" "&Paste" "")
			      (make-between 'edit-menu 'between-paste-and-clear #f)
			      (make-an-item 'edit-menu:clear "" #f #f
					    '(if (eq? wx:platform 'macintosh)
						 "Clear"
						 "&Delete")
					    "")
			      (make-between 'edit-menu 'between-clear-and-select-all #f)
			      (make-an-item 'edit-menu:select-all "" #f "a" "Select A&ll" "")
			      (make-between 'edit-menu 'between-select-all-and-find #t)
			      (make-an-item 'edit-menu:find "Search for a string in the buffer"
					    '(lambda () (send this move-to-search-or-search) #t)
					    "f" "Find" "")
			      (make-an-item 'edit-menu:replace "Search and replace a string in the buffer"
					    #f #f "Replace" "")
			      (make-between 'edit-menu 'between-replace-and-preferences #t)
			      (make-an-item 'edit-menu:preferences "Configure your preferences"
					    '(lambda () (mred:preferences:show-preferences-dialog) #t)
					    #f "Preferences..." "")
			      (make-between 'edit-menu 'after-standard-items #f)
			      
			      (make-an-item 'help-menu:about "About this application"
					    '(lambda () (mred:console:credits))
					    #f
					    "About "
					    "...")
			      (make-between 'help-menu 'after-about #f))])
	       `(rec mred:instance-standard-menus-frame%
		     (class*-asi super% (mred:standard-menus-frameI)
		       (inherit make-menu on-close)
		       (rename [super-make-menu-bar make-menu-bar]
			       [super-show show]
			       [super-do-close do-close])
		       (private [get-standard-menu-close-item 
				 (lambda (frame)
				   (let* ([close-string (if (eq? wx:platform 'windows)
							    "&Close"
							    "Close")]
					  [file-menu (ivar frame file-menu)])
				     (if file-menu 
					 (send file-menu find-item close-string)
					 #f)))]
				[set-close-menu-item-state! 
				 (lambda (frame state)
				   (when (is-a? frame mred:standard-menus-frameI)
					 (let ([close-menu-item 
						(get-standard-menu-close-item frame)])
					   (when close-menu-item
						 (send (ivar frame file-menu) 
						       enable close-menu-item state)))))])
		       (public [file-menu 'file-menu-uninitialized]
			       [edit-menu 'edit-menu-uninitialized]
			       [windows-menu 'windows-menu-uninitialized]
			       [help-menu 'help-menu-uninitialized]
			       [show 
				(lambda (on?)
				  (super-show on?)
				  (when on?
					(let ([frames (send mred:group:the-frame-group get-frames)])

					  (if (eq? (length frames) 1)

					      ; disable File|Close if frame is singleton

					      (set-close-menu-item-state! this #f)

					      ; otherwise, enable for all frames

					      (send mred:group:the-frame-group
						    for-each-frame
						    (lambda (a-frame)
						      (set-close-menu-item-state! a-frame #t)))))))]
			       [do-close
				(lambda ()

				  (super-do-close)

				  (let ([frames (send mred:group:the-frame-group 
						      get-frames)])

				    ; disable File|Close if remaining frame is singleton

				    (when (eq? (length frames) 1)
					  (set-close-menu-item-state! (car frames) #f))))])

		       ,@(map (lambda (x)
				(if (between? x)
				    (build-between-public-clause x)
				    (build-public-clause x)))
			      items)
		       (public
			 [make-menu-bar
			  ,(build-make-menu-bar items)]))))))))))
  
  
  (mred:preferences:set-preference-default 'mred:print-output-mode
					   0
					   (lambda (x) (or (= x 0) (= x 1))))
  
  (define make-simple-frame%
    (lambda (super%)
      (rec mred:simple-frame%
	   (class super% ([name (mred:application:current-app-name)])
	     (inherit panel get-client-size set-icon get-menu-bar
		      make-menu show active-edit active-canvas)
	     (rename [super-can-close? can-close?]
		     [super-make-menu-bar make-menu-bar]
		     [super-set-title set-title])
	     (public
	       [WIDTH frame-width]
	       [HEIGHT frame-height])
	     
	     (public
	       [can-close?
		(lambda ()
		  (and (send (get-edit) do-close)
		       (super-can-close?)))])
	     
	     (public
	       [get-panel%  (lambda () mred:panel:vertical-edit-panel%)]
	       [title-prefix name])
	     
	     (private
	       [title ""]
	       
	       [do-title
		(lambda ()
		  (super-set-title (get-entire-title))
		  (send mred:group:the-frame-group frame-title-changed this))])
	     
	     (public
	       [get-entire-title
		(lambda ()
		  (if (or (string=? "" title)
			  (string=? "" title-prefix))
		      (string-append title-prefix title)
		      (string-append title " - " title-prefix)))]
	       
	       [get-title (lambda () title)]
	       [set-title
		(lambda (t)
		  (when (and (string? t)
			     (not (string=? t title)))
		    (set! title t)
		    (do-title)))]
	       [get-title-prefix (lambda () title-prefix)]
	       [set-title-prefix
		(lambda (s)
		  (when (and (string? s)
			     (not (string=? s title-prefix)))
		    (set! title-prefix s)
		    (do-title)))]
	       [get-canvas% (lambda () mred:canvas:frame-title-canvas%)]
	       [get-edit% (lambda () mred:edit:media-edit%)]
	       [make-edit (lambda () (make-object (get-edit%)))])
	     
	     (public
	       [save-as
		(opt-lambda ([format wx:const-media-ff-same])
		  (let ([file (parameterize ([mred:finder:dialog-parent-parameter
					      this])
				(mred:finder:put-file))])
		    (when file
		      (send (get-edit) save-file file format))))]
	       [file-menu:revert 
		(lambda () 
		  (let* ([b (box #f)]
			 [edit (get-edit)]
			 [filename (send edit get-filename b)])
		    (if (or (null? filename) (unbox b))
			(wx:bell)
			(let-values ([(start end)
				      (if (is-a? edit wx:media-edit%)
					  (values (send edit get-start-position)
						  (send edit get-end-position))
					  (values #f #f))])
			  (send edit begin-edit-sequence)
			  (let ([status (send edit load-file
					      filename
					      wx:const-media-ff-same
					      #f)])
			    (if status
				(begin
				  (when (is-a? edit wx:media-edit%)
				    (send edit set-position start end))
				  (send edit end-edit-sequence))
				(begin
				  (send edit end-edit-sequence)
				  (mred:gui-utils:message-box
				   (format "could not read ~a" filename)
				   "Error Reverting"))))))
		    #t))]
	       [file-menu:save (lambda ()
				 (send (get-edit) save-file)
				 #t)]
	       [file-menu:save-as (lambda () (save-as) #t)]
	       [file-menu:between-print-and-close
		(lambda (file-menu)
		  (send file-menu append-separator)
		  (let ([split
			 (lambda (panel%)
			   (lambda ()
			     (when (active-canvas)
			       (send panel split (active-canvas) panel%))))])
		    (send file-menu append-item "Split Horizontally" (split mred:container:horizontal-panel%))
		    (send file-menu append-item "Split Vertically" (split mred:container:vertical-panel%))
		    (send file-menu append-item "Collapse"
			  (lambda ()
			    (when (active-canvas)
			      (send panel collapse (active-canvas))))))
		  (send file-menu append-separator))]
	       [file-menu:print (lambda ()
				  (send (get-edit) print
					'()
					#t
					#t
					(mred:preferences:get-preference 'mred:print-output-mode))
				  #t)])
	     
	     
	     (private
	       [edit-menu:do (lambda (const)
			       (lambda () 
				 (let ([edit (active-edit)])
				   (when edit
				     (send edit do-edit const)))
				 #t))])
	     
	     (public
	       [edit-menu:undo (edit-menu:do wx:const-edit-undo)]
	       [edit-menu:redo (edit-menu:do wx:const-edit-redo)]
	       [edit-menu:cut (edit-menu:do wx:const-edit-cut)]
	       [edit-menu:clear (edit-menu:do wx:const-edit-clear)]
	       [edit-menu:copy (edit-menu:do wx:const-edit-copy)]
	       [edit-menu:paste (edit-menu:do wx:const-edit-paste)]
	       [edit-menu:select-all (edit-menu:do wx:const-edit-select-all)]
	       [edit-menu:replace (lambda ()
				    (when (active-canvas)
				      (mred:find-string:find-string
				       (active-canvas)
				       (active-edit)
				       -1 -1 (list 'replace 'ignore-case))))]
	       
	       [edit-menu:between-replace-and-preferences
		(lambda (edit-menu)
		  (send edit-menu append-separator)
		  (send edit-menu append-item "Insert Text Box"
			(edit-menu:do wx:const-edit-insert-text-box))
		  (send edit-menu append-item "Insert Graphic Box"
			(edit-menu:do wx:const-edit-insert-graphic-box))
		  (send edit-menu append-item "Insert Image..."
			(edit-menu:do wx:const-edit-insert-image))
		  (send edit-menu append-item "Toggle Wrap Text"
			(lambda ()
			  (let ([edit (active-edit)])
			    (when edit
			      (send edit set-auto-set-wrap (not (ivar edit auto-set-wrap?)))
			      (send (active-canvas) force-redraw)))))
		  (send edit-menu append-separator))])
	     
	     (public
	       [help-menu:about (lambda () (mred:console:credits))]
	       [help-menu:about-string (mred:application:current-app-name)]
	       [help-menu:compare string-ci<?]
	       [help-menu:insert-items
		(lambda (items)
		  (for-each (lambda (x) (apply (ivar (ivar this help-menu) append-item) x))
			    items))]
	       [help-menu:after-about
		(let ([reg (regexp "<TITLE>(.*)</TITLE>")])
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
					  (begin (mred:debug:printf 'help-menu "couldn't find ~a" f)
						 output))))]
				 [item-pairs 
				  (mzlib:function:quicksort
				   (mzlib:function:foldl build-item null dirs)
				   (lambda (x y) (help-menu:compare (car x) (car y))))])
			    (unless (null? item-pairs)
			      (send help-menu append-separator))
			    (help-menu:insert-items item-pairs))
			  (mred:debug:printf 'help-menu "couldn't find PLTHOME/doc directory")))))])
	     
	     (sequence
	       (mred:debug:printf 'super-init "before simple-frame%")
	       (super-init () name -1 -1 WIDTH HEIGHT
			   (+ wx:const-default-frame wx:const-sdi)
			   name)
	       (mred:debug:printf 'super-init "after simple-frame%"))
	     
	     (public
	       [get-canvas (let ([c #f])
			     (lambda () 
			       (unless c
				 (set! c (make-object (get-canvas%) panel))
				 (send c set-media (get-edit)))
			       c))]
	       [get-edit (let ([e #f])
			   (lambda () 
			     (unless e 
			       (set! e (make-edit))
			       (send (get-canvas) set-media e))
			     e))])
	     (sequence
	       (let ([icon (mred:icon:get-icon)])
		 (when (send icon ok?)
		   (set-icon icon)))
	       (mred:debug:printf 'matthew "initial call to do-title~n")
	       (do-title)
	       (mred:debug:printf 'matthew "finished initial call to do-title~n")
	       (let ([canvas (get-canvas)])
		 (mred:debug:printf 'matthew "got canvas.1 ~a~n" canvas)
		 (mred:debug:printf 'matthew "got canvas.2 ~a~n" (ivar canvas set-focus))
		 (send canvas set-focus))
	       (mred:debug:printf 'matthew "after initial call to set-focus~n"))))))
  
  (define make-searchable-frame%
    (let* ([anchor 0]
	   [searching-direction 1]
	   [old-highlight void]
	   [get-active-embedded-edit
	    (lambda (edit)
	      (let loop ([edit edit])
		(let ([snip (send edit get-focus-snip)])
		  (if (or (null? snip)
			  (not (is-a? snip wx:media-snip%)))
		      edit
		      (loop (send snip get-this-media))))))]
	   [clear-highlight
	    (lambda ()
	      (begin (old-highlight)
		     (set! old-highlight void)))]
	   [reset-anchor
	    (let ([color (make-object wx:colour% "BLUE")])
	      (lambda (edit)
		(old-highlight)
		(let ([position 
		       (if (= 1 searching-direction)
			   (send edit get-end-position)
			   (send edit get-start-position))])
		  (set! anchor position)
		  (set! old-highlight
			(send edit highlight-range position position color #f)))))]
	   [replace-edit (make-object mred:edit:media-edit%)]
	   [find-edit
	    (make-object
	     (class-asi mred:edit:media-edit%
	       (inherit get-text)
	       (rename [super-after-insert after-insert]
		       [super-after-delete after-delete]
		       [super-on-focus on-focus])
	       (public
		 [searching-frame #f]
		 [set-searching-frame
		  (lambda (frame)
		    (set! searching-frame frame))]
		 [get-searching-edit
		  (lambda ()
		    (get-active-embedded-edit
		     (send searching-frame get-edit-to-search)))]
		 [search
		  (opt-lambda ([reset-anchor? #t] [beep? #t] [wrap? #t])
		    (when searching-frame
		      (let* ([string (get-text)]
			     [searching-edit (get-searching-edit)]
			     [not-found
			      (lambda (found-edit)
				(send found-edit set-position anchor)
				(when beep?
				  (wx:bell))
				#f)]
			     [found
			      (lambda (edit first-pos)
				(let ([last-pos (+ first-pos (* searching-direction 
								(string-length string)))])
				  (send* edit 
				    (set-caret-owner null wx:const-focus-display)
				    (set-position
				     (min first-pos last-pos)
				     (max first-pos last-pos)))
				  #t))])
			(when reset-anchor?
			  (reset-anchor searching-edit))
			(let-values ([(found-edit first-pos)
				      (send searching-edit
					    find-string-embedded
					    string
					    searching-direction
					    anchor
					    -1 #t #t #t)])
			  (cond
			    [(= -1 first-pos)
			     (if wrap?
				 (let-values ([(found-edit pos)
					       (send searching-edit
						     find-string-embedded
						     string 
						     searching-direction
						     (if (= 1 searching-direction)
							 0
							 (send searching-edit last-position)))])
				   (if (= -1 pos)
				       (not-found found-edit)
				       (found found-edit 
					      ((if (= searching-direction 1)
						   +
						   -)
					       pos
					       (string-length string)))))
				 (not-found found-edit))]
			    [else
			     (found found-edit first-pos)])))))]
		 [on-focus
		  (lambda (on?)
		    (when on?
		      (reset-anchor (get-searching-edit)))
		    (super-on-focus on?))]
		 [after-insert
		  (lambda args
		    (apply super-after-insert args)
		    (search #f))]
		 [after-delete
		  (lambda args
		    (apply super-after-delete args)
		    (search #f))])))]
	   [canvas% 
	    (class-asi mred:canvas:one-line-canvas%
	      (inherit get-parent frame)
	      (rename [super-on-set-focus on-set-focus])
	      (public
		[lines 2]
		[style-flags wx:const-mcanvas-hide-h-scroll]
		[on-set-focus
		 (lambda ()
		   (mred:debug:printf 'matthew "searching frame::on-set-focus.1~n")
		   (send find-edit set-searching-frame frame)
		   (mred:debug:printf 'matthew "searching frame::on-set-focus.2~n")
		   (super-on-set-focus)
		   (mred:debug:printf 'matthew "searching frame::on-set-focus.3~n"))]))])
      (for-each (lambda (keymap)
		  (send keymap chain-to-keymap
			mred:keymap:global-search-keymap
			#t))
		(list (send find-edit get-keymap)
		      (send replace-edit get-keymap)))
      (lambda (super%)
	(rec mred:searchable-frame%
	     (class super% args
	       (inherit active-edit active-canvas get-edit)
	       (rename [super-make-root-panel make-root-panel]
		       [super-on-activate on-activate]
		       [super-do-close do-close])
	       (private
		 [super-root 'unitiaialized-super-root])
	       (public
		 [make-root-panel
		  (lambda (% parent)
		    (let* ([s-root (super-make-root-panel
				    mred:container:vertical-panel%
				    parent)]
			   [root (make-object % s-root)])
		      (set! super-root s-root)
		      root))])
	       (public
		 [on-activate
		  (lambda (on?)
		    (unless hidden?
		      (if on?
			  (reset-anchor (get-edit-to-search))
			  (clear-highlight)))
		    (super-on-activate on?))]	
		 [get-edit-to-search
		  (lambda () 
		    (get-edit))]
		 [hide-search
		  (opt-lambda ([startup? #f])
		    (send super-root delete-child search-panel)
		    (clear-highlight)
		    (unless startup?
			    (send 
			     (send (get-edit-to-search) get-canvas) 
			     set-focus))
		    (set! hidden? #t))]
		 [unhide-search
		  (lambda ()
		    (set! hidden? #f)
		    (send super-root add-child search-panel)
		    (reset-anchor (get-edit-to-search)))])
	       (public
		 [do-close
		  (lambda ()
		    (super-do-close)
		    (let ([close-canvas
			   (lambda (canvas edit)
			     (send edit remove-canvas canvas)
			     (send canvas set-media ()))])
		      (close-canvas find-canvas find-edit)
		      (close-canvas replace-canvas replace-edit))
		    (when (eq? this (ivar find-edit searching-frame))
		      (send find-edit set-searching-frame #f)))]
		 [set-search-direction 
		  (lambda (x) 
		    (set! searching-direction x)
		    (send dir-radio set-selection (if (= x 1) 0 1)))]
		 [replace&search
		  (lambda ()
		    (when (replace)
		      (search)))]
		 [replace-all
		  (lambda ()
		    (let* ([replacee-edit (get-edit-to-search)]
			   [pos (if (= searching-direction 1)
				    (send replacee-edit get-start-position)
				    (send replacee-edit get-end-position))]
			   [get-pos 
			    (if (= searching-direction 1)
				(ivar replacee-edit get-end-position)
				(ivar replacee-edit get-start-position))]
			   [done? (if (= 1 searching-direction)
				      (lambda (x) (>= x (send replacee-edit last-position)))
				      (lambda (x) (<= x 0)))])
		      (send* replacee-edit 
			(begin-edit-sequence)
			(set-position pos))
		      (when (search)
			(send replacee-edit set-position pos)
			(let loop ()
			  (when (send find-edit search #t #f #f)
			    (replace)
			    (loop))))
		      (send replacee-edit end-edit-sequence)))]
		 [replace
		  (lambda ()
		    (let* ([search-text (send find-edit get-text)]
			   [replacee-edit (get-edit-to-search)]
			   [replacee-start (send replacee-edit get-start-position)]
			   [new-text (send replace-edit get-text)]
			   [replacee (send replacee-edit get-text
					   replacee-start
					   (send replacee-edit get-end-position))])
		      (if (string=? replacee search-text)
			  (begin (send replacee-edit insert new-text)
				 (send replacee-edit set-position
				       replacee-start
				       (+ replacee-start (string-length new-text)))
				 #t)
			  #f)))]
		 [toggle-search-focus
		  (lambda ()
		    (when hidden?
		      (unhide-search))
		    (send (cond
			    [(send find-canvas is-focus-on?)
			     replace-canvas]
			    [(send replace-canvas is-focus-on?)
			     (send (get-edit-to-search) get-canvas)]
			    [else
			     find-canvas])
			  set-focus))]
		 [move-to-search-or-search
		  (lambda ()
		    (when hidden?
		      (unhide-search))
		    (if (or (send find-canvas is-focus-on?)
			    (send replace-canvas is-focus-on?))
			(search 1)
			(send find-canvas set-focus)))]
		 [move-to-search-or-reverse-search
		  (lambda ()
		    (when hidden?
		      (unhide-search))
		    (if (or (send find-canvas is-focus-on?)
			    (send replace-canvas is-focus-on?))
			(search -1)
			(send find-canvas set-focus)))]
		 [search
		  (opt-lambda ([direction searching-direction] [beep? #t])
		    
		    (send find-edit set-searching-frame this)
		    (when hidden?
		      (unhide-search))
		    (set-search-direction direction)
		    (send find-edit search #t beep?))])
	       (sequence
		 (mred:debug:printf 'super-init "before searchable-frame%")
		 (apply super-init args)
		 (mred:debug:printf 'super-init "after searchable-frame%"))
	       (private
		 [search-panel (make-object mred:container:horizontal-panel% super-root)]
		 
		 [left-panel (make-object mred:container:vertical-panel% search-panel)]
		 [find-canvas (make-object canvas% left-panel)]
		 [replace-canvas (make-object canvas% left-panel)]
		 
		 [middle-left-panel (make-object mred:container:vertical-panel% search-panel)]
		 [middle-middle-panel (make-object mred:container:vertical-panel% search-panel)]
		 [middle-right-panel (make-object mred:container:vertical-panel% search-panel)]
		 
		 [search-button (make-object mred:container:button% middle-left-panel 
					     (lambda args (search)) "Search")]
		 
		 [replace&search-button (make-object mred:container:button% middle-middle-panel 
						     (lambda x (replace&search)) "Replace && Search")]
		 [replace-button (make-object mred:container:button% middle-left-panel (lambda x (replace)) "Replace")]
		 [replace-all-button (make-object mred:container:button% middle-middle-panel
						  (lambda x (replace-all)) "Replace To End")]
		 
		 [dir-radio (make-object mred:container:radio-box% middle-right-panel
					 (lambda (dir-radio evt)
					   (let ([forward (if (= 0 (send evt get-command-int))
							      1
							      -1)])
					     (set-search-direction forward)
					     (reset-anchor (get-edit-to-search))))
					 null
					 -1 -1 -1 -1
					 (list "Forward" "Backward"))]
		 [close-button (make-object mred:container:button% middle-right-panel
					    (lambda args (hide-search)) "Hide")]
		 [hidden? #f])
	       (sequence
		 (let ([align
			(lambda (x y)
			  (let ([m (max (send x get-width)
					(send y get-width))])
			    (send x user-min-width m)
			    (send y user-min-width m)))])
		   (align search-button replace-button)
		   (align replace&search-button replace-all-button))
		 (for-each (lambda (x) (send x major-align-center))
			   (list middle-left-panel middle-middle-panel))
		 (for-each (lambda (x) (send x stretchable-in-y #f))
			   (list search-panel left-panel middle-left-panel middle-middle-panel middle-right-panel))
		 (for-each (lambda (x) (send x stretchable-in-x #f))
			   (list middle-left-panel middle-middle-panel middle-right-panel))
		 (send find-canvas set-media find-edit)
		 (send replace-canvas set-media replace-edit) 
		 (send find-edit add-canvas find-canvas)
		 (send replace-edit add-canvas replace-canvas)
		 (hide-search #t)))))))
  
  (let ([boolean? 
	 (lambda (x)
	   (or (not x)
	       (eq? x #t)))])
    (mred:preferences:set-preference-default 'mred:show-status-line
					     #t
					     boolean?)
    (mred:preferences:set-preference-default 'mred:line-offsets
					     #t
					     boolean?))
  
  
  (define make-info-frame%
    (let* ([time-edit (make-object mred:edit:media-edit%)]
	   [time-semaphore (make-semaphore 1)]
	   [wide-time "00:00pm"]
	   [_ (send time-edit lock #t)]
	   [update-time
	    (lambda ()
	      (dynamic-wind
	       (lambda ()
		 (semaphore-wait time-semaphore)
		 (send time-edit lock #f))
	       (lambda ()
		 (send* time-edit 
		   (erase)
		   (insert 
		    (let* ([date (seconds->date
				  (current-seconds))]
			   [hours (date-hour date)]
			   [minutes (date-minute date)])
		      (format "~a:~a~a~a"
			      (cond
				[(= hours 0) 12]
				[(<= hours 12) hours]
				[else (- hours 12)])
			      (quotient minutes 10)
			      (modulo minutes 10)
			      (if (< hours 12) "am" "pm"))))))
	       (lambda ()
		 (send time-edit lock #t)
		 (semaphore-post time-semaphore))))]
	   [time-thread
	    (thread
	     (rec loop
		  (lambda ()
		    (update-time)
		    (sleep 30)
		    (loop))))])
      (lambda (super-info%)
	(rec mred:info-frame%
	     (class super-info% args
	       (rename [super-make-root-panel make-root-panel])
	       (private
		 [rest-panel 'uninitialized-root]
		 [super-root 'uninitialized-super-root])
	       (public
		 [make-root-panel
		  (lambda (% parent)
		    (let* ([s-root (super-make-root-panel
				    mred:container:vertical-panel%
				    parent)]
			   [r-root (make-object % s-root)])
		      (set! super-root s-root)
		      (set! rest-panel r-root)
		      r-root))])
	       
	       (public
		 [determine-width
		  (let ([magic-space 25])
		    (lambda (string canvas edit)
		      (send edit set-autowrap-bitmap null)
		      (send canvas call-as-primary-owner
			    (lambda ()
			      (let ([lb (box 0)]
				    [rb (box 0)])
				(send edit erase)
				(send edit insert string)
				(send edit position-location 
				      (send edit last-position)
				      rb)
				(send edit position-location 0 lb)
				(send canvas user-min-width 
				      (+ magic-space (- (unbox rb) (unbox lb)))))))))])
	       
	       (rename [super-do-close do-close])
	       (private
		 [close-panel-callback
		  (mred:preferences:add-preference-callback
		   'mred:show-status-line
		   (lambda (p v)
		     (if v 
			 (register-gc-blit)
			 (wx:unregister-collecting-blit gc-canvas))
		     (send super-root change-children
			   (lambda (l)
			     (if v
				 (list rest-panel info-panel)
				 (list rest-panel))))))])
	       (public
		 [do-close
		  (lambda ()
		    (super-do-close)
		    (send time-canvas set-media null)
		    (wx:unregister-collecting-blit gc-canvas)
		    (close-panel-callback))])
	       
	       (inherit get-edit)
	       (public
		 [get-info-edit
		  (lambda ()
		    (and (procedure? get-edit)
			 (get-edit)))])
	       
	       (public
		 [lock-status-changed
		  (let ([icon-currently-locked? #f])
		    (lambda ()
		      (let ([info-edit (get-info-edit)])
			(when info-edit
			  (let ([locked-now? (ivar info-edit locked?)])
			    (unless (eq? locked-now? icon-currently-locked?)
			      (mred:debug:printf 'lock-icon 
						 "lock-icon: setting to: ~a"
						 locked-now?)
			      (set! icon-currently-locked? locked-now?)
			      (let ([label
				     (if locked-now?
					 (cons (mred:icon:get-lock-mdc)
					       (mred:icon:get-lock-bitmap))
					 (cons (mred:icon:get-unlock-mdc)
					       (mred:icon:get-unlock-bitmap)))])
				(send lock-message
				      set-label
				      (if (send (car label) ok?)
					  label
					  (if locked-now? "Locked" "Unlocked"))))))))))])
	       (public
		 [update-info
		  (lambda ()
		    (lock-status-changed))])
	       (sequence 
		 (apply super-init args))
	       
	       (public
		 [info-panel (make-object mred:container:horizontal-panel% 
					  super-root)])
	       (private
		 [lock-message (make-object mred:container:canvas-message%
					    info-panel 
					    (let ([b (mred:icon:get-unlock-bitmap)])
					      (if (send b ok?)
						  (cons (mred:icon:get-unlock-mdc) b)
						  "Unlocked"))
					    -1 -1 wx:const-border)]
		 [time-canvas (make-object mred:canvas:one-line-canvas% 
					   info-panel)]
		 [gc-canvas (make-object mred:container:canvas% info-panel
					 -1 -1 -1 -1 wx:const-border)]
		 [register-gc-blit
		  (lambda ()
		    (let ([mdc (mred:icon:get-gc-on-dc)])
		      (when (send mdc ok?)
			(wx:register-collecting-blit gc-canvas 
						     0 0
						     (mred:icon:get-gc-width)
						     (mred:icon:get-gc-height)
						     (mred:icon:get-gc-on-dc)
						     (mred:icon:get-gc-off-dc)))))])
	       
	       (sequence
		 (unless (mred:preferences:get-preference 'mred:show-status-line)
		   (send super-root change-children
			 (lambda (l)
			   (list rest-panel))))
		 (register-gc-blit)
		 
		 (let ([bw (box 0)]
		       [bh (box 0)]
		       [gc-width (mred:icon:get-gc-width)]
		       [gc-height (mred:icon:get-gc-height)])
		   (send* gc-canvas
		     (set-size 0 0 gc-width gc-height)
		     (get-client-size bw bh))
		   (send* gc-canvas
		     (user-min-client-width gc-width)
		     (user-min-client-height gc-height)
		     (stretchable-in-x #f)
		     (stretchable-in-y #f)))
		 (send* info-panel 
		   (major-align-right)
		   (stretchable-in-y #f)
		   (spacing 3)
		   (border 3))
		 (send* time-canvas 
		   (set-media time-edit)
		   (stretchable-in-x #f))
		 (semaphore-wait time-semaphore)
		 (determine-width wide-time time-canvas time-edit)
		 (semaphore-post time-semaphore)
		 (update-time)))))))
  
  (define make-edit-info-frame%
    (lambda (super-info%)
      (rec mred:edit-info-frame%
	   (class super-info% args
	     (inherit get-info-edit)
	     (rename [super-do-close do-close])
	     (private
	       [remove-pref-callback
		(mred:preferences:add-preference-callback
		 'mred:line-offsets
		 (lambda (p v)
		   (edit-position-changed-offset v)
		   #t))])
	     (public
	       [do-close
		(lambda ()
		  (super-do-close)
		  (remove-pref-callback))])
	     
	     (public
	       [overwrite-status-changed
		(let ([last-state? #f])
		  (lambda ()
		    (let ([info-edit (get-info-edit)])
		      (when info-edit
			(let ([overwrite-now? (send info-edit get-overwrite-mode)])
			  (unless (eq? overwrite-now? last-state?)
			    (send overwrite-message
				  show
				  overwrite-now?)
			    (set! last-state? overwrite-now?)))))))]
	       [anchor-status-changed
		(let ([last-state? #f])
		  (lambda ()
		    (let ([info-edit (get-info-edit)])
		      (when info-edit
			(let ([anchor-now? (send info-edit get-anchor)])
			  (unless (eq? anchor-now? last-state?)
			    (send anchor-message
				  show
				  anchor-now?)
			    (set! last-state? anchor-now?)))))))]
	       
	       [edit-position-changed-offset
		(let ([last-start #f]
		      [last-end #f])
		  (lambda (offset?)
		    (let* ([edit (get-info-edit)]
			   [make-one
			    (lambda (pos)
			      (let* ([line (send edit position-line pos)]
				     [line-start (send edit line-start-position line)]
				     [char (- pos line-start)])
				(format "~a:~a"
					(if offset?
					    (add1 line)
					    line)
					(if offset?
					    (add1 char)
					    char))))])
		      (when edit
			(let ([start (send edit get-start-position)]
			      [end (send edit get-end-position)])
			  (unless (and last-start
				       (= last-start start)
				       (= last-end end))
			    (set! last-start start)
			    (set! last-end end)
			    (when (object? position-edit)
			      (send* position-edit
				(lock #f)
				(erase)
				(insert 
				 (if (= start end)
				     (make-one start)
				     (string-append (make-one start)
						    "-"
						    (make-one end))))
				(lock #t)))))))))]
	       [edit-position-changed
		(lambda ()
		  (edit-position-changed-offset
		   (mred:preferences:get-preference 'mred:line-offsets)))])
	     (rename [super-update-info update-info])
	     (public
	       [update-info
		(lambda ()
		  (super-update-info)
		  (overwrite-status-changed)
		  (anchor-status-changed)
		  (edit-position-changed))])
	     (sequence 
	       (apply super-init args))
	     
	     (inherit info-panel)
	     (private
	       [anchor-message 
		(make-object mred:container:canvas-message%
			     info-panel
			     (let ([b (mred:icon:get-anchor-bitmap)])
			       (if (send b ok?)
				   (cons (mred:icon:get-anchor-mdc) b)
				   "Anchor"))
			     -1 -1 wx:const-border)]
	       [overwrite-message 
		(make-object mred:container:canvas-message%
			     info-panel
			     "Overwrite"
			     -1 -1 wx:const-border)]
	       [position-canvas (make-object mred:canvas:one-line-canvas%
					     info-panel)]
	       [position-edit (make-object mred:edit:media-edit%)])
	     
	     (inherit determine-width)
	     (sequence
	       (let ([move-front
		      (lambda (x l)
			(cons x (mzlib:function:remq x l)))])
		 (send info-panel change-children
		       (lambda (l)
			 (move-front
			  anchor-message
			  (move-front
			   overwrite-message
			   (move-front
			    position-canvas
			    l))))))
	       (send anchor-message show #f)
	       (send overwrite-message show #f)
	       (send* position-canvas
		 (set-media position-edit)
		 (stretchable-in-x #f))
	       (determine-width "0000:000-0000:000" 
				position-canvas
				position-edit)
	       (edit-position-changed)
	       (send position-edit lock #t))))))
  
  (define make-file-frame%
    (lambda (super%)
      (rec mred:file-frame%
	   (class-asi super%
	     (inherit get-edit)
	     (rename [super-can-close? can-close?])
	     (public
	       [can-close?
		(lambda ()
		  (let* ([edit (get-edit)]
			 [user-allowed-or-not-modified
			  (or (not (send edit modified?))
			      (case (mred:gui-utils:unsaved-warning
				     (let ([fn (send edit get-filename)])
				       (if (string? fn)
					   fn
					   "Untitled"))
				     "Close"
				     #t)
				[(continue) #t]
				[(save) (send edit save-file)]
				[else #f]))])
		    (and user-allowed-or-not-modified
			 (super-can-close?))))])))))
  
  (define make-pasteboard-frame%
    (lambda (super%)
      (rec mred:pasteboard-frame%
	   (class-asi super%
	     (public
	       [get-canvas% (lambda () mred:container:media-canvas%)]
	       [get-edit% (lambda () mred:edit:pasteboard%)])))))
  
  (define make-pasteboard-file-frame%
    (lambda (class%)
      (rec mred:pasteboard-file-frame%
	   (class-asi class%
	     (public
	       [get-edit% (lambda () mred:edit:file-pasteboard%)])))))
  
  (define make-pasteboard-info-frame%
    (lambda (class%)
      (rec mred:pasteboard-info-frame%
	   (class-asi class%
	     (public
	       [get-edit% (lambda () mred:edit:info-pasteboard%)])))))

  (define empty-frame% (make-empty-frame% mred:container:frame%))
  (define menu-frame% (make-menu-frame% empty-frame%))
  (define standard-menus-frame% (make-standard-menus-frame% menu-frame%))
  (define simple-menu-frame% (make-simple-frame% standard-menus-frame%))
  (define searchable-frame% (make-searchable-frame% simple-menu-frame%))
  (define info-frame% (make-edit-info-frame% 
		       (make-info-frame% searchable-frame%)))
  (define info-file-frame% (make-file-frame% info-frame%))
  
  (define pasteboard-frame% (make-pasteboard-frame% simple-menu-frame%))
  (define pasteboard-info-frame% (make-pasteboard-info-frame%
				  (make-info-frame% pasteboard-frame%)))
  (define pasteboard-info-file-frame% (make-pasteboard-file-frame%
				       (make-file-frame%
					pasteboard-info-frame%))))
