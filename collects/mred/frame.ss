;;; Define the standard editing window class

;; mini-panels must inheirit from wx:panel% and must provide a
;; desired-height method that returns the desired height in pixels.

(define mred:frame@
  (unit/sig mred:frame^
    (import [mred:debug : mred:debug^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:icon : mred:icon^]
	    [mred:menu : mred:menu^] 
	    [mred:group : mred:group^]
	    [mred:finder : mred:finder^]
	    [mred:handler : mred:handler^]
	    [mred:exit : mred:exit^]
	    [mred:autosave : mred:autosave^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:frame@")

    (define frame-name "MrEd")

    (define frame-width 600)
    (define frame-height 600)
    (let ([w (box 0)]
	  [h (box 0)])
      (wx:display-size w h)
      (if (< (unbox w) frame-width)
	  (set! frame-width (- (unbox w) 65)))
      (if (< (unbox h) frame-height)
	  (set! frame-height (- (unbox h) 65))))

    (define empty-frame%
      (class mred:container:frame% args
	(rename [super-pre-on-char pre-on-char]
		[super-pre-on-event pre-on-event])
	(rename [super-show show])
	(public
	  [get-panel% 
	   (lambda ()
	     (class-asi mred:container:vertical-panel%
	       (public
		 [default-spacing-width 2]
		 [default-border-width 2])))]
	  [on-close (lambda () #t)])
	(sequence 
	  (apply super-init args))
	(public
	  [shown #f]
	  [show (lambda (x) 
		  (set! shown x)
		  (super-show x))]
	  [keymap (make-object wx:keymap%)]
	  [panel (make-object (get-panel%) this)])
	(public
	  [pre-on-char
	   (lambda (receiver event)
	     (let ([ans (send keymap handle-key-event this event)])
	       (or ans
		   (super-pre-on-char receiver event))))]
	  [pre-on-event
	   (lambda (receiver event)
	     (or (send keymap handle-mouse-event this event)
		 (super-pre-on-event receiver event)))])))

    (define make-menu-frame%
      (lambda (super%)
	(class super% args
	  (inherit set-menu-bar get-menu-bar keymap)
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
	       (send (get-menu-bar) dispatch op))])
	  (sequence
	    (mred:debug:printf 'super-init "before mred:menu-frame%")
	    (apply super-init args)
	    (mred:debug:printf 'super-init "after mred:menu-frame%")
	    ; Build and install the menu bar:
	    (let ([menu-bar (make-menu-bar)])
	      (set-menu-bar menu-bar)
	      (send menu-bar set-frame this))))))

    (define menu-frame% (make-menu-frame% empty-frame%))

    (define tab (string #\tab))
    (define make-standard-menus-frame%
      (lambda (super%)
	(class-asi super%
	  (inherit make-menu on-close show)
	  (rename [super-make-menu-bar make-menu-bar])

	  (public [file-menu 'file-menu-not-yet-set]
		  [edit-menu 'file-menu-not-yet-set])

	  (public
	    [file-menu:new-string ""]
	    [file-menu:new (lambda () (mred:handler:edit-file #f) #t)]
	    [file-menu:new-id #f]
	    [file-menu:new-help-string "Creates a new empty window for editing"]

	    [file-menu:between-new-and-open (lambda (file-menu) (void))]

	    [file-menu:open-string ""]
	    [file-menu:open mred:handler:open-file]
	    [file-menu:open-id #f]
	    [file-menu:open-help-string "Opens a new file for editing"]

	    [file-menu:between-open-and-save (lambda (file-menu) (send file-menu append-separator))]

	    [file-menu:revert #f]
	    [file-menu:revert-id #f]
	    [file-menu:revert-help-string ""]

	    [file-menu:save-string ""]
	    [file-menu:save #f]
	    [file-menu:save-id #f]
	    [file-menu:save-help-string ""]

	    [file-menu:save-as #f]
	    [file-menu:save-as-id #f]
	    [file-menu:save-as-help-string ""]

	    [file-menu:between-save-and-print (lambda (file-menu) (send file-menu append-separator))]

	    [file-menu:print-string ""]
	    [file-menu:print #f]
	    [file-menu:print-id #f]
	    [file-menu:print-help-string ""]

	    [file-menu:between-print-and-close (lambda (file-menu) (send file-menu append-separator))]

	    [file-menu:close (lambda () (when (on-close) (show #f)) #t)]
	    [file-menu:close-string ""]
	    [file-menu:close-id #f]
	    [file-menu:close-help-string ""]

	    [file-menu:between-close-and-quit (lambda (file-menu) (void))]

	    [file-menu:quit (lambda ()
			      (mred:exit:exit)
			      #t)]
	    [file-menu:quit-id #f]
	    [file-menu:quit-help-string "Exits MrEd"]

	    [file-menu:after-quit (lambda (file-menu) (void))]

	    [edit-menu:undo #f]
	    [edit-menu:undo-id #f]
	    [edit-menu:undo-help-string "Undoes the last action"]

	    [edit-menu:redo #f]
	    [edit-menu:redo-id #f]
	    [edit-menu:redo-help-string "Redoes the last undone action"]

	    [edit-menu:between-redo-and-cut (lambda (edit-menu) (send edit-menu append-separator))]

	    [edit-menu:cut #f]
	    [edit-menu:cut-id #f]
	    [edit-menu:cut-help-string "Cuts the selection and copies it to the clipboard"]

	    [edit-menu:copy #f]
	    [edit-menu:copy-id #f]
	    [edit-menu:copy-help-string "Copies the selection to the clipboard"]

	    [edit-menu:between-copy-and-paste (lambda (edit-menu) (void))]

	    [edit-menu:paste #f]
	    [edit-menu:paste-id #f]
	    [edit-menu:paste-help-string "Inserts contents of the clipboard at the caret"]

	    [edit-menu:clear #f]
	    [edit-menu:clear-id #f]
	    [edit-menu:clear-help-string "Clears the selected text"]

	    [edit-menu:select-all #f]
	    [edit-menu:select-all-id #f]
	    [edit-menu:select-all-help-string "Selects all of the current window"]

	    [edit-menu:between-select-all-and-find (lambda (edit-menu) (send edit-menu append-separator))]

	    [edit-menu:find (lambda () 
			      (send this search)
			      #t)]
	    [edit-menu:find-id #f]
	    [edit-menu:find-help-string "Search for a string in the buffer"]

	    [edit-menu:replace #f]
	    [edit-menu:replace-id #f]
	    [edit-menu:replace-help-string "Search and replace in the buffer"]

	    [edit-menu:between-replace-and-preferences
	     (lambda (edit-menu) (send edit-menu append-separator))]

	    [edit-menu:preferences mred:preferences:show-preferences-dialog]
	    [edit-menu:preferences-id #f]
	    [edit-menu:preferences-help-string "Displays the preferences dialog"]

	    [edit-menu:after-standard-items (lambda (edit-menu) (void))]

	    [make-menu-bar
	     (lambda ()
	       (let ([mb (super-make-menu-bar)]
		     [join (opt-lambda (base special [suffix ""])
			     (if (string=? special "")
				 (string-append base suffix)
				 (string-append base " " special suffix)))])
		 (set! file-menu (make-menu))
		 (set! edit-menu (make-menu))
		 (send mb append file-menu (if (eq? wx:platform 'windows) "&File" "F&ile"))
		 (send mb append edit-menu "&Edit")
		 
		 (when file-menu:new
		   (set! file-menu:new-id
			 (send file-menu append-item (join "&New" file-menu:new-string)
			       file-menu:new file-menu:new-help-string #f "n")))
		 (file-menu:between-new-and-open file-menu)
		 (when file-menu:open
		   (set! file-menu:open-id
			 (send file-menu append-item (join "&Open" file-menu:open-string "...")
			       file-menu:open file-menu:open-help-string #f "o")))
		 (file-menu:between-open-and-save file-menu)
		 (when file-menu:revert
		   (set! file-menu:revert-id
			 (send file-menu append-item "&Revert"
			       file-menu:revert file-menu:revert-help-string)))
		 
		 (when file-menu:save
		   (set! file-menu:save-id
			 (send file-menu append-item (join "&Save" file-menu:save-string)
			       file-menu:save file-menu:save-help-string #f "s")))
		 (when file-menu:save-as
		   (set! file-menu:save-as-id
			 (send file-menu append-item (join "Save" file-menu:save-string " &As...")
			       file-menu:save-as)))
		 (file-menu:between-save-and-print file-menu)
		 (when file-menu:print
		   (set! file-menu:print-id
			 (send file-menu append-item (join "&Print" file-menu:print-string "...")
			       file-menu:print file-menu:print-help-string #f "p")))
		 (file-menu:between-print-and-close file-menu)
		 (when file-menu:close
		   (set! file-menu:close-id
			 (send file-menu append-item (join "&Close" file-menu:close-string)
			       file-menu:close file-menu:close-help-string #f "w")))
		 (file-menu:between-close-and-quit file-menu)
		 (when file-menu:quit
		   (set! file-menu:quit-id
			 (send file-menu append-item (if (eq? wx:platform 'macintosh)
							 "Quit"
							 "E&xit")
			       file-menu:quit file-menu:quit-help-string #f "q")))
		 (file-menu:after-quit file-menu)
		 
		 
		 (when edit-menu:undo
		   (set! edit-menu:undo-id
			 (send edit-menu append-item "&Undo"
			       edit-menu:undo edit-menu:undo-help-string #f "z")))
			     
		 (when edit-menu:redo
		   (set! edit-menu:redo-id
			 (send edit-menu append-item "&Redo"
			       edit-menu:redo edit-menu:redo-help-string #f "y")))
		 (edit-menu:between-redo-and-cut edit-menu)
		 (when edit-menu:cut
		   (set! edit-menu:cut-id
			 (send edit-menu append-item "Cu&t"
			       edit-menu:cut edit-menu:cut-help-string #f "x")))
		 (when edit-menu:copy
		   (set! edit-menu:copy-id
			 (send edit-menu append-item "&Copy"
			       edit-menu:copy edit-menu:copy-help-string #f "c")))
		 (edit-menu:between-copy-and-paste edit-menu)
		 (when edit-menu:paste
		   (set! edit-menu:paste-id
			 (send edit-menu append-item "&Paste"
			       edit-menu:paste edit-menu:paste-help-string #f "v")))
		 (when edit-menu:clear
		   (set! edit-menu:clear-id
			 (send edit-menu append-item (if (eq? wx:platform 'macintosh)
							 "Clear"
							 "&Delete")
			       edit-menu:clear edit-menu:clear-help-string #f
			       (lambda (wx:platform) (begin "del" #f)))))
		 (when edit-menu:select-all
		   (set! edit-menu:select-all-id
			 (send edit-menu append-item "Select A&ll"
			       edit-menu:select-all edit-menu:select-all-help-string #f "a")))
		 (edit-menu:between-select-all-and-find edit-menu)

		 (when edit-menu:find
		   (set! edit-menu:find-id
			 (send edit-menu append-item "Find" 
			       edit-menu:find
			       edit-menu:find-help-string
			       #f "f")))
		 (when edit-menu:replace
		   (set! edit-menu:replace-id
			 (send edit-menu append-item "Replace..." 
			       edit-menu:replace
			       edit-menu:replace-help-string)))
		 (edit-menu:between-replace-and-preferences edit-menu)
		 (when edit-menu:preferences
		   (set! edit-menu:preferences-id
			 (send edit-menu append-item "Prefere&nces..." edit-menu:preferences
			       edit-menu:preferences-help-string)))
		 (edit-menu:after-standard-items edit-menu)
		 mb))]))))

    (define standard-menus-frame% (make-standard-menus-frame% menu-frame%))

    (define make-simple-frame%
      (lambda (super%)
	(class super% ([name frame-name])
	  (inherit panel get-client-size get-title set-title set-icon)
	  (rename [super-on-close on-close])
	  (public
	    [WIDTH frame-width]
	    [HEIGHT frame-height])

	  (public
	    [edit% mred:edit:edit%]
	    [canvas% mred:canvas:simple-frame-canvas%]
	    
	    [set-last-focus-canvas
	     (lambda (c)
	       (set! last-focus-canvas c))]
	    [title-prefix ""])
	  
	  ; methods
	  (public
	    [on-frame-active (lambda () (void))]
	    [set-title-prefix
	     (lambda (s)
	       (if (string? s)
		   (set! title-prefix s)))]
	    [get-canvas% (lambda () canvas%)]
	    [make-canvas
	     (lambda ()
	       (make-object (get-canvas%) panel))]
	    [get-edit% (lambda () edit%)]
	    [make-edit
	     (lambda ()
	       (let ([% (get-edit%)])
		 (make-object %)))]
	    [add-canvas
	     (opt-lambda ([canvas (make-canvas)][prefix? #f])
	       (unless (member canvas canvases)
		 (set! canvases 
		       (if prefix?
			   (cons canvas canvases)
			   (append canvases (list canvas))))
		 (send canvas set-frame this))	       
	       canvas)]
	    [remove-canvas
	     (lambda (canvas)
	       (let ([pre-len (length canvases)])
		 (set! canvases
		       (mzlib:function:remove 
			canvas canvases
			(lambda (canvas c)
			  (if (eq? c canvas)
			      (begin
				(send canvas set-frame #f)
				(send canvas set-size 50000 50000 10 10)
				(if (eq? canvas last-focus-canvas)
				    (set! last-focus-canvas #f))
				#t)
			      #f))))
		 (send panel change-children (lambda (l) canvases))))]
	    [active-canvas
	     (lambda ()
	       (let loop ([canvases canvases])
		 (cond
		   [(null? canvases) last-focus-canvas]
		   [(send (car canvases) is-focus-on?) (car canvases)]
		   [else (loop (cdr canvases))])))]
	    [active-edit
	     (lambda ()
	       (send (active-canvas) get-media))])
	  
	  ; Initialization:
	  (sequence
	    (mred:debug:printf 'super-init "before simple-frame%")
	    (super-init () name -1 -1 WIDTH HEIGHT
			(+ wx:const-default-frame wx:const-sdi)
			name)
	    (mred:debug:printf 'super-init "after simple-frame%"))

	  

	  (public
	    [last-focus-canvas #f] ; Does this need to be inited during make-canvas?
	    [canvas (make-canvas)])
	  (sequence
	    (when (null? (send canvas get-media))
	      (send canvas set-media (make-edit)))
	    (send canvas set-frame this)
	    (set! last-focus-canvas canvas))

	  (public
	    [edit (send canvas get-media)]
	    [canvases (list canvas)])
	  (sequence
	    (when (send mred:icon:icon ok?)
	      (set-icon mred:icon:icon))))))

    (define simple-menu-frame% (make-simple-frame% standard-menus-frame%))))
