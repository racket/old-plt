;;; Define the standard editing window class

;; mini-panels must inheirit from wx:panel% and must provide a
;; desired-height method that returns the desired height in pixels.

(define mred:frame@
  (unit/sig mred:frame^
    (import [mred:debug : mred:debug^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
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

    (define empty-frame%
      (class-asi wx:frame%
	(inherit show)
	(public
	  [on-close (lambda () #t)])))

    (define frame-width 600)
    (define frame-height 600)
    (let ([w (box 0)]
	  [h (box 0)])
      (wx:display-size w h)
      (if (< (unbox w) frame-width)
	  (set! frame-width (- (unbox w) 65)))
      (if (< (unbox h) frame-height)
	  (set! frame-height (- (unbox h) 65))))

    ; This defines a simple editing frame without a menu bar.
    ; On initialization, it doesn't automatically show itself
    (define make-simple-frame%
      (lambda (super%)
	(class super% ([name frame-name])
	  (inherit get-client-size get-title set-title set-icon)
	  (rename [super-on-size on-size]
		  [super-on-close on-close])
	  ; Overrideable constants
	  (public
	    [MINI-PANEL-HEIGHT 30]
	    
	    [WIDTH frame-width]
	    [HEIGHT frame-height]
	    [MARGIN 2]
	    
	    [TOP-MARGIN 0]
	    [BOTTOM-MARGIN 0]
	    [LEFT-MARGIN 0]
	    [RIGHT-MARGIN 0])
	  
	  (public
	    [edit% mred:edit:edit%]
	    [canvas% mred:canvas:simple-frame-canvas%]
	    
	    [set-last-focus-canvas
	     (lambda (c)
	       (set! last-focus-canvas c))]
	    [title-prefix ""])
	  
	  
	  ; we need to call this one directly
	  (public
	    [on-simple-size 
	     ; Window resize -> resize canvases
	     (lambda ()
	       (when mini-panel
		 (set! MINI-PANEL-HEIGHT (send mini-panel desired-height)))
	       (if (not (void? canvas))
		   (let ((client-w (box 0))
			 (client-h (box 0)))
		     (get-client-size client-w client-h)
		     (let* ([w (- (unbox client-w) (* 2 MARGIN)
				  LEFT-MARGIN RIGHT-MARGIN)]
			    [total-h (- (unbox client-h) (* 2 MARGIN)
					TOP-MARGIN BOTTOM-MARGIN
					(if mini-panel
					    MINI-PANEL-HEIGHT
					    0))]
			    [count (length canvases)]
			    [h (if (zero? count) 10 (floor (/ total-h count)))])
		       (when mini-panel
			 (send mini-panel set-size 0 total-h w MINI-PANEL-HEIGHT))
		       (let loop ([canvases canvases]
				  [v (+ MARGIN TOP-MARGIN)])
			 (unless (null? canvases)
			   (let* ([canvas (car canvases)]
				  [c-panel (if (ivar canvas use-panel?)
					       (ivar canvas panel)
					       #f)]
				  [panel-w-box (box 0)]
				  [panel-h-box (box 0)])
			     (when c-panel
			       (send c-panel set-size 
				     (+ MARGIN LEFT-MARGIN) v
				     w -1)
			       (send c-panel get-size
				     panel-w-box panel-h-box))
			     (send canvas set-size 
				   (+ MARGIN LEFT-MARGIN) 
				   (+ v (unbox panel-h-box))
				   w (- h (unbox panel-h-box)))
			     (let ([edit (send canvas get-media)])
			       (if (and (not (null? edit))
					(ivar edit auto-set-wrap?))
				   (let ([admin (send edit get-admin)]
					 [w-box (box 0)]
					 [h-box (box 0)])
				     (unless (null? admin)
				       (send admin get-view 
					     () () w-box h-box #f)
				       (send edit set-max-width 
					     (unbox w-box)))))))
			   (loop (cdr canvases) (+ v h))))))))])
	  
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
	       (let ([canvas (make-object (get-canvas%) this)])
		 (when (ivar canvas use-panel?)
		   (send canvas create-panel this))
		 canvas))]
	    [get-edit% (lambda () edit%)]
	    [make-edit
	     (lambda ()
	       (make-object (get-edit%)))]
	    [on-size
	     (lambda (w h)
	       (super-on-size w h)
	       (on-simple-size))]
	    
	    [add-canvas
	     (opt-lambda ([canvas (make-canvas)][prefix? #f])
	       (unless (member canvas canvases)
		 (set! canvases 
		       (if prefix?
			   (cons canvas canvases)
			   (append canvases (list canvas))))
		 (send canvas set-frame this)
		 (on-simple-size))
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
				(when (ivar canvas use-panel?)
				  (send (ivar canvas panel)
					set-size 50000 50000 -1 -1))
				(if (eq? canvas last-focus-canvas)
				    (set! last-focus-canvas #f))
				#t)
			      #f))))
		 (if (< (length canvases) pre-len)
		     (on-simple-size))))]
	    
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
	    (super-init () name -1 -1 WIDTH HEIGHT
			(+ wx:const-default-frame wx:const-sdi)
			name))
	  
	  (private
	    [mini-panel #f])
	  (public
	    [set-mini-panel%
	     (lambda (c%)
	       (set! mini-panel (make-object c% this 50000 50000 100 100))
	       (send mini-panel set-focus)
	       (on-simple-size))]
	    [get-mini-panel (lambda () mini-panel)]
	    [clear-mini-panel%
	     (lambda ()
	       (when mini-panel
		 (send (active-canvas) set-focus)
		 (send mini-panel show #f)
		 (send mini-panel set-size 50000 50000 30 30)
		 (set! mini-panel #f)
		 (on-simple-size)))])
	  
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
	    (if (send mred:icon:icon ok?)
		(set-icon mred:icon:icon))
	    (on-simple-size)))))

    (define simple-frame% (make-simple-frame% empty-frame%))

    (define make-menu-frame%
      (lambda (super%)
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
	       (send (get-menu-bar) dispatch op))])
	  (sequence
	    (apply super-init args)
	    
	    ; Build and install the menu bar:
	    (let ([menu-bar (make-menu-bar)])
	      (set-menu-bar menu-bar))))))

    (define menu-frame% (make-menu-frame% empty-frame%))

    (define simple-menu-frame% (make-menu-frame% simple-frame%))

    (define tab (string #\tab))

    (define make-standard-menus-frame%
      (lambda (super%)
	(class super% ([name frame-name])
	  (inherit make-menu)
	  (rename [super-make-menu-bar make-menu-bar])

	  (public [file-menu 'file-menu-not-yet-set]
		  [edit-menu 'file-menu-not-yet-set])

	  (public
	    [file-menu:new-string ""]
	    [file-menu:new (lambda () (mred:handler:edit-file #f))]
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

	    [file-menu:close #f]
	    [file-menu:close-id #f]
	    [file-menu:close-help-string ""]

	    [file-menu:between-close-and-quit (lambda (file-menu) (void))]

	    [file-menu:quit mred:exit:exit]
	    [file-menu:quit-id #f]
	    [file-menu:quit-help-string "Exits MrEd"]

	    [file-menu:after-quit (lambda (file-menu) (void))]

	    [edit-menu:undo (lambda () (void))]
	    [edit-menu:undo-id #f]
	    [edit-menu:undo-help-string "Undoes the last action"]

	    [edit-menu:redo (lambda () (void))]
	    [edit-menu:redo-id #f]
	    [edit-menu:redo-help-string "Redoes the last undone action"]

	    [edit-menu:cut (lambda () (void))]
	    [edit-menu:cut-id #f]
	    [edit-menu:cut-help-string "Cuts the selection and copies it to the clipboard"]

	    [edit-menu:copy (lambda () (void))]
	    [edit-menu:copy-id #f]
	    [edit-menu:copy-help-string "Copies the selection to the clipboard"]

	    [edit-menu:between-copy-and-paste (lambda (edit-menu) (void))]

	    [edit-menu:paste (lambda () (void))]
	    [edit-menu:paste-id #f]
	    [edit-menu:paste-help-string "Inserts contents of the clipboard at the caret"]

	    [edit-menu:clear (lambda () (void))]
	    [edit-menu:clear-id #f]
	    [edit-menu:clear-help-string "Clears the selected text"]

	    [edit-menu:select-all #f]
	    [edit-menu:select-all-id #f]
	    [edit-menu:select-all-help-string "Selects all of the current window"]

	    [edit-menu:between-select-all-and-preferences (lambda (edit-menu) (send edit-menu append-separator))]

	    [edit-menu:preferences mred:preferences:show-preferences-dialog]
	    [edit-menu:preferences-id #f]
	    [edit-menu:preferences-help-string "Displays the preferences dialog"]

	    [edit-menu:after-standard-items (lambda (edit-menu) (void))]

	    [make-menu-bar
	     (lambda ()
	       (set! file-menu (make-menu))
	       (set! edit-menu (make-menu))
	       
	       (when file-menu:new
		 (set! file-menu:new-id
		       (send file-menu append-item (case wx:platform 
						     [(windows) (string-append "&New" file-menu:new-string tab "Ctrl+N")]
						     [(macintosh) (string-append "New" file-menu:new-string tab "Cmd+N")]
						     [else (string-append "&New" file-menu:new-string)])
			     file-menu:new
			     file-menu:new-help-string)))
	       (file-menu:between-new-and-open file-menu)
	       (when file-menu:open
		 (set! file-menu:open-id
		       (send file-menu append-item (case wx:platform
						     [(windows) (string-append "&Open" file-menu:open-string "..." "Ctrl+O")]
						     [(macintosh) (string-append "Open" file-menu:open-string "..." tab "Cmd+O")]
						     [else (string-append "&Open" file-menu:open-string "..." tab "Ctl+x Ctl+f")])
			     file-menu:open
			     file-menu:open-help-string)))
	       (file-menu:between-open-and-save file-menu)
	       (when file-menu:revert
		 (set! file-menu:revert-id
		       (send file-menu append-item (case wx:platform
						     [(windows) "&Revert"]
						     [(macintosh) "Revert"]
						     [else "&Revert"])
			     file-menu:revert
			     file-menu:revert-help-string)))
	       (when file-menu:save
		 (set! file-menu:save-id
		       (send file-menu append-item (case wx:platform
						     [(windows) (string-append "&Save" tab "Ctrl+S")]
						     [(macintosh) (string-append "Save" tab "Cmd+S")]
						     [else (string-append "&Save" tab "Ctl+x Ctl+s")])
			     file-menu:save
			     file-menu:save-help-string)))
	       (when file-menu:save-as
		 (set! file-menu:save-as-id
		       (send file-menu append-item (case wx:platform
						     [(windows) "Save &As..."]
						     [else "Save &As..."])
			     file-menu:save-as)))
	       (file-menu:between-save-and-print file-menu)
	       (when file-menu:print
		 (set! file-menu:print-id
		       (send file-menu append-item (case wx:platform
						     [(windows) (string-append "&Print" file-menu:print-string "..." tab "Ctrl+P")]
						     [(macintosh) (string-append "Print" file-menu:print-string "..." tab "Cmd+P")]
						     [else (string-append "&Print" file-menu:print-string "...")])
			     file-menu:print
			     file-menu:print-help-string)))
	       (file-menu:between-print-and-close file-menu)
	       (when file-menu:close
		 (set! file-menu:close-id
		       (send file-menu append-item (case wx:platform
						     [(windows) (string-append "&Close" tab "Ctrl+W")]
						     [(macintosh) (string-append "Close" tab "Cmd+W")]
						     [else "&Close"])
			     file-menu:close
			     file-menu:close-help-string)))
	       (file-menu:between-close-and-quit file-menu)
	       (when file-menu:quit
		 (set! file-menu:quit-id
		       (send file-menu append-item (case wx:platform
						     [(windows) "E&xit"]
						     [(macintosh) (string-append "Quit" tab "Cmd+Q")]
						     [else (string-append "E&xit" tab "Ctl+x Ctl+c")])
			     file-menu:quit
			     file-menu:quit-help-string)))
	       (file-menu:after-quit file-menu)
	       
	       
	       (set! edit-menu:undo-id
		     (send edit-menu append-item (case wx:platform
						   [(windows) (string-append "&Undo" tab "Ctrl+Z")]
						   [(macintosh) (string-append "Undo" tab "Cmd+Z")]
						   [else (string-append "&Undo" tab "Ctl+x u")]) 
			   edit-menu:undo
			   edit-menu:undo-help-string))
	       (set! edit-menu:redo-id
		     (send edit-menu append-item (case wx:platform
						   [(windows) (string-append "&Redo" tab "Ctrl+Y")]
						   [(macintosh) (string-append "Redo" tab "Cmd+Y")]
						   [else "&Redo"]) edit-menu:redo))
	       (send edit-menu append-separator)
	       (set! edit-menu:cut-id
		     (send edit-menu append-item (case wx:platform
						   [(windows) (string-append "Cu&t" tab "Ctrl+X")]
						   [(macintosh) (string-append "Cut" tab "Cmd+X")]
						   [else (string-append "Cu&t" tab "Ctl+w")])
			   edit-menu:cut
			   edit-menu:cut-help-string))
	       (set! edit-menu:copy-id
		     (send edit-menu append-item (case wx:platform
						   [(windows) (string-append "&Copy" tab "Ctrl+C")]
						   [(macintosh) (string-append "Copy" tab "Cmd+C")]
						   [else (string-append "&Copy" tab "Alt+w")])
			   edit-menu:copy
			   edit-menu:copy-help-string))
	       (edit-menu:between-copy-and-paste edit-menu)
	       (set! edit-menu:paste-id
		     (send edit-menu append-item (case wx:platform
						   [(windows) (string-append "&Paste" tab "Ctrl+V")]
						   [(macintosh) (string-append "Paste" tab "Cmd+V")]
						   [else (string-append "&Paste" tab "Ctl+y")])
			   edit-menu:paste
			   edit-menu:paste-help-string))
	       (set! edit-menu:clear-id
		     (send edit-menu append-item (case wx:platform
						   [(windows) (string-append "&Delete" tab "Del")]
						   [(macintosh) "Clear"]
						   [else (string-append "Clear" tab "Del")])
			   edit-menu:clear
			   edit-menu:clear-help-string))
	       (set! edit-menu:select-all-id
		     (send edit-menu append-item (case wx:platform
						   [(windows) (string-append "Select A&ll" tab "Ctrl+A")]
						   [(macintosh) (string-append "Select All" tab "Cmd+A")]
						   [else "Select A&ll"])
			   edit-menu:select-all
			   edit-menu:select-all-help-string))
	       (edit-menu:between-select-all-and-preferences edit-menu)
	       (when edit-menu:preferences
		 (set! edit-menu:preferences-id
		       (send edit-menu append-item "&Preferences..." edit-menu:preferences
			     edit-menu:preferences-help-string)))
	       (edit-menu:after-standard-items edit-menu)
	       
	       (let ([mb (super-make-menu-bar)])
		 (send mb append file-menu "&File")
		 (send mb append edit-menu "&Edit")
		 mb))])
	  (sequence
	    (super-init name)))))

    ; This defines the standard editing window.
    (define standard-menus-frame% (make-standard-menus-frame% simple-menu-frame%))

    (define make-editor-frame%
      (lambda (super%)
	(class super% ([filename #f][show? #t][frameset mred:group:frames])
	  (inherit active-edit active-canvas make-menu make-edit
		   show add-canvas remove-canvas canvases on-simple-size)
	  (rename [super-on-close on-close]
		  [super-make-menu-bar make-menu-bar]
		  [super-on-menu-command on-menu-command]
		  [super-next-menu-id next-menu-id])
	  (public
	    [allow-split? #t])
	  (private 
	    [other-offset 0]
	    [keep-buffers? (is-a? frameset mred:group:frame-group%)]
	    
	    [frames frameset]
	    [buffers (if keep-buffers? (ivar frames buffers))]
	    
	    [save-as
	     (lambda (format)
	       (let ([file (mred:finder:put-file)])
		 (if file
		     (send edit save-file file format))))])
	  
	  (public
	    [auto-save? #t]
	    [on-frame-active
	     (lambda ()
	       (if keep-buffers?
		   (send frames set-active-frame this)))]
	    [check-saved
	     (opt-lambda (canvas [reason "Close"])
	       (let* ([edit (send canvas get-media)]
		      [name (send edit get-filename)]
		      [name (if (string? name)
				name
				"Untitled")])
		 (or (not (send edit modified?))
		     (let ([action
			    (mred:gui-utils:unsaved-warning name reason #t)])
		       (case action
			 [(save) (send edit save-file)]
			 [(continue) (send edit remove-autosave) #t]
			 [else #f])))))]
	    [get-frame-group
	     (lambda ()
	       (if keep-buffers?
		   frames
		   #f))]
	    [next-menu-id
	     (lambda ()
	       other-offset)]
	    [open-file
	     (opt-lambda (orig-filename [canvas (active-canvas)]
					[check-save? (not keep-buffers?)])
	       ; filename = () => ask user
	       ; filename = #f => no file, make untitled
	       ; filename = <buffer> => use buffer
	       ; otherwise, filename = name string
	       (if (or (not check-save?)
		       (check-saved canvas))
		   (let* ([filename
			   (if (null? orig-filename)
			       (mred:finder:get-file)
			       orig-filename)]
			  ; at this point, name is either a string, buffer, or #f
			  ; Is the buffer going to be untitled?
			  [untitled? (not filename)]
			  ; If filename is still #f, make up a name
			  [filename 
			   (cond
			     ((object? filename)
			      (let ([name (send filename get-filename)])
				(if (null? name)
				    "Untitled"
				    name)))
			     (filename filename)
			     (else (if keep-buffers?
				       (send buffers 
					     make-untitled-name)
				       "Untitled")))]
			  ; Look for an existing buffer for this file
			  [edit
			   (cond
			     ((object? orig-filename) orig-filename)
			     ((or untitled? (not keep-buffers?))
			      #f)
			     (else
			      (send buffers find-buffer-by-name 'file filename)))])
		     ; If we didn't find a buffer, create one
		     (if (not edit)
			 (let ([edit (make-edit)])
			   ; Load in the file, if it exists
			   (if (and (not untitled?)
				    (file-exists? filename))
			       (send edit load-file filename)
			       (begin
				 (send edit erase)
				 (send edit set-filename filename 
				       untitled?)))
			   ; Pick the editing mode
			   (let* ([filename (send edit get-filename)]
				  [mode-handler (if (null? filename)
						    #f
						    (mred:handler:find-mode-handler
						     filename))])
			     (if mode-handler
				 (let ([mode (mode-handler edit)])
				   (send edit set-mode mode))))
			   ; We created this buffer; add to the global list
			   (if keep-buffers?
			       (send buffers add-buffer 'file #f edit))
			   (send canvas set-media edit))
			 ; Found an existing edit
			 (begin
			   (if keep-buffers?
			       (send buffers reset-buffer-state edit))
			   (send canvas set-media edit)))
		     #t)
		   #f))])
	  (private [font-offset 0])
	  (public
	    [on-menu-command
	     (lambda (op)
	       (cond
		 [(< op font-offset) (begin (send (active-edit) do-font op)
					    #t)]
		 [else (super-on-menu-command op)]))]
	    
	    [check-nonunique-or-saved
	     (lambda (canvas)
	       (let ([e (send canvas get-media)])
		 (if (ormap
		      (lambda (c)
			(and (not (eq? c canvas))
			     (eq? e (send c get-media))))
		      canvases)
		     #t
		     (check-saved canvas))))]
	    
	    [check-all-saved-for-quit
	     (lambda () 
	       (check-all-saved "Quit"))]
	    [check-all-saved
	     (opt-lambda ([reason "Close"])
	       (andmap (lambda (c) (check-saved c reason)) canvases))]
	    
	    [on-close
	     (lambda ()
	       (if (and (super-on-close)
			(or (not keep-buffers?)
			    (send frames remove-frame this)))
		   (if keep-buffers?
		       (begin
			 (for-each (lambda (canvas)
				     (send canvas set-media '()))
				   canvases)
			 #t)
		       (if (check-all-saved)
			   (begin
			     (mred:exit:remove-exit-callback exit-callback-tag)
			     (set! auto-save? #f)
			     #t)
			   #f))
		   #f))]
	    
	    [do-autosave
	     (lambda ()
	       (if auto-save?
		   (for-each (lambda (canvas)
			       (let ([m (send canvas get-media)])
				 (unless (null? m)
				   (send m do-autosave))))
			     canvases)))]

	    [file-menu:new (lambda () 
			     (if (is-a? frameset mred:group:frame-group%)
				 (send frames new-frame #f)
				 (mred:handler:edit-file #f)))]
	    [file-menu:revert (lambda () (send (active-edit) load-file))]
	    [file-menu:save (lambda () (send (active-edit) save-file))]
	    [file-menu:save-as (lambda () (save-as wx:const-media-ff-same))]
	    [file-menu:close (lambda () (if (on-close) (show #f)))]
	    [file-menu:between-open-and-save
	     (lambda (file-menu)
	       (if keep-buffers?
		   (send file-menu append-item "Switch to..."
			 (lambda () (send buffers pick (active-canvas)))))
	       (send file-menu append-separator))]
	    [file-menu:print (lambda () (send (active-edit) print '()))]
	    [file-menu:between-save-and-print
	     (lambda (file-menu)
	       (send file-menu append-item "Save As Text..."
		     (lambda () (save-as wx:const-media-ff-text)))
	       (send file-menu append-item "Save As Text and Styles..."
		     (lambda () (save-as wx:const-media-ff-std)))
	       (when allow-split?
		 (send file-menu append-separator)
		 (send file-menu append-item "Split"
		       (lambda () 	       
			 (let ([new-canvas (add-canvas)])
			   (send new-canvas set-media (active-edit)))))
		 (send file-menu append-item "Collapse"
		       (lambda ()
			 (when (> (length canvases) 1)
			   (let ([canvas (active-canvas)])
			     (if (and canvas
				      (or keep-buffers?
					  (check-nonunique-or-saved canvas)))
				 (remove-canvas canvas)))))))
	       (send file-menu append-item "Set Mode..."
		     (lambda ()
		       (let* ([modes (map mred:handler:handler-name 
					  mred:handler:mode-handlers)]
			      [name (wx:get-single-choice
				     "Select a Mode" "Mode"
				     modes)])
			 (unless (null? name)
			   (let* ([handler (mred:handler:find-named-mode-handler name)]
				  [mode (handler (active-edit))])
			     (send (active-edit) set-mode mode))))))
	       (send file-menu append-separator))])

	  (private
	    [edit-menu:do (lambda (const) (lambda () (send (active-edit) do-edit const)))])

	  (public
	    [edit-menu:undo (edit-menu:do wx:const-edit-undo)]
	    [edit-menu:redo (edit-menu:do wx:const-edit-redo)]
	    [edit-menu:cut (edit-menu:do wx:const-edit-cut)]
	    [edit-menu:clear (edit-menu:do wx:const-edit-clear)]
	    [edit-menu:copy (edit-menu:do wx:const-edit-copy)]
	    [edit-menu:paste (edit-menu:do wx:const-edit-paste)]
	    [edit-menu:select-all
	     (lambda ()
	       (send (active-edit) set-position
		     0 (send (active-edit) last-position)))]

	    [edit-menu:between-select-all-and-preferences
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
			 (send edit set-auto-set-wrap
			       (not (ivar edit auto-set-wrap?)))
			 (on-simple-size))))
	       (send edit-menu append-separator))]
	    [edit-menu:after-standard-items
	     (lambda (edit-menu)
	       (send edit-menu append-separator)
	       (set! font-offset (send (active-edit) append-font-items edit-menu 0)))])

	  (sequence
	    (super-init))
	  
	  (public
	    [exit-callback-tag
	     (if (not keep-buffers?)
		 (mred:exit:insert-exit-callback check-all-saved-for-quit))])
	  
	  (sequence
	    (when show? (show #t))
	    
	    (if keep-buffers?
		(send frames insert-frame this)
		(mred:autosave:register-autosave this))
	    
	    (let ([filename (if (string? filename)
				(mzlib:file:normalize-path filename)
				filename)])
	      (open-file filename))
	    
	    (when show?
	      (wx:yield)
	      (send (active-canvas) set-focus)))
	  (public
	    [edit (active-edit)]))))

    (define editor-frame% (make-editor-frame% standard-menus-frame%))

    (define make-pasteboard-frame%
      (lambda (super%)
	(class-asi super%
		   (public
		    [edit% mred:edit:pasteboard%]))))
    (define pasteboard-frame% (make-pasteboard-frame% editor-frame%))

    (define make-status-frame%
      (lambda (super%)
	(class-asi super%
	  (rename [super-get-canvas% get-canvas%])
	  (public
	    [get-canvas%
	     (lambda ()
	       (class-asi super-get-canvas%
		 (public [use-panel? #t])))]))))))

