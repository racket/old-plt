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
	(let ([join (opt-lambda (base special [suffix ""])
		      (if (string=? special "")
			  (string-append base suffix)
			  (string-append base " " special suffix)))])
	  (let-expansion-time 
	   build-class-macro
	   (let-struct between (menu name separator)
	     (let-struct an-item (name help-string proc key menu-string-before menu-string-after)
	       (letrec* ([build-id
			  (lambda (name post)
			    (let ([name-string (symbol->string name)])
			      (string->symbol (string-append name-string post))))]
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
				   [file-menu? (string=? (substring name-string 0 9) "file-menu")])
			      `(when ,name
				 (set! ,(build-id name "-id")
				       (send ,(if file-menu? 'file-menu 'edit-menu) append-item 
					     (join ,menu-before-string 
						   ,(build-id name "-string")
						   ,menu-after-string)
					     ,name ,(build-id name "-help-string")
					     #f ,key)))))]
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
				     '(lambda (m) (send m append-separator))
				     '(lambda (x) (void)))]))]
			 [build-make-menu-bar
			  (lambda (items)
			    (let ([mb (gensym "mb")])
			      `(lambda ()
				 (let ([,mb (super-make-menu-bar)])
				   (set! file-menu (make-menu))
				   (set! edit-menu (make-menu))
				   (send* ,mb (append file-menu
						      (if (eq? wx:platform 'windows)
							  "&File" "F&ile"))
				     (append edit-menu "&Edit"))
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
					      'mred:handler:open-file
					      "n" "&Open" "...")
				(make-between 'file-menu 'between-open-and-save #f)
				(make-an-item 'file-menu:revert 
					      "Revert this file to the copy on disk"
					      #f #f "&Revert" "")
				(make-an-item 'file-menu:save "" #f "s" "&Save" "")
				(make-an-item 'file-menu:save-as "" #f "n" "Save" " &As...")
				(make-between 'file-menu 'between-save-and-print #t)
				(make-an-item 'file-menu:print "" #f "p" "&Print..." "")
				(make-between 'file-menu 'between-print-and-close #t)
				(make-an-item 'file-menu:close "" 
					      '(lambda () (when (on-close) (show #f)) #t)
					      "w" "&Close" "")
				(make-between 'file-menu 'between-close-and-quit #f)
				(make-an-item 'file-menu:quit "" '(lambda () (mred:exit:exit))
					      (if (eq? 'wx:platform 'macintosh) "q" "x")
					      (if (eq? wx:platform 'macintosh) "Quit" "E&xit") "")
				(make-between 'file-menu 'after-quit #f)
				
				(make-an-item 'edit-menu:undo "" #f "z" "&Undo" "")
				(make-an-item 'edit-menu:redo "" #f #f "&Redo" "")
				(make-between 'edit-menu 'between-redo-and-cut #t)
				(make-an-item 'edit-menu:cut "" #f "x" "Cu&t" "")
				(make-between 'edit-menu 'between-cut-and-copy #f)
				(make-an-item 'edit-menu:copy "" #f "c" "&Copy" "")
				(make-between 'edit-menu 'between-copy-and-paste #f)
				(make-an-item 'edit-menu:paste "" #f "v" "&Paste" "")
				(make-between 'edit-menu 'between-paste-and-clear #f)
				(make-an-item 'edit-menu:clear "" #f #f
					      (if (eq? wx:platform 'macintosh)
						  "Clear"
						  "&Delete")
					      "")
				(make-between 'edit-menu 'between-clear-and-select-all #f)
				(make-an-item 'edit-menu:select-all "" #f "z" "Select A&ll" "")
				(make-between 'edit-menu 'between-select-all-and-find #t)
				(make-an-item 'edit-menu:find "Search for a string in the buffer"
					      '(lambda () (send this search))
					      "z" "Find" "")
				(make-an-item 'edit-menu:replace "Search and replace a string in the buffer"
					      #f "z" "Replace" "")
				(make-between 'edit-menu 'between-replace-and-preferences #t)
				(make-an-item 'edit-menu:preferences ""
					      '(lambda () (mred:preferences:show-preferences-dialog))
					      #f "Preferences..." "")
				(make-between 'edit-menu 'after-standard-items #f))])
		 (lambda ()
		   `(class-asi super%
		      (inherit make-menu on-close show)
		      (rename [super-make-menu-bar make-menu-bar])
		      (public [file-menu 'file-menu-uninitialized]
			      [edit-menu 'edit-menu-uninitialized])
		      ,@(map (lambda (x)
			       (if (between? x)
				   (build-between-public-clause x)
				   (build-public-clause x)))
			     items)
		      (public
			[make-menu-bar
			 ,(build-make-menu-bar items)]))))))
	   (let-macro build-class
	     (lambda ()
	       ((local-expansion-time-value 'build-class-macro)))
	     (build-class))))))

    (define standard-menus-frame% (make-standard-menus-frame% menu-frame%))

    (define make-simple-frame%
      (lambda (super%)
	(class super% ([name frame-name])
	  (inherit panel get-client-size set-icon)
	  (rename [super-on-close on-close]
		  [super-set-title set-title])
	  (public
	    [WIDTH frame-width]
	    [HEIGHT frame-height])

	  (public
	    [edit% mred:edit:edit%]
	    [canvas% mred:canvas:simple-frame-canvas%]
	    
	    [set-last-focus-canvas
	     (lambda (c)
	       (set! last-focus-canvas c))]
	    [title-prefix "MrEd"])
	  
	  (private
	    [title ""]
	    [do-title
	     (lambda ()
	       (let ([t (if (or (string=? "" title)
				(string=? "" title-prefix))
			    (string-append title-prefix title)
			    (string-append title-prefix ": " title))])
		 '(printf "setting-title to ~a~n" t)
		 (super-set-title t)))])

	  ; methods
	  (public
	    [on-frame-active (lambda () (void))]
	    [get-title (lambda () title)]
	    [set-title
	     (lambda (t)
	       (when (and (string? t)
			  (not (string=? t title)))
		 (set! title t)
		 (do-title)))]
	    [set-title-prefix
	     (lambda (s)
	       (when (and (string? s)
			  (not (string=? s title-prefix)))
		 (set! title-prefix s)
		 (do-title)))]
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
	      (set-icon mred:icon:icon))
	    (do-title)))))

    (define simple-menu-frame% (make-simple-frame% standard-menus-frame%))))
