
  (unit/sig mred:frame^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    [mred:icon : mred:icon^]
	    [mred:menu : mred:menu^] 
	    [mred:group : mred:group^]
	    [mred:finder : mred:finder^]
	    [mred:find-string : mred:find-string^]
	    [mred:handler : mred:handler^]
	    [mred:exit : mred:exit^]
	    [mred:autosave : mred:autosave^]
	    [mred:panel : mred:panel^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:application : mred:application^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:frame@")

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
	(rename [super-on-frame-active on-frame-active]
		[super-pre-on-char pre-on-char]
		[super-on-close on-close]
		[super-pre-on-event pre-on-event])
	(sequence (mred:debug:printf 'creation "creating a frame"))
	(rename [super-show show])
	(public
	  [get-panel% (lambda () mred:container:vertical-panel%)]
	  [show
	   (lambda (on?)
	     (when on?
	       (unless (member this (send mred:group:the-frame-group get-frames))
		 (send mred:group:the-frame-group insert-frame this)))
	     (super-show on?))]
	  [on-close
	   (lambda ()
	     (and (send mred:group:the-frame-group remove-frame this)
		  (super-on-close)))])
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
          (begin-elaboration-time (reference-library "macro.ss"))
	  (begin-elaboration-time
            (let-struct between (menu name separator)
              (let-struct an-item (name help-string proc key menu-string-before menu-string-after)
                (letrec* ([wx:platform
			   ;; mrspidey hack
			   (if (defined? 'wx:platform)
			       (global-defined-value 'wx:platform)
			       'unix)]
			  [build-id
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
					       '(lambda () (mred:handler:open-file) #t)
					       "o" "&Open" "...")
				 (make-an-item 'file-menu:open-url "Open a Uniform Resource Locater"
					       '(lambda () (mred:handler:open-url) #t)
					       #f "Open &Url" "...")
				 (make-between 'file-menu 'between-open-and-save #f)
				 (make-an-item 'file-menu:revert 
					       "Revert this file to the copy on disk"
					       #f #f "&Revert" "")
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
					       '(lambda () (send this search 1) #t)
					       "f" "Find" "")
				 (make-an-item 'edit-menu:replace "Search and replace a string in the buffer"
					       #f #f "Replace" "")
				 (make-between 'edit-menu 'between-replace-and-preferences #t)
				 (make-an-item 'edit-menu:preferences ""
					       '(lambda () (mred:preferences:show-preferences-dialog) #t)
					       #f "Preferences..." "")
				 (make-between 'edit-menu 'after-standard-items #f))])
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
                         ,(build-make-menu-bar items)])))))))))

    (define standard-menus-frame% (make-standard-menus-frame% menu-frame%))

    (define make-simple-frame%
      (lambda (super%)
	(class super% ([name mred:application:app-name])
	  (inherit panel get-client-size set-icon get-menu-bar
		   make-menu show active-edit active-canvas)
	  (rename [super-on-close on-close]
		  [super-set-title set-title])
	  (public
	    [WIDTH frame-width]
	    [HEIGHT frame-height])
	  
	  (public
	    [on-close
	     (lambda ()
	       (and (super-on-close)
		    (send (get-edit) on-close)))])

	  (public
	    [get-panel%  (lambda () mred:panel:vertical-edit-panel%)]
	    [title-prefix name])
	  
	  (private
	    [title ""]
	    [do-title
	     (lambda ()
	       (mred:debug:printf 'matthew "do-title.1~n")
	       (let ([t (if (or (string=? "" title)
				(string=? "" title-prefix))
			    (string-append title-prefix title)
			    (string-append title " - " title-prefix))])
		 (mred:debug:printf 'matthew "do-title.2~n")
		 (super-set-title t)
		 (mred:debug:printf 'matthew "super-set-title finished~n")))])
	  
	  (public
	    [get-title (lambda () title)]
	    [set-title
	     (lambda (t)
	       (mred:debug:printf 'matthew "set-title~n")
	       (when (and (string? t)
			  (not (string=? t title)))
		 (set! title t)
		 (do-title))
	       (mred:debug:printf 'matthew "end set-title~n"))]
	    [set-title-prefix
	     (lambda (s)
	       (when (and (string? s)
			  (not (string=? s title-prefix)))
		 (set! title-prefix s)
		 (mred:debug:printf 'matthew "set-title-prefix calling do-title~n")
		 (do-title)
		 (mred:debug:printf 'matthew "set-title-prefix returned from do-title~n")))]
	    [get-canvas% (lambda () mred:canvas:frame-title-canvas%)]
	    [get-edit% (lambda () mred:edit:backup-autosave-edit%)]
	    [make-edit (lambda () (make-object (get-edit%)))])
	  
	  (public
	    [save-as
	     (opt-lambda ([format wx:const-media-ff-same])
	       (let ([file (mred:finder:put-file)])
		 (when file
		   (send (get-edit) save-file file format))))]
	    [file-menu:revert 
	     (lambda () 
	       (let* ([b (box #f)]
		      [edit (get-edit)]
		      [filename (send edit get-filename b)]
		      [start (send edit get-start-position)]
		      [end (send edit get-end-position)])
		 (if (or (null? filename) (unbox b))
		     (wx:bell)
		     (begin (send edit begin-edit-sequence)
			    (send edit load-file filename)
			    (send edit set-position start end)
			    (send edit end-edit-sequence)))
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
	    [file-menu:print (lambda () (send (get-edit) print '()) #t)])
	  
	  
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
	  
	  (sequence
	    (mred:debug:printf 'super-init "before simple-frame%")
	    (super-init () name -1 -1 WIDTH HEIGHT
			(+ wx:const-default-frame wx:const-sdi)
			name)
	    (mred:debug:printf 'super-init "after simple-frame%"))
	  
	  (public
	    [get-canvas (let ([c (make-object (get-canvas%) panel)])
			  (lambda () c))]
	    [get-edit (let ([e (make-edit)]) 
			(send (get-canvas) set-media e)
			(lambda () e))])
	  (sequence
	    (when (send mred:icon:icon ok?)
	      (set-icon mred:icon:icon))
	    (mred:debug:printf 'matthew "initial call to do-title~n")
	    (do-title)
	    (mred:debug:printf 'matthew "finished initial call to do-title~n")
	    (let ([canvas (get-canvas)])
	      (mred:debug:printf 'matthew "got canvas.1 ~a~n" canvas)
	      (mred:debug:printf 'matthew "got canvas.2 ~a~n" (ivar canvas set-focus))
	      (send canvas set-focus))
	    (mred:debug:printf 'matthew "after initial call to set-focus~n")))))

    (define simple-menu-frame% (make-simple-frame% standard-menus-frame%)))
