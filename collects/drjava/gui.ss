(unit/sig gui^
  (import gui-text^ gjc^ mred^)
  
  (define global-keymap (make-object keymap%))
  
  ;; new-document : (U String #f) -> Void
  (define (new-document filename)
    (let* ([f (make-object frame% "DrJava" #f 500 500)]
	   [menu-bar (make-object menu-bar% f)])
      (add-parts
       f
       add-top-canvas
       add-bottom-canvas
       (lambda (top-editor repl-editor) 
         (when filename
           (send top-editor load-file filename))
         (add-file-menu menu-bar top-editor repl-editor f)
         (send f show #t)))))
  
  ;; add-file-menu : menu-bar% text% text% frame% -> Void
  (define (add-file-menu bar top bottom frame)
    (let* ([file-menu (make-object menu% "File" bar)]
	   [add (lambda (n f) (make-object menu-item% n file-menu f))])
      (add "New"
	   (lambda (item event)
	     (new-document #f)))
      (add "Open"
	   (lambda (item event)
	     (let ([fname (get-file)])
	       (when fname
		 (if (or (send top is-modified?) (send top get-filename))
		     (new-document fname)
		     (send top load-file fname))))))
      (add "Revert"
	   (lambda (item event)
	     (send top load-file)))
      (add "Save"
	   (lambda (item event)
	     (send top save-file)))
      (add "Save As"
	   (lambda (item event)
	     (let ([fname (put-file)])
	       (when fname
		 (send top save-file fname)))))
      (add "Set Output Directory"
	   (lambda (item event)
	     (let ([new (get-text-from-user "Class File Directory"
					    "Enter a directory name:"
					    #f gjc-output-dir)])
	       (when new (set-gjc-output-dir! new)))))
      (make-object separator-menu-item% file-menu) 
      (add "Close" (lambda (_ __) (send frame show #f)))
      (add "Quit" (lambda (_ __) (exit)))))
  
  ;; add-top-canvas : frame% message% -> text%
  (define (add-top-canvas frame message)
    (let ([canvas (make-object editor-canvas% frame)]
	  [editor (make-object definitions-text% message)]
	  [keymap (make-object keymap%)])
      (add-editor-keymap-functions keymap)
      (add-text-keymap-functions keymap)
      (send editor set-keymap keymap)
      (install-standard-text-bindings editor)
      (send canvas set-editor editor)
      editor))
  
  (define definitions-text% (definitions-text-mixin text%))
  (define repl-text% (repl-text-mixin text%))
  
  ;; add-bottom-canvas : frame% -> text%
  (define (add-bottom-canvas frame)
    (let ([canvas (make-object editor-canvas% frame)]
	  [editor (make-object repl-text%)]
	  [keymap (make-object keymap%)])
      (add-editor-keymap-functions keymap)
      (add-text-keymap-functions keymap)
      (send keymap chain-to-keymap global-keymap #f)
      (send editor set-keymap keymap)
      (install-standard-text-bindings editor)
      (send canvas set-editor editor)
      editor))
  
  ;; initialization
  (add-editor-keymap-functions global-keymap)
  (add-text-keymap-functions global-keymap)
  )