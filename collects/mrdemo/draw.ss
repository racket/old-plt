
; Defines a simple text and/or pasteboard editor program, which
; demonstrates how to use the basic editor classes. The program also
; demonstrates how to create menus.

(require-relative-library "graph.ss") ; defines the box and graph snips

(define (new-text-frame) (new-frame text%))
(define (new-pasteboard-frame) (new-frame pasteboard%))

(define (new-frame editor%)
  (define f (make-object frame% "Simple Editor" #f 400 400))
  (define c (make-object editor-canvas% f))
  (define e (make-object editor%))
  (define mb (make-object menu-bar% f))

  (define file-menu (make-object menu% "File" mb))
  (define edit-menu (make-object menu% "Edit" mb))
  (define font-menu (make-object menu% "Font" mb))

  (make-object menu-item% "New Text Frame" file-menu
	       (lambda (item event) 
		 (new-text-frame))
	       #\N)
  (make-object menu-item% "New Pasteboard Frame" file-menu
	       (lambda (item event) 
		 (new-pasteboard-frame)))

  (make-object menu-item% "Open..." file-menu
	       (lambda (item event) 
		 (send e load-file ""))
	       #\O)
  (make-object menu-item% "Save As..." file-menu
	       (lambda (item event) 
		 (send e save-file ""))
	       #\S)
  (make-object separator-menu-item% file-menu)
  (make-object menu-item% "Close" file-menu
	       (lambda (item event)
		 (send f show #f))
	       #\Q)

  (append-editor-operation-menu-items edit-menu #f)
  (make-object menu-item% "Insert Plain Box" edit-menu
	       (lambda (item event)
		 (send e insert (make-object draw-snip% 100 100))))
  (make-object menu-item% "Insert Graph..." edit-menu
	       (lambda (item event)
		 (let ([s (get-text-from-user "Formula"
					      "Formula to plot in [0,1]:"
					      f
					      "(lambda (x) x)")])
		   (when s
		     (let ([v (read-from-string s)])
		       (send e insert 
			     (make-object graph-snip% (list v))))))))

  (append-editor-font-menu-items font-menu)
  (when (is-a? e text%)
    (install-standard-text-bindings e))
  (send c set-editor e)
  (send f show #t))

(new-text-frame)
