(unit/sig drscheme:compound-unit^
  (import [mred : mred-interfaces^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [drscheme:graph : drscheme:graph^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:face : drscheme:face^])
  
  (define BLACK-PEN (send mred:the-pen-list find-or-create-pen "BLACK" 0 'solid))
  (define WHITE-BRUSH (send mred:the-brush-list find-or-create-brush "WHITE" 'solid))
  
  (define project-pasteboard%
    (class drscheme:graph:graph-pasteboard% (unit . args)
      (inherit find-first-snip get-snip-location get-canvas
	       find-next-selected-snip get-dc find-snip
	       invalidate-bitmap-cache begin-write-header-footer-to-file
	       end-write-header-footer-to-file)
      (override
	[interactive-adjust-resize
	 (lambda (snip wb hb)
	   (set-box! wb (max (unbox wb) (ivar snip min-width)))
	   (set-box! hb (max (unbox hb) (ivar snip min-height))))]
	[interactive-adjust-move
	 (lambda (snip xb yb)
	   (let ([min-x 2]
		 [min-y 2]
		 [x (unbox xb)]
		 [y (unbox yb)])
	     (if (< x y)
		 (begin '(set-box! xb 0)
			(set-box! yb (max y min-y)))
		 (begin (set-box! xb (max x min-x))
			'(set-box! yb 0)))))])
      (rename [super-on-insert on-insert])
      (override
	[on-insert
	 (opt-lambda (snip before x y)
	   (and (super-on-insert snip before x y)
		(let ([name (send (send snip get-unit) get-name)])
		  (let loop ([snip (find-first-snip)])
		    (cond
		      [(null? snip) #t]
		      [(string=? (send (send snip get-unit) get-name) name)
		       (begin (mred:message-box 
			       (format "only insert one unit with the name ~a"
				       snip))
			      #f)]
		      [else (loop (send snip next))])))))]
	[on-double-click
	 (lambda (snip evt)
	   (let loop ([s (find-next-selected-snip null)])
	     (unless (null? s)
	       (let ([unit (send s get-unit)])
		 (send unit create-frame)
		 (send (send unit get-frame) show #t))
	       (loop (find-next-selected-snip s)))))])
      (sequence
	(apply super-init args))))
  
  (define super-frame% 
    (drscheme:frame:mixin
     fw:frame:pasteboard-info%))
  
  (define frame%
    (class* super-frame% (drscheme:face:compound-unit-frameI) (unit)
      (inherit show show-menu get-area-container)
      (rename [super-on-close on-close])
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (send unit frame-closed))])
      
      (rename [super-update-shown update-shown])
      (private
	[evaluation-order-id #f])
      (public
	[update-shown
	 (lambda ()
	   (super-update-shown)
	   (send (get-area-container) change-children
		 (lambda (l)
		   (let ([removed (mzlib:function:remq eval-panel l)])
		     (if (send evaluation-order-item is-checked?)
			 (cons eval-panel removed)
			 removed)))))])
      (public
	[after-change-name void]
	[after-add-import 
	 (lambda (unit)
	   (let ([imp (make-object import-snip% unit)])
	     (send (get-editor) insert imp)))]
	[after-add-export void]
	[after-remove-import void]
	[after-remove-export void]
	[get-unit (lambda () unit)])

      (public
	[file-menu:save-as 
	 (lambda () 
	   (let ([file (mred:put-file)])
	     (when file
	       (send (get-editor) save-file file)))
	   #t)])
      
      (override
	[get-area-container% (lambda () mred:horizontal-panel%)]
	[get-canvas% (lambda () mred:editor-canvas%)]
	[get-editor% (lambda () project-pasteboard%)]
	[get-editor (lambda () (send unit get-buffer))])
      
      (sequence
	(super-init unit)
	(let* ([mb (get-make-menu)]
	       [add-menu (make-object mred:menu% mb "Add")])
	  (set! evaluation-order-item
		(make-object mred:checkable-menu-item%
		  "Evaluation Order"
		  show-menu
		  (lambda () (update-shown))
		  #t
		  "Show or hide the evaluation order list"))
	  (send evaluation-order-item check #t)
	     
	  (send add-menu append-item "Unit..."
		(lambda ()
		  (let ([unit (drscheme:unit:make-unit #f)])
		    (send (get-editor) insert (send unit create-snip)))))
	  (send add-menu append-item "Compound Unit..."
		(lambda ()
		  (let ([cu (make-compound-unit #f)])
		    (send (get-editor) insert (send cu create-snip)))))))

      (private
	[eval-panel (make-object mred:vertical-panel% (get-area-container))]
	[eval-msg (make-object mred:message% eval-panel "Evaluation Order")]
	[eval-list (make-object mred:list-box%
		     "Evaluation Order"
		     null eval-panel void)])
      
      (sequence
	(update-shown)
	(send eval-list stretchable-in-x #f)
	(send eval-panel stretchable-in-x #f)
	(show #t))))
  
  (define snip%
    (class* drscheme:graph:node-snip% (drscheme:face:compound-unit-snipI) (unit)
      (inherit width height set-width set-height invalidate-to)
      (rename [super-draw draw])
      (public
	[get-name 
	 (lambda ()
	   (send unit get-name))])

      (public
	[after-change-name (lambda () (invalidate-to this))]
	[after-add-import void]
	[after-add-export void]
	[after-remove-import void]
	[after-remove-export void]
	[get-unit (lambda () unit)])

      ;;; this won't work, use on-delete from the pasteboard to
      ;;;  send a message to the snip
      (rename [super-release-from-owner release-from-owner])
      (public
	[release-from-owner
	 (lambda ()
	   (and (super-release-from-owner)
		(send unit remove-snip this)))])
      (public
	[copy (lambda () (send unit create-snip))]
	[snipclass compound-unit-snipclass]
	[draw
	 (lambda (dc x y left top right bottom dx dy draw-caret)
	   (let ([space 2]
		 [old-pen (send dc get-pen)]
		 [old-brush (send dc get-brush)])
	     (send dc set-pen BLACK-PEN)
	     (send dc set-brush WHITE-BRUSH)
	     (send dc draw-rectangle x y width height)
	     (send dc set-pen old-pen)
	     (send dc set-brush old-brush)
	     
	     (set-width (- width (* 2 space)))
	     (set-height (- height (* 2 space)))
	     (super-draw dc (+ x space) (+ y space) left top right bottom dx dy draw-caret)
	     (set-width (+ width (* 2 space)))
	     (set-height (+ height (* 2 space)))))])
      (sequence
	(super-init))))
  
  (define snip-class%
    (let ([s% snip%])
      (class-asi drscheme:unit:snip-class%
	(public
	  [snip% s%]
	  [version 1]
	  [classname "drscheme:compound-unit:snip%"]))))
  
  (define compound-unit-snipclass (make-object snip-class%))
  
  (define import-snip%
    (class-asi drscheme:unit:snip%
      (inherit width height set-width set-height)
      (rename [super-draw draw])
      (public
	[snipclass import-snipclass]
	[this% snip%]
	[add-parent
	 (lambda (c)
	   (mred:message-box "cannot import into an import"))]
	[draw-border 
	 (lambda (dc x y width height)
	   (send dc draw-ellipse x y width height))])))
  
  (define import-snip-class%
    (let ([s% import-snip%])
      (class-asi drscheme:unit:snip-class%
	(public
	  [snip% s%]
	  [version 1]
	  [classname "drscheme:compound-unit:import-snip%"]))))
  
  (define import-snipclass (make-object import-snip-class%))
  
  (define compound-unit%
    (let ([f% frame%]
	  [s% snip%])
      (class* drscheme:unit:unit% (drscheme:face:compound-unitI) (fn . cn)
	(public
	  [buffer% project-pasteboard%]
	  [frame% f%]
	  [snip% s%])
	(sequence
	  (apply super-init fn cn)))))

  (define (make-compound-unit filename . collections)
    (apply make-object compound-unit% filename collections))

  (fw:handler:insert-format-handler "Compound Units"
				    (list "cut")
				    (opt-lambda (name)
				      (make-object frame% name #f))))
