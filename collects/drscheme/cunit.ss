(unit/sig drscheme:compound-unit^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [drscheme:unit : drscheme:unit^]
	  [drscheme:frame : drscheme:frame^]
	  [drscheme:face : drscheme:face^])
  
  (mred:debug:printf 'invoke "drscheme:compound-unit@")
  
  (define BLACK-PEN (send wx:the-pen-list find-or-create-pen
			  "BLACK" 0 wx:const-solid))
  (define WHITE-BRUSH (send wx:the-brush-list find-or-create-brush
			    "WHITE" wx:const-solid))
  
  (define project-pasteboard%
    (class mred:graph-pasteboard% (unit . args)
      (inherit find-first-snip get-snip-location get-frame get-canvas
	       find-next-selected-snip get-dc find-snip
	       invalidate-bitmap-cache begin-write-header-footer-to-file
	       end-write-header-footer-to-file)
      (public
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
      (public
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
	       '(printf "evt: button-d-click? ~a button-down? ~a button-up? ~a
button? 1 ~a button? 2 ~a button? 3 ~a
dragging? ~a entering? ~a get-alt-down ~a get-control-down ~a
get-left-down ~a get-meta-down ~a get-middle-down ~a get-right-down ~a
get-shift-down ~a  get-time-stamp ~a  get-x ~a  get-y ~a  
is-button? ~a  leaving? ~a  moving?~a~n"
			(send evt button-d-click?) (send evt button-down?) (send evt button-up?)
			(send evt button? 1) (send evt button? 2) (send evt button? 3)
			(send evt dragging?) (send evt entering?) (send evt get-alt-down) (send evt get-control-down)
			(send evt get-left-down) (send evt get-meta-down) (send evt get-middle-down) (send evt get-right-down)
			(send evt get-shift-down) (send evt get-time-stamp) (send evt get-x) (send evt get-y)
			(send evt is-button?) (send evt leaving?) (send evt moving?))
	       '(printf "get-event-class ~a get-event-object ~a~n get-event-type ~a~n~n"
			(send evt get-event-class)
			(send evt get-event-object)
			(send evt get-event-type))
	       (let ([unit (send s get-unit)])
		 (send unit create-frame)
		 (send (send unit get-frame) show #t))
	       (loop (find-next-selected-snip s)))))])
      (sequence
	(apply super-init args))))
  
  (define super-frame% 
    (drscheme:frame:make-frame%
     mred:simple-menu-frame%))
  
  (define frame%
    (class* super-frame% (drscheme:face:compound-unit-frameI) (unit)
      (inherit show show-menu panel)
      (rename [super-do-close do-close])
      (public
	[do-close
	 (lambda ()
	   (super-do-close)
	   (send unit frame-closed))])
      
      (rename [super-make-menu-bar make-menu-bar]
	      [super-update-shown update-shown])
      (private
	[evaluation-order-id #f])
      (public
	[update-shown
	 (lambda ()
	   (super-update-shown)
	   (send panel change-children
		 (lambda (l)
		   (let ([removed (mzlib:function@:remq eval-panel l)])
		     (if (send show-menu checked? evaluation-order-id)
			 (cons eval-panel removed)
			 removed)))))]
	[make-menu-bar
	 (lambda ()
	   (let ([mb (super-make-menu-bar)]
		 [add-menu (make-object mred:menu%)])
	     (set! evaluation-order-id
		   (send show-menu append-item
			 "Evaluation Order"
			 (lambda () (update-shown))
			 "Show or hide the evaluation order list"
			 #t))
	     (send show-menu check evaluation-order-id #t)
	     
	     (send mb append add-menu "Add")
	     (send add-menu append-item "Unit..."
		   (lambda ()
		     (let ([unit (drscheme:unit:make-unit #f)])
		       (send (get-edit) insert (send unit create-snip)))))
	     (send add-menu append-item "Compound Unit..."
		   (lambda ()
		     (let ([cu (make-compound-unit #f)])
		       (send (get-edit) insert (send cu create-snip)))))
	     mb))])
      (public
	[after-change-name void]
	[after-add-import 
	 (lambda (unit)
	   (let ([imp (make-object import-snip% unit)])
	     (send (get-edit) insert imp)))]
	[after-add-export void]
	[after-remove-import void]
	[after-remove-export void]
	[get-unit (lambda () unit)])

      (public
	[file-menu:save-as 
	 (lambda () 
	   (let ([file (mred:put-file)])
	     (when file
	       (send (get-edit) save-file file)))
	   #t)])
      
      (public
	[get-panel% (lambda () mred:horizontal-panel%)]
	[get-canvas% (lambda () mred:media-canvas%)]
	[get-edit% (lambda () project-pasteboard%)]
	[get-edit (lambda () (send unit get-buffer))])
      
      (sequence
	(super-init unit))
      (private
	[eval-panel (make-object mred:vertical-panel% panel)]
	[eval-msg (make-object mred:message% eval-panel "Evaluation Order")]
	[eval-list (make-object mred:list-box% eval-panel null "")])
      
      (sequence
	(update-shown)
	(send eval-list stretchable-in-x #f)
	(send eval-panel stretchable-in-x #f)
	(show #t))))
  
  (define snip%
    (class* mred:node-snip% (drscheme:face:compound-unit-snipI) (unit)
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

  (mred:insert-format-handler "Compound Units"
			      (list "cut")
			      (opt-lambda (name)
				(make-object frame% name #f))))
