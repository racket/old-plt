;;
;; $Id: stdrs.ss,v 1.4 1997/08/15 22:34:43 krentel Exp mflatt $
;;
;; Helper functions for finding pieces of DrScheme console,
;; at least how I think it's put together.
;; Of course, if you reorganize the panels in DrScheme,
;; then you should change this code accordingly.
;;
;; Yeah, I know that methods here are cheating, but I still think
;; that some helper functions similar to these would be very useful
;; in testing drscheme.
;;

(unit/sig mred:test:drscheme^
  
  (import 
    mred:wx^
    [mred : mred:testable-window^])
  
  (define drs-error error)  ;; naive error reporting.
  
  ;;
  ;; Helper functions that used to be in stglobals.ss.
  ;; Only stdrs.ss uses them now.
  ;;
  
  ;; #f means no active frame.
  
  (define top-frame
    (lambda ()
      (let ([frame  (mred:test:get-active-frame)])
        (if frame
            frame
            (drs-error 'top-frame "no active frame")))))
  
  ;; get-top-panel returns () for no top panel.
  
  (define top-panel
    (opt-lambda ([frame #f])
      (let ([panel  (send (or frame (top-frame)) get-top-panel)])
        (if (null? panel)
            (drs-error 'top-panel "frame has no top panel")
            panel))))
  
  ;;
  ;; Return the n-th child from l (counting from 1)
  ;; and make sure l has at least n children.
  ;; It's just number of children that I don't trust.
  ;;
  
  (define safe-list-ref
    (lambda (l n)
      (cond
	[(not (pair? l))  (drs-error 'panel-descendant "panel has too few children")]
	[(= n 1)  (car l)]
	[else  (safe-list-ref (cdr l) (- n 1))])))
  
  (define panel-descendant
    (lambda (panel path)
      (cond
	[(null? path)  panel]
	[(pair? path)
	 (if (is-a? panel wx:panel%)
	     (panel-descendant
	      (safe-list-ref (ivar panel children) (car path))
	      (cdr path))
	     (drs-error 'panel-descendant "panel is not wx:panel%"))]
	[else  (drs-error 'panel-descendant "path is not proper list")])))
  
  (define get-defns-canvas
    (opt-lambda ([frame  #f])
      (panel-descendant (top-panel frame) '(1 1 1 2))))
  
  (define get-repl-canvas
    (opt-lambda ([frame #f])
      (panel-descendant (top-panel frame) '(1 1 1 3))))
  
  (define get-save-button
    (opt-lambda ([frame  #f])
      (panel-descendant (top-panel frame) '(1 1 1 1 2))))
  
  (define get-check-syntax-button
    (opt-lambda ([frame  #f])
      (panel-descendant (top-panel frame) '(1 1 1 1 6 1))))
  
  (define get-execute-button
    (opt-lambda ([frame  #f])
      (panel-descendant (top-panel frame) '(1 1 1 1 6 4))))
  
  )
