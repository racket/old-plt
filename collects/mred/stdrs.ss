;;
;; $Id$
;;
;; Helper functions for finding pieces of DrScheme console,
;; at least how I think it's put together.
;; Of course, if you reorganize the panels in DrScheme,
;; then you should change this code accordingly.
;;

(unit/sig mred:test:drscheme^
  
  (import 
    [wx : mred:wx^]
    [mred:test : mred:test:struct^]
    [mred:test : mred:test:globals^])
  
  (define drs-error error)

;; *** ADD CHECKING FOR ERRORS, NON-EXISTENT
;; *** CHILDREN, ETC HERE.
  
  (define get-defns-canvas
    (let ([tag  'get-defns-canvas])
      (opt-lambda ([frame  #f])
	(let* 
	    ([f   (or frame (mred:test:top-frame))]
	     [p1  (send f get-top-panel)]
	     [p2  (car (ivar p1 children))]
	     [p3  (car (ivar p2 children))]
	     [p5  (car (ivar p3 children))]
	     [canvas  (cadr (ivar p5 children))])
	  canvas))))
  
  (define get-repl-canvas
    (let ([tag  'get-defns-canvas])
      (opt-lambda ([frame  #f])
	(let* 
	    ([f   (or frame (mred:test:top-frame))]
	     [p1  (send f get-top-panel)]
	     [p2  (car (ivar p1 children))]
	     [p3  (car (ivar p2 children))]
	     [p5  (car (ivar p3 children))]
	     [canvas  (caddr (ivar p5 children))])
	  canvas))))
  
  )
