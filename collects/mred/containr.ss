; resizes too small really confuse things, since client sizes wrap (-2
; becomes 65534.  Matthew knows; I'm waiting to hear from him before I do
; anything.)

;-----------------------------------------------------------------------

; exported ID's:
;  const-default-size
;  const-default-posn
;  const-default-spacing
;  make-child-info, etc.
;  frame%
;  dialog-box%
;  canvas%
;  media-canvas%
;  button%
;  check-box%
;  choice%
;  gauge%
;  list-box%
;  message%
;  radio-box%
;  slider%
;  text-window%
;  text%
;  multi-text%
;  panel%
;  horizontal-panel%
;  vertical-panel%
;  single-panel%

  (compound-unit/sig
    (import [constants : mred:constants^]
	    [connections : mred:connections^]
            [function : mzlib:function^])
    (link [container-frames : mred:container-frames^
	    ((reference-unit/sig "contfram.ss")
	     constants connections container-children container-panels)]

	  [container-children : mred:container-children^
	    ((reference-unit/sig "contkids.ss")
	     constants connections container-frames container-panels)]
	  [container-panels : mred:container-panels^
	    ((reference-unit/sig "contpanl.ss")
	     constants connections function container-children)])
    (export
     (open (container-frames : mred:container-frames^))
     (open (container-children : mred:container-children-export^))
     (open (container-panels : mred:container-panels^))))
