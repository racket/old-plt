(require-library "functios.ss")
(require-library "macro.ss")
(require-library "stsigs.ss" "mred")

(define-signature mred:constants^
  (original-input-port
   original-error-port
   original-output-port))

(define-signature mred:connections^
  (connections-frame%
   connections-dialog-box%
   connections-media-edit%
   connections-media-pasteboard%
   connections-media-canvas%
   connections-panel%

   make-connections-frame%
   make-connections-media-buffer%
   make-connections-media-canvas%
   make-connections-panel%))

(define-signature mred:container-children^
  (const-default-size
   const-default-posn
   const-default-spacing
   const-default-border
   (struct child-info (x-posn y-posn x-min y-min x-margin y-margin x-stretch y-stretch))
   get-two-int-values
   non-negative-number?
   same-dimension?
   make-item%
   std-enable
   button%
   check-box%
   choice%
   gauge%
   list-box%
   message%
   radio-box%
   slider%
   text%
   multi-text%
   canvas%
   media-canvas%
   text-window%
   canvas-message%))

(define-signature mred:container-frames^
  (frame% dialog-box%))

(define-signature mred:container-children-export^
  (const-default-size
   const-default-posn
   const-default-spacing
   const-default-border
   (struct child-info (x-posn y-posn x-min y-min x-stretch y-stretch))
   button%
   check-box%
   choice%
   gauge%
   list-box%
   message%
   radio-box%
   slider%
   text%
   multi-text%
   canvas%
   media-canvas%
   text-window%
   canvas-message%))

(define-signature mred:container-panels^
  (debug-borders
   panel%
   horizontal-panel%
   vertical-panel%
   single-panel%))

(define-signature mred:container^
  ((open mred:container-frames^)
   (open mred:container-children-export^)
   (open mred:container-panels^)))

(define-signature mred:minimal^
  ((unit constants : mred:constants^)
   (unit testable : mred:testable-window^)
   (unit connections : mred:connections^)
   (unit container : mred:container^)))
