
(define-signature mred:hyper-edit^
  ((struct hypertag (name position))
   (struct hyperlink (anchor-start anchor-end reference-file reference-tag))
   hyper-buffer-data%
   hyper-data-class
   make-hyper-edit%
   hyper-edit%))

(define-signature mred:hyper-dialog^
  (hyper-tag-dialog%
   hyper-get-current-tags))

(define-signature mred:hyper-frame^
  (hyper-frame-group
   make-hyper-canvas%
   hyper-canvas%
   make-hyper-basic-frame%
   hyper-basic-frame%
   make-hyper-view-frame%
   hyper-view-frame%
   make-hyper-make-frame%
   hyper-make-frame%
   open-hyper-view
   open-hyper-make
   hyper-text-require))
