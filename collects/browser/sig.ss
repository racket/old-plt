
(begin-elaboration-time
 (require-library "files.ss")
 (require-library "functios.ss")
 (require-library "strings.ss")
 (require-library "urls.ss" "net")

 (require-library "plt-installers.ss" "setup")

 (require-library "frameworks.ss" "framework")

 (require-relative-library "btrees.ss")
 (require-relative-library "bullets.ss"))

(define-signature browser:html^
  (html-convert
   html-status-handler))

(define-signature browser^
  (open-url
   (struct exn:file-saved-instead (pathname))
   (struct exn:cancelled ())
   
   hyper-style-list
   hyper-text-mixin
   hyper-text%

   hyper-canvas-mixin
   hyper-canvas%

   hyper-panel-mixin
   hyper-panel%

   hyper-frame-mixin
   hyper-frame%

   editor->page
   page->editor

   bullet-size))
