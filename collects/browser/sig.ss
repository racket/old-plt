
(begin-elaboration-time
 (require-library "files.ss")
 (require-library "functios.ss")
 (require-library "strings.ss")
 (require-library "urls.ss" "net")

 (require-relative-library "btrees.ss")
 (require-relative-library "bullets.ss"))

(define-signature browser:html^
  (html-convert
   html-status-handler))

(define-signature browser^
  (open-url
   (struct exn:file-saved-instead (pathname))
   (struct exn:cancelled ())
   
   hyper-text-mixin
   hyper-text%

   hyper-canvas-mixin
   hyper-canvas%

   hyper-panel-mixin
   hyper-panel%

   editor->page
   page->editor

   on-installer-run

   bullet-size))
