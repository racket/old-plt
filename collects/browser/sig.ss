
(begin-elaboration-time
 (require-library "files.ss")
 (require-library "functios.ss")
 (require-library "strings.ss")
 (require-library "urls.ss" "net")

 (require-relative-library "btrees.ss")
 (require-relative-library "bullets.ss"))

(define-signature browser:html^
  (html-convert))

(define-signature browser^
  (open-url
   
   hyper-text-mixin
   hyper-text%

   hyper-canvas-mixin
   hyper-canvas%

   hyper-panel-mixin
   hyper-panel%

   editor->page
   page->editor))
