
(begin-elaboration-time
 (require-library "files.ss")
 (require-library "functios.ss")
 (require-library "strings.ss")
 (require-library "urls.ss" "net"))

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

   hyper-frame-mixin
   hyper-frame%

   editor->page
   page->editor))
