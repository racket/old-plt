(require-library "frameworks.ss" "framework")

(define-signature drscheme:bundle:misc^
  (set-box/f!
   
   set-dc-pen
   set-dc-brush
   white
   black))

(define-signature drscheme:bundle:compound-unit^
  ())

(define-signature drscheme:bundle:bundle^
  (new-bundle-table-frame
   
   bundle-manager% bundle-manager<%>
   
   bundle% bundle<%>
   leaf-bundle% leaf-bundle<%>
   node-bundle% node-bundle<%>
   
   bundle-pasteboard%
   leaf-bundle-snip%
   node-bundle-snip%))
