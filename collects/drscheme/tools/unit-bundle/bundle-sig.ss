(require-library "frameworks.ss" "framework")

(define-signature drscheme:bundle:misc^
  (set-box/f!
   
   set-dc-pen
   set-dc-brush
   white
   black))

(define-signature drscheme:bundle:compound-unit^
  ())

(define-signature drscheme:bundle:bundle-model^
  (bundle-manager% bundle-manager<%>
   
   bundle% bundle<%>
   leaf-bundle% leaf-bundle<%>
   node-bundle% node-bundle<%>))

(define-signature drscheme:bundle:bundle-view/control^
  (new-bundle-table-frame
   
   bundle-pasteboard%
   leaf-bundle-snip%
   node-bundle-snip%))