
(begin-elaboration-time
 (require-library "sig.ss" "browser"))

(define-signature help:option^
  (startup-url))

(define-signature help:help^
  (add-doc-section
   add-kind-section
   add-choice))

(define-signature help:search^ (do-search))