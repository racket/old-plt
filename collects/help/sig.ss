(require-library "classd.ss")

(begin-elaboration-time
 (require-library "sig.ss" "mred")
 (require-library "frameworks.ss" "framework"))

(require-library "get-infos.ss" "setup")

(define-signature help:doc-position^
  (user-defined-doc-position))

(define-signature help:drscheme-interface^
  (help-desk
   open-url
   open-users-url))

(begin-elaboration-time
 (require-relative-library "search-sig.ss"))

(begin-elaboration-time
 (require-library "sig.ss" "browser"))

(define-signature help:help-window^
  (new-help-frame
   open-url-from-user
   set-font-size))

(define-signature help:help^
  ((open help:help-window^)
   doc-collections-changed))