(begin-elaboration-time
 (require-library "sig.ss" "mred")
 (require-library "frameworks.ss" "framework"))

(define-signature help:drscheme-interface^
  (help-desk
   open-url
   open-users-url))

(begin-elaboration-time
 (require-relative-library "search-sig.ss"))

(begin-elaboration-time
 (require-library "sig.ss" "browser"))

(define-signature help:help^
  (new-help-frame
   open-url-from-user))
