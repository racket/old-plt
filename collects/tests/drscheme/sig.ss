(require-library "function.ss")
(require-library "file.ss")
(require-library "guis.ss" "tests" "utils")

(let ([jr-test-sig
       (build-path (collection-path "mzlib") 'up 'up "tests" "drscheme-jr" "sig.scm")])
  (when (file-exists? jr-test-sig)
    (load-relative jr-test-sig)))

(define-signature drscheme:test-util^
  )
