
(require-library "make.ss" "make")
(require-library "file.ss" "dynext")
(require-library "functio.ss")
(require-library "file.ss")

(require-relative-library "collections.ss")

(begin-elaboration-time
  (require-library "invoke.ss"))

(define-values/invoke-unit/sig make:collection^
  (require-relative-library "collectionr.ss")
  #f
  make:make^
  mzlib:function^
  mzlib:file^
  dynext:file^)
