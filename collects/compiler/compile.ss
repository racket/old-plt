
(require-relative-library "sig.ss")
(require-relative-library "option.ss")

(require-library "functio.ss")
(require-library "pretty.ss")
(require-library "file.ss")
(require-library "string.ss")
(require-library "compile.ss")

(require-library "compile.ss" "dynext")
(require-library "link.ss" "dynext")
(require-library "file.ss" "dynext")

(require-library "makes.ss" "make")
(require-library "collections.ss" "make")

(require-library "get-info.ss" "setup")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig compiler^
  (require-relative-library "compiler.ss")
  #f
  (compiler:option : compiler:option^)
  mzlib:function^
  mzlib:pretty-print^
  mzlib:file^
  mzlib:string^
  mzlib:compile^
  setup:info^
  dynext:compile^
  dynext:link^
  dynext:file^)
