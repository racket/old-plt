
(require-library "refer.ss")

(require-relative-library "sigload.ss")

(require-library "functio.ss")
(require-library "file.ss")
(require-library "pretty.ss")

(require-library "compile.ss" "dynext")
(require-library "link.ss" "dynext")
(require-library "file.ss" "dynext")

(require-library "option.ss" "compiler")

(when (compiler:option:use-mrspidey)
  (require-relative-library "spsigload.ss"))

(invoke-open-unit/sig
 (if (compiler:option:use-mrspidey)
     (require-relative-library-unit/sig "sploadr.ss")
     (require-relative-library-unit/sig "loadr.ss"))
 mzc
 mzlib:function^
 mzlib:pretty-print^
 mzlib:file^
 dynext:compile^
 dynext:link^
 dynext:file^
 (compiler:option : compiler:option^))

(invoke-open-unit/sig
 (require-relative-library-unit/sig "ldr.ss")
 mzc
 dynext:compile^
 dynext:link^
 dynext:file^
 mzlib:function^
 (compiler:option : compiler:option^))
