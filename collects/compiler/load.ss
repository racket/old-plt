
(reference-library "refer.ss")

(reference-relative-library "sigload.ss")

(reference-library "functio.ss")
(reference-library "file.ss")
(reference-library "pretty.ss")

(reference-library "compile.ss" "mzscheme" "dynext")
(reference-library "link.ss" "mzscheme" "dynext")
(reference-library "file.ss" "mzscheme" "dynext")

(reference-library "option.ss" "compiler")

(invoke-open-unit/sig
 (reference-relative-library-unit/sig "loadr.ss")
 mzc
 mzlib:function^
 mzlib:pretty-print^
 mzlib:file^
 dynext:compile^
 dynext:link^
 dynext:file^
 (compiler:option : compiler:option^))

(invoke-open-unit/sig
 (reference-relative-library-unit/sig "ldr.ss")
 mzc
 dynext:compile^
 dynext:link^
 dynext:file^
 mzlib:function^
 (compiler:option : compiler:option^))
