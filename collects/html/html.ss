;; copyright by Paul Graunke June 2000 AD
(require-library "htmls.ss" "html")
(require-library "function.ss")
(require-library "file.ss")

(define-values/invoke-unit/sig
 html^
 (compound-unit/sig
   (import (F : mzlib:function^) (FILE : mzlib:file^))
   (link
    [X : xml^ ((require-library "xmlr.ss" "xml") F)]
    [S : sgml-reader^ ((require-library "sgml-reader.ss" "html") (x : xml-structs^) F)]
    [H : html^ ((require-library "htmlr.ss" "html") X S F FILE)])
   (export (open H)))
 #f
 mzlib:function^)
