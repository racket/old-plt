;; copyright by Paul Graunke June 2000 AD
(require-library "htmls.ss" "html")
(require-library "function.ss")
(require-library "file.ss")
(require-library "xml.ss" "xml")

(define-values/invoke-unit/sig
 html^
 (compound-unit/sig
   (import (f : mzlib:function^) (file : mzlib:file^) [x : xml^])
   (link
    [s : sgml-reader^ ((require-library "sgml-reader.ss" "html") (x : xml-structs^) f)]
    [h : html^ ((require-library "htmlr.ss" "html") x s f file)])
   (export (open h)))
 html
 mzlib:function^
 mzlib:file^
 xml^)
