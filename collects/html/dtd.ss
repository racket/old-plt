;; copyright by Paul Graunke June 2000 AD
(load-relative "dtds.ss")
(require-library "invoke.ss")

(require-library "function.ss")
(require-library "string.ss")

(define-values/invoke-unit/sig
 ((open dtd-ast^) (open dtd^))
 (compound-unit/sig
   (import (F : mzlib:function^) (S : mzlib:string^))
   (link
    [AST : dtd-ast^ ((require-library "dtd-ast.ss" "html"))]
    [ENT : entity-expander^ ((require-library "entity-expander.ss" "html"))]
    [DTD : dtd^ ((require-library "dtdr.ss" "html") AST ENT F S)])
   (export (open AST) (open DTD)))
 #f
 mzlib:function^
 mzlib:string^)

;; loosedtd.txt is from www.w3c.org's html4.0.1 spec.
;(define dtd (call-with-input-file "/home/ptg/Docs/Standards/loosedtd.txt" read-sgml-dtd))
