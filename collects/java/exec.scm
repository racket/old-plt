;; Mario Latendresse, July 2000
;;
;; All files that should be loaded to use the Java compiler.
;;

(define Gambit? #f)

(require-library "errortrace.ss" "errortrace")
(require-library "match.ss")
(require-library "pretty.ss")
(require-library "pconvert.ss")
(constructor-style-printing #t)
(print-struct #t)

;; Lexer and parser tables and drivers.

(load "SILexSource/multilex.scm")
(load "lr-dvr.scm")
(load "java.table.scm")
(load "java.lal.scm")

(load "java_utilites.scm")

;;(load "gambitMacros.scm")
(load "mzSchemeSpecifics.scm")

;; All the define-structs are in the following file.
(load "java_struct.scm")

;; Reading libraries and convert them to AST.

(load "classToAst.scm")
(load "readWriteClass.scm")

;; For generating lexer and parser.
(load "genParserLexer.scm")

;; Semantic analysis.

(load "semantic.scm")

;; Bytecode generator

(load "genJVMcode.scm")

;; The main file.

(load "compiler.scm")

;; Quick test

(load "testSuite.scm")