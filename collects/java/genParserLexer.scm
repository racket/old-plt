;; Mario Latendresse, 11 May 2000
;;
;; The main functions to generate the Java lexer and parser.
;;

(define DIR-LEXER #f)
(define (gen-java-lexer)
  (set! DIR-LEXER "~/java/comp/SILexSource/")
  (load "~/java/comp/SILexSource/lex.scm")
  (lex-tables "~/java/comp/java.lex" "java-lex-table" "~/java/comp/java.table.scm" 
	      'counters 'all))

(define (gen-java-parser)
  (load "~/java/comp/lalr.scm")
  (load "~/java/comp/java.lal")
  (gen-lalr1 java-grammar "~/java/comp/java.lal.scm" 'java))






