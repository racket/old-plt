(module default-lexer mzscheme
  (require (lib "lex.ss" "parser-tools"))
  
  (provide default-lexer)
  
  (define-lex-abbrev parens (: #\( #\) #\[ #\] #\{ #\}))
  
  (define default-lexer
    (lexer
     ((+ (^ parens))
      (values 'white-space #f (position-offset start-pos) (position-offset end-pos)))
     (parens
      (values 'paren (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos)))
     ((eof)
      (values 'eof #f #f #f)))))
             
             