(module default-lexer mzscheme
  (require (lib "lex.ss" "parser-tools"))
  
  (provide default-lexer)
  
  (define-lex-abbrevs 
   (parens (: #\( #\) #\[ #\] #\{ #\}))
   (white-space (: #\newline #\return #\tab #\space #\vtab)))
                     
  
  (define default-lexer
    (lexer
     ((+ (^ parens white-space))
      (values 'no-color #f (position-offset start-pos) (position-offset end-pos)))
     ((+ white-space)
      (values 'white-space #f (position-offset start-pos) (position-offset end-pos)))
     (parens
      (values 'no-color (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos)))
     ((eof)
      (values 'eof #f #f #f)))))
             
             