#cs
(module parser mzscheme
  (require "parsers/full-parser.ss"
           "parsers/advanced-parser.ss"
           "parsers/intermediate-parser.ss"
           "parsers/beginner-parser.ss"
           "parsers/beginner-error.ss"
           "parsers/lexer.ss"
           "parameters.ss")
  
  (require (lib "lex.ss" "parser-tools"))
  
  (provide parse parse-interactions)
  
  ;main parsing function
  
  ;parse: port string symbol -> package
  (define (parse is filename level)
    (port-count-lines! is)
    (file-path filename)
    (let ((getter (lambda () (get-token is))))
      (case level
        ((beginner) 
         (determine-error (lambda () (find-beginner-error is))) 
         (parse-beginner getter))
        ((intermediate) (parse-intermediate getter))
        ((advanced) (parse-advanced getter))
        ((full) (parse-full getter)))))
  
  ;parse-interactions: port string symbol -> (U Statement Expression)
  (define (parse-interactions is loc level)
    (port-count-lines! is)
    (file-path loc)
    (let ((getter (lambda () (get-token is))))
      (case level
        ((beginner) 
         (determine-error (lambda () (find-beginner-error-interactions is)))
         (parse-beginner-interactions getter))
        ((intermediate) (parse-intermediate-interactions getter))
        ((advanced) (parse-advanced-interactions getter))
        ((full) (parse-full-interactions getter)))))
  )
