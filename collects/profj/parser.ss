#cs
(module parser mzscheme
  (require "parsers/full-parser.ss"
           "parsers/advanced-parser.ss"
           "parsers/intermediate-parser.ss"
           "parsers/beginner-parser.ss"
           "parsers/parse-error.ss"
           "parsers/lexer.ss"
           "parameters.ss")
  
  (require (lib "lex.ss" "parser-tools"))
  
  (provide parse parse-interactions parse-method)
  
  ;main parsing function
  
  ;parse: port string symbol -> package
  (define (parse is filename level)
    (port-count-lines! is)
    (file-path filename)
    (let ((getter (lambda () (get-token is))))
      (case level
        ((beginner) 
         (determine-error find-beginner-error) 
         (parse-beginner getter))
        ((intermediate) 
         (determine-error find-intermediate-error)
         (parse-intermediate getter))
        ((advanced) 
         (determine-error find-advanced-error)
         (parse-advanced getter))
        ((full) (parse-full getter)))))
  
  ;parse-interactions: port string symbol -> (U Statement Expression)
  (define (parse-interactions is loc level)
    (port-count-lines! is)
    (file-path loc)
    (let ((getter (lambda () (get-token is))))
      (case level
        ((beginner) 
         (determine-error find-beginner-error-interactions)
         (parse-beginner-interactions getter))
        ((intermediate) 
         (determine-error find-intermediate-error-interactions)
         (parse-intermediate-interactions getter))
        ((advanced) 
         (determine-error find-advanced-error-interactions)
         (parse-advanced-interactions getter))
        ((full) (parse-full-interactions getter)))))
  
  ;parse-method: port string symbol -> method
  (define (parse-method is loc level)
    (port-count-lines! is)
    (file-path loc)
    (let ((getter (lambda () (get-token is))))
      (case level
        ((beginner)
         (determine-error find-beginner-error-method)
         (parse-beginner-method getter))
        ((intermediate)
         (determine-error find-intermediate-error-method)
         (parse-intermediate-method getter))
        ((advanced)
         (determine-error find-advanced-error-method)
         (parse-advanced-method getter))
        ((full) (parse-full-method getter)))))
  )
