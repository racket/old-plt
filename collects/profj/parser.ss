#cs
(module parser mzscheme
  (require "parsers/full-parser.ss"
           "parsers/advanced-parser.ss"
           "parsers/intermediate-parser.ss"
           "parsers/beginner-parser.ss"
           "parsers/parse-error.ss"
           "parsers/lexer.ss"
           "parameters.ss")
  
  (require (all-except (lib "lex.ss" "parser-tools") input-port))
  (provide parse parse-interactions parse-method lex-stream)
  
  ;function to lex in the entire port
  ;lex-port: port string -> (list position-token)
  (define (lex-port port filename)
    (port-count-lines! port)
    (file-path filename)
    (letrec ((getter
              (lambda (acc)
                (let ((cur-tok (get-token port)))
                  (if (eq? 'EOF (position-token-token cur-tok))
                      (cons cur-tok acc)
                      (getter (cons cur-tok acc)))))))
      (reverse! (getter null))))
  ;getter: (list position-token) -> (-> position-token)
  (define (getter token-list)
    (lambda ()
      (begin0 (car token-list)
              (unless (null? (cdr token-list))
                (set! token-list (cdr token-list))))))

  ;main parsing function
  
  ;parse: port string symbol -> package
  (define (parse is filename level)
    (let* ((lexed (lex-port is filename))
           (my-get (getter lexed)))
      (lex-stream (lambda () (getter lexed)))
      (case level
        ((beginner) 
         (determine-error find-beginner-error) 
         (parse-beginner my-get))
        ((intermediate) 
         (determine-error find-intermediate-error)
         (parse-intermediate my-get))
        ((advanced) 
         (determine-error find-advanced-error)
         (parse-advanced my-get))
        ((full) (parse-full my-get)))))
  
  ;parse-interactions: port string symbol -> (U Statement Expression)
  (define (parse-interactions is loc level)
    (let* ((lexed (lex-port is loc))
           (my-get (getter lexed)))
      (lex-stream (lambda () (getter lexed)))
      (case level
        ((beginner) 
         (determine-error find-beginner-error-interactions)
         (parse-beginner-interactions my-get))
        ((intermediate) 
         (determine-error find-intermediate-error-interactions)
         (parse-intermediate-interactions my-get))
        ((advanced) 
         (determine-error find-advanced-error-interactions)
         (parse-advanced-interactions my-get))
        ((full) (parse-full-interactions my-get)))))
  
  ;parse-method: port string symbol -> method
  (define (parse-method is loc level)
    (let* ((lexed (lex-port is loc))
           (my-get (getter lexed)))
      (lex-stream (lambda () (getter lexed)))
      (case level
        ((beginner)
         (determine-error find-beginner-error-method)
         (parse-beginner-method my-get))
        ((intermediate)
         (determine-error find-intermediate-error-method)
         (parse-intermediate-method my-get))
        ((advanced)
         (determine-error find-advanced-error-method)
         (parse-advanced-method my-get))
        ((full) (parse-full-method my-get)))))
  )
