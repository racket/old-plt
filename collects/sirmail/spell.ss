(module spell mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "class.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "framework.ss" "framework")
           (lib "contract.ss"))
  
  (provide/contract [activate-spelling ((is-a?/c color:text<%>) . -> . void?)])
  
  (define-lex-abbrevs
   (letter (: (- #\A #\Z) (- #\a #\z)))
   (paren (: #\( #\) #\[ #\] #\{ #\}))
   (white (: #\page #\newline #\return #\tab #\vtab #\space)))
                                   
  
  (define get-word
    (lexer 
     ((+ white)
      (values lexeme 'white-space #f (position-offset start-pos) (position-offset end-pos)))
     (paren
      (values lexeme 'no-color (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos)))
     ((+ (^ (: letter white paren)))
      (values lexeme 'no-color #f (position-offset start-pos) (position-offset end-pos)))
     ((+ letter)
      (let ((ok ((current-spell-checker) lexeme)))
        (values lexeme (if ok 'other 'error) #f (position-offset start-pos) (position-offset end-pos))))
     ((eof)
      (values lexeme 'eof #f #f #f))))
  
  (define (activate-spelling t)
    (send t start-colorer
          (lambda (s) (format "framework:syntax-coloring:scheme:~a" s))
          get-word
          `((|(| |)|)
            (|[| |]|)
            (|{| |}|))))

  (define extra-words '("sirmail" "mred" "drscheme" "mzscheme" "plt"))
  
  ;; get-dictionary : -> (union #f hash-table)
  ;; fetches the dictionary the first time it is called.
  (define get-dictionary
    (let ([dictionary #f])
      (lambda ()
        (let* ((possible-file-names '("/usr/share/dict/words" "/usr/dict/words"))
               (good-file-names (filter file-exists? possible-file-names)))
          (unless (null? good-file-names)
            (let ([d (make-hash-table 'equal)])
              (for-each (lambda (word) (hash-table-put! d word #t)) extra-words)
              (for-each 
               (lambda (good-file-name)
                 (call-with-input-file good-file-name
                   (lambda (i)
                     (let loop ()
                       (let ((word (read-line i)))
                         (unless (eof-object? word)
                           (hash-table-put! d word #t)
                           (loop)))))))
               good-file-names)
              (set! dictionary d))))
        (set! get-dictionary (lambda () dictionary))
        dictionary)))
      
  ;; naive-check : string -> bool
  (define (naive-check word)
    (let ([dict (get-dictionary)])
      (if dict
          (or (hash-table-get dict word (lambda () #f))
              (let ((s (string-copy word)))
                (string-lowercase! s)
                (hash-table-get dict s (lambda () #f))))
          #t)))
  
  (define current-spell-checker 
    (make-parameter naive-check))
  

  )
