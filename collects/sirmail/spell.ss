(module spell mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "class.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "framework.ss" "framework")
           (lib "contract.ss")
	   (lib "file.ss"))
  
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
      (let ((ok (check-word lexeme)))
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
  
  (define ask-chan (make-channel))
  
  (let ()
    ;; fetch-dictionary : -> (union #f hash-table)
    ;; computes a dictionary, if any of the possible-file-names exist
    (define (fetch-dictionary)
      (let* ([possible-file-names '("/usr/share/dict/words"
                                    "/usr/share/dict/connectives"
                                    "/usr/share/dict/propernames"
                                    "/usr/dict/words")]
             [good-file-names (filter file-exists? possible-file-names)])
        (unless (null? good-file-names)
          (let ([d (make-hash-table 'equal)])
            (for-each (lambda (word) (hash-table-put! d word #t)) extra-words)
            (for-each 
             (lambda (good-file-name)
               (call-with-input-file* good-file-name
                 (lambda (i)
                   (let loop ()
                     (let ((word (read-line i)))
                       (unless (eof-object? word)
                         (hash-table-put! d word #t)
                         (loop)))))))
             good-file-names)
            d))))
  
    (thread
     (lambda ()
       (let loop ([computed? #f]
                  [dict #f])
         (let-values ([(answer-chan give-up-chan) (apply values (channel-get ask-chan))])
           (let ([computed-dict (if computed?
                                    dict
                                    (fetch-dictionary))])
             (object-wait-multiple
              #f
              (make-wrapped-waitable
               (make-channel-put-waitable answer-chan computed-dict)
               (lambda (done)
                 (loop #t computed-dict)))
              (make-wrapped-waitable
               give-up-chan
               (lambda (done)
                 (loop #t computed-dict))))))))))
  
  (define (get-dictionary)
    (object-wait-multiple
     #f
     (make-nack-guard-waitable
      (lambda (failed)
        (let ([result (make-channel)])
          (channel-put ask-chan (list result failed))
          result)))))
      
  ;; check-word : string -> boolean
  (define (check-word word)
    (let* ([dict (get-dictionary)]
           [word-ok (lambda (w) (hash-table-get dict w (lambda () #f)))]
           [subword-ok (lambda (reg)
                         (let ([m (regexp-match reg word)])
                           (and m
                                (word-ok (cadr m)))))])
      (if dict
          (or (word-ok word)
              ;(word-ok (string-lower-case! (string-copy word))) ;; should re-add?
              (subword-ok #rx"(.*)ing$")
              (subword-ok #rx"(.*)ed$")
              (subword-ok #rx"(.*)s$"))
          #t))))
