(module spell mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "class.ss")
           (lib "string.ss")
           (lib "list.ss")
           (lib "framework.ss" "framework")
           (lib "contract.ss")
	   (lib "file.ss")
           (lib "process.ss"))
  
  (provide/contract [activate-spelling ((is-a?/c color:text<%>) . -> . void?)])
  
  (define-lex-abbrevs
   (letter (: (- #\A #\Z) (- #\a #\z) #\'))
   (paren (: #\( #\) #\[ #\] #\{ #\}))
   (white (: #\page #\newline #\return #\tab #\vtab #\space)))
                                   
  (define get-word
    (lexer 
     ((+ white)
      (values lexeme 'white-space #f (position-offset start-pos) (position-offset end-pos)))
     (paren
      (values lexeme 'other (string->symbol lexeme) (position-offset start-pos) (position-offset end-pos)))
     ((+ (^ (: letter white paren)))
      (values lexeme 'other #f (position-offset start-pos) (position-offset end-pos)))
     ((+ letter)
      (let ((ok (spelled-correctly? lexeme)))
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

  (define ask-chan (make-channel))
  
  ;; spelled-correctly? : string -> boolean
  (define (spelled-correctly? word)
    (object-wait-multiple
     #f
     (make-nack-guard-waitable
      (lambda (failed)
        (let ([result (make-channel)])
          (channel-put ask-chan (list result failed word))
          result)))))
  
  (thread
   (lambda ()
     (let loop ([computed? #f]
                [dict #f])
       (let-values ([(answer-chan give-up-chan word) (apply values (channel-get ask-chan))])
         (let ([computed-dict (if computed?
                                  dict
                                  (fetch-dictionary))])
           (object-wait-multiple
            #f
            (make-wrapped-waitable
             (make-channel-put-waitable answer-chan (check-word computed-dict word))
             (lambda (done)
               (loop #t computed-dict)))
            (make-wrapped-waitable
             give-up-chan
             (lambda (done)
               (loop #t computed-dict)))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; The code below all runs in the above thread (only)
  ;;;
  
  (define extra-words '("sirmail" "mred" "drscheme" "mzscheme" "plt"))
  
  (define has-ispell? 'dontknow)
  (define ispell-prog #f)
  (define ispell-in #f)
  (define ispell-out #f)
  (define ispell-err #f)
  (define (ispell-word word)
    (when (eq? has-ispell? 'dontknow)
      (let ([existings (filter file-exists? '("/sw/bin/ispell"
                                              "/usr/bin/ispell"
                                              "/bin/ispell"
                                              "/usr/local/bin/ispell"))])
        (if (null? existings)
            (set! has-ispell? #f)
            (begin
              (set! has-ispell? #t)
              (set! ispell-prog (car existings))))))
    (cond
      [has-ispell?
       (unless (and ispell-in ispell-out ispell-err)
         (let-values ([(out in pid err status) (apply values (process* ispell-prog "-a"))])
           ;; fetch the version line
           (read-line out)
           
           (set! ispell-in in)
           (set! ispell-out out)
           (set! ispell-err err)))
       (display word ispell-in)
       (newline ispell-in)
       (let ([answer-line (read-line ispell-out)]
             [blank-line (read-line ispell-out)])
         (unless (equal? blank-line "")
           (fprintf (current-error-port) "expected blank line from ispell, got (word ~s):\n~a\nrestarting ispell\n\n" 
                    word
                    blank-line)
           (close-output-port ispell-in)
           (close-input-port ispell-out)
           (close-input-port ispell-err)
           (set! ispell-in #f)
           (set! ispell-out #f)
           (set! ispell-err #f))
         (not (not (regexp-match #rx"^[\\+\\-\\*]" answer-line))))]
      [else #f]))

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
  
  ;; check-word : hash-table string -> boolean
  (define (check-word dict word)
    (let* ([word-ok (lambda (w) (hash-table-get dict w (lambda () #f)))]
           [subword-ok (lambda (reg)
                         (let ([m (regexp-match reg word)])
                           (and m
                                (word-ok (cadr m)))))])
      (if dict
          (or (word-ok word)
              (word-ok (string-lowercase! (string-copy word))) ;; check beginning of sentence-ness?
              (let ([ispell-ok (ispell-word word)])
                (when ispell-ok 
                  (hash-table-put! dict word #t))
                ispell-ok))
          #t))))
