(load-relative "sig.ss")
(unit/sig gui-text^
  (import mred^ repl^ jvm^ scanner^ split^ gjc^ goober^)
  
  ;; This doesn't work for argument lists that span lines,
  ;; nor does it work when } are not at the front of the line.
  
  ;; Lines here correspond to paragraphs in the text% class.
  ;; i.e. soft line breaks don't count for indenting purposes
  
  ;; definitions-text-mixin : text<%> -> text<%>
  (define (definitions-text-mixin super-class)
    (class super-class (message)
      (inherit paragraph-start-position paragraph-end-position insert get-character
               get-start-position get-end-position position-paragraph
               begin-edit-sequence end-edit-sequence is-modified?)
      (rename [super-on-default-char on-default-char]
              [super-set-modified set-modified])
      (override
        (on-load-file
         (lambda (fname format)
           (when fname
             (let*-values ([(base name dir?) (split-path fname)])
               (send message set-label name)))))
        (on-save-file on-load-file)
        (set-modified
         (lambda (modified)
           (when save-button
             (send save-button show modified))
           (super-set-modified modified)))
        (on-default-char
         (lambda (c)
           (case (send c get-key-code)
             ((#\tab) (indent-selection))
             ((#\newline #\return) (super-on-default-char c) (indent-selection))
             (else (super-on-default-char c))))))
      (public
        (set-save-button! (lambda (button) (set! save-button button))))
      (private
        (save-button #f)
        (indent-selection
         ;; -> Void
         (lambda ()
           (let ([start (position-paragraph (get-start-position))]
                 [stop (position-paragraph (get-end-position))])
             (begin-edit-sequence)
             (let loop ([line start])
               (indent-line line)
               (unless (= line stop)
                 (loop (add1 line))))
             (end-edit-sequence))))
        (indent-line
         ;; indent-line : Nat -> Void
         ;; should run on statements or case 'x': labels indent differently?
         (lambda (line)
           (let ([prev-line (prev-non-blank-line line)])
             (cond
               [(not prev-line) (set-indentation! line 0)]
               [else
                (let* ([prev-indent (indentation prev-line)]
                       [end (ends-with prev-line)]
                       [indent
                        (cond
                          [(eq? end #\{) (+ indent-width prev-indent)]
                          [else prev-indent])]
                       [outdent
                        (if (eq? #\} (starts-with line))
                            indent-width
                            0)])
                  (set-indentation! line (- indent outdent)))]))))
        (prev-non-blank-line
         ;; Nat -> (U Nat #f)
         (lambda (line)
           (cond
             [(zero? line) #f]
             [else 
              (let ([prev (sub1 line)])
                (if (>= (non-blank-pos (paragraph-start-position prev)) (paragraph-end-position prev))
                    (prev-non-blank-line prev)
                    prev))])))
        (indent-width 2)
        (indentation
         ;; indentation : Nat -> Nat
         (lambda (line)
           (let ([start (paragraph-start-position line)])
             (- (non-blank-pos start)
                start))))
        (non-blank-pos
         ;; non-blank-pos : Nat -> Nat
         (lambda (pos)
           (if (my-char-whitespace? (get-character pos))
               (non-blank-pos (add1 pos))
               pos)))
        (my-char-whitespace?
         ;; Char -> Boolean
         (lambda (c)
           (or (eq? c #\space) (eq? c #\tab))))
        (ends-with
         ;; ends-with : Nat -> Character
         ;; returns the last non-newline character on the line
         (lambda (line)
           (get-character (sub1 (paragraph-end-position line)))))
        (starts-with
         ;; Nat -> Character
         (lambda (line)
           (get-character (non-blank-pos (paragraph-start-position line)))))
        (set-indentation!
         ;; set-indentation! : Nat Integer -> Void
         ;; negative indentation = zero indentation
         (lambda (line indentation)
           (let* ([start (paragraph-start-position line)]
                  [old-indent-end (non-blank-pos start)])
             (insert "" start old-indent-end)
             (let loop ([n indentation])
               (unless (<= n 0)
                 (insert #\space start)
                 (loop (sub1 n))))))))
      (sequence (super-init))))
  
  (define banner "Welcome to DrJava")
  (define prompt (format "~n> "))
  
  ;; repl-format : Value -> String
  (define (repl-format val)
    (cond
      [(void? val) ""]
      [(or (number? val) (boolean? val)) (format "~n~a" val)]
      [else (toString val)]))
  
  ;; toString : jobject -> Void
  (define toString
    (let* ([oc (jfind-class "java/lang/Object")]
           [toString-method (jfind-method oc "toString" "()Ljava/lang/String;")])
      (lambda (obj)
        (format "~n~a"(jstring->string (jcall obj toString-method))))))
  
  (define (repl-text-mixin super-class)
    (class super-class ()
      (inherit insert get-text last-position erase find-snip set-position insert-box set-caret-owner get-end-position)
      (private
        (make-print-char
         (lambda (style)
           (lambda (char)
             (set-position (last-position))
             (let* ([editor (send (get-output-snip) get-editor)]
                    [start (send editor last-position)])
               (send editor insert char start)
               (send editor change-style style start (send editor last-position))))))
        (make-color-style
         (lambda (color-name)
           (send (make-object style-delta% 'change-normal-color) set-delta-foreground color-name)))
        (not-evaluating #t)
        (eval-semaphore (make-semaphore))
        (goobers #f))
      (public
        (eval
         (lambda ()
           (let ([text (get-text prompt-pos 'eof #t)])
             (unless (zero? (string-length text))
               (fluid-let ([not-evaluating #f])
                 (thread (lambda ()
                           (goobers
                            (lambda ()
                              (with-handlers ([(lambda (x) (eq? x 'bad-squiglies))
                                               (lambda _ (insert #\newline))]
                                              [void (lambda _ (for-each print-red-char (string->list  (format "Oh, no!~n"))) (new-prompt))])
                                (insert (repl-format (eval-str repl text)))
                                (new-prompt))))
                           (semaphore-post eval-semaphore)))
                 (yield eval-semaphore))))))
        (reset
         ; -> jobject(Env)
         (lambda ()
           (set! prompt-pos 0)
           (set! repl (new-repl))
           (erase)
           (init)
           (repl-env repl)))
        (print-char
         (make-print-char (make-color-style "black")))
        (print-red-char
         (make-print-char (make-color-style "red")))
        (set-goobers! (lambda (goobs) (set! goobers goobs))))
      (private
        (prompt-pos 0)
        (repl (new-repl))
        (init (lambda () (insert banner) (new-prompt)))
        (print-stream
         (lambda (f)
           (let* ([w-class (jfind-class "edu/rice/cs/drj/WriteToScheme")]
                  [w-init (jfind-method w-class "<init>" "(Ledu/rice/cs/drj/SchemeFunction;)V")]
                  [w (jnew w-class w-init f)]
                  [p-class (jfind-class "java/io/PrintStream")]
                  [p-init (jfind-method p-class "<init>" "(Ljava/io/OutputStream;)V")])
             (jnew p-class p-init w))))
        (new-prompt
         (lambda ()
           (insert prompt (last-position))
           (set! prompt-pos (last-position))
           (set-caret-owner #f 'global)))
        (get-output-snip
         (lambda ()
           (let ([last (find-snip (last-position) 'before)])
             (if (is-a? last editor-snip%)
                 last
                 (begin (insert #\newline) (set-position (last-position))
                        (insert (make-object editor-snip%))
                        (get-output-snip)))))))
      (rename (super-on-default-char on-default-char))
      (override
        (can-insert?
         (lambda (start len)
           (>= start prompt-pos)))
        (can-delete? can-insert?)
        (on-focus
         (let* ([sys-class (jfind-class "java/lang/System")]
                [out (jfind-static-field sys-class "out" "Ljava/io/PrintStream;")]
                [err (jfind-static-field sys-class "err" "Ljava/io/PrintStream;")]
                [out-stream (print-stream print-char)]
                [err-stream (print-stream print-red-char)])
           (lambda (got-it)
             (when got-it
               (jset-field! sys-class out out-stream)
               (jset-field! sys-class err err-stream)))))
        (on-default-char
         (lambda (event)
           (when not-evaluating
             (if (and (eq? #\return (send event get-key-code))
                      (= (last-position) (get-end-position)))
                 (eval)
                 (super-on-default-char event))))))
      (sequence
        (super-init)
        (init))))
  
  
  ;; add-parts :
  ;;  area-container<%> (parent message% -> definitions-text-mixin)
  ;;  (parent -> repl-text-mixin) (definitions-text-mixin repl-text-mixin -> a) -> a
  (define (add-parts parent add-top add-repl k)
    (let* ([button-bar (make-object horizontal-pane% parent)]
	   [name-message
	    (make-object message% "" button-bar)]
           [top-editor (add-top parent name-message)]
           [repl-editor (add-repl parent)]
           [goobers (make-object goober-panel% parent top-editor)]
           [make-button
            (lambda (name action)
              (make-object button%
                           (make-object bitmap% (build-path (collection-path "icons") name))
                           button-bar
                           action))]
           [save-button
            (make-button "save.bmp"
                         (lambda _
                           (send top-editor save-file)))]
           [call-with-goobers
            (lambda (thunk)
              (send goobers reset)
              (fluid-let ([current-goobers goobers])
                (thunk))
              (send goobers appear))]
           [execute-button
            (make-button "execute.bmp"
                         (lambda _
                           (call-with-goobers
                            (lambda ()
                              (compile-string (send top-editor get-text 0 'eof #t)
                                              (send repl-editor reset))))))]
           [blank (make-object pane% button-bar)])
      (send save-button show #f)
      (send top-editor set-save-button! save-button)
      (send repl-editor set-goobers! call-with-goobers)
      (send name-message stretchable-width #t)
      (send blank stretchable-width #t)
      (send button-bar stretchable-height #f)
      (k top-editor repl-editor)))
  
  ;; compile-string : String jobject(Env) -> Void
  (define (compile-string source env)
    ;    (compile-classes (scan-classes (string->scanner source)) env))
    ; leave out the env so the .class files go to disk
    (unless (zero? (string-length source))
      (compile-classes (scan-classes (string->scanner source)))))
  
  (define current-goobers #f)
  
  ;; report-error : Int jobject(String) -> Void
  (define (report-error pos str)
    (let ([str (jstring->string str)])
      (if current-goobers
	  (send current-goobers error pos str)
	  (printf "Compilation error: ~a : ~a ~n" pos str))))
  
  ;; report-error : Int jobject(String) -> Void
  (define (report-warning pos str)
    (let ([str (jstring->string str)])
      (if current-goobers
	  (send current-goobers warning pos str)
	  (printf "Warning: ~a : ~a ~n" pos str)))))
