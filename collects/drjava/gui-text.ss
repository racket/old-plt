(load-relative "sig.ss")
(unit/sig gui-text^
  (import mred^ repl^ jvm^ scanner^ split^ gjc^ goober^ mzlib:file^)
  
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
             ((#\newline #\return #\}) (super-on-default-char c) (indent-selection))
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
      (sequence
        (super-init)
        (fix-style this))))
  
  (define banner "Welcome to DrJava")
  (define prompt (format "~n> "))
  
  ;; repl-format : Value -> String
  (define (repl-format val)
    (if (void? val)
        ""
        (format "~n~a"
                (cond
		  [(char? val) (char->integer val)]
                  [(number? val) val]
                  [(eq? val #t) "true"]
                  [(eq? val #f) "false"]
                  [else (toString val)]))))
  
  ;; Hist = (make-hist/text Hist Hist String)
  ;;      | (make-hist/creation Hist 'fell-off-begining-of-time)
  ;;      | (make-hist/apocalypse 'fell-off-end-of-time Hist)
  ;; Symbols would not appear if we had multiple inheritance on struct fields
  (define-struct hist (prev next))
  (define-struct (hist/text struct:hist) (text))
  (define-struct (hist/creation struct:hist) ())
  (define-struct (hist/apocalypse struct:hist) ())
  
  ;; toString : jobject -> Void
  ;; Actually this uses String.valueOf(Object) since it works for null.
  (define toString
    (let* ([sc (jfind-class "java/lang/String")]
           [toString-method (jfind-static-method sc "valueOf" "(Ljava/lang/Object;)Ljava/lang/String;")])
      (lambda (obj)
        (jstring->string (jcall sc toString-method obj)))))
  
  (define (repl-text-mixin super-class)
    (class super-class ()
      (inherit insert get-text last-position erase find-snip set-position insert-box set-caret-owner get-end-position get-keymap set-keymap get-character delete)
      (private
        (make-printer
         (lambda (style)
           (lambda (to-print)
             (set-position (last-position))
             (let* ([editor (send (get-output-snip) get-editor)]
                    [start (send editor last-position)])
               (send editor insert to-print start)
               (send editor change-style style start (send editor last-position))))))
        (make-color-style
         (lambda (color-name)
           (send (make-object style-delta% 'change-normal-color) set-delta-foreground color-name)))
        (not-evaluating #t)
        (eval-semaphore (make-semaphore))
        (goobers #f)
        (hist-apocalypse (make-hist/apocalypse 5 'fell-off-end-of-time))
        (hist-creation (make-hist/creation 'fell-off-begining-of-time hist-apocalypse)))
      (sequence (set-hist-prev! hist-apocalypse hist-creation))
      (private
        ;; history : Hist
        (history hist-apocalypse)
        (set-current
         (lambda ()
           (insert (if (hist/text? history)
		       (hist/text-text history)
		       "")
		   prompt-pos (last-position))))
        (hist-back
         (lambda (_ event)
           (unless (hist/creation? history)
             (set! history (hist-prev history))
             (set-current))))
        (hist-forward
         (lambda (_ event)
           (unless (hist/apocalypse? history)
             (set! history (hist-next history))
             (set-current))))
        (hist-add!
         (lambda (text)
           (set! history hist-apocalypse)
           (let* ([apoc-prev (hist-prev hist-apocalypse)]
                  [new (make-hist/text apoc-prev hist-apocalypse text)])
             (set-hist-next! apoc-prev new)
             (set-hist-prev! hist-apocalypse new)))))
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
                                              [void (lambda _ (print-red-char (format "Runtime Error.~n")) (new-prompt))])
                                (insert (repl-format (eval-str repl text)))
                                (hist-add! text)
                                (new-prompt))))
                           (semaphore-post eval-semaphore)))
                 (yield eval-semaphore))))))
        (reset
         ; -> jobject(Env)
         (lambda ()
           (set! prompt-pos 0)
           (erase)
           (init)
           (set! repl (new-repl))
           (repl-env repl)))
        (print-char
         (make-printer (make-color-style "black")))
        (print-red-char
         (make-printer (make-color-style "red")))
        (set-goobers! (lambda (goobs) (set! goobers goobs))))
      (private
        (prompt-pos 0)
        (repl (new-repl))
        (class-dir ".")
        (init (lambda () (insert banner) (new-prompt)
                (set! class-dir (make-tmp-directory))
                (set-gjc-output-dir! class-dir)))
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
                        (get-output-snip))))))
        (all-white?
         (lambda (current end)
           (let loop ([current current])
             (or (>= current end)
                 (and (char-whitespace? (get-character current))
                      (loop (add1 current))))))))
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
             (let ([key (send event get-key-code)])
               (if (and (eq? #\return key)
                        (let ([start (get-end-position)]
                              [end (last-position)])
                          (or (= start end); for short cutting only
                              (and (all-white? start end)
                                   (delete start end)))))
                   (eval)
                   (super-on-default-char event)))))))
      (sequence
        (super-init)
        (fix-style this)
        (let ([keymap (make-object keymap%)]
              [prev-name "hist-prev"]
              [next-name "hist-next"])
          (send keymap chain-to-keymap (get-keymap) #f)
          (send keymap add-function prev-name hist-back)
          (send keymap map-function ":esc;p" prev-name)
          (send keymap add-function next-name hist-forward)
          (send keymap map-function ":esc;n" next-name)
          (set-keymap keymap))
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
            (lambda (iname name action)
              (make-object button%
                           (let ([bits (make-object bitmap% (build-path (collection-path "icons") iname))])
                             (if (send bits ok?)
                                 (build-bitmap bits name)
                                 name))
                           button-bar
                           action))]
           [save-button
            (make-button "save.bmp" "Save"
                         (lambda _
                           (send top-editor save-file)))]
           [call-with-goobers
            (lambda (thunk)
              (send goobers reset)
              (fluid-let ([current-goobers goobers])
                (with-handlers ([split-error?
                                 (lambda (err)
                                   (send current-goobers error (split-error-pos err) (split-error-message err)))])
                  (thunk)))
              (send goobers appear))]
           [execute-button
            (make-button "execute.bmp" "Compile"
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
  
  ;; build-bitmap : Bitmap% String -> Bitmap%
  (define build-bitmap
    (let* ([font (make-object font% 12 'system 'normal 'normal)]
           [border 3]
           [border*2 (* border 2)])
      (lambda (icon text)
        (let-values ([(width height _ __)
                      (send (make-object bitmap-dc% (make-object bitmap% 1 1))
                            get-text-extent text font)])
          (let* ([icon-width (+ border*2 (send icon get-width))]
                 [text-height (inexact->exact height)]
                 [total-height (+ border*2
                                  (max text-height (send icon get-height)))]
                 [big (make-object bitmap% (+ icon-width (inexact->exact width) border) total-height)]
                 [big-dc (make-object bitmap-dc% big)])
            (send big-dc clear)
            (send big-dc draw-bitmap icon border border)
            (send big-dc set-font font)
            (send big-dc draw-text text icon-width (/ (- total-height text-height) 2))
            
            (send big-dc set-bitmap #f)
            big)))))
  
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
	  (printf "Warning: ~a : ~a ~n" pos str))))

  ;; fix-style : text% -> Void
  (define fix-style
    (let ([fixed (make-object style-delta% 'change-family 'modern)])
      (lambda (this)
        (let ([style-list (send this get-style-list)])
          (send style-list replace-named-style "Standard"
                (send style-list find-or-create-style
                      (send style-list basic-style) fixed))))))
  
  (define directories-to-delete null)
  
  ;; make-tmp-directory : -> String
  (define (make-tmp-directory)
    (let ([tmp-dir (find-system-path 'temp-dir)])
      (let loop ([n 0])
        (let ([my-dir (build-path tmp-dir (format "drjava-~a.~a" (current-seconds) n))])
          (if (or (directory-exists? my-dir) (file-exists? my-dir) (link-exists? my-dir))
              (loop (add1 n))
              (begin
                (make-directory my-dir)
                (set! directories-to-delete (cons my-dir directories-to-delete))
                my-dir))))))
  
  (exit-handler
   (let ([super (exit-handler)])
     (lambda (arg)
       (for-each (lambda (d)
                   (when (directory-exists? d)
                     (delete-directory/files d)))
                 directories-to-delete)
       (super arg)))))
