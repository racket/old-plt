(unit/sig ()
  (import jvm^ mred^ framework^ channel^)
  
  (define input-frame%
    (class frame:basic% (name)
      (sequence (super-init name #f 200 200))
      (inherit show get-area-container)
      (private
        (buttons (make-object horizontal-pane% (get-area-container)))
        (can (make-object editor-canvas% (get-area-container)))
        (new-ed
         (lambda ()
           (let ([ed (make-object input-text%)])
             (send can set-editor ed)
             ed)))
        (ed (new-ed))
        (close-action
         (lambda ()
           (send ed close)
           (set! action reset-action)
           (send reset set-label go-label)))
        (reset-action
         (lambda ()
           (set! ed (new-ed))
           (set! action close-action)
           (send reset set-label stop-label)))
        (action close-action)
        (icon (lambda (str) (make-object bitmap% (build-path (collection-path "icons") str))))
        (stop-label (icon "break.bmp"))
        (go-label (icon "execute.bmp"))
        (reset (make-object button%
                            stop-label
                            buttons
                            (lambda _ (action)))))
      (public
        (read-char
         (lambda ()
           (show #t)
           (send ed read-char)))
        (available (lambda () (send ed available))))
      (sequence
        (send buttons stretchable-height #f)
        '(send (group:get-the-frame-group) remove-frame this))))
  
  (define input-text%
    (class text:basic% ()
      (sequence (super-init))
      (inherit insert get-end-position get-character last-position)
      (private
        (closed #f)
        (frozen-pos 0)
        (read-at-pos 0); (<= read-at-pos limit)
        (limit 0); (<= limit frozen-pos))
        (channel (make-channel)) ; strictly increasing numbers
        (flush
         (lambda (to)
           (set! frozen-pos to)
           (channel-put channel frozen-pos))))
      (public
        (close
         (lambda ()
           (set! closed #t)
           (flush (last-position))))
        (read-char
         (lambda ()
           (if (and closed (= frozen-pos read-at-pos))
               -1
               (begin
                 (when (= read-at-pos limit)
                   (set! limit (channel-get channel)))
                 (begin0
                   (char->integer (get-character read-at-pos))
                   (set! read-at-pos (add1 read-at-pos)))))))
        (available (lambda () (- frozen-pos read-at-pos))))
      (rename (super-on-default-char on-default-char))
      (override
        (can-insert?
         (lambda (start len)
           (and (not closed) (>= start frozen-pos))))
        (can-delete? can-insert?)
        (on-default-char
         (lambda (event)
           (let ([char (send event get-key-code)])
             (super-on-default-char event)
             (when (eq? #\return char)
               (flush (get-end-position)))))))))
  
  (define the-frame (make-object input-frame% "System.in"))
  
  (define read-from-scheme (jfind-class "edu/rice/cs/drj/ReadFromScheme"))
  (define rfs-init (jfind-method read-from-scheme "<init>"
                                 "(Ledu/rice/cs/drj/SchemeFunction;)V"))
  
  (define system (jfind-class "java/lang/System"))
  (define in (jfind-static-field system "in" "Ljava/io/InputStream;"))

  (jset-field! system in
               (jnew read-from-scheme rfs-init
                     (lambda () (send the-frame read-char)))))