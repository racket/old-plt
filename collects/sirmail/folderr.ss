
(module folderr mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "class100.ss")
           (lib "mred-sig.ss" "mred"))
  
  (require (lib "string.ss")
           (lib "list.ss")
           (lib "etc.ss"))
  
  (require "sirmails.ss")
  
  (require (lib "imap-sig.ss" "net"))
  
  (require (lib "hierlist-sig.ss" "hierlist"))
  
  (provide folder@)
  (define folder@
    (unit/sig ()
      (import sirmail:environment^
              (shutdown-folders-window)
              sirmail:options^
              mred^
              net:imap^
              hierlist^)
      
      (define mailbox-cache-file (build-path LOCAL* "folder-window-mailboxes"))
      
      (define (imap-open-connection)
	(imap-connect IMAP-SERVER
		      USERNAME
		      (or (get-PASSWORD) (get-text-from-user "Enter Password" "Enter Password"))
		      mailbox-name))

      (define imap-mailbox-name-mixin
        (lambda (list%)
          (class100 list% args
            (private-field
             [full-mailbox-name 'unknown-full-mailbox-name])
            (public
              [set-full-mailbox-name
               (lambda (fm)
                 (set! full-mailbox-name fm))]
              [get-full-mailbox-name
               (lambda ()
                 full-mailbox-name)])
            (sequence
              (apply super-init args)))))
      
      (define imap-mailbox-list-mixin
        (lambda (list%)
          (class100 list% args
            (private-field
             [mailbox-name 'unknown-mailbox-name])
            (public
              [get-mailbox-name
               (lambda ()
                 mailbox-name)]
              [set-mailbox-name
               (lambda (m)
                 (set! mailbox-name m))])
            
            (inherit new-list new-item delete-item get-items)
            (sequence
              (apply super-init args)))))
      
      ;; mailbox-folder = (make-deep-folder string string nested-mailbox-folder)
      ;; nested-mailbox-folder = 
      ;; (union (make-flat-folder string string)
      ;;        (make-deep-folder string string (listof mailbox-folder)))
      (define-struct folder (name short-name))
      (define-struct (deep-folder folder) (children))
      (define-struct (flat-folder folder) ())

      ;; refresh-mailboxes : -> void
      (define (refresh-mailboxes)
        (let ([mailboxes (fetch-mailboxes)])
          (when mailboxes
            (write-mailbox-folder mailboxes)
            (update-gui mailboxes))))

      ;; write-mailbox-folder : mailbox-folder -> void
      (define (write-mailbox-folder mbf)
        (let ([raw-datum
               (let loop ([mbf mbf])
                 (cond
                   [(flat-folder? mbf) (list (folder-name mbf)
                                             (folder-short-name mbf))]
                   [(deep-folder? mbf)
                    (list (folder-name mbf)
                          (folder-short-name mbf)
                          (map loop (deep-folder-children mbf)))]
                   [else (error 'write-mailbox-folder "unknown mailbox folder: ~e"
                                mbf)]))])
          (call-with-output-file mailbox-cache-file
            (lambda (port)
              (write raw-datum port))
            'truncate 'text)))
      
      ;; read-mailbox-folder : -> mailbox-folder
      (define (read-mailbox-folder)
        (if (file-exists? mailbox-cache-file)
            (let ([raw-datum (call-with-input-file mailbox-cache-file read 'text)])
              (let loop ([rd raw-datum])
                (cond
                  [(= 2 (length rd)) (make-flat-folder (car rd) (cadr rd))]
                  [(= 3 (length rd))
                   (make-deep-folder (car rd)
                                     (cadr rd)
                                     (map loop (caddr rd)))])))
            (make-deep-folder ROOT-MAILBOX-FOR-LIST
                              ROOT-MAILBOX-FOR-LIST
                              null)))

      
      ;; fetch-mailboxes : -> (union #f mailbox-folder)
      ;; gets the current mailbox list from the server
      (define (fetch-mailboxes)
        (with-handlers ([(lambda (x) #t)
                         (lambda (x)
                           (message-box "Error getting IMAP directory"
                                        (if (exn? x)
                                            (exn-message x)
                                            (format "uncaught exception: ~e" x)))
                           #f)])
          (let-values ([(imap msg-count recent-count) (imap-open-connection)])
            (begin0
              (make-deep-folder
               ROOT-MAILBOX-FOR-LIST
               ROOT-MAILBOX-FOR-LIST
               (let loop ([mailbox-name ROOT-MAILBOX-FOR-LIST])
                 (let ([mailbox-name-length (string-length mailbox-name)]
                       [get-child-mailbox-name (lambda (item) (format "~a" (second item)))]
                       [child-mailboxes (imap-list-child-mailboxes imap mailbox-name)])
                   (map (lambda (item)
                          (let* ([child-mailbox-name (get-child-mailbox-name item)]
                                 [child-mailbox-flags (first item)]
                                 [flat-mailbox?
                                  (member 'noinferiors
                                          (map imap-flag->symbol child-mailbox-flags))]
                                 [child-name-length (string-length child-mailbox-name)]
                                 [strip-prefix?
                                  (and (> child-name-length mailbox-name-length)
                                       (string=? 
                                        (substring child-mailbox-name 0 mailbox-name-length)
                                        mailbox-name))]
                                 [short-name
                                  (if strip-prefix?
                                      (substring child-mailbox-name
                                                 ;; strip separator (thus add1)
                                                 (add1 mailbox-name-length)
                                                 child-name-length)
                                      child-mailbox-name)])
                            (if flat-mailbox?
                                (make-flat-folder child-mailbox-name short-name)
                                (make-deep-folder 
                                 child-mailbox-name
                                 short-name
                                 (loop child-mailbox-name)))))
                        (quicksort
                         child-mailboxes
                         (lambda (x y)
                           (string<=? (get-child-mailbox-name x)
                                      (get-child-mailbox-name y))))))))
              (imap-disconnect imap)))))

      (define imap-mailbox-mixin
        (compose 
         imap-mailbox-list-mixin
         imap-mailbox-name-mixin))
      
      (define imap-top-list% 
        (class100 (imap-mailbox-list-mixin hierarchical-list%) (frame)
          (private-field
           [selected-mailbox #f])
          (public
            [get-selected-mailbox
             (lambda ()
               selected-mailbox)])
          (rename [super-on-select on-select])
          (override
            [on-select
             (lambda (i)
               (set! selected-mailbox (and i (send i get-full-mailbox-name)))
               (send open-button enable i)
               (send open-button set-label
                     (if i
                         (format "Open ~a" (send i get-full-mailbox-name))
                         "Open ..."))
               (super-on-select i))])
          (sequence (super-init frame))))
      
      (define (update-gui orig-mbf)
        (define (add-child hl mbf)
          (let* ([deep? (deep-folder? mbf)]
                 [new-item (if deep?
                               (send hl new-list imap-mailbox-mixin)
                               (send hl new-item imap-mailbox-name-mixin))]
                 [text (send new-item get-editor)])
            (send new-item set-full-mailbox-name (folder-name mbf))
            (when deep?
              (send new-item set-mailbox-name (folder-name mbf)))
            (send text insert (folder-short-name mbf))
            new-item))
        (send (send top-list get-editor) begin-edit-sequence)
        (for-each (lambda (x) (send top-list delete-item x))
                  (send top-list get-items))
        (for-each
         (lambda (mbf)
           (let loop ([hl top-list]
                      [mbf mbf])
             (let ([new-item (add-child hl mbf)])
               (when (deep-folder? mbf)
                 (for-each (lambda (child) (loop new-item child))
                           (deep-folder-children mbf))))))
         (deep-folder-children orig-mbf))
        (send (send top-list get-editor) end-edit-sequence))
      
      (define folders-frame%
        (class frame%
          (define/override (on-close)
            (shutdown-folders-window))
          (define/override (on-message msg)
            (and (list? msg)
                 (number? (car msg))
                 (number? (cadr msg))
                 (let ([gx (car msg)]
                       [gy (cadr msg)])
                   (let-values ([(x y) (send top-list screen->client gx gy)])
                     (let ([lxb (box 0)]
                           [lyb (box 0)])
                       (let loop ([ed (send top-list get-editor)])
                         (set-box! lxb x)
                         (set-box! lyb y)
                         (send ed global-to-local lxb lyb)
                         (let* ([on-it-b (box #f)]
                                [pos (send ed find-position (unbox lxb) (unbox lyb) #f on-it-b)])
                           (and (unbox on-it-b)
                                (let ([snip (send ed find-snip pos 'after-or-none)])
                                  (cond
                                    [(is-a? snip hierarchical-item-snip%)
                                     (let ([item (send snip get-item)])
                                       (send item get-full-mailbox-name))]
                                    [(is-a? snip hierarchical-list-snip%)
                                     (let ([ed (send snip get-content-buffer)])
                                       (loop ed))]
                                    [else #f]))))))))))
          
          (define/public (get-mailbox-name)
            (send top-list get-selected-mailbox))
          (super-instantiate ())))
      
      (define icon (make-object bitmap% (build-path (collection-path "sirmail")
                                                    "postmark.bmp")))
      (define icon-mask (make-object bitmap% (build-path (collection-path "sirmail")
                                                         "postmark-mask.xbm")))
      (define frame (make-object folders-frame% "Folders"))
      (define top-panel (make-object horizontal-panel% frame))
      (send top-panel stretchable-height #f)
      
      (define re:setup-mailboxes "^([^/]*)/(.*)$")
      (define (setup-mailboxes-file mailbox-name)
        (define mailboxes-file (build-path LOCAL* "mailboxes"))
        (define mailboxes
          (with-handlers ([void (lambda (x) '(("Inbox" "inbox")))])
            (with-input-from-file mailboxes-file
              read)))
        
        (define mailbox-loc (assoc mailbox-name mailboxes))
        
        (unless mailbox-loc
          
          (let ([fns (let loop ([str mailbox-name])
                       (cond
                         [(regexp-match re:setup-mailboxes str)
                          =>
                          (lambda (m)
                            (cons (cadr m)
                                  (loop (caddr m))))]
                         [else
                          (if (string=? str "")
                              null
                              (list str))]))])
            
            (unless (null? fns)
              (let ([mailbox-dir
                     (let loop ([fns (if (string=? (car fns) "")
                                         (cdr fns)
                                         fns)]
                                [local-dir 'same]
                                [fs-dir LOCAL*])
                       (cond
                         [(null? fns) local-dir]
                         [else (let ([new-fs-dir (build-path fs-dir (car fns))])
                                 (unless (directory-exists? new-fs-dir)
                                   (make-directory new-fs-dir))
                                 (loop (cdr fns)
                                       (build-path local-dir (car fns))
                                       new-fs-dir))]))])
                
                (with-output-to-file (build-path LOCAL* "mailboxes")
                  (lambda () (write
                              (append mailboxes
                                      (list (list mailbox-name mailbox-dir)))))
                  'truncate))))))
      
      
      (define open-button
        (make-object button% "Open ..."
          top-panel
          (lambda xxx
            (let ([mail-box (send top-list get-selected-mailbox)])
              (when mail-box
                (setup-mailboxes-file mail-box)
                (open-mailbox mail-box))))))
      (define refresh-mailbox-button
        (instantiate button% ()
          (label "Refresh")
          (parent top-panel)
          (callback (lambda (x y)
                      (refresh-mailboxes)))))
          
      (send open-button stretchable-width #t)
      (send open-button enable #f)
      
      (define top-list (make-object imap-top-list% frame))
      
      (when (and (send icon ok?) (send icon-mask ok?))
        (send frame set-icon icon icon-mask 'both))
      
      (send frame show #t)
      (send frame min-width 350)
      (send frame min-height 450)
      (send top-list set-mailbox-name ROOT-MAILBOX-FOR-LIST)
      (update-gui (read-mailbox-folder))
      frame)))
