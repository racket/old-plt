(require-library "hierlist.ss" "hierlist")
(require-library "file.ss")

(define calculate-sub-hierarchical-list%
  (class hierarchical-list% args
    (inherit delete-item)
    (override
      [on-item-opened
       (lambda (i)
         (when (is-a? i hierarchical-list-compound-item<%>)
           ((send i user-data))))]
      [on-item-closed
       (lambda (i)
         (when (is-a? i hierarchical-list-compound-item<%>)
           (for-each (lambda (bye) (send i delete-item bye))
                     (send i get-items))))])
    (sequence (apply super-init args))))

(define (add-collection-path hl collection-path)
  (let ([new-list (send hl new-list)])
    (send (send new-list get-editor)
          insert
          collection-path)
    (send new-list user-data
          (lambda ()
            (for-each (lambda (coll-name)
                        (when (and 
                               (not (string=? "CVS" coll-name))
                               (directory-exists? (build-path collection-path coll-name)))
                          (add-collection 
                           coll-name
                           collection-path
                           new-list)))
                      (directory-list collection-path))))))

(define (add-collection coll-name coll-parent list)
  (let ([real-coll-dir (collection-path coll-name)])
    (if (string=? (normal-case-path (normalize-path real-coll-dir))
                  (normal-case-path (normalize-path 
                                     (build-path coll-parent coll-name))))
        (add-real-collection coll-name list)
        (add-shadowed-collection coll-name list))))

(define grey-style (make-object style-delta%))
(send grey-style set-delta-foreground "gray")
(define (add-shadowed-collection coll-name list)
  (let* ([item (send list new-item)]
         [editor (send item get-editor)])
    (send editor insert coll-name)
    (send editor change-style grey-style 0 (send editor last-position))))

(define (add-real-collection coll-name list)
  (let ([new-list (send list new-list)])
    (send (send new-list get-editor) insert coll-name)
    (send new-list user-data
          (lambda ()
            (for-each (lambda (lib) (add-lib/sub-dir 
                                     (collection-path coll-name)
                                     lib
                                     new-list))
                      (directory-list (collection-path coll-name)))))))

(define (add-lib/sub-dir dir lib list)
  (cond
    [(string=? "CVS" lib) (void)]
    [(directory-exists? (build-path dir lib))
     (let ([new-list (send list new-list)])
       (send (send new-list get-editor) insert lib)
       (send new-list user-data
             (lambda ()
               (for-each (lambda (sub-lib) 
                           (add-lib/sub-dir (build-path dir lib)
                                            sub-lib
                                            new-list))
                         (directory-list (build-path dir lib))))))]
    [(file-exists? (build-path dir lib))
     (let ([new-item (send list new-item)])
       (send (send new-item get-editor) insert lib))]))

(define (get-file/collection)
  (define dialog (make-object frame% "Select Library"))
  (define hl (make-object calculate-sub-hierarchical-list% dialog))
  (for-each (lambda (c) (add-collection-path hl c))
            (current-library-collection-paths))
  (send dialog show #t))

(get-file/collection)