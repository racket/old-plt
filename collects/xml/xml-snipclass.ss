(module xml-snipclass mzscheme
  (require "private/xml-snip-helpers.ss"
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide snip-class)

  (define xml-snip%
    (class* editor-snip% (xml-snip<%> readable-snip<%>)
      (init-field eliminate-whitespace-in-empty-tags?)
      
      (define/public (read-one-special index file line col pos)
        (xml-read-one-special eliminate-whitespace-in-empty-tags?
                              translate-xml-exn-to-rep-exn
                              this
                              file
                              line
                              col
                              pos))
      
      (super-instantiate ())))
  
  ;; this simpler version of translate-...-exn 
  ;; doesn't allow for showing the red highlighting
  ;; (but this code doesn't run inside DrScheme anyways)
  (define (translate-xml-exn-to-rep-exn editor)
    (lambda (exn)
      (raise exn)))
  
  (define xml-snipclass%
    (class snip-class%
      (define/override (read stream-in)
        (let* ([eliminate-whitespace-in-empty-tags? (zero? (send stream-in get-exact))]
               [snip (instantiate xml-snip% ()
                       (eliminate-whitespace-in-empty-tags? eliminate-whitespace-in-empty-tags?))])
          (send (send snip get-editor) read-from-file stream-in)
          snip))
      (super-instantiate ())))
  
  (define snip-class (make-object xml-snipclass%))
  (send snip-class set-version 1)
  (send snip-class set-classname (format "~s" '(lib "xml-snipclass.ss" "xml")))
  (send (get-the-snip-class-list) add snip-class))