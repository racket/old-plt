(module pict-snipclass mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide snip-class)
  (require "private/pict-box-lib.ss")

  (define pict-snip%
    (class* editor-snip% (readable-snip<%>)
      (define/public (read-special file line col pos)
        (build-lib-pict-stx 
         (lambda (ids) (syntax (void)))
         (get-snp/poss this)))
      
      (super-new)))
  
  (define lib-pict-snipclass%
    (class snip-class%
      (define/override (read stream-in)
        (let* ([snip (new pict-snip%)]
               [editor (send snip get-editor)]
               [show-picts? (not (zero? (send stream-in get-exact)))]
               [up-to-date? (not (zero? (send stream-in get-exact)))])
          (send editor read-from-file stream-in #f)
          snip))
      (super-new)))
  
  (define snip-class (make-object lib-pict-snipclass%))
  (send snip-class set-version 2)
  (send snip-class set-classname (format "~s" '(lib "pict-snipclass.ss" "slideshow")))
  (send (get-the-snip-class-list) add snip-class))