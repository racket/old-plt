(module colorer mzscheme
  (require (lib "class.ss"))
  
  (provide colorer)
  
  (define (colorer %)
    (class %
      ;; get-token takes an input port and returns 4 values:
      ;; A symbol in `(keyword string literal comment error identifier default)
      ;; Data to be kept with the token
      ;; The token's starting offset
      ;; The token's ending offset
      ;;
      ;; matches is a list of lists of matching paren types.
      ;; For example, '((|(| |)|) (|[| |]|))
      (init-field get-token prefix (matches null))
      
      (rename (super-on-disable-surrogate on-disable-surrogate))
      (define/override (on-disable-surrogate text)
        (super-on-disable-surrogate text)
        (send text stop))
      
      (rename (super-on-enable-surrogate on-enable-surrogate))
      (define/override (on-enable-surrogate text)
        (super-on-enable-surrogate text)
        (send text start prefix get-token matches))

      (super-instantiate ())
      )))
  