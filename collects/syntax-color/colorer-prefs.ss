(module colorer-prefs mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "color-selection.ss")
  
  (provide add-to-colorer-prefs)
  
  (define color-selection-panel%
    (class vertical-panel%
      (init symbols prefix)

      (super-instantiate ())

      (for-each
       (lambda (s)
         (new color-selection% (prefix prefix) (symbol s) (parent this)))
       symbols)
      ))
    
  (define (add-to-colorer-prefs tab-name symbols/defaults)
    (let ((prefix (string->symbol (format "drscheme:editor-modes:~a" tab-name))))
      (for-each (lambda (s)
                  (set-default (string->symbol (format "~a:~a" prefix (car s)))
                               (cadr s)))
                symbols/defaults)
      (preferences:add-panel `("Editing" "Colors" ,tab-name)
                             (lambda (p)
                               (new color-selection-panel%
                                    (parent p)
                                    (prefix prefix)
                                    (symbols (map car symbols/defaults)))))))

               
  
  )