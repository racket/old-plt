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
    (let* ((prefix (string->symbol (format "syntax-coloring:~a" tab-name)))
           (active-pref (string->symbol (format "~a:active" prefix))))
      (for-each (lambda (s)
                  (set-default (string->symbol (format "~a:~a" prefix (car s)))
                               (cadr s)))
                symbols/defaults)
      (preferences:set-default active-pref #t (lambda (x) #t))
      (preferences:add-panel `("Editing" "Colors" ,tab-name)
                             (lambda (p)
                               (let ((vp (new vertical-panel% (parent p))))
                                 (new color-selection-panel%
                                      (parent vp)
                                      (prefix prefix)
                                      (symbols (map car symbols/defaults)))
                                 (let ((cb (new check-box%
                                                (parent vp)
                                                (label "Activate Coloring?")
                                                (callback (lambda (checkbox y)
                                                            (preferences:set 
                                                             active-pref
                                                             (send checkbox get-value)))))))
                                   (send cb set-value (preferences:get active-pref)))
                                 vp)))))

               
  
  )