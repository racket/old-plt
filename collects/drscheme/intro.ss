(unit/sig drscheme:intro^
  (import mred^
          framework^
          [help-desk : help:drscheme-interface^])
  
  (define (check-new-version)
    (let ([this-version (version:version)]
          [last-version (preferences:get 'drscheme:last-version)])
      
      (when (or (not last-version)
                (not (equal? last-version this-version)))
        
        (show-introduction))))
  
  (define (same-widths items)
    (let ([max-width (apply max (map (lambda (x) (send x get-width)) items))])
      (for-each (lambda (x) (send x min-width max-width)) items)))
  
  (define (show-introduction)
    (let* ([f (make-object frame:basic% "Welcome to DrScheme")]
           [main (make-object vertical-panel% (send f get-area-container))]
           [top-hp (make-object horizontal-panel% main)]
           [bot-hp (make-object horizontal-panel% main)]
           
           [version-panel (make-object vertical-panel% bot-hp)]
           [plt-image (make-object message% 
                        (make-object bitmap% 
                          (build-path (collection-path "icons") "plt.gif")
                          'gif)
                        top-hp)]
           
           [close-panel (make-object horizontal-panel% bot-hp)]
           
           [this-version (version:version)]
           [last-version (preferences:get 'drscheme:last-version)])
      
      (preferences:set 'drscheme:last-version this-version)
      
      (send (make-object button% "Close" 
              close-panel
              (lambda x (send f close)))
            stretchable-width #t)
      (make-object grow-box-spacer-pane% close-panel)
      
      (send top-hp stretchable-height #f)
      (send bot-hp stretchable-height #f)
      (send bot-hp set-alignment 'center 'center)
      (send close-panel set-alignment 'center 'center)
      
      (make-object message% 
        (format "Welcome to DrScheme, version ~a" this-version)
        version-panel)
      (when (and last-version 
                 (not (equal? this-version last-version)))
        (make-object message% 
          (format "(previous version ~a)" last-version)
          version-panel))
      (send version-panel stretchable-height #f)

      (let* ([vp (make-object vertical-panel% top-hp)]
             [buttons
              (list

               (make-object button% "Take a Tour!" vp
                 (lambda x 
                   (help-desk:open-url
                    (string-append
                     "file:"
                     (build-path (collection-path "doc" "help" "tour")
                                 "index.html")))))
               
               (make-object button% "Release Notes" vp
                 (lambda x 
                   (help-desk:open-url 
                    (string-append
                     "file:"
                     (build-path (collection-path "doc" "help" "release")
                                 "notes.html"))))))])
        (send vp set-alignment 'center 'center)
        (same-widths (cons close-panel buttons)))
      
      (same-widths (list version-panel plt-image)) 
      
      (send f show #t))))
