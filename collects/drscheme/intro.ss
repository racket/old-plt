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
  
  (define (show-introduction)
    (let* ([f (make-object frame:basic% "Welcome to DrScheme")]
           [hp (make-object horizontal-panel% (send f get-area-container))]
           [this-version (version:version)]
           [last-version (preferences:get 'drscheme:last-version)])
      
      (preferences:set 'drscheme:last-version this-version)
      
      (let ([vp (make-object vertical-panel% hp)])
        (make-object message% 
                     (make-object bitmap% 
                                  (build-path (collection-path "icons") "plt.gif")
                                  'gif)
                     vp)
        (make-object 
         message% 
         (format "Welcome to DrScheme, version ~a" this-version)
         vp)
        (when (and last-version 
                   (not (equal? this-version last-version)))
          (make-object 
           message% 
           (format "(previous version ~a)" last-version)
           vp)))
      
      
      (let* ([vp (make-object vertical-panel% hp)]
             [buttons
              (list
               
               (make-object button% "Take a Tour!" vp
                            (lambda x 
                              (bell)))
               
               (make-object button% "Release Notes" vp
                            (lambda x 
                              (help-desk:open-url 
                               (string-append
                                "file:"
                                (build-path (collection-path "doc" "help" "release")
                                            "notes.html"))))))])
        (send vp set-alignment 'center 'center)
        (let ([max-width (apply max (map (lambda (x) (send x get-width)) buttons))])
          (for-each (lambda (x) (send x min-width max-width)) buttons)))
      
      (send f show #t))))
