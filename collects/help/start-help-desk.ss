(unit/sig help:drscheme-interface^
  (import mzlib:function^
	  mzlib:string^
	  mzlib:file^
	  mzlib:url^
	  mred^
	  framework^
          [drscheme:frame : drscheme:frame^])

  (define new-help-frame #f)
  (define open-url-from-user #f)
  (define help-desk-frame #f)

  (include "startup-url.ss")

  (define (load-help-desk)
    (define frame-mixin drscheme:frame:basics-mixin)
    (set!-values (new-help-frame
                  open-url-from-user)
                 (invoke-unit/sig
                  (require-library "helpr.ss" "help")
                  mzlib:function^
                  mzlib:string^
                  mzlib:file^
                  mzlib:url^
                  mred^
                  framework^
                  (frame-mixin)))
    (set! load-help-desk void))

  (define (open-url url)
    (load-help-desk)
    (new-help-frame url))

  (define (open-users-url frame)
    (load-help-desk)
    (open-url-from-user 
     frame 
     (if (ivar-in-interface? 'goto-url (object-interface frame))
         (lambda (url)
           (send frame goto-url url))
         new-help-frame)))
  
  (define help-desk
    (case-lambda
     [()
      (begin-busy-cursor)
      (load-help-desk)
      (set! help-desk-frame (new-help-frame startup-url))
      (end-busy-cursor)]
     [(key)
      (let ([turn-cursor-off? (not help-desk-frame)])
        (if help-desk-frame
            (send help-desk-frame show #t)
            (begin (begin-busy-cursor)
                   (help-desk)))
        (send help-desk-frame search-for-help key 'keyword+index 'exact)
        (when turn-cursor-off?
          (end-busy-cursor)))])))


      

