(module gui-defs mzscheme
  (require (lib "unitsig.ss"))
  (require (lib "class.ss"))
  (require (lib "mred.ss" "mred"))
  (require (lib "string-constant.ss" "string-constants"))

  (require "checksigs.ss")

  (provide gui-defs@) 

  (define gui-defs@
    (unit/sig defs^
      (import)

      (define (run-thunk th)
	(parameterize
	 ([current-eventspace (make-eventspace)])
	 (queue-callback th)))

 (define (show-ok title caption details)
        (letrec ([frame 
                  (instantiate frame% ()
                    (label title)
		    (alignment '(center center))
		    (min-width 200)
                    (stretchable-height #f)
                    (stretchable-width #f)
                    (style '(no-resize-border)))]
                 [main-panel (instantiate vertical-panel% () 
                               (parent frame) 
                               (stretchable-height #f)
                               (alignment '(center center)))]
                 [panel-sep 4]
                 [msg-width 100]	       
                 [make-hpanel
                  (lambda ()
                    (instantiate horizontal-panel% () 
                      (parent main-panel)
                      (vert-margin panel-sep)
                      (alignment '(center center))))]
                 [row-panel (make-hpanel)]
                 [make-msg
                  (lambda (msg panel)
                    (instantiate message% () 
                      (min-width msg-width)
                      (label msg) (parent panel)))]
                 [status-msg (make-msg caption row-panel)]               
                 [details-panel #f]
                 [showing-details #f]
                 [details-text "Details "]
                 [show-details-button-text (string-append details-text ">>")]
                 [hide-details-button-text (string-append details-text "<<")]
                 [hide-details
                  (lambda () 
                    (set! showing-details #f)
                    (send main-panel delete-child details-panel)
                    (send details-button set-label show-details-button-text)
                    (set! details-panel #f))]
                 [show-details
                  (lambda ()
                    (set! showing-details #t)
                    (send details-button set-label hide-details-button-text)
                    (set! details-button-callback hide-details)
                    (unless details-panel
                      (set! details-panel                         
                            (instantiate horizontal-panel% () 
                              (parent main-panel)
                              (style '(border))
                              (border 2)
                              (vert-margin panel-sep)
                              (alignment '(center center))))                  
                      (make-msg details details-panel)))]
                 [details-button-callback
                  (lambda (e bv)
                    (if showing-details
                        (hide-details)
                        (show-details)))]
                 [buttons-panel (make-hpanel)]
                 [ok-button (instantiate button% ()
                              (label "OK")
                              (min-width 50)
                              (parent buttons-panel)
                              (callback (lambda (b ev) 
                                          (send frame show #f))))]
                 [spacer 
		  (and details
		       (instantiate message% () 
				    (min-width 20)
				    (label "") (parent buttons-panel)))]
                 [details-button 
		  (and details
		       (instantiate button% ()
				    (label show-details-button-text)
				    (min-width 50)
				    (parent buttons-panel)
				    (callback details-button-callback)))])
	  (send frame center)
          (send frame show #t)))
   
     (define (show-error-ok title caption)
       (show-ok title (format (string-constant vc-error-format)
			      caption) #f))

     (define (make-wait-dialog parent title caption close-fun)
       (let ([dialog 
	      (instantiate 
	       dialog% () 
	       (label title)
	       (parent parent)
	       (width 100)
	       (height 50)
	       (stretchable-width #t)
	       (stretchable-height #t))])
	 (instantiate 
	  message% ()
	  (label caption)
	  (parent dialog))
	 (instantiate 
	  button% () 
	  (label (string-constant cancel))
	  (parent dialog)
	  (callback (lambda (button ce)
		      (close-fun)
		      (send dialog show #f))))
	 dialog))

     (define (show-wait-dialog dialog)
       (send dialog center)
       (thread
	(lambda () 
	  (send dialog show #t)))
       (send dialog focus))

     (define (hide-wait-dialog dialog)
       (send dialog show #f)))))
