; Modes

(define-sigfunctor (mred:mode@ mred:mode^)
  (import mred:debug^ mred:keymap^)

  (define basic-mode%
    (class '() ()
      (public
       [name "undefined"]
       [install (lambda (edit) (void))]
       [deinstall (lambda (edit) (void))]
       
       [on-char (lambda (edit event) #f)]
       [on-event (lambda (edit event) #f)]
       
       [on-insert (lambda (edit start len) #t)]
       [after-insert (lambda (edit start len) (void))]
       [on-delete (lambda (edit start len) #t)]
       [after-delete (lambda (edit start len) (void))]
       
       [after-set-position (lambda (edit) (void))]

       [on-focus (lambda (edit on?) #f)]


       [file-format wx:const-media-ff-same]
       [standard-style-delta (make-object wx:style-delta% 
					  wx:const-change-nothing)])))

  (define make-mode%
    (lambda (super%)
      (class super% ()
	(public
	 [name "default"]
	 [make-keymap 
	  (lambda ()
	    (let ([keymap (make-object wx:keymap%)])
	      (mred:keymap^:set-keymap-error-handler keymap)
	      (mred:keymap^:set-keymap-implied-shifts keymap)))]
	 [deinstall 
	  (lambda (edit) 
	    (send (send edit get-keymap) remove-chained-keymap keymap))]
	 [install 
	  (lambda (edit)
	    (send (send edit get-keymap) chain-to-keymap keymap #t))])
	(sequence
	  (super-init))
	(public
	 [keymap (make-keymap)]))))

  (define mode% (make-mode% basic-mode%)))
