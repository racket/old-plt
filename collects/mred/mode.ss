; Modes

(define mred:mode@
  (unit/sig mred:mode^
    (import [mred:debug : mred:debug^]
	    [mred:keymap : mred:keymap^])
    
    (mred:debug:printf 'invoke "mred:mode@")

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
	      
	      [on-set-style (lambda (edit start len) #t)]
	      [after-set-style (lambda (edit start len) (void))]

	      [on-edit-sequence (lambda (edit) (void))]
	      [after-edit-sequence (lambda (edit) (void))]

	      [on-set-size-constraint (lambda (edit) #t)]
	      [after-set-size-constraint (lambda (edit) (void))]

	      [after-set-position (lambda (edit) (void))]

	      [on-focus (lambda (edit on?) (void))]


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
		     (mred:keymap:set-keymap-error-handler keymap)
		     (mred:keymap:set-keymap-implied-shifts keymap)))]
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

    (define mode% (make-mode% basic-mode%))))
