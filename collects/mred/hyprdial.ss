; Dialog box for picking a tag.

  (unit/sig mred:hyper-dialog^
    (import [wx : mred:wx^]
	    [mred:constants : mred:constants^]
	    [mred:hyper-edit : mred:hyper-edit^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:hyper-dialog@")

    (define hyper-tag-dialog%
      (class wx:dialog-box% (tags-list)
	     (inherit show new-line fit)
	     (sequence
	       (super-init () "Tag Name" #t)
	       (make-object wx:message% this "Select a tag name:")
	       (new-line))
	     (private
	      result
	      (list-box
	       (make-object wx:list-box% this '() "" wx:const-single 
			    -1 -1 -1 -1 tags-list)))
	     (public 
	      [get-answer
	       (lambda ()
		 result)]
	      [do-ok
	       (lambda (button event)
		 (set! result (send list-box get-string-selection))
		 (if (not (null? result))(show #f)))]
	      [do-cancel 
	       (lambda (button event)
		 (set! result #f)
		 (show #f))]
	      [do-other
	       (lambda (button event)
		 (let ([tag-str (wx:get-text-from-user "Enter the tag name:" "Tag Name"
						       "" this)])
		   (unless (null? tag-str)
		     (set! result tag-str)
		     (show #f))))])
	     (sequence
	       (new-line)
	       (make-object wx:button% this do-ok "OK")
	       (make-object wx:button% this do-cancel "Cancel")
	       (make-object wx:button% this do-other "Other")
	       (fit)
	       (show #t))))

    (define hyper-get-current-tags
      (lambda (directory filename)
	(let ([temp-edit (make-object mred:hyper-edit:hyper-edit% ())])
	  (if (send temp-edit load-file  
		    (mzlib:file:normalize-path filename directory))
	      (reverse (map mred:hyper-edit:hypertag-name (ivar temp-edit hypertags-list)))
	      (list "top"))))))