(define mred:startup-application
  (lambda (collection info extra-args init-param)
    (let ([orig-param (current-parameterization)]
	  [new-param (make-parameterization)])
      (with-parameterization new-param
	(lambda ()
	  (let-values ([(mred:change-splash-message mred:shutdown-splash mred:close-splash)
			(mred:splash info)])
	    (current-will-executor (make-will-executor))
	    (current-namespace (make-namespace 'wx))
	    (current-custodian (make-custodian))
	    (user-break-poll-handler wx:check-for-break)
	    (init-param)
	    (wx:current-eventspace (wx:make-eventspace))
	    ;; test for working eventspaces
	    ;; should have both names the same in the list
	    '(let ([eh (lambda (param)
			 (with-parameterization param 
			   (lambda () (exit-handler))))])
	       (wx:message-box (format
				"~a"
				(list (eh (wx:eventspace-parameterization
					   (wx:current-eventspace)))
				      (eh new-param)))))
	    
	    (require-library "invsig.ss" "system")
	    (require-library "invsig.ss" "system")
	    (require-library "debug.ss" "system")
	    (require-library "refer.ss")
	    
	    (when (and (eq? wx:platform 'windows))
	      (let ([hd (getenv "HOMEDRIVE")]
		    [hp (getenv "HOMEPATH")])
		(when (and hd hp)
		  (let ([path (build-path hd hp)])
		    (when (directory-exists? path)
		      (current-directory path))))))
	    
	    (let* ([default-path (with-handlers ([void (lambda (x) #f)])
				   (collection-path "afm"))]
		   [path-box (box (or default-path ""))])
	      (wx:get-resource "MrEd" "afmPath" path-box)
	      (when (directory-exists? (unbox path-box))
		(wx:set-afm-path (unbox path-box))))
	    
	    (require-library (info 'app-sig-library) collection)
	    
	    (let ([app (require-library (info 'app-unit-library) collection)])
	      (mred:change-splash-message "Invoking...")
	      (mred:shutdown-splash)
	      (let ([argv (list->vector extra-args)])
		;; the non unitized approach relies on this being invoke-open
		(invoke-open-unit/sig app #f mred:application-imports^)))
	    
	    (mred:close-splash)))))))
