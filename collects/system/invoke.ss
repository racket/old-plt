(define mred:startup-application
  (lambda (collection info extra-args)
    (mred:open-splash info)

    (user-break-poll-handler wx:check-for-break)

    (let* ([default-path (with-handlers ([void (lambda (x) #f)])
			   (collection-path "afm"))]
	   [path-box (box (or default-path ""))])
      (wx:get-resource "MrEd" "afmPath" path-box)
      (when (directory-exists? (unbox path-box))
	(wx:set-afm-path (unbox path-box))))

    (require-library (info 'app-sig-library) collection)

    (let ([app (require-library (info 'app-unit-library) collection)])
       (mred:change-splash-message "Invoking...")
       (mred:no-more-splash-messages)
       (let ([argv (list->vector extra-args)])
	 ;; the non unitized approach relies on this being invoke-open
	 (invoke-open-unit/sig app #f mred:application-imports^)))

    (when (and (eq? wx:platform 'windows))
      (let ([hd (getenv "HOMEDRIVE")]
	    [hp (getenv "HOMEPATH")])
	(when (and hd hp)
	  (let ([path (build-path hd hp)])
	    (when (directory-exists? path)
	      (current-directory path))))))
    
    (mred:close-splash)))
