;; this contains some extra definitions that go with these units that
;; used to be in handler.ss


(load "hypersig.ss")

(load "hypredit.ss")
(load "hyprfram.ss")
(load "hyprdial.ss")

#|

  (define get-url-from-user
    (lambda ()
      (let* ([frame (make-object dialog-box% (get-top-level-focus-window) "Choose URL")]
	     [main (make-object vertical-panel% frame)]
	     [one-line (make-object editor-canvas% main)]
	     [_ (send one-line set-line-count 1)]
	     [valid? #f]
	     [ok-callback (lambda x (set! valid? #t) (send frame show #f))]
	     [answer (make-object edit:return% ok-callback)]
	     [bottom (make-object horizontal-panel% main)]
	     [space (make-object horizontal-panel% bottom)]
	     [bookmarks (preferences:get 'framework:bookmarks)]
	     [bk-choice
	      (make-object choice% bottom
			   (lambda (box evt)
			     (let ([which (send evt get-command-int)])
			       (when (<= 0 which)
				 (send* answer
					(begin-edit-sequence)
					(erase)
					(insert (list-ref bookmarks which))
					(end-edit-sequence)))))
			   "Bookmarks" -1 -1 -1 -1 bookmarks)]
	     [browse (make-object button%
		       bottom
		       (lambda x
			 (let ([ans (finder:get-file)])
			   (when ans
			     (send* answer
				    (begin-edit-sequence)
				    (erase)
				    (insert "file:")
				    (insert ans)
				    (end-edit-sequence)))))
		       "Browse...")]
	     [cancel (make-object button% bottom
				  (lambda x 
				    (send frame show #f))
				  "Cancel")]
	     [ok (make-object button% bottom
			      ok-callback
			      "Ok")])
	(let ([w (max (send ok get-width)
		      (send cancel get-width)
		      (send browse get-width))])
	  (send ok user-min-width w)
	  (send cancel user-min-width w)
	  (send browse user-min-width w))
	(unless (null? bookmarks)
	  (send answer insert (car bookmarks))
	  (send answer set-position 0 -1))
	(send one-line set-focus)
	(send one-line set-media answer)
	(send frame set-size -1 -1 20 20)
	(send frame center 'both)
	(send frame show #t)
	(and valid? 
	     (send answer get-text)))))

  (define open-url
    (opt-lambda ([input-url #f])
      (let ([url (or input-url (get-url-from-user))])
	(and url
	     (make-object hyper:frame:hyper-view-frame% url)))))




(define-sigfunctor (mred:hyper-loader@ mred:hyper-loader~)
  (import mred:edit~ mred:frame~ mred:canvas~ mred:group~
	  mred:handler~)

  (mred:handler~:insert-format-handler  "Hyper-Text" "htx" 
					(lambda (filename group)
					  (open-hyper-make filename group)))

  (define loaded? #f)
  (define real-open-make #f)
  (define real-open-view #f)

  (define open-hyper-make
    (lambda args
      (hyper-text-require)
      (apply real-open-make args)))
      
  (define open-hyper-view
    (lambda args
      (hyper-text-require)
      (apply real-open-make args)))
      
  (define hyper-text-require
    (lambda ()
      (when (not loaded?)
	    (let ([dir (build-path (global-defined-value 
				    mred:system-source-directory) 
				   "hyper")])
	      (load (build-path dir "hypersig.ss"))
	      (load (build-path dir "hypredit.ss"))
	      (load (build-path dir "hyprdial.ss"))
	      (load (build-path dir "hyprfram.ss"))
	      
	      link [& open] somehow
	       

  )

|#
