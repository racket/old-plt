(require-library "refer.ss")
(require-library "debug.ss" "system")
(require-library "invsig.ss" "system")
(require-library "minsig.ss" "mred")

(define mzos@
  (let ([gui@
	 (unit/sig (add-button open-gui)
	   (import (wx : wx^)
		   (mred:container : mred:container^))

	   (define frame%
	     (class-asi mred:container:frame%
	       (rename [super-on-close on-close])
	       (public
		 [on-close
		  (lambda ()
		    (and (super-on-close)
			 (if (= wx:const-yes
				(wx:message-box "Are you sure you want to quit MzOS?"
						"MzOS Quit?"
						(bitwise-ior wx:const-centre
							     wx:const-yes-no
							     wx:const-icon-question)))
			     (exit)
			     #f)))])))

	   (define frame (make-object frame% null "MzOS"))
	   (define panel (make-object mred:container:vertical-panel% frame))
	   (define add-button
	     (lambda (name callback)
	       (make-object mred:container:button% panel
			    (lambda (b e) (callback))
			    name)))
	   (define open-gui
	     (lambda ()
	       (let ([width
		      (let loop ([buttons (ivar panel children)]
				 [width 10])
			(if (null? buttons)
			    width
			    (loop (cdr buttons)
				  (max width (send (car buttons) get-width)))))])
		 (for-each (lambda (button)
			     (send* button
				    (user-min-width width)
				    (stretchable-in-y #t)
				    (stretchable-in-x #t)))
			   (ivar panel children)))
	       (send frame show #t))))]
	[app@
	 (unit/sig (start-app)
	   (import [wx : wx^])

	   (include (begin-elaboration-time
		     (build-path (collection-path "system") "splash.ss")))
	   (include (begin-elaboration-time
		     (build-path (collection-path "system") "invoke.ss")))

	   (define (start-app collection info)
	     (thread
	      (lambda ()
		(mred:startup-application
		 collection info null
		 (lambda ()
		   (let ([mzos-exit-handler
			  (lambda (code)
			    (custodian-shutdown-all (current-custodian)))])
		     (exit-handler mzos-exit-handler))))))))]
	[main@
	 (unit/sig ()
	   (import (gui : (add-button open-gui))
		   (app : (start-app))
		   (function : mzlib:function^))

	   (define required-requests '(app-sig-library app-unit-library splash-image-path))

	   (define ht (make-hash-table))
	   (define add-to-hash-table
	     (lambda (path dir)
	       (let ([info-file (build-path path dir "info.ss")]
		     [key (string->symbol dir)])
		 (when (file-exists? info-file)
		   (hash-table-put! ht key #f)
		   (with-handlers ([(lambda (x) #t)
				    (lambda (x)
				      (when (and (getenv "MREDDEBUG")
						 (not (eq? x 'failure)))
					(printf "~a " info-file)
					(if (exn? x)
					    (display (exn-message x))
					    (write x))
					(newline))
				      (void))])
		     (let* ([info (load info-file)]
			    [name (info 'name (lambda () (raise 'failure)))])
		       (for-each (lambda (x) (info x (lambda () (raise 'failure))))
				 required-requests)
		       (hash-table-put! ht key info)))))))
	   (define pull-from-hash-table
	     (lambda ()
	       (let* ([ht-list
		       (hash-table-map 
			ht 
			(lambda (p v)
			  (and v
			       (vector (symbol->string p) v))))]
		      [valid-list 
		       (function:foldl
			(lambda (x l)
			  (if x
			      (cons x l)
			      l))
			null
			ht-list)]
		      [sorted-list
		       (function:quicksort
			valid-list
			(lambda (x y) (string<=? ((vector-ref x 1) 'name (lambda () "")) 
						 ((vector-ref y 1) 'name (lambda () "")))))])
		 sorted-list)))

	   (define collections/infos
	     (let loop ([paths (reverse (current-library-collection-paths))])
	       (cond
		 [(null? paths) (pull-from-hash-table)]
		 [else (let ([path (car paths)])
			 (when (directory-exists? path)
			   (for-each (lambda (dir) (add-to-hash-table path dir))
				     (directory-list path)))
			 (loop (cdr paths)))])))

	   (define (add-collection collection/info)
	     (let ([collection (vector-ref collection/info 0)]
		   [info (vector-ref collection/info 1)])
	       (gui:add-button (info 'name (lambda () "Impossible Name"))
			       (lambda ()
				 (app:start-app collection info)))))
	   
	   (for-each add-collection collections/infos)
	   (gui:open-gui))])
    (compound-unit/sig (import)
      (link [W : wx^ (wx@)]
	    [F : mzlib:function^ ((require-library-unit/sig "functior.ss"))]
	    [S : mred:minimal^ ((require-library-unit/sig "minimal.ss" "mred") F W)]
	    [G : (add-button open-gui) (gui@ W (S container))]
	    [A : (start-app) (app@ W)]
	    [M : () (main@ G A F)])
      (export))))

(define mred:initialize
  (lambda args
    (invoke-unit/sig mzos@)))