(require-library "load-handlers.ss" "drscheme")
(require-library "guiutils.ss" "framework")

(let* ([main-unit
	(let ([settings settings]
	      [teachpacks teachpacks]
	      [filename filename]
	      [mred@ mred@])
	  (unit/sig drscheme-jr:settings^
	    (import [prims : prims^]
		    [basis : plt:basis^]
		    [mzlib : mzlib:core^]
		    mred^
		    [drscheme:load-handler : drscheme:load-handler^])
	    
	    (basis:teachpack-changed teachpacks)

	    (define show-banner? #f)
	    (define repl? #f)

	    (define orig-eventspace (current-eventspace))
	    (define user-eventspace #f)

	    (define (run-in-new-user-thread thunk)
	      (set! user-eventspace (make-eventspace))
	      (parameterize ([current-eventspace user-eventspace])
		(let ([thread #f]
		      [sema (make-semaphore 0)])
		  (queue-callback (lambda ()
				    (set! thread (current-thread))
				    (semaphore-post sema)))
		  (semaphore-wait sema)
		  (queue-callback
		   (lambda ()
		     (thunk)))
		  thread)))
	    
	    (define (number-open-windows)
	      (parameterize ([current-eventspace user-eventspace])
		   (length (get-top-level-windows))))

	    (define (load-and-repl-done)
	      (if (= 0 (number-open-windows))
		  (exit)
		  (thread
		   (rec f
			(lambda ()
			  (sleep 1/2)
			  (if (= 0 (number-open-windows))
			      (exit)
			      (f)))))))

	    (define (initialize-userspace)
	      (current-load drscheme:load-handler:drscheme-load-handler)
	      (global-define-values/invoke-unit/sig mred^ mred@))

	    (define setting (apply basis:make-setting (cdr (vector->list settings))))
	    (define startup-file filename)))])
  (compound-unit/sig
    (import [prims : prims^]
	    [basis : plt:basis^]
	    [mzlib : mzlib:core^]
	    [zodiac : zodiac:system^])
    (link [mred : mred^ (mred@)]
	  [gui-utils : framework:gui-utils^ ((require-library "guiutilsr.ss" "framework")
					     mred)]
	  [load-handler : drscheme:load-handler^
			((require-library "load-handler.ss" "drscheme")
			 mred
			 zodiac
			 basis
			 gui-utils)]
	  [main : drscheme-jr:settings^ (main-unit prims basis mzlib mred load-handler)])
    (export (open main))))
