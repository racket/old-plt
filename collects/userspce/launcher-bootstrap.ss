;; set things up so that the load-handler opens files into
;; a text when the file begins with WXME so that mred saved
;; files still load properly.

(require-library "errortrace.ss" "errortrace")
(require-library "teachpacks.ss" "drscheme")
(require-library "core.ss" "drscheme-jr")

(define main-unit
  (let ([settings settings]
	[teachpacks teachpacks]
	[filename filename]
	[mred@ mred@])
    (unit/sig drscheme-jr:settings^
      (import [prims : prims^]
	      [basis : userspace:basis^]
	      [mzlib : mzlib:core^]
              [drscheme:teachpack : drscheme:teachpack^]
	      mred^)
      
      ;; teachpacks
      (define thnks (mzlib:function:filter 
                     (lambda (x) x)
                     (map drscheme:teachpack:build-teachpack-thunk teachpacks)))
      
      (define show-banner? #f)
      (define repl? #f)
      (define (run-in-new-user-thread thunk)
	(parameterize ([current-eventspace (make-eventspace)])
	  (let ([thread #f]
		[sema (make-semaphore 0)])
	    (queue-callback (lambda ()
			      (set! thread (current-thread))
			      (semaphore-post sema)))
	    (semaphore-wait sema)
	    (queue-callback
	     thunk)
	    thread)))
      (define (initialize-userspace)

        ;; invoke the teachpacks
	(for-each (lambda (thnk) (thnk)) thnks)

        ;; add mred to the namespace
	(global-define-values/invoke-unit/sig mred^ mred@))

      (define setting (apply basis:make-setting (cdr (vector->list settings))))
      (define startup-file filename))))

(define go 
  (make-go
   (compound-unit/sig
     (import [prims : prims^]
	     [basis : userspace:basis^]
	     [mzlib : mzlib:core^])
     (link [mred : mred^ (mred@)]
           [teachpack : drscheme:teachpack^ ((require-library "teachpackr.ss" "drscheme") mred)]
	   [main : drscheme-jr:settings^ (main-unit prims basis mzlib teachpack mred)])
     (export (open main)))))

(go)
