;; set things up so that the load-handler opens files into
;; a text when the file begins with WXME so that mred saved
;; files still load properly.

(require-library "errortrace.ss" "errortrace")

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
	      mred^)
      
      (message-box "starting launcher" "starting launcher")

      (define show-banner? #f)
      (define repl? #f)
      (define (initialize-userspace)
	(message-box "initialize-userspace" "initialize-userspace")
	;; need to invoke teachpacks here.
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
	   [main : () (main prims basis mzlib mred)])
     (export))))

(go)
