;; set things up so that the load-handler opens files into
;; a text when the file begins with WXME so that mred saved
;; files still load properly.

(require-library "errortrace.ss" "errortrace")

(require-library "core.ss" "drscheme-jr")

(define go 
  (make-go
   (let ([settings settings]
         [teachpacks teachpacks]
         [filename filename]
         [mred@ mred@])
   (unit/sig drscheme-jr:settings^
     (import [prims : prims^]
             [basis : userspace:basis^]
             [mzlib : mzlib:core^])
     
     (define show-banner? #f)
     (define repl? #f)
     (define (initialize-userspace)
       ;; need to invoke teachpacks here.
       (global-define-values/invoke-unit/sig mred^ mred@))

     (define setting (apply basis:make-setting (cdr (vector->list settings))))
     (define startup-file filename)))))

(go)
