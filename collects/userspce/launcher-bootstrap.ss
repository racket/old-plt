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
