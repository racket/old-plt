
;; #%ized so it can be loaded in the "R5RS" namespace
;; Weird begin-elaboration-time structure lets it
;;  be compiled correctly, too.

(#%begin-elaboration-time 
 (define define-values/invoke-unit #%void)
 (define define-values/invoke-unit/sig #%void)
 (define global-define-values/invoke-unit #%void)
 (define global-define-values/invoke-unit/sig #%void))

(#%define-values (define-values/invoke-unit
		  define-values/invoke-unit/sig
		  global-define-values/invoke-unit
		  global-define-values/invoke-unit/sig)
   (#%invoke-unit (#%require-library "invoker.ss")))

(#%define-macro define-values/invoke-unit define-values/invoke-unit)
(#%define-macro define-values/invoke-unit/sig define-values/invoke-unit/sig)
(#%define-macro global-define-values/invoke-unit global-define-values/invoke-unit)
(#%define-macro global-define-values/invoke-unit/sig global-define-values/invoke-unit/sig)

 