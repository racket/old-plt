(module base "empty-base.ss" ;mzscheme
  (#%require (lib "list.ss")
            ; (all-except mzscheme sqrt map)
           (lib "etc.ss")
           ;"primitives.ss"
	   "runtime-support.ss"
           ;"python-import.ss"
           ;"built-in.ss"
           ;"compile-python.ss"
           ;"built-in-exceptions.ss"
           "c-bindings.ss"
           ;"built-in-os.ss")
           )
  
;
  (#%provide ; (all-from mzscheme)
           ;(all-from (lib "list.ss"))
           ;(all-from "empty-base.ss")
          ; (all-from (lib "etc.ss"))

           ;#%top
           ;#%app
           ;#%provide
           ;#%define
           (rename #%begin begin)
           (rename #%provide provide)
           (rename #%define define)
           (rename #%require require)
           (rename #%datum->syntax-object datum->syntax-object)
           syntax
           #%app
           #%top
           #%datum
           #%module-begin
           
           opt-lambda
           (all-from "c-bindings.ss")
	   ;(all-from "primitives.ss")
           (all-from "runtime-support.ss")
           ;(all-from "python-import.ss")
           ;(all-from "compile-python.ss")
           ;(all-from "built-in.ss")
           ;(all-from "built-in-exceptions.ss")
           
          ; (all-from "built-in-os.ss")
           )
  
  
           )
