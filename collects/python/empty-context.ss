(module empty-context "empty-base.ss"
  ;(#%require ;"built-in.ss"
             ;"built-in-exceptions.ss"
             ;"built-in-os.ss")
  (#%provide empty-context)
            ; (all-from "built-in.ss"))
  (#%define empty-context #'i_am_empty))
