(module empty-context "empty-base.ss"
  (#%require "built-in.ss")
  (#%provide empty-context)
            ; (all-from "built-in.ss"))
  (#%define empty-context #'i_am_empty))