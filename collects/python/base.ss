(module base "empty-base.ss" ;mzscheme
  (#%require (lib "list.ss")
             (all-except mzscheme sqrt map)
           "primitives.ss"
	   "runtime-support.ss"
           "built-in.ss")

  (provide ;(all-from mzscheme)
           (all-from (lib "list.ss"))
	   (all-from "primitives.ss")
           (all-from "runtime-support.ss")
           (all-from "built-in.ss")))
