(module runtime-context "runtime-base.ss"
  (#%provide runtime-context py-so)
  (#%define runtime-context #'my_context)

  (#%define (py-so s-exp)
        (#%datum->syntax-object runtime-context s-exp)))
  