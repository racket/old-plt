(module info (lib "infotab.ss" "setup")
  (define name "JavaWizard")
  (define tools (list (list "tool.ss")))
  (define comment '(define compile-subcollections (list (list "profj" "parsers")
                                       (list "profj" "libs" "java" "lang")
				       (list "profj" "libs" "java" "io"))))
  (define compile-omit-files '("draw-txt.ss" "macro-class.scm"))

  )
