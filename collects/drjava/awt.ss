(unit/sig ()
  (import jvm^ mred^)
  #|
  (define toolkit-c (jfind-class "java/awt/Toolkit"))
  (define kit-field (jfind-static-field toolkit-c "toolkit" "Ljava/awt/Toolkit;"))
  (define mred-toolkit-c (jfind-class "edu/rice/cs/drj/MrEdToolkit"))
  (define mred-toolkit-init (jfind-method mred-toolkit-c "<init>" "()V"))
  (define mred-toolkit (jnew mred-toolkit-c mred-toolkit-init))
  (jset-field! toolkit-c kit-field mred-toolkit)
  
  ;; should be auto generated, but isn't
  (let ([class-name (jfind-class "edu/rice/cs/drj/MrEdToolkit")]
        [args (list (lambda () (bell)))])
    (apply jcall class-name (jfind-static-method class-name "SchemeNativeInit$$$"
						 "(Ledu/rice/cs/drj/SchemeFunction;)V")
           args)) |#)