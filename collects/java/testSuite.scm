;; Mario Latendresse, October 2000.
;;
;; Simply compile several programs for a quick test of the compiler.
;; Adjust directory according to your system.

(define (quickCompilerTest)
  (let* ((dir "/home/latendre/java/comp/jikestst/derek/src/"))
    (java-compile-file dir "TestConstants.java")
    (java-compile-file dir "TestCompare.java")
    (java-compile-file dir "TestArithmetic.java" )
    (java-compile-file dir "TestArrayAccess.java")
    ;; (java-compile-file dir "TestClassInitializer.java")
    (java-compile-file dir "TestCompare.java"    )
    (java-compile-file dir "TestConstants.java"  )
    (java-compile-file dir "TestConversions.java")
    (java-compile-file dir "TestFieldAccess.java")
    (java-compile-file dir "TestFinally.java"    )
    (java-compile-file dir "TestIO.java"         )
    (java-compile-file dir "TestInstanceOf.java" )
    (java-compile-file dir "TestInterfaceCall.java")
    (java-compile-file dir "TestReturn.java"     )
    (java-compile-file dir "TestStackAccess.java")
    (java-compile-file dir "TestStackTrace.java" )
    (java-compile-file dir "TestStaticCall.java" )
    (java-compile-file dir "TestSwitch.java"     )
    (java-compile-file dir "TestThrow.java"      )
    (java-compile-file dir "TestVirtualCall.java")
    ))
