(require (lib "unitsig.ss")
	 (lib "servlet-sig.ss" "web-server")
         
	 (homework-servlet "websubmit-sig.scm")
	 (homework-servlet "websubmit-machine.scm")
         (homework-servlet "websubmit-pages.scm"))


(define websubmit-servlet@
  (unit/sig ()
    (import websubmit-machine^)
    
    ;; EDIT HERE
    ;;;; The current-directory should be set to the directory that contains
    ;;;; the following files and directories:
    ;;;;;; file: graders 
    ;;;;;; file: student-accounts
    ;;;;;; assignment directories
    (current-directory 
     (build-path "/" "some" "path" "goes" "here")
     )
    (start-machine)))

(define course@
  (unit/sig course-specific^
    (import)
    (define course-title "Test Programming Title")
    (define course-number "tst123")))

(define config@
  (unit/sig websubmit-configuration^
    (import)
    (define min-partners 1)
    (define max-partners 2)))

(invoke-unit/sig
 (compound-unit/sig
   (import (SVT : servlet^))
   (link
    [CRS : course-specific^ (course@)]
    [CFG : websubmit-configuration^ (config@)]
    [WSP : websubmit-pages^ (websubmit-pages@ CRS SVT)]
    [WSM : websubmit-machine^ (websubmit-machine@ CFG WSP)]
    [WSVT : () (websubmit-servlet@ WSM)])
   (export))
 servlet^)
