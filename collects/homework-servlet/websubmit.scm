(require (lib "unitsig.ss")
	 (lib "servlet-sig.ss" "web-server")
         
	 "websubmit-sig.scm"
	 "websubmit-machine.scm"
         "websubmit-pages.scm")


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
     (build-path "/Users" "gregp" "projects" "websubmit-test")
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
