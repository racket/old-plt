#|

This file contains all of the initialization of the Help Desk application.
It is only loaded when Help Desk is run by itself (outside DrScheme).

|#

(module help-app-main mzscheme 
  (require (lib "cmdline.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "browser-extensions.ss"
           "server.ss"
           "cookie.ss"
           "standard-urls.ss")
  
  (command-line
   "help-desk"
   (current-command-line-arguments))
  
  (preferences:add-warnings-checkbox-panel)
  
  (define the-hd-cookie (start-help-server (lambda (x) x)))
  (unless the-hd-cookie
    (printf "Help Desk: could not start server\n")
    (exit -1))

   ;; for use by the bug report frame.
  (namespace-set-variable-value! 'help-desk:frame-mixin (make-bug-report/help-desk-mixin the-hd-cookie))
  
  (handler:current-create-new-window (lambda (filename) 
                                       (let ([browser-frame ((hd-cookie-new-browser the-hd-cookie))])
                                         (when (and filename
                                                    (file-exists? filename))
                                           (send (send (send browser-frame get-hyper-panel) get-canvas) goto-url
                                                 (string-append "file://" filename)
                                                 #f))
                                         browser-frame)))
  (goto-hd-location the-hd-cookie 'front-page))
  