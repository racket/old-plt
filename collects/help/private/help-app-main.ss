(module help-app-main mzscheme 
  (require (lib "cmdline.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           "browser-extensions.ss"
           "server.ss"
           "browser.ss")
  
  (command-line
   "help-desk"
   (current-command-line-arguments))
  
  ;; for use by the bug report frame.
  (namespace-set-variable-value! 'help-desk:frame-mixin bug-report/help-desk-mixin)
  
  (define hd-cookie (start-help-server (lambda (x) x)))
  (unless hd-cookie 
    (printf "Help Desk: could not start server\n")
    (exit -1))

  (help-desk-browser hd-cookie))
