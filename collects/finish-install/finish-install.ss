(module finish-install mzscheme
  (require (lib "mred.ss" "mred"))

  (putenv "OSX_PLT_INSTALL" "yes")

  (current-directory (build-path (collection-path "finish-install") 'up 'up))
  (load "install"))
