#!/bin/sh
#|
exec mzscheme -mvt "$0" "$@"
|#

(module make mzscheme
  (require (lib "launcher.ss" "launcher"))

  (make-mred-launcher
   '("-mvq"
     "-e" "(use-compiled-file-kinds 'none)"
     "-e" "(current-directory(build-path(collection-path\"mzlib\") 'up 'up))"
     "-e" "(current-command-line-arguments '(\"-i\"))"
     "-e" "(load \"install\")"
     "-mv")
   (mred-program-launcher-path "Finish Install")
   (cons
    '(forget-exe? . #t)
    (build-aux-from-path (build-path
                          (collection-path "finish-install")
                          "finish-install")))))
