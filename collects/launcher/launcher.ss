(require-library "file.ss")
(require-library "compile.ss" "dynext")
(require-library "link.ss" "dynext")
                         
(require-relative-library "launchers.ss")

(begin-elaboration-time
  (require-library "invoke.ss"))

(define-values/invoke-unit/sig launcher-maker^
  (require-relative-library "launcherr.ss")
  #f
  mzlib:file^
  dynext:compile^
  dynext:link^)
