(module info (lib "infotab.ss" "setup")
  (define name "Plot library")
  (define compile-omit-files '("make-archive.ss" "build-from-source.ss" "test/run-low-level-tests.ss" "run-tests.ss" "plplot-low-level.ss" "fit-low-level.ss"))
  (define pre-install-collection "pre-installer.ss"))
