(module run-graphical-tests mzscheme

  (require "classic-java-tests.ss"
           (lib "graphical-ui.ss" "schemeunit"))

  (test/graphical-ui classic-java-tests))
