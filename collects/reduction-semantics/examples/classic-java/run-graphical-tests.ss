(module run-graphical-tests mzscheme

  (require "classic-java-tests.ss"
           (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1)))

  (test/graphical-ui classic-java-tests))
