(module run-text-tests mzscheme

  (require "classic-java-tests.ss"
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1)))

  (test/text-ui classic-java-tests))
