(module run-text-tests mzscheme

  (require "classic-java-tests.ss"
           (lib "text-ui.ss" "schemeunit"))

  (test/text-ui classic-java-tests))
