#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#

(require (lib "text-ui.ss" "schemeunit")
         (lib "etc.ss")
         "test-servlet.ss"
         )

(test/text-ui test-servlet)
