
; Test MzLib
; See also pptest.ss and ztest.ss

(load-relative "loadtest.ss")

(load-relative "function.ss")

(load-relative "string.ss")

(load-relative "filelib.ss")

(load-relative "date.ss")

(load-relative "compat.ss")

(load-relative "cmdline.ss")

(load-relative "pconvert.ss")

(load-relative "pretty.ss")

(load-relative "contracts.ss")

; Next-to-last, because it `require's mzscheme
(load-relative "shared.ss")

; Last - so macros are not present by accident
(load-relative "macrolib.ss")

(report-errs)
