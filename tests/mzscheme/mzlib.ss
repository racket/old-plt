
; Test MzLib
; See also pptest.ss and ztest.ss

(load-relative "loadtest.ss")

(load-relative "function.ss")

(load-relative "date.ss")

(load-relative "compat.ss")

(load-relative "cmdline.ss")

(load-relative "pconvert.ss")

(load-relative "pretty.ss")

(load-relative "shared.ss")

(load-relative "contracts.ss")

; Last - so macros are not present by accident
(load-relative "macrolib.ss")

(report-errs)
