(require-library "compat.ss")   ; gets lots of good stuff
(require-library "file.ss")     ; gets normalize-path
(require-library "function.ss") ; gets identity, compose, etc.
(require-library "cmdline.ss")

(require-library "options.ss" "compiler") ; if not already here

(require-library "invoke.ss" "zodiac")
(zodiac:invoke-system)
(require-library "zlayer.ss" "compiler")
(compiler:register-with-zodiac)

(require-library "compiler.ss" "compiler")
(compiler:initialize-zodiac-errors)


