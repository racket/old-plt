; @echo off
; mzscheme -r %0 
; goto :end
(require "servlet-integrity.ss")
(require "link-check.ss")

(run-servlet-check)
(run-link-checker)
; :end
