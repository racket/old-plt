
(require-relative-library "compiles.ss")
(require-relative-library "links.ss")
(require-relative-library "files.ss")

(define-signature dynext^
  ((unit compile@ : dynext:compile^)
   (unit link@ : dynext:link^)
   (unit file@ : dynext:file^)))
