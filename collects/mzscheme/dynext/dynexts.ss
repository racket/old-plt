
(reference-relative-library "compiles.ss")
(reference-relative-library "links.ss")
(reference-relative-library "files.ss")

(define-signature dynext^
  ((unit compile@ : dynext:compile^)
   (unit link@ : dynext:link^)
   (unit file@ : dynext:file^)))
