
(reference-relative-library "prettys.ss")
(reference-relative-library "files.ss")
(reference-relative-library "functios.ss")
(reference-relative-library "compats.ss")
(reference-relative-library "strings.ss")
(reference-relative-library "compiles.ss")
(reference-relative-library "threads.ss")

(define-signature mzlib:core^
  ((unit pretty-print@ : mzlib:pretty-print^)
   (unit file@ : mzlib:file^)
   (unit function@ : mzlib:function^)
   (unit compat@ : mzlib:compat^)
   (unit string@ : mzlib:string^)
   (unit compile@ : mzlib:compile^)
   (unit thread@ : mzlib:thread^)))

