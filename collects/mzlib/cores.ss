
(reference-library "prettys.ss")
(reference-library "files.ss")
(reference-library "functios.ss")
(reference-library "compats.ss")
(reference-library "strings.ss")
(reference-library "compiles.ss")
(reference-library "threads.ss")

(define-signature mzlib:core^
  ((unit pretty-print@ : mzlib:pretty-print^)
   (unit file@ : mzlib:file^)
   (unit function@ : mzlib:function^)
   (unit compat@ : mzlib:compat^)
   (unit string@ : mzlib:string^)
   (unit compile@ : mzlib:compile^)
   (unit thread@ : mzlib:thread^)))

