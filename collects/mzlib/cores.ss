
(begin-elaboration-time
 (require-relative-library "prettys.ss")
 (require-relative-library "files.ss")
 (require-relative-library "functios.ss")
 (require-relative-library "compats.ss")
 (require-relative-library "strings.ss")
 (require-relative-library "compiles.ss")
 (require-relative-library "threads.ss"))

(define-signature mzlib:core^
  ((unit pretty-print@ : mzlib:pretty-print^)
   (unit file@ : mzlib:file^)
   (unit function@ : mzlib:function^)
   (unit compat@ : mzlib:compat^)
   (unit string@ : mzlib:string^)
   (unit compile@ : mzlib:compile^)
   (unit thread@ : mzlib:thread^)))

