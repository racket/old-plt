
(reference "prettys.ss")
(reference "files.ss")
(reference "functios.ss")
(reference "compats.ss")
(reference "strings.ss")
(reference "compiles.ss")
(reference "threads.ss")

(define-signature mzlib:core^
  ((unit pretty-print@ : mzlib:pretty-print^)
   (unit file@ : mzlib:file^)
   (unit function@ : mzlib:function^)
   (unit compat@ : mzlib:compat^)
   (unit string@ : mzlib:string^)
   (unit compile@ : mzlib:compile^)
   (unit thread@ : mzlib:thread^)))

