(load-relative "sig.ss")

(compound-unit/sig
  (import)
  (link
   (F : mzlib:function^ (mzlib:function@))
   (STRING : mzlib:string^ (mzlib:string@))
   (FILE : mzlib:file^ (mzlib:file@ STRING F))
   (JVM : jvm^ (jvm@))
   (Q : queue^ ((load-relative "queue.ss")))
   (ERROR : error^ ((load-relative "text-error.ss") JVM))
   (GJC : gjc^ ((load-relative "gjc.ss") JVM ERROR))
   (SCAN : scanner^ ((load-relative "scanner.ss") JVM GJC Q))
   (SPLIT : split^ ((load-relative "split.ss") JVM Q GJC SCAN F FILE))
   (REPL : repl^ ((load-relative "repl.ss") JVM SCAN Q GJC SPLIT F)))
  (export (open (SPLIT : (compile))) (open REPL)))