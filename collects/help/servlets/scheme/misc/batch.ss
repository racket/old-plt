(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../../private/util.ss")
(require "../../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

 `(HTML 
   (HEAD ,hd-css
         (TITLE  "How to write Windows batch files"))
   (BODY 
    (H1  "How to write Windows batch files")
    (A ((NAME "sh") (VALUE "Batch files")))
    (A ((NAME "sh2") (VALUE ".bat files")))
    "You can put MzScheme code in a Windows batch file, that is, a "
    "file with a .BAT extension.  Batch files can be executed "
    "directly from the command line.  In Windows 95, 98, and Me, "
    "the batch file looks like:"
    (PRE 
     " ; @echo off" (BR)
     " ; d:\\plt\\mzscheme -r %0 %1 %2 %3 %4 %5 %6 %7 %8 %9" (BR)
     " ; goto :end" (BR)
     "   ... " (I  "scheme-program") " ..." (BR)
     " ; :end")
    "With this code, your batch file can use as many as nine "
    "parameters."
    (P)
    "In Windows NT, Windows 2000, and Windows XP, you can instead write "
    (PRE 
     " ; @echo off" (BR)
     " ; d:\\plt\\mzscheme -r %0 %*" (BR)
     " ; goto :end" (BR)
     "   ... " (I  "scheme-program") " ..." (BR)
     " ; :end")
    "This code allows an arbitrary number of parameters to your " 
    "batch file."
    (P)
    "The batch file code works by combining both batch and MzScheme "
    "syntax in a single file.  When invoked from the command line, "
    "the semicolons are ignored.  The second line invokes MzScheme "
    "with the batch file as an argument.  MzScheme interprets the "
    "lines beginning with semicolons as comments, and runs the "
    "Scheme code.  When the Scheme program is "
    "done, control returns to the batch file, and the "
    (TT  "goto") " jumps around the Scheme code.")))
