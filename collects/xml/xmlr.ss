(compound-unit/sig
  (import (FUN : mzlib:function^))
  (link
   (L : xml-lex^ ((require-library "xml-lex.ss" "xml")))
   (R : reader^ ((require-library "reader.ss" "xml") L))
   (U : writer^ ((require-library "writer.ss" "xml") R FUN))
   (T : xexpr^ ((require-library "xexpr.ss" "xml") R FUN)))
  (export (open R) (open U) (open T)))
