(compound-unit/sig
  (import (FUN : mzlib:function^))
  (link
   (L : xml-lex^ ((require-library "xml-lex.ss" "xml")))
   (S : xml-structs^ ((require-library "structures.ss" "xml")))
   (R : reader^ ((require-library "reader.ss" "xml") S L))
   (U : writer^ ((require-library "writer.ss" "xml") S FUN))
   (T : xexpr^ ((require-library "xexpr.ss" "xml") S FUN)))
  (export (open S) (open R) (open U) (open T)))
