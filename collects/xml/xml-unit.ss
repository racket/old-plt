
(module xml-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "xml-sig.ss" "private/sig.ss"
	  "private/structures.ss"
	  "private/reader.ss"
	  "private/writer.ss"
	  "private/xexpr.ss"
	  "private/space.ss")

  (provide xml@)

  (define xml@
    (compound-unit/sig
     (import)
     (link
      (S : xml-structs^ (xml-structs@))
      (R : reader^ (reader@ S))
      (U : writer^ (writer@ S))
      (T : xexpr^ (xexpr@ S U))
      (W : space^ (space@ S)))
     (export (open S) (open R) (open U) (open T) (open W)))))


