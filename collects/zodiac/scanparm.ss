;;
;;  zodiac:scanner-parameters@
;;  $Id: scanparm.ss,v 1.2 1997/05/06 18:36:21 krentel Exp krentel $
;;
;;  Scanner/Reader Parameters.
;;
;;  The scan values (outside make-scanner) mostly can
;;  be reset at will.  But don't use letters, digits, #, etc.
;;  The parameters inside make-scanner should not be reset.
;;
;;  The char lists can be either chars or ints.
;;

(unit/sig  zodiac:scanner-parameters^
  (import  zodiac:structures^)

   ;; Only #\space and #\newline are always builtin,
   ;; so we specify the rest with ascii codes.

   (define  space    #\space)
   (define  nul       0)
   (define  backsp    8)
   (define  tab       9)
   (define  newline  10)
   (define  vtab     11)
   (define  page     12)
   (define  return   13)
   (define  rubout  127)
   
   (define  scan:paren-relation  '( (#\( #\) )  (#\[ #\] )))
   
   (define  scan:self-delim-symbols  (list #\{ #\} ))
   
   (define  scan:newline-list  (list  newline  return))
   (define  scan:tab-list      (list  tab))
   (define  scan:whitespace-list
     (list  space  tab  newline  vtab  page  return))

   (define  scan:delim-list
     (append  scan:whitespace-list
              (map  car   scan:paren-relation)
              (map  cadr  scan:paren-relation)
	      scan:self-delim-symbols
	      (list  #\;  #\"  #\,  #\'  #\` )))
   
   (define  scan:special-char-list
     `(("space"      ,space)
       ("newline"    ,newline)
       ("linefeed"   ,newline)
       ("nul"        ,nul)
       ("null"       ,nul)
       ("backspace"  ,backsp)
       ("tab"        ,tab)
       ("vtab"       ,vtab)
       ("page"       ,page)
       ("return"     ,return)
       ("rubout"     ,rubout)))
   
   (define default-initial-location  (make-location 1 1 0 'nofile))
   (define scan:def-first-col  1)
   (define scan:def-vect-val  0)
 )

