(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server"))

(require "private/util.ss")
(require "private/headelts.ss")

(unit/sig ()
  (import servlet^)

 (report-errors-to-browser send/finish)

 (define (link-stuff url txt)
   `(LI (B (A ((HREF ,url)) ,txt))))

 `(HTML 
   (HEAD ,hd-css
         ,@hd-links
	 (TITLE "Release Information"))
   (BODY 
    (H1  "Release Information") 
    (P)
    (I "Version: " ,(plt-version))
    (P)
    (UL  
     ,(link-stuff "/servlets/release/license.ss" "License")
     ,(link-stuff "/servlets/release/notes.ss" "Release Notes")
     ,(link-stuff "/servlets/release/bugs.ss" "Known Bugs")
     ,(link-stuff "/servlets/bug-report.ss" "Submit a bug report")
     ,(link-stuff "/servlets/release/patches.ss" "Downloadable Patches"))
    (P)
    "The PLT software is installed on this machine at" (BR)
    (PRE 'nbsp nbsp
	 ,(let-values ([(base file dir?) (split-path (collection-path "mzlib"))])
		      base)))))
