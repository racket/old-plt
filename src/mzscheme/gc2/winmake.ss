
(require (lib "restart.ss")
	 (lib "process.ss"))

(define (system- s)
  (fprintf (current-error-port) "~a~n" s)
  (system s))

(define opt-flags "")
(define re:only #f)

(define srcs
  '("salloc"
    "bignum"
    "bool"
    "builtin"
    "char"
    "complex"
    "dynext"
    "env"
    "error"
    "eval"
    "file"
    "fun"
    "hash"
    "image"
    "list"
    "module"
    "network"
    "numarith"
    "number"
    "numcomp"
    "numstr"
    "port"
    "portfun"
    "print"
    "rational"
    "read"
    "regexp"
    "sema"
    "setjmpup"
    "string"
    "struct"
    "symbol"
    "syntax"
    "stxobj"
    "thread"
    "type"
    "vector"))

(define (try src deps dest objdest includes use-precomp extra-compile-flags expand-extra-flags)
  (when (or (not re:only) (regexp-match re:only dest))
  (unless (and (file-exists? dest)
	       (let ([t (file-or-directory-modify-seconds dest)])
		 (andmap
		  (lambda (dep)
		    (> t (file-or-directory-modify-seconds dep)))
		  deps)))
    (unless (restart-mzscheme #() (lambda (x) x)
			      (list->vector 
			       (append
				(list "-r"
				      "xform.ss")
				(if objdest
				    (if use-precomp
					(list "--precompiled" use-precomp)
					null)
				    (list "--precompile"))
				(list
				 (format "cl.exe /MT /E ~a ~a" expand-extra-flags includes)
				 src
				 dest)))
			      void)
      (when (file-exists? dest)
	(delete-file dest))
      (error "error xforming")))
  (when objdest
    (compile dest objdest null extra-compile-flags))))

(define (compile c o deps flags)
  (unless (and (file-exists? o)
	       (let ([t (file-or-directory-modify-seconds o)])
		 (and (>= t (file-or-directory-modify-seconds c))
		      (andmap
		       (lambda (f)
			 (>= t (file-or-directory-modify-seconds f)))
		       deps))))
    (unless (system- (format "cl.exe ~a /MT /Zi ~a /c ~a /Fdxsrc/ /Fo~a" flags opt-flags c o))
      (error "failed compile"))))

(define common-deps (list "xform.ss"))
(define (find-obj f d) (format "../../worksp/~a/release/~a.obj" d f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(try "precomp.c" common-deps "xsrc/precomp.h" #f "/I ../include /I ../src" #f "" "")

(for-each
 (lambda (x)
   (try (format "../src/~a.c" x)
	(list* (find-obj x "libmzsch")
	       (format "../src/~a.c" x)
	       common-deps)
	(format "xsrc/~a.c" x)
	(format "xsrc/~a.obj" x)
	"/I ../include"
	"xsrc/precomp.h"
	""
	""))
 srcs)

(try "../main.c"
     (list* (find-obj "main" "mzscheme")
	    "../main.c"
	    common-deps)
     "xsrc/main.c"
     "xsrc/main.obj"
     "/I ../include"
     #f
     ""
     "")

(compile "gc2.c" "xsrc/gc2.obj" '("compact.c") "/D GC2_AS_EXPORT")
(compile "../src/mzsj86.c" "xsrc/mzsj86.obj" '() "/I ../include")

(define dll "libmzsch3mxxxxxxx.dll")
(define exe "mzscheme3m.exe")

(define libs "kernel32.lib user32.lib wsock32.lib shell32.lib")

(define (link-dll objs sys-libs dll link-options exe?)
  (let ([ms (if (file-exists? dll)
		(file-or-directory-modify-seconds dll)
		0)])
    (when (ormap
	   (lambda (f)
	     (> (file-or-directory-modify-seconds f)
		ms))
	   objs)
      (unless (system- (format "cl.exe ~a /MT /Zi /Fe~a ~a ~a ~a"
			       (if exe? "" "/LD /DLL")
			       dll
			       (let loop ([objs (append objs sys-libs)])
				 (if (null? objs)
				     ""
				     (string-append
				      (car objs)
				      " "
				      (loop (cdr objs)))))
			       libs
			       link-options))
	(error 'winmake "~a link failed" (if exe? "EXE" "DLL"))))))

(let ([objs (list*
	     "xsrc/gc2.obj"
	     "xsrc/mzsj86.obj"
	     (find-obj "gmp" "libmzsch")
	     (map
	      (lambda (n)
		(format "xsrc/~a.obj" n))
	      srcs))])
  (link-dll objs null dll "" #f))

(let ([objs (list
	     "xsrc/main.obj"
	     "libmzsch3mxxxxxxx.lib")])
  (link-dll objs null exe "" #t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wx-inc (string-append "/I ../include "
			      "/I ../gc2 "
			      "/I ../../wxwindow/include/msw "
			      "/I ../../wxwindow/include/base "
			      "/I ../../mred/wxme "
			      "/I ../../wxwindow/contrib/wxxpm/libxpm.34b/lib "
			      "/I ../../wxWindow/contrib/fafa "
			      "/I ../../wxcommon/jpeg /I ../../worksp/jpeg "))
(try "wxprecomp.cxx" common-deps "xsrc/wxprecomp.h" #f wx-inc #f "" "-DGC2_AS_IMPORT")

(define (wx-try base proj x use-precomp? suffix)
  (let ([cxx-file (format "../../~a/~a.~a" base x suffix)])
    (try cxx-file
	 (list* (find-obj x proj)
		cxx-file
		common-deps)
	 (format "xsrc/~a.~a" x suffix)
	 (format "xsrc/~a.obj" x)
	 wx-inc
	 (and use-precomp? "xsrc/wxprecomp.h")
	 "-DGC2_JUST_MACROS /FI../gc2.h"
	 "-DGC2_AS_IMPORT")))

(define wxwin-base-srcs
  '("wb_canvs"
    "wb_cmdlg"
    "wb_data"
    "wb_dc"
    "wb_dialg"
    "wb_frame"
    "wb_gdi"
    "wb_hash"
    "wb_item"
    "wb_list"
    "wb_main"
    "wb_obj"
    "wb_panel"
    "wb_print"
    "wb_ps"
    "wb_stdev"
    "wb_sysev"
    "wb_timer"
    "wb_types"
    "wb_utils"
    "wb_win"))

(map (lambda (x)
       (wx-try "wxwindow/src/base" "wxwin" x #t "cxx"))
     wxwin-base-srcs)

(define wxwin-msw-srcs
  '("wx_buttn"
    "wx_canvs"
    "wx_check"
    "wx_choic"
    "wx_clipb"
    "wx_cmdlg"
    "wx_dc"
    "wx_dialg"
    "wx_frame"
    "wx_gauge"
    "wx_gbox"
    "wx_gdi"
    "wx_item"
    "wx_lbox"
    "wx_main"
    "wx_menu"
    "wx_messg"
    "wx_panel"
    "wx_pdf"
    "wx_rbox"
    "wx_slidr"
    "wx_tabc"
    "wx_timer"
    "wx_utils"
    "wx_win"
    "wximgfil"))

(map (lambda (x)
       (wx-try "wxwindow/src/msw" "wxwin" x #t "cxx"))
     wxwin-msw-srcs)

(define wxs-srcs
  '("wxs_bmap"
    "wxs_butn"
    "wxs_chce"
    "wxs_ckbx"
    "wxs_cnvs"
    "wxs_dc"
    "wxs_evnt"
    "wxs_fram"
    "wxs_gage"
    "wxs_gdi"
    "wxs_glob"
    "wxs_item"
    "wxs_lbox"
    "wxs_madm"
    "wxs_mede"
    "wxs_medi"
    "wxs_menu"
    "wxs_mio"
    "wxs_misc"
    "wxs_mpb"
    "wxs_obj"
    "wxs_panl"
    "wxs_rado"
    "wxs_slid"
    "wxs_snip"
    "wxs_styl"
    "wxs_tabc"
    "wxs_win"
    "wxscheme"))

(map (lambda (x)
       (wx-try "mred/wxs" "wxs" x #t "cxx"))
     wxs-srcs)

(define wxme-srcs
  '("wx_cgrec"
    "wx_keym"
    "wx_mbuf"
    "wx_medad"
    "wx_media"
    "wx_medio"
    "wx_mline"
    "wx_mpbrd"
    "wx_mpriv"
    "wx_msnip"
    "wx_snip"
    "wx_style"))

(map (lambda (x)
       (wx-try "mred/wxme" "wxme" x #t "cxx"))
     wxme-srcs)

(define mred-srcs
  '("mred"
    "mredmsw"))

(map (lambda (x)
       (wx-try "mred" "libmred" x #t "cxx"))
     mred-srcs)

(wx-try "wxcommon" "wxme" "wxJPEG" #t "cxx")
(wx-try "mzscheme/utils" "wxme" "xcglue" #f "c")
(compile "../../wxcommon/wxGC.cxx"
	 "xsrc/wxGC.obj"
	 (list "../../worksp/wxme/Release/wxGC.obj")
	 (string-append wx-inc " -DMZ_PRECISE_GC -DGC2_AS_IMPORT -Dwx_msw"))

(let ([objs (append (list
		     "xsrc/wxGC.obj"
		     "xsrc/wxJPEG.obj"
		     "xsrc/xcglue.obj")
		    (map
		     (lambda (n)
		       (format "xsrc/~a.obj" n))
		     (append wxwin-base-srcs
			     wxwin-msw-srcs
			     wxs-srcs
			     wxme-srcs
			     mred-srcs)))]
      [libs (list
	     "libmzsch3mxxxxxxx.lib"
	     "../../worksp/wxutils/Release/wxutils.lib"
	     "../../worksp/jpeg/Release/jpeg.lib")]
      [win-libs (list
		 "comctl32.lib" "glu32.lib" "opengl32.lib"
		 "gdi32.lib" "comdlg32.lib" "advapi32.lib" 
		 "shell32.lib" "ole32.lib" "oleaut32.lib"
		 "winmm.lib")])
  (link-dll (append objs libs) win-libs "libmred3mxxxxxxx.dll" "" #f))

(wx-try "mred" "mred" "mrmain" #f "cxx")

(let ([objs (list
	     "xsrc/mrmain.obj"
	     "libmzsch3mxxxxxxx.lib"
	     "libmred3mxxxxxxx.lib")])
  (link-dll objs null "MrEd3m.exe" "/link /subsystem:windows" #t))
