;; This program reads MzScheme/MrEd C/C++ source and transforms it
;; to work with precise garbage collection or(!) PalmOS. The source
;; is C-pre-processed first, then run though a `lex'-like lexer,
;; ctok.ss.
;;
;; It probably won't work for other C/C++ code, because it
;; doesn't bother *parsing* the source. Instead, it relies on
;; various heuristics that work for MzScheme/MrEd code.
;;
;; There are also some input hacks, such as START_XFORM_SKIP.
;; 
;; Notable assumptions:
;;  No calls of the form (f)(...).
;;  For arrays, records, and non-pointers, pass by address only.
;;  No gc-triggering code in .h files.
;;  No instance vars declared as function pointers without a typedef
;;    for the func ptr type.
;;
;; BUGS: Doesn't check for pointer comparisons where one of the
;;       comparees is a function call. This doesn't happen in
;;       MzScheme/MrEd (or, because of this bug, shouldn't!).
;;
;;       Passing the address of a pointer is dangerous; make sure
;;       that the pointer is used afterward, otherwise it pointer
;;       might not get updated during GC.
;;
;;       A "return;" can get converted to "{ <something>; return; };",
;;       which can break "if (...) return; else ...".

;; To call for Precise GC:
;;   mzscheme -qr xform.ss [--notes] [--depends] [--cgc] <cpp> <src> <dest>
;;
;; To call for Palm:
;;   mzscheme -qr xform.ss [--notes] [--depends] --palm <cpp> <src> <dest> <mapdest>

;; General code conventions:
;;   e means a list of tokens, often ending in a '|;| token
;;   -e means a reversed list of tokens

;; Setup an xform-collects tree for running xform.
;; Delete existing xform-collects tree if it's for an old version
(unless (and (file-exists? "xform-collects/version.ss")
	     (equal? (version)
		     (with-input-from-file "xform-collects/version.ss" read)))
  (load-relative "setup.ss"))

(current-library-collection-paths (list (build-path (current-directory) "xform-collects")))

(error-print-width 100)

(require (lib "xform-mod.ss" "xform"))
