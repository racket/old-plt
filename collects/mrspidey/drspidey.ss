; drspidey.ss
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------
; ported to MrEd 100 by Paul Steckler

;; The code to be loaded from DrScheme

(require-relative-library "pltrc-co.ss")
(require-relative-library "macros.ss")

(require-library "load.ss" "zodiac")

(require-library "sigs.ss" "mrspidey" "Sba")

(define mrspidey:sba@
  (require-library "link.ss" "mrspidey" "Sba"))

(require-library "loadu.ss" "mrspidey" "Gui")

(define mrspidey:interaction@
  (unit/sig mrspidey:interaction^
    (import	
      [zodiac : zodiac:system^]
      mzlib:file^)

    (include "handlers.ss")

    (define mrspidey:error-handler
      (make-parameter
       (case-lambda
        [(message object)
	 (unless (zodiac:zodiac? object)
		 (error "Bad object in mrspidey:error-handler" object)
		 ((mrspidey:error-handler) message))
	 (let* ([loc (zodiac:zodiac-start object)])
	   (unless (zodiac:location? loc)
		   (error "Bad location in mrspidey:error-handler" loc)
		   ((mrspidey:error-handler) message))
	   ((mrspidey:error-handler)
	    (format "~a in ~s, line ~s, column ~s~n"
		    message
		    (file-name-from-path (zodiac:location-file loc))
		    (zodiac:location-line loc)
		    (zodiac:location-column loc))))]
        [(message) 
	 (error message)]
	[else
	 (error "Unknown MrSpidey error")])))))

;; ----------------------------------------------------------------------

(define mrspidey:file-read@
  (unit/sig mrspidey:file-read^
    (import 
      [mred : mred^]
      framework^)
    (define (make-file-thunk-thunk filename)   	
      (lambda () 
 	(let ([txt (make-object mred:text%)])
 	  (send txt load-file filename)
	  (gui-utils:read-snips/chars-from-text txt))))))

;; ------------------------------------------------------------------------

(define mrspidey-tool@
  (unit/sig ()
    (import
      mred^
      framework^
      mrspidey-gui^)
    (lambda (frame)
      (let* ( [edit (ivar frame definitions-text)]
              [name (send edit get-filename)])
        (if (string? name)
          (when
            (or (not (send edit is-modified?))
              (let ([action (gui-utils:unsaved-warning name "Analyze" #t)])
                (case action
                  [(save) (send edit save-file)]
                  [(continue) #t]
                  [else #f])))
            (with-handlers ([(lambda (x) (eq? x 'mrspidey-raise))
			     (lambda (x) (void))])
              (let*-values ([(filename) (send edit get-filename)]
			    [(dir name is-dir) (split-path (path->complete-path filename))])
                  (parameterize ([current-load-relative-directory dir])
		  (send spidey run-mrspidey filename)))))
          (message-box
	   "MrSpidey Error"
	   "MrSpidey can only process programs that are saved to a file"))))))

;; ----------------------------------------------------------------------

(define mrspidey:zodiac:interface@
  (unit/sig zodiac:interface^
    (import mrspidey:interaction^)
    (define default-error-handler
      (lambda (keyword)
        (lambda (where fmt-spec . args)
          (apply mrspidey:internal-error
            keyword fmt-spec args))))
    (define internal-error
      (lambda (where fmt-spec . args)
        (let ([msg (parameterize ([print-struct #t])
				 (apply format fmt-spec args))])
	  (mrspidey:error msg where))))
    (define static-error internal-error) 
    (define dynamic-error
      (default-error-handler 'zodiac-run-time))))

;; ----------------------------------------------------------------------

(define tool@
  (compound-unit/sig 
   (import [FRAMEWORK : framework^]
	   [MRED : mred^]
	   [MZLIB : mzlib:core^]
	   [PCONVERT : mzlib:print-convert^]
	   [DRSCHEME : drscheme:export^]
	   [ZODIAC-UNUSED : zodiac:system^])
    (link [INTERFACE : zodiac:interface^
		     (mrspidey:zodiac:interface@ INTERACTION)]
	  [ZODIAC : zodiac:system^
		  (zodiac:system@ 
		   INTERFACE (MZLIB pretty-print) 
		   (MZLIB file))]

	  [URL : mzlib:url^
	       ((require-library "urlr.ss" "net") (MZLIB file))]
	  [BROWSER : browser^
		   ((require-library "browserr.ss" "browser")
		    (MZLIB function)
		    (MZLIB string)
		    (MZLIB file)
		    URL MRED)]
	  [INTERACTION : mrspidey:interaction^
		      (mrspidey:interaction@ 
		       ZODIAC
		       (MZLIB file))]
	  [FILE-READ : mrspidey:file-read^
		      (mrspidey:file-read@ 
		       MRED
		       FRAMEWORK)]
	  [SBA : mrspidey:sba^	
	       (mrspidey:sba@ 
		INTERACTION 
		FILE-READ	
		((MZLIB function) : mrspidey:mzlib:function^)
		(MZLIB pretty-print)
		(MZLIB file)
		(MZLIB string)
		ZODIAC)]
	  [GUI : mrspidey-gui^
	       (mrspidey-gui@ 
		MRED 
		FRAMEWORK
		DRSCHEME
		BROWSER
		URL
		((MZLIB function) : mrspidey:mzlib:function^)
		(MZLIB pretty-print)
		(MZLIB file)
		(MZLIB string)
		SBA INTERACTION ZODIAC)]
	  [TOOL : () 
		(mrspidey-tool@ 
		 MRED FRAMEWORK GUI)])
    (export)))





