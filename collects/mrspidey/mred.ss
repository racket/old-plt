;; mred.ss
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
; Pulls up MrSpidey without DrScheme
; Loads all UI and analysis files into MrEd
;; ----------------------------------------------------------------------
;; ported to MrEd 100 and unitized by Paul Steckler

(require-library "errortrace.ss" "errortrace")

(require-library "match.ss")                

(require-library "browser.ss" "browser")
(require-library "framework.ss" "framework")
(require-library "mred-interfaces.ss" "framework")

(load-relative "macros.ss")
(load-relative "pltrc-co.ss")
(load-relative "Sba/sigs.ss")

(define-signature mrspidey:mred^ (mred-analyze-file))

(define mrspidey:mred@ 
  (unit/sig mrspidey:mred^
	    
    (import
				  
     mrspidey:sba^
     mrspidey:mzlib:function^
     mred^
     framework^
     browser^
     mzlib:file^
     mzlib:pretty-print^
     [zodiac : zodiac:system^]
     mrspidey:interaction^)

    (include "Gui/deltas.ss")
    (include "Gui/statedit.ss")
    (include "Gui/dyn-edit.ss")
    (include "Gui/graphics.ss")
    (include "Gui/arrow.ss")
    (include "Gui/annotat.ss")
    (include "Gui/prefs.ss")
    (include "Gui/Tframe.ss")
    (include "Gui/main.ss")

    (define mrspidey:load-progress 
      (lambda (str)
	(printf "Loading ~s~n" str)
	(flush-output)))
    
    (define mred-analyze-file
      (lambda (filename)
	(with-handlers 
	 ([(lambda (x) (eq? x 'mrspidey-raise))
	   (lambda (x) (void))])
	 (let*-values 
	   ([(dir name is-dir) 
	     (split-path (path->complete-path filename))])
	 (parameterize ([current-load-relative-directory dir])
		       (send spidey run-mrspidey filename))))))))

(require-library "load.ss" "zodiac")
(require-library "sigs.ss" "mrspidey" "Sba")
(require-library "drspidey.ss" "mrspidey")

(define mrspidey:interaction@
  (unit/sig mrspidey:interaction^
    (import 
      [mred : mred^]
      [zodiac : zodiac:system^]
      mzlib:file^)

    (include "handlers.ss")

    (mrspidey:error-handler
      (case-lambda
        [(message object)
          (unless (zodiac:zodiac? object)
            (printf "Bad object in mrspidey:error-handler ~s~n" object)
            ((mrspidey:error-handler) message))
          (let* ([loc (zodiac:zodiac-start object)])
            (unless (zodiac:location? loc)
              (printf "Bad location in mrspidey:error-handler ~s~n" loc)
              ((mrspidey:error-handler) message))
            ((mrspidey:error-handler)
              (format "~a at ~s line ~s, column ~s~n"
                message
                (file-name-from-path (zodiac:location-file loc))
                (zodiac:location-line loc)
                (zodiac:location-column loc))))]
        [(message)
          (mred:message-box
            (format "~a~n" message)
            "MrSpidey Error")
          (raise 'mrspidey-raise)]))
    ))


(define mred:mrspidey@
  (compound-unit/sig ; mrspidey:mred^ 
   (import)
   (link
    [ZODIAC : zodiac:system^
	      (zodiac:system@
	       INTERFACE 
	       (MZLIB pretty-print)
	       (MZLIB file))]
    [MZLIB : mzlib:core^
	      ((require-library "corer.ss"))]
    [MRED : mred^ (mred@)]
    [MRED-INTERFACE : mred-interfaces^
	       (mred-interfaces@)]
    [FRAMEWORK : framework^ 
	       ((require-library "frameworkr.ss" "framework")
		MZLIB MRED-INTERFACE)]
    [INTERACTION : mrspidey:interaction^
		 (mrspidey:interaction@ 
		  MRED ZODIAC (MZLIB file))]
    [INTERFACE : zodiac:interface^
		    (mrspidey:zodiac:interface@ INTERACTION)]
    
    ; browser units

    [URL : mzlib:url^
	 ((require-library "urlr.ss" "net") (MZLIB file))]
    [BTREE : relative-btree^ ((require-library "btree.ss" "browser"))]
    [BULLET : bullet-snip^ ((require-library "bullet.ss" "browser") MRED)]
    [HTML : browser:html^ ((require-library "html.ss" "browser") 
			   (MZLIB file)
			   (MZLIB string)
			   BTREE URL BULLET MRED)]	
    [BROWSER : browser^
	     ((require-library "hyper.ss" "browser")
	      HTML 
	      (MZLIB function)
	      (MZLIB string)
	      URL MRED)]

    ; end browser units

    ; Spidey units

    [SBA : mrspidey:sba^
	 ((require-library "link.ss" "mrspidey" "Sba")
	  INTERACTION
	  ((MZLIB function) :  mrspidey:mzlib:function^)
	  (MZLIB pretty-print)
	  (MZLIB file)
	  (MZLIB string)
	  ZODIAC)]
   [MRED-SPIDEY : mrspidey:mred^
    (mrspidey:mred@
      SBA
      ((MZLIB function) : mrspidey:mzlib:function^)
      MRED
      FRAMEWORK
      BROWSER
      (MZLIB file)
      (MZLIB pretty-print)
      ZODIAC
      INTERACTION)])
    (export
     (open MRED-SPIDEY))))

(invoke-open-unit/sig mred:mrspidey@)

(define (go) (mred-analyze-file "info.ss"))

