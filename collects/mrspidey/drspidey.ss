;; drspidey.ss
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

;; (printf "loading drspidey.ss (cd ~s)~n" (current-directory))

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
	   "MrSpidey Error"
            (format "~a~n" message))
          (raise 'mrspidey-raise)]))
    ))

;; ----------------------------------------------------------------------

(define mrspidey-tool@
  (unit/sig ()
    (import
      mred^
      framework^
      mrspidey-gui^)
    (version:add-spec 'sd 1)
    (lambda (frame)
      (let* ( [edit (ivar frame definitions-edit)]
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
        (let ([msg 
                (parameterize ([print-struct #t])
                  (string-append "Syntax error: " 
                    (apply format fmt-spec args)))])
          (if #t                        ;(zodiac:zodiac? where)
            (mrspidey:error msg where)                    
            (mrspidey:error msg)))))
    (define static-error
      (lambda (where fmt-spec . args)
        (let ([msg
                (parameterize ([print-struct #t])
                  (string-append "Syntax error: " 
                    (apply format fmt-spec args)))])
          (if #t                        ;(zodiac:zodiac? where)
            (mrspidey:error msg where)                    
            (mrspidey:error msg)))))
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
		       MRED ZODIAC
		       (MZLIB file))]
	 [SBA : mrspidey:sba^
	      (mrspidey:sba@ 
	       INTERACTION 
	       ((MZLIB function) : mrspidey:mzlib:function^)
	       (MZLIB pretty-print)
	       (MZLIB file)
	       (MZLIB string)
	       ZODIAC)]
	 [GUI : mrspidey-gui^
	      (mrspidey-gui@ 
	       MRED 
	       FRAMEWORK
	       BROWSER
	       ((MZLIB function) : mrspidey:mzlib:function^)
	       (MZLIB pretty-print)
	       (MZLIB file)
	       (MZLIB string)
	       SBA INTERACTION ZODIAC)]
	 [TOOL : () 
	       (mrspidey-tool@ 
		MRED FRAMEWORK GUI)])
   (export)))

;; (printf "tool@ defined~n")

;; ----------------------------------------------------------------------








