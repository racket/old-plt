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

;; The code to be loaded from DrScheme

;;(printf "loading drspidey.ss (cd ~s)~n" (current-directory))

(reference-relative-library "pltrc-co.ss")
(reference-relative-library "macros.ss")

#|
(begin-elaboration-time
  (unless (getenv "MREDCOMPILE") 
    (match:set-error-control 'match)))
|#

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
            (format "~a~n" message)
            "MrSpidey Error")
          (raise 'mrspidey-raise)]))
    ))

;; ----------------------------------------------------------------------

(define mrspidey-tool@
  (unit/sig ()
    (import
      [mred : mred^]
      mrspidey-gui^)
    (mred:add-version-spec 'sd 1)
    (lambda (frame)
      (let* ( [edit (ivar frame definitions-edit)]
              [name (send edit get-filename)])
        (if (string? name)
          (when
            (or (not (send edit modified?))
              (let ([action (mred:unsaved-warning name "Analyze" #t)])
                (case action
                  [(save) (send edit save-file)]
                  [(continue) #t]
                  [else #f])))
            (with-handlers
              ([ (lambda (x) (eq? x 'mrspidey-raise))
                 (lambda (x) (void))])
              (send spidey run-mrspidey (send edit get-filename))))
          (mred:message-box
            "MrSpidey can only process programs that are saved to a file"
            "MrSpidey Error"))))))

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
   (import [WX : wx^]
	   [MRED : mred^]
	   [MZLIB : mzlib:core^]
	   [PCONVERT : mzlib:print-convert^]
	   [DRSCHEME : drscheme:export^]
	   [ZODIAC-UNUSED : zodiac:system^])
   (link [PARAMETERS : plt:parameters^
		     ((unit/sig plt:parameters^
			(import)
			(define check-syntax-level 'advanced)))]
	 [INTERFACE : zodiac:interface^
		    (mrspidey:zodiac:interface@ INTERACTION)]
	 [ZODIAC : zodiac:system^
		 (zodiac:system@ 
		  INTERFACE PARAMETERS 
		  (MZLIB pretty-print@)
		  (MZLIB file@))]
	 [INTERACTION : mrspidey:interaction^
		      (mrspidey:interaction@ 
		       MRED ZODIAC
		       (MZLIB file@))]
	 [SBA : mrspidey:sba^
	      (mrspidey:sba@ 
	       INTERACTION 
	       ((MZLIB function@) : mrspidey:mzlib:function^)
	       (MZLIB pretty-print@)
	       (MZLIB file@)
	       (MZLIB string@)
	       ZODIAC)]
	 [GUI : mrspidey-gui^
	      (mrspidey-gui@ 
	       WX MRED 
	       ((MZLIB function@) : mrspidey:mzlib:function^)
	       (MZLIB pretty-print@)
	       (MZLIB file@)
	       (MZLIB string@)
	       SBA INTERACTION ZODIAC)]
	 [TOOL : () 
	       (mrspidey-tool@ 
		MRED GUI)])
   (export)))

;;(printf "tool@ defined~n")

;; ----------------------------------------------------------------------
