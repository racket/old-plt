; deltas.ss
; Loads configuration from .Xresources
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

;; Given a resource name, this returns a style delta corresponding to the 
;; attributes of the file or the default if there are none.

(define (get-resource-maybe s2 default)
  (let ([val-box (box default)])
    (get-resource 
     (if (is-color-display?) "mrspidey" "mrspidey-bw")
     s2 
     val-box)
    (unbox val-box)))

(define delta-eval-namespace (make-namespace 'mred))

(define delta-add-string! 
  (lambda (delta string)
  (let ([p (open-input-string string)])
    (recur loop ()
      (let ([e (read p)])
        (unless (eof-object? e)
          (apply (ivar/proc delta (car e)) (eval `(list ,@(cdr e)) delta-eval-namespace))
          (loop)))))))

(define set-resource-delta
  (lambda (name default-string default-string-bw delta)
    (delta-add-string! delta 
                       (if (is-color-display?) 
                           default-string 
                           default-string-bw))
    (delta-add-string! delta (get-resource-maybe name ""))))

(define make-resource-delta
  (lambda (name default-string default-string-bw)
    (let ([delta (make-object style-delta% 
                              'change-alignment 'top)])
      '(set-resource-delta "base-delta"
                          "(set-alignment-on 'top)"
                          "(set-alignment-on 'top)"
                          delta)
      (set-resource-delta name default-string default-string-bw delta)
      delta)))

;; ----------------------------------------------------------------------
;; These deltas are defined when the application starts, either from defaults
;; or from resources, their names and their strings agree.

(define base-delta
  (make-resource-delta "base-delta" 
                       "(set-alignment-on 'top) "
                          "(set-alignment-on 'top)"))

(define normal-delta
  (make-resource-delta "normal-delta"
                       "" ""))

(define type-link-delta 
  (make-resource-delta "type-link-delta"
                       "(set-delta 'change-bold)"
                       "(set-delta 'change-bold)"))
(define type-delta
  (make-resource-delta "type-link-delta"
                       ""
                       ""))

(define check-delta 
  (make-resource-delta "check-delta"
                       "(set-delta-foreground \"RED\")"
                       "(set-delta 'change-underline #t)"))
(define uncheck-delta
  (make-resource-delta "uncheck-delta"
                       "(set-delta-foreground \"FOREST GREEN\")"
                       ""))

(define check-link-delta
  (make-resource-delta "check-link-delta" 
                       "(set-delta-foreground \"BLUE\") \
                        (set-delta 'change-underline #t)"
                       "(set-delta 'change-underline #t)"))

;; ----------------------------------------------------------------------
