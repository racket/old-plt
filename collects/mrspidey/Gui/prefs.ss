;; prefs.ss - loads preferences
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

(define p #f) ; temp!!!!

(define parameter-radio-boxes
  (lambda (name param sym p direction)
    (preferences:set-default sym (param) 
      (lambda (x)
        (with-handlers ((exn? (lambda (exn) #f)))
          (param x)
          #t)))
    (param (preferences:get sym))
    (let* ([o
             (make-object
               radio-box% 
	       name 
               (map cadr (param '?))
	       p 
               (lambda (bx event)
                 ;;(printf "~s~n" (param '?))
                 (match
                   (list-ref (param '?)
                     (send event get-command-int))
                   [(tag . _) 
                     (param tag)
                     (preferences:set sym tag)]))
               direction)]
            [pairs
              (map
                (match-lambda [(tag name . _) (cons name tag)])
                (param '?))]
            [default-ndx
              (recur loop ([n 0][pairs pairs])
                (cond 
                  [(null? pairs)
                    (error 'make-parameter-menu 
                      "Can't find para in pairs ~s" (param))]
                  [(equal? (param) (cdar pairs)) n]
                  [else (loop (add1 n) (cdr pairs))]))])
      (send o stretchable-width #t)
      (send o set-selection default-ndx)
      o
      )))

(define parameter-check-box
  (lambda (name param sym p)
    (preferences:set-default sym (param) 
      (lambda (x)
        (with-handlers ((exn? (lambda (exn) #f)))
          (param x)
          #t)))
    (param (preferences:get sym))
    (let* ( [hp (make-object horizontal-panel% p)]
            [o
              (make-object check-box% 
			   name 
			   hp 
			   (lambda (bx event)
			     ;;(printf "~s~n" (param '?))
			     (match
			      (list-ref (param '?)
					(send event get-command-int))
			      [(tag . _) 
			       (param tag)
			       (preferences:set sym tag)])))]
            [_ (make-object horizontal-panel% hp)])
      (send o set-value (param))
      o
      )))

;; ======================================================================
;; MrSpidey Type Display

(define callbacks-sdl-alg-changed '())

(define (add-callback-sdl-alg-changed! fn)
  (set! callbacks-sdl-alg-changed (cons fn callbacks-sdl-alg-changed)))

(define (remq-callback-sdl-alg-changed! fn)
  (set! callbacks-sdl-alg-changed (remq fn callbacks-sdl-alg-changed)))

(define (sdl-alg-changed)
  (for-each 
    (lambda (f) (f)) 
    callbacks-sdl-alg-changed))

(define (param-ctrls-sdl-alg param)
  (lambda args
    (sdl-alg-changed)
    (apply param args)))

;; ======================================================================

(preferences:set-default 'st:const-merge-size (st:const-merge-size) 
  (lambda (x)
    (with-handlers ((exn? (lambda (exn) #f)))
      (st:const-merge-size x)
      #t)))
(st:const-merge-size (preferences:get 'st:const-merge-size))

(define mrspidey-mk-analysis-pref-panel
  (lambda (panel)
    (let*
      ( [p (make-object vertical-panel% panel)]
        
        [vp (make-object vertical-panel% p)]
        [_ (parameter-check-box
             "Accurate constant types"
             st:constants 'st:constants
             vp)]
        [g (make-object slider% 
             "Constant merge size"
             1 100 
	     vp 
             (lambda (slider event)
               (st:const-merge-size (send event get-command-int))
               (preferences:set 'merge-size 
				(send event get-command-int)))
             (st:const-merge-size))]
        [_ (send g enable #t)]
        [_ (parameter-check-box
             "If splitting"
             st:if-split 'st:if-split
             vp)]
        [_ (parameter-check-box
             "Flow sensitivity"
             st:flow-sensitive 'st:flow-sensitive
             vp)]
        [_ (parameter-check-box
             "Accurate analysis of numeric operations"
             st:numops 'st:numops
             vp)]
        [_2 (parameter-radio-boxes
              "Polymorphism:"
              st:polymorphism
              'st:polymorphism
              p '(horizontal))]

        [vp (make-object vertical-panel% p)]
        [vphp (make-object horizontal-panel% vp)]
        [_0 (make-object message% 
			 "Polymorphism simplification algorithms:" 
			 vphp)]
        [vphphp (make-object horizontal-panel% vphp)]
        [_1 (parameter-radio-boxes
              "        "
              st:constraint-simplification-poly
              'st:constraint-simplification-poly
              vp '(vertical))]

        [_ (parameter-radio-boxes
             "Save .za files in:"
             st:save-za-in
             'st:save-za-in
             p
             '(horizontal))]
        )
      p)))

(preferences:add-panel
  "MrSpidey Analysis"
  mrspidey-mk-analysis-pref-panel)

(mrspidey-mk-analysis-pref-panel
  (make-object horizontal-panel%
    (make-object frame% "dummy")))

;; ======================================================================

;(mred:set-preference-default 'st:sdl-size-k (st:sdl-size-k))
;(st:sdl-size-k (mred:get-preference 'st:sdl-size-k))

(define (indented-vertical-radio-box p name param sym)
  (let*
    ( [vp (make-object vertical-panel% p)]
      [vphp1 (make-object horizontal-panel% vp)]
      [_0 (make-object message% name vphp1)]
      [vphp2 (make-object horizontal-panel% vp)]
      [spc (make-object horizontal-panel% vphp2)]
      [_ (send spc min-width 20)]
      [_ (send spc stretchable-width #f)]
      [radio-box
        (parameter-radio-boxes
          #f
          param sym
          vphp2 '(vertical))]
      [_ (send radio-box stretchable-width #t)]
      [_ (make-object horizontal-panel% vphp2)])
    (void)))

(define mrspidey-mk-type-display-prefs-panel
  (lambda (panel)
    (let*
      ( [p (make-object vertical-panel% panel )])

      (let* 
        (
          [sdl-fo-container-panel
            (make-object horizontal-panel% p)]
          [sdl-fo-sub-panel
            (make-object horizontal-panel% p)]
          [spc (make-object horizontal-panel% sdl-fo-sub-panel)]
          [_ (send spc min-width 20)]
          [_ (send spc stretchable-width #f)]
          [sdl-fo-sub-sub-panel
            (make-object vertical-panel% sdl-fo-sub-panel)]
          [see-ivars-panel
            (parameter-check-box
              "Show instance variables"
              (param-ctrls-sdl-alg st:sdl-fo-ivars)
              'st:sdl-fo-ivars
              sdl-fo-sub-sub-panel)]
          [see-struct-fields-panel
            (parameter-check-box
              "Show structure fields"
              (param-ctrls-sdl-alg st:sdl-fo-struct-fields)
              'st:sdl-fo-struct-fields
              sdl-fo-sub-sub-panel)] 
          [_ (parameter-radio-boxes
                "Show types as:"
               (match-lambda*
                 [('?) (st:sdl-fo '?)]
                 [() (st:sdl-fo)]
                 [(x)
                   (sdl-alg-changed)
                   (let ([enable-sub-controls (eq? x 'basic-types)])
                     (for-each
                       (lambda (control)
                         (send control enable enable-sub-controls))
                       (list 
                         see-ivars-panel
                         see-struct-fields-panel)))
                   (st:sdl-fo x)])
               'st:sdl-fo
                sdl-fo-container-panel
               '(horizontal))])
        (void))
        
      (indented-vertical-radio-box p 
        "Constraint simplification algorithms:" 
        (param-ctrls-sdl-alg st:sdl-constraint-simplification)
        'st:sdl-constraint-simplification)

      (parameter-radio-boxes
        "Type naming:"
        (param-ctrls-sdl-alg st:naming-strategy)
        'st:naming-strategy
        p '(horizontal))
      (parameter-radio-boxes
        "Primitive types:"
        (param-ctrls-sdl-alg st:primitive-types)
        'st:primitive-types
        p '(horizontal))

      (let* 
        (
          [st:expand-output-type-container-panel
            (make-object horizontal-panel% p)]
          [st:expand-output-type-sub-panel
            (make-object horizontal-panel% p)]
          [spc (make-object horizontal-panel% 
                 st:expand-output-type-sub-panel)]
          [_ (send spc min-width 20)]
          [_ (send spc stretchable-width #f)]
          [sdl-tidy-object
            (parameter-check-box
              "Uses equivalences that make types tidy"
              (param-ctrls-sdl-alg st:sdl-tidy)
              'st:sdl-tidy
              st:expand-output-type-sub-panel)] 
          [_ (parameter-check-box
               "Use equivalences to simplify types"
               (match-lambda*
                 [('?) (st:expand-output-type '?)]
                 [() (st:expand-output-type)]
                 [(x)
                   (sdl-alg-changed)
                   (send sdl-tidy-object enable x)
                   (st:expand-output-type x)])
               'st:expand-output-type
               st:expand-output-type-container-panel)])
        (void))

      p)))

(preferences:add-panel
  "MrSpidey Type Display"
  mrspidey-mk-type-display-prefs-panel)

(mrspidey-mk-type-display-prefs-panel
  (make-object horizontal-panel%
    (make-object frame% "dummy")))

;; ======================================================================

'(define (make-parameter-menu parameter)
   (let* ([pairs
            (map
              (match-lambda [(tag name . _) (cons name tag)])
              (parameter '?))]
           [default-ndx
             (recur loop ([n 0][pairs pairs])
               (cond 
                 [(null? pairs)
                   (error 'make-parameter-menu "Can't find para in pairs")]
                 [(equal? (parameter) (cdar pairs)) n]
                 [else (loop (add1 n) (cdr pairs))]))])
     (let ([menu (make-object menu%)])
       (send menu append-check-set pairs parameter default-ndx)
       ;;(parameter (cdar pairs))
       menu)))



