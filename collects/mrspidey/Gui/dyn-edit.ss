; dyn-edit
;
; Defines spidey:dynamic+margin-edit%, a subclass of spidey:static-edit%
; with an insert-line method that also inserts a margin
; and it handles adding and deleting snips
; while still allowing static positions in the buffer
;
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

(define spidey:dynamic+margin-edit%
  (class spidey:static-edit% (arg-margin . init-args)
    (inherit 
     edit-sequence
     editor-position-line
     editor-change-style
     editor-set-clickback
     editor-flash-on
     editor-set-position
     editor-scroll-to-position
     editor-get-text
     editor-insert
     editor-delete
     editor-set-tabs)

    (public
      ;; ----------
      margin 
      margin-length
      ;[margin "  "]
      ;[margin-length 2]

      ;; keeps a list of all the snips in their source-locations
      [sniplist ()]

      [insert-line
        (lambda (s)
          (editor-insert margin)
          (editor-insert s)
          (editor-insert (string #\newline)))]

      ;; ----------

      [account-for-margin
        (lambda (pos) 
          (recur loop ([try pos])
            ;;(pretty-debug-gui `(accout-for-margin ,pos ,try))
            (let ([better
                    (+ pos 
                      (* margin-length (add1 (editor-position-line (+ try 0)))))])
              (if (= better try)
                try
                (loop better)))))]
      [real-start-position
        (lambda (pos)
          (account-for-margin
            (let loop ([l sniplist])
              (cond
                [(null? l) pos]
                [(> pos (car l)) (add1 (loop (cdr l)))]
                [else (loop (cdr l))]))))]
      [real-end-position
        (lambda (pos)
          (account-for-margin
            (let loop ([l sniplist])
              (cond
                [(null? l) pos]
                [(>= pos (car l)) (add1 (loop (cdr l)))]
                [else (loop (cdr l))]))))]
      [old-real-end-position
        (lambda (pos)
          (let ([pos pos])
            (let loop ([l sniplist][pos pos])
              (cond
                [(null? l) (account-for-margin pos)]
                [(>= pos (car l))  
                  (loop (cdr l) (add1 pos))]
                [else (loop (cdr l) pos)]))))]
      [frame-pos->source-pos
        (lambda (pos)
          (assert (number? pos) 'frame-pos->source-pos)
          (let ([pos (- pos (* margin-length (add1 (editor-position-line pos))))])
            (let loop ([l sniplist])
              (cond
                [(null? l) pos]
                [(> pos (car l))
                  (sub1 (loop (cdr l)))]
                [else (loop (cdr l))]))))]

      ;; ----------

      [relocate-change-style
        (lambda (delta src-start src-end)
          (let ([s (real-start-position src-start)]
                 [e (real-end-position src-end)])
            (pretty-debug-gui `(change-style ,src-start ,src-end ,s ,e))
            (editor-change-style delta s e)))]
			
      [relocate-set-clickback
        (lambda (src-start src-end . args)
          (apply editor-set-clickback
            (real-start-position src-start)
            (real-end-position src-end)
            args))]
      [relocate-flash-on
        (lambda (src-start src-end a b c)
          (editor-flash-on (real-start-position src-start)
            (real-end-position src-end) a b c))]
      [relocate-scroll-to-position
        (lambda (pos)
          (let ([real-pos (real-start-position pos)])
            '(pretty-print `(scroll-to-position ,pos ,real-pos))
            (editor-set-position real-pos)
            (editor-scroll-to-position real-pos)))]
      ;;
      ;; watch this function......
      [relocate-set-position
        (opt-lambda
          (pos [end 'same][eol #f][scroll #t])
          (let ([end (if (eq? end 'same) 'same (real-end-position end))])
            (editor-set-position (real-start-position pos) end
              eol scroll)))]
;      [match-paren-forward
;        (lambda (source-start)
;          (frame-pos->source-pos (scheme-paren:forward-match 
;                                   this (real-start-position source-start)
;                                   (last-position))))]
	       
      [relocate-get-text
        (opt-lambda ([start -1][end -1][flat #f])
          (editor-get-text (real-start-position start)
            (real-end-position end) flat))]
      [select-snip
        (lambda (pos snip)
          (edit-sequence
            (lambda ()
              (send snip own-caret #f)
              (editor-set-position 
                (real-end-position pos) (real-start-position pos)))))]
      [relocate-insert-snip
        (lambda (snip pos)
          ;;(lock #f)
          (when (member pos sniplist)
            (error "Cannot put two snips in the same position~n"))
          (let ([real-pos (real-start-position pos)])
            (set! sniplist (cons pos sniplist))
            (editor-insert snip real-pos))
          ;;(lock #t)
          )]
      [relocate-delete-snip
        (lambda (pos)
          (unless (member pos sniplist)
            (error  "Cannot remove snip from ~s" pos))
          (set! sniplist (remv pos sniplist))
          (let ([pos (real-start-position pos)])
            (editor-delete pos (add1 pos))))]
      )

    (sequence
      (apply super-init init-args)
      (set! margin arg-margin)
      (set! margin-length (string-length margin))

      ;; set-tabs doesn't work right past list of specified tabs
      ;; so specify all tabs to column 200
      (editor-set-tabs 
        (recur loop ([p margin-length])
          (if (< p 200) 
            (cons p (loop (+ p 8)))
            '()))
        8
        #f)
      )))

