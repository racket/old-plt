;; macros.ss
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

(reference-library "defstruc.ss")

(begin-elaboration-time
  (if (not (defined? 'MRSPIDEY-DEBUGGING))
    (begin
      ; (printf "Debugging off~n")
      (define-macro pretty-debug (lambda args           '(#%void)))
      (define-macro pretty-debug-traverse (lambda args  '(#%void)))
      (define-macro pretty-debug-object (lambda args    '(#%void)))
      (define-macro pretty-debug-front (lambda args     '(#%void)))
      (define-macro pretty-debug-min (lambda args       '(#%void)))
      (define-macro pretty-debug-min2 (lambda args      '(#%void)))
      (define-macro pretty-debug-check (lambda args     '(#%void)))
      (define-macro pretty-debug-atenv (lambda args     '(#%void)))
      (define-macro pretty-debug-atype (lambda args     '(#%void)))
      (define-macro pretty-debug-few (lambda args       '(#%void)))
      (define-macro pretty-debug-gram (lambda args      '(#%void)))
      (define-macro pretty-debug-sdl (lambda args       '(#%void)))
      (define-macro pretty-debug-sdl2 (lambda args      '(#%void)))
      (define-macro pretty-debug-dfa-min (lambda args   '(#%void)))
      (define-macro pretty-debug-min-table (lambda args '(#%void)))
      (define-macro pretty-debug-traverse-small (lambda args '(#%void)))
      (define-macro pretty-debug-unit (lambda args      '(#%void)))
      (define-macro pretty-debug-gui (lambda args       '(#%void)))
      (define-macro assert (lambda args                 '(#%void)))
    
      )
  
    (begin
      (define-macro pretty-debug (lambda args
        `(when debugging (pretty-print-debug ,@args))))
      (define-macro pretty-debug-traverse (lambda args
        `(when debugging-traverse (pretty-print-debug ,@args))))
      (define-macro pretty-debug-object (lambda args
        `(when debugging-object (pretty-print-debug ,@args))))
      (define-macro pretty-debug-front (lambda args
        `(when debugging-front (pretty-print-debug ,@args))))
      (define-macro pretty-debug-min (lambda args
        `(when debugging-min (pretty-print-debug ,@args))))
      (define-macro pretty-debug-min2 (lambda args
        `(when debugging-min2 (pretty-print-debug ,@args))))
      (define-macro pretty-debug-check (lambda args
        `(when debugging-check (pretty-print-debug ,@args))))
      (define-macro pretty-debug-atenv (lambda args
        `(when debugging-atenv (pretty-print-debug ,@args))))
      (define-macro pretty-debug-atype (lambda args
        `(when debugging-atype (pretty-print-debug ,@args))))
      (define-macro pretty-debug-few (lambda args
        `(when debugging-few (pretty-print-debug ,@args))))
      (define-macro pretty-debug-gram (lambda args 
        `(when debugging-gram (pretty-print-debug ,@args))))
      (define-macro pretty-debug-sdl (lambda args
        `(when debugging-sdl (pretty-print-debug ,@args))))
      (define-macro pretty-debug-sdl2 (lambda args
        `(when (or debugging-sdl2 debugging-sdl) (pretty-print-debug ,@args))))
      (define-macro pretty-debug-dfa-min (lambda args
        `(when debugging-dfa-min (pretty-print-debug ,@args))))
      (define-macro pretty-debug-min-table (lambda args
        `(when debugging-min-table (pretty-print ,@args))))
      (define-macro pretty-debug-gui (lambda args
        `(when debugging-gui (pretty-print ,@args))))

      (define-macro pretty-debug-traverse-small (lambda args
        `(when debugging-traverse 
           (dynamic-let ([pretty-print-depth 4]) (pretty-print-debug ,@args)))))

      (define-macro pretty-debug-unit (lambda args
        (match args
          [(arg) `(when debugging-unit (pretty-print-debug ,arg))]
          [(arg depth)
            `(when debugging-unit
               (dynamic-let ([pretty-print-depth ,depth]) (pretty-print-debug ,arg)))])))
      )))

