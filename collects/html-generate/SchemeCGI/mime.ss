;;; -*- Scheme -*-
;;;
;;; Emit objects with correct MIME codes.
;;;
;;; Copyright (C) 1996 Emergent Technologies Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;;; MA 02139, USA.
;;;

;;;
;;; Revision History
;;;
;;; September 1, 1996 - Version 0.1
;;;  Created and released initial version.
;;;

(require-library "pretty.ss")

;;; The problem with using DISPLAY to incrementally write HTML is that
;;; you are in trouble if you have to abort in the middle of page
;;; generation.  In other words, page generation should be atomic.
;;;
;;; To help solve this problem the script is expected to return a value
;;; rather than write text to a port.  Mime-emit does a type dispatch
;;; on the object returned and formats it for output.

(define *mime-table* '())		;alist of type predicates and
					;emission procedures
(define *mime-emit-default-handler* #f)

(define (add-mime-handler! predicate handler)
  (set! *mime-table* (cons (cons predicate handler) *mime-table*)))

(define (mime-emit object port)
  (define (loop table)
    (cond ((null? table) *mime-emit-default-handler*)
	  (((caar table) object) (cdar table))
	  (else (loop (cdr table)))))
  ((loop *mime-table*) object port))

(define (mime-emit-header header port)
  (display "Content-type: " port)
  (display header port)
  (write-char #\newline port)
  (write-char #\newline port)
  )

;;; The regular default is to pretty print the object and return it as text.
;;;
;;; We might want to change the default to format an HTML page with
;;; the pretty printed text in it, so there is a hook here.

(define (mime-emit-default object port)
  (mime-emit-header "text/plain" port)
  (pretty-print object port)
  )

(set! *mime-emit-default-handler* mime-emit-default)