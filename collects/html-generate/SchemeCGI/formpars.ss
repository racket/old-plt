;;; -*- Scheme -*-
;;;
;;; Convert http form into an alist.
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

;;; Revision History
;;; September 5, 1996 - Version 0.2
;;;  Rename PARSE-CURRENT-FORM
;;;
;;; September 1, 1996 - Version 0.1
;;;  Created and released initial version.

(require-library "string.ss")

;;;
;;; PARSE-CURRENT-FORM-INPUT
;;;
;;; This function takes no arguments and returns an alist.
;;; The car of each element in the alist is a symbol derived from
;;; the NAME parameter of the HTML INPUT tag.  The cdr of each alist
;;; element is a STRING that contains the actual value submitted.
;;; All URI escaping has been removed, so the alist should represent
;;; the user's input fairly directly.

(define (parse-current-form-input)
  (let ((method (getenv-workaround "REQUEST_METHOD" "Request Method")))
    (cond ((string-ci=? method "GET") 
	   (parse-form-string (getenv-workaround "QUERY_STRING" "Query String")))
	  ((string-ci=? method "POST")
	   (let ((nchars (string->number 
			  (getenv-workaround "CONTENT_LENGTH" "Content Length"))))
	     (parse-form-string (read-n-chars nchars))))
	  (else (error "Unexpected REQUEST_METHOD:  " method)))))

;;; Should be a primitive?
(define (read-n-chars size)
  (let ((result (make-string size)))
    (define (loop index)
      (if (< index size)
	  (begin (string-set! result index (read-char))
		 (loop (+ index 1)))))
    (loop 0)
    result))

;;; Workaround
;;; GETENV fails under certain circumstances
;;; so we keep the output of the CGI-ENVIRONMENT parsing around
;;; and look there.

(define (getenv-workaround unix-name cgi-win-name)
  (or (getenv unix-name)
      (cgi-getenv cgi-win-name)))

(define (parse-form-string query-string)
  (map (lambda (element)
	 (parse-association element
           (lambda (key value)
	     (cons (read-string (unescape-uri key)) ;convert to symbol
		   (unescape-uri value)))))
       (split-query query-string)))

(define (split-query query-string)
  (let ((limit (string-length query-string)))
    
    (define (loop previous index associations)
      (cond ((= index limit) (reverse
			      (cons 
			       (substring query-string previous index)
			       associations)))
	    ((char=? (string-ref query-string index) #\&)
	     (loop (+ index 1)
		   (+ index 2)
		   (cons (substring query-string previous index)
			 associations)))
	    (else
	     (loop previous (+ index 1) associations))))

    (loop 0 0 '())))

(define (parse-association string receiver)
  (let ((limit (string-length string)))
    
    (define (loop index)
      (cond ((= index limit) (receiver string ""))
	    ((char=? (string-ref string index) #\=)
	     (receiver (substring string 0 index)
		   (substring string (+ index 1) limit)))
	    (else (loop (+ index 1)))))

    (loop 0)))

(define (unescape-uri original-string)
  (let* ((limit (string-length original-string))
	 (result (make-string limit)))

    (define (loop source-index dest-index)
      (cond ((= source-index limit) (substring result 0 dest-index))
	    ((char=? (string-ref original-string source-index) #\%)
	     (string-set! result dest-index
			  (integer->char
			   (string->number (substring original-string 
						      (+ source-index 1)
						      (+ source-index 3))
					   16))) ;hex
	     (loop (+ source-index 3) (+ dest-index 1)))
	    ((char=? (string-ref original-string source-index) #\+)
	     (string-set! result dest-index #\Space)
	     (loop (+ source-index 1) (+ dest-index 1)))
	    (else
	     (string-set! result dest-index
			  (string-ref original-string source-index))
	     (loop (+ source-index 1) (+ dest-index 1)))
	    ))
    (loop 0 0)
    ))
