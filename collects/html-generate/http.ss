;;; -*- Scheme -*-
;;;
;;; Wrapper for ``parsed header'' scripts.
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

(require-library "mime.ss" "html-generate")

;;; Error reporter.
;;;
;;; This is where scheme is really good.
;;; We catch all errors and turn them into
;;; a printed representation.  Then we put out a minimal HTML
;;; file that describes the error.  This way we can get
;;; meaningful errors back even from this trampoline code.

(define (error-reporter title header . extra-text)
  (lambda (cgi-stdout)
    (lambda (error)
      (call-with-output-file cgi-stdout
	(lambda (cgi-output-port)
	  (for-each 
	   (lambda (line)
	     (fprintf cgi-output-port "~a~%" line))
	   ;; Canned output.  Not fancy, but very little has
	   ;; to work to get this error off to the client.
	   `(
	     "HTTP/1.0 200 OK"
	     "Content-type: text/html"
	     ""
	     "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
	     "<HTML>"
	     "<HEAD>" "<TITLE>" title "</TITLE>" "</HEAD>"
	     "<BODY>" 
	     "<H2>" ,header "</H2>"
	     ;;
	     ;; will break if the error has ``</plaintext>'' in it,
	     ;; but is safer otherwise.
	     "<PLAINTEXT>" ,(format "~a" error) "</PLAINTEXT>"
	     ,@(append-map 
		(lambda (text) (list "<P>" text "</P>")) 
		extra-text)
	     "</BODY>"
	     "</HTML>"
	     )))
	'truncate))))

(define (cgi-script-error cgi-stdout)
  ((error-reporter "Scheme CGI Script Error"
		   "Scheme CGI Script Error:  An unexpected error occurred while running the script."
		  )
   cgi-stdout))

;;; Client errors
;;;
;;; Client errors are errors that originate in the HTTP client (the
;;; browser).  This is for things like bogus form bindings.

(define-struct (exn:client struct:exn) ())

(define (client-error-handler client-error)
  (exn-message client-error))

(define (client-error format-string . args)
  (raise (make-exn:client (string-append "Client error: "
					 (apply format format-string args))
			  #f)))

;;; SCRIPT-ENTRY <thunk>
;;;
;;; Calls <thunk> with no arguments.  Thunk is expected to either
;;; return a value or raise a client error.  NOTE:  THUNK SCRIPTS ARE
;;; EXPECTED TO NOT PRINT ANYTHING.

(define (script-entry thunk)
  (print-struct #t)			;for debugging
    (mime-emit
     (with-handlers ((exn:client? client-error-handler))
       (thunk)
       )
     (current-output-port))
   )

