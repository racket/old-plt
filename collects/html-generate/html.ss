;;; -*- Scheme -*-
;;;
;;; HTML generation
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

;;; Revision History
;;; September 14, 1996 - Version 0.3
;;;  Cleanup.
;;;
;;; September 13, 1996 - Version 0.2
;;;  Rewrote.
;;;
;;; September 12, 1996 - Version 0.1
;;;  Created and released initial version.

(require-library "colors.ss" "html-generate")
(require-library "mime.ss" "html-generate") ; added JBC 4/99
(require-library "string.ss")

; safe-html : takes a symbol or string or number and replaces all occurrences
; of the dangerous characters with their HTML equivalents
; ((union string symbol number) -> (union string number))

(define safe-html
  (let ((translation-table '((#\< "&lt;")
			     (#\> "&gt;")
			     (#\& "&amp;")
			     (#\" "&quot;"))))

    (define (do-it results rest)
      (let ((limit (string-length rest)))
	(define (loop index)
	  (if (= index limit)
	      (apply string-append (reverse (cons rest results)))
	      (let* ((this-char (string-ref rest index))
		     (entry (assoc this-char translation-table)))
		(if entry
		    (do-it (list* (cadr entry)
				  (substring rest 0 index)
				  results) 
			   (substring rest (+ index 1) limit))
		    (loop (+ index 1))))))
	(loop 0)))

    (lambda (input)
      (cond ((string? input) 
	     (do-it (list "") input))
	    ((symbol? input) 
	     (string->symbol (do-it (list "") (symbol->string input))))
	    ((number? input) input)
	    (else "Cannot canonicalize this option" input)))))

; canonicalize-html : take an html object (?) and leave it alone if it is a
; symbol or html, make it safe if it is a string, and otherwise run it through
; format and then recur. I'm a wee bit confused.
; (union string symbol html other) -> (union string symbol html number)

(define (canonicalize-html object)
  (cond 
	((string? object)        (safe-html object))
	((symbol? object)        object)
	((html?   object)        object)
	(else                    (canonicalize-html (format "~s" object)))
	))

(define (symbol-append . symbols)
  (string->symbol
   (apply string-append
	  (map symbol->string symbols))))

(define-struct html-type (name tag))
(define-struct html (type))

(define html-type:comment (make-html-type 'comment "<!-- "))
(define html-type:doctype (make-html-type 'DOCTYPE "!DOCTYPE"))
(define-struct (html:comment   struct:html)          (text))
(define-struct (html:optionals struct:html)          (options))
(define-struct (html:doctype   struct:html:optionals)   ())

;;; HTML objects that are not containers.

(define-struct (html:element   struct:html:optionals)   ())

;; ad hack
(define (string-upcase string) 
  (let ((new  (string-copy string)))
    (string-uppercase! new)
    new))

;;; DEFINE-HTML-ELEMENT <namespec> <options>
;;; Where <namespec> is either a symbol, or a list of a symbol and a
;;; string.
;;; Where <options> is a list of <optionspec>, and <optionspec> is a
;;; list of a symbol and a predicate.  Values for options must pass
;;; the predicate, but I didn't bother to make strict predicates.

(define-macro define-html-element
  (lambda (name-spec options)
    (let* ((name (if (list? name-spec) 
		     (car name-spec)
		     name-spec))
	   (tag  (if (list? name-spec)
		     (cadr name-spec)
		     (string-upcase (symbol->string name-spec))))
	   (type      (symbol-append 'HTML-TYPE: name))
	   (maker     (symbol-append 'HTML- name))
	   (predicate (symbol-append 'HTML- name '?)))

      (define (make-option-checker option)
	(if (symbol? option)
	    `((EQ? OPTION ',option) OPTION)
	    (let ((option-name (car option))
		  (option-type (cadr option)))
	    `((AND (PAIR? OPTION)
		   (EQ? (CAR OPTION) ',option-name)
		   (,option-type (CADR OPTION)))
	      (LIST (CAR OPTION) (SAFE-HTML (CADR OPTION)))))))

      `(BEGIN
	 (DEFINE ,type (MAKE-HTML-TYPE ',name ,tag))

	 (DEFINE [,maker . OPTIONS]
	     (DEFINE (CANONICALIZE-OPTION OPTION)
	       (COND ,@(map make-option-checker options)
	  	     (ELSE (ERROR "Illegal Option:" ',maker option))))
	     (MAKE-HTML:ELEMENT ,type
			        (MAP CANONICALIZE-OPTION OPTIONS)))
	 (DEFINE (,predicate OBJECT)
	   (AND (HTML? OBJECT)
		(EQ? ',name (HTML-TYPE-NAME (HTML-TYPE OBJECT)))))
	 ))))

(define (member-of? list)
  (lambda (thing)
    (memq thing list)))

(define-html-element AREA
  ((COORDS coords?)
   (HREF   url?)
   NOHREF
   (SHAPE (member-of? '(RECT RECTANGLE CIRC CIRCLE POLY POLYGON)))
   (TARGET window?)))

(define-html-element BASE
  ((HREF   url?)
   (TARGET window?)))

(define-html-element BASEFONT
  ((COLOR color?)
   (NAME  name?)
   (SIZE  integer?)))

(define-html-element BGSOUND
  ((SRC  url?)
   (LOOP (lambda (x)
	   (or (eq? x 'INFINITE)
	       (integer? x))))))

(define-html-element (BREAK "BR")
  ((CLEAR (member-of? '(LEFT RIGHT ALL)))
   ))

(define-html-element COL
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))
   (SPAN  integer?)))

(define-html-element COLGROUP
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))
   (SPAN integer?)))

(define-html-element FRAME
  ((ALIGN (enum LEFT CENTER RIGHT TOP BOTTOM))
   (FRAMEBORDER (member-of? (list 1 0)))
   (MARGINHEIGHT integer?)
   (MARGINWIDTH  integer?)
   (NAME         string?)
   NORESIZE
   (SCROLLING   (member-of? '(YES NO)))
   (SRC         url?)))

(define-html-element HR
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))
   (COLOR color?)
   NOSHADE
   (SIZE  integer?)
   (WIDTH integer-or-percent?)))

(define-html-element IMG
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))
   (ALT   string?)
   (BORDER integer?)
   CONTROLS
   (DYNSRC url?)
   (HEIGHT integer?)
   (HSPACE integer?)
   ISMAP
   (LOOP   integer?)
   (SRC    url?)
   (START  (listof? FILEOPEN MOUSEOVER))
   (USEMAP map-name?)
   (VSPACE integer?)
   (WIDTH  integer?)))

(define-html-element INPUT
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))
   CHECKED
   (MAXLENGTH integer?)
   (NAME      string?)
   (SIZE      integer?) ;; Kathi fixed -- was width/height?)
   (SRC       url?)
   (TYPE      (member-of? 
	       '(CHECKBOX
		 HIDDEN
		 IMAGE
		 PASSWORD
		 RADIO
		 RESET
		 SUBMIT
		 TEXT)))
   (VALUE    string?)))

(define-html-element ISINDEX
  ((ACTION url?)
   (PROMPT string?)))

(define-html-element LINK
  ((HREF url?)
   (REL  symbol?)
   (REV  symbol?)
   (TITLE string?)
   (TYPE  string?)
   ))

(define-html-element META
  ((HTTP-EQUIV string?)
   (CONTENT    string?)
   (NAME       string?)
   (URL        url?)))

(define-html-element OPTION
  (SELECTED
   (VALUE string?)))

(define-html-element PARAM
  ((NAME  string?)
   (VALUE string?)
   (VALUETYPE (member-of? '(DATA REF OBJECT)))
   (TYPE  string?)))

(define-html-element TFOOT ())

(define-struct (html:container struct:html:optionals)   (contents))

(define (extend-container container . extensions)
  (if (html:container? container)
      (make-html:container
       (html-type container)
       (html:optionals-options container)
       (append
	(html:container-contents container)
	(map canonicalize-html extensions)))
      (error "Extend container called on non-container" container)))

(define-macro define-html-container
  (lambda (name-spec options)
    (let* ((name (if (list? name-spec) 
		     (car name-spec)
		     name-spec))
	   (tag  (if (list? name-spec)
		     (cadr name-spec)
		     (string-upcase (symbol->string name-spec))))
	   (type      (symbol-append 'HTML-TYPE: name))
	   (maker     (symbol-append 'HTML- name))
	   (maker*    (symbol-append 'HTML- name '*))
	   (predicate (symbol-append 'HTML- name '?)))

      (define (make-option-checker option)
	(if (symbol? option)
	    `((EQ? OPTION ',option) OPTION)
	    (let ((option-name (car option))
		  (option-type (cadr option)))
	    `((AND (PAIR? OPTION)
		   (EQ? (CAR OPTION) ',option-name)
		   (,option-type (CADR OPTION)))
	      (LIST (CAR OPTION) (SAFE-HTML (CADR OPTION)))))))

      `(BEGIN
	 (DEFINE ,type (MAKE-HTML-TYPE ',name ,tag))

	 (DEFINE [,maker* OPTIONS]
	   (DEFINE (CANONICALIZE-OPTION OPTION)
	     (COND ,@(map make-option-checker options)
		   (ELSE (ERROR "Illegal Option:" ',maker option))))
	   (LAMBDA (CONTENTS)
	       (MAKE-HTML:CONTAINER ,type 
				    (MAP CANONICALIZE-OPTION OPTIONS)
				    (MAP CANONICALIZE-HTML   CONTENTS))))

	 (DEFINE [,maker . OPTIONS]
	     (DEFINE (CANONICALIZE-OPTION OPTION)
	       (COND ,@(map make-option-checker options)
		     (ELSE (ERROR "Illegal Option:" ',maker option))))
               (LAMBDA CONTENTS
	         (MAKE-HTML:CONTAINER ,type
				      (MAP CANONICALIZE-OPTION OPTIONS)
				      (MAP CANONICALIZE-HTML   CONTENTS))))

	 (DEFINE (,predicate OBJECT)
	   (AND (HTML? OBJECT)
		(EQ? ',name (HTML-TYPE-NAME (HTML-TYPE OBJECT)))))
	 ))))


(define color? string?)
(define url?   string?)
(define window? string?)

;; Obviously, these are only some of the myriad container objects.
;; The rest will be added as they become necessary.

(define-html-container (ANCHOR "A")
  ((HREF   url?)
   (NAME   string?)
   (TARGET window?) 
   (TITLE  string?))) ;cannot nest?
(define-html-container ADDRESS ())
(define-html-container (BOLD "B") ())
(define-html-container (EMPHASIZED "EM") ())
(define-html-container (STRONG "STRONG") ())
(define-html-container (TYPEWRITER "TT") ())
(define-html-container BIG ())
(define-html-container BLOCKQUOTE ())
(define-html-container BODY
  ((background url?)
   (bgcolor color?)
   (bgproperties (member-of? '(FIXED)))
   (bottommargin integer?)
   (language string?)
   (leftmargin integer?)
   (link    color?)
   (onload  string?)
   (rightmargin integer?)
   (text    color?)
   (topmargin integer?)
   (vlink   color?)
   (alink   color?)))
(define-html-container CAPTION
  ((align (member-of? '(TOP BOTTOM)))))
(define-html-container CENTER ())
(define-html-container CITE ())
(define-html-container CODE ())
;(define-html-container COMMENT ())
(define-html-container DFN ())
(define-html-container DIV
  ((align (member-of? '(LEFT CENTER RIGHT JUSTIFY)))))
(define-html-container FONT
  ((FACE string?)
   (COLOR string?)
   (SIZE string?)))
(define-html-container FORM
  ((ACTION	string?)
   (TARGET	string?)
   (METHOD	(member-of? '(GET POST)))
   (NAME	string?)))
(define-html-container FRAMESET
  ((COLS          string?)
   (FRAMEBORDER  integer?)
   (FRAMESPACING integer?)
   (ROWS          string?)
   (NAME          string?)
   NORESIZE
   NOSCROLL
   (SRC           url?)))
(define-html-container H1
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container H2
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container H3
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container H4
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container H5
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container H6
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container HEAD ())
(define-html-container NOFRAMES ())
(define-html-container OBJECT
  ((CLASSID classid?)
   (ID      string?)
   (STYLE   style?)))
(define-html-container (PARAGRAPH "P")
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container SMALL ())
(define-html-container TABLE
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))
   ;; Kathi added
   (border integer?)
   (width string?)
   (height string?) ; added by jbc 5/1999
   (cellpadding integer?)
   (cellspacing integer?)
   (bgcolor color?)))
(define-html-container (TABLE-DATA "TD")
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))
   (VALIGN (member-of? '(TOP CENTER BOTTOM)))
   (bgcolor color?)
   ;; following added by jbc 5/1999
   (colspan number?)))
(define-html-container (TABLE-ROW "TR")
  ((ALIGN (member-of? '(LEFT CENTER RIGHT)))))
(define-html-container TEXTAREA
                       ((NAME string?)
                        (ROWS integer?)
                        (COLS integer?)))
(define-html-container TITLE ())
(define-html-container SPAN
  ((STYLE string?)))

;; ----- ADDED BY KATHI (kfisler@cs.rice.edu) 6/19/98 ------

(define-html-container UL ())
(define-html-container OL ())
(define-html-container LI ())
(define-html-element (HRULE "HR") ())
(define-html-container SELECT
  ((NAME string?)
   (SIZE integer?)))

;;; Special cases.

(define [html-header n options]
  ((cond ((= n 1) HTML-H1)
	 ((= n 2) HTML-H2)
	 ((= n 3) HTML-H3)
	 ((= n 4) HTML-H4)
	 ((= n 5) HTML-H5)
	 ((= n 6) HTML-H6)) options))

(define [html-doctype elements]
  (make-html:doctype html-type:doctype (map canonicalize-html elements)))

(define [html-comment comments]
  (make-html:comment html-type:comment (map canonicalize-html comments)))

(define [html-verbatim-comment comment]
  (make-html:comment html-type:comment (list comment)))

(define-struct (html-page struct:html)
  (head 
   style 
   body))

(define-struct (html-frames struct:html)
  (head
   frames
   noframes))

(define-struct script
  (language
   contents))

(define (VBScript text)
  (make-script "VBScript" text))

(define-struct (html-script-page struct:html)
  (head 
   script
   body))

(define (make-page head style body)
  (if (and (html-head? head)
	   (not style)
	   (html-body? body))
      (make-html-page 'page head style body)
      (error "Illegal page components" head style body)))

(define (make-frames head frameset noframes)
  (if (html-body? noframes)
      (make-html-frames 'frames head frameset ([HTML-NOFRAMES] noframes))
      (error "Illegal frame components" head frameset noframes)))
      
(define (make-script-page head script body)
  (if (and (html-head? head)
	   (script? script)
	   (html-body? body))
      (make-html-script-page 'script-page head script body)
      (error "Illegal script page components" head script body)))


(define [html-alist-table]
  (lambda (alist)
    (let loop ((remaining alist))
      (if (null? remaining)
	  '()
	  (cons ([html-table-row]
		 ([html-table-data] (caar remaining))
		 ([html-table-data] (cadar remaining)))
		(loop (cdr remaining)))))))

;;; Emission is somewhat ad hoc.
;;; The goal here is to create something both machine readable
;;; and human readable.

(define-struct html-emitter-state
  (port
   depth))

(define (emit-html-script state script)
  (let ((port (html-emitter-state-port state)))
    (newline port)
    (display "<SCRIPT LANGUAGE=\"" port)
    (display (script-language script) port)
    (display "\">" port)
    (newline port)
    (display "<!--" port)
    (newline port)
    (display (script-contents script) port)
    (newline port)
    (display "-->" port)
    (newline port)
    (display "</SCRIPT>" port)
    (newline port)
    ))

(define (emit-html-begin-tag state object)
  (let ((port (html-emitter-state-port state))
	(depth (html-emitter-state-depth state)))

  (define (emit-element element)
    (cond ((symbol? element) (fprintf port "~a" 
				      (string-upcase
				       (symbol->string element))))
	  ((integer? element) ;(write-char #\" port)
			      (fprintf port "~s" element)
			      ;(write-char #\" port)
			      )
	  ((string? element) (fprintf port "~s" element))
	  (else (client-error "Non canonical option element ~s" element))))

  (define (emit-option option)
    (if (list? option)
	(begin (emit-element (car option))
	       (emit-element '=)
	       (emit-element (cadr option)))
	(emit-element option)))

  ;; handle comments specially for good looks
  (if (html:comment? object)
      (begin (fprintf port "<!--") ;; Kathi removed space after --
	     (let loop ((comments (html:comment-text object)))
	       (cond ((null? comments) (fprintf port "-->~%"))
		     ((null? (cdr comments)) 
		      (fprintf port "~a -->~%" (car comments)))
		     (else (fprintf "~a -- " (car comments))
			   (loop (cdr comments))))))
      (begin
	 (fprintf port "<~a" 
		  (html-type-tag (html-type object)))
	 (for-each (lambda (option)
		     (write-char #\space port)
		     (emit-option option))
		   (html:optionals-options object))
	 (newline port)
	 (let loop ((i 0))
	   (if (= i depth)
	       (write-char #\> port)
	       (begin (write-char #\space port)
		      (loop (+ i 1))))))
	)))

(define (emit-html-end-tag state object)
  (let ((port  (html-emitter-state-port state))
	(depth (html-emitter-state-depth state)))
  (fprintf port "</~a" (html-type-tag (html-type object)))
  (newline port)
  (let loop ((i 0))
    (if (= i depth)
	(write-char #\> port)
	(begin (write-char #\space port)
	       (loop (+ i 1)))))))

(define (emit-html-contents state object)
  (let ((port (html-emitter-state-port state)))
    (for-each (lambda (content)
		(emit-html state content))
	      (html:container-contents object))))

(define (standard-html-comments)
  (list
   [html-doctype '(html public "-//W3C//DTD HTML 3.2//EN")]
   ""
   [html-comment '("This page was generated by Scheme CGI by Emergent Technologies Inc.")]
   [html-comment '("For more information about dynamic page creation using Scheme CGI,")]
   [html-comment '("including FREE software, visit our web site at")]
   [html-comment '("http://www.eval-apply.com")]
   ))

(define (emit-html state object)
  (let ((port  (html-emitter-state-port state))
	(depth (html-emitter-state-depth state)))
    (cond ((string? object) (fprintf port "~a" object))
	  ((html?   object) 
	   (emit-html-begin-tag (make-html-emitter-state port (+ depth 1)) object)
	   (if (html:container? object)
	       (begin 
		 (emit-html-contents (make-html-emitter-state port (+ depth 1))
				     object)
		 (emit-html-end-tag state object))))
	  (else (error "Illegal html emissions:  " object)))))

(define (raw-emit-html-top-level html port)
  (let ((state (make-html-emitter-state port 0)))
    (map (lambda (html) (emit-html state html)) (standard-html-comments))
    (cond ((html-page? html)
	   (emit-html state (html-page-head html))
	   (emit-html state (html-page-body html)))
	  ((html-frames? html)
	   (emit-html state (html-frames-head html))
	   (emit-html state (html-frames-frames html))
	   (emit-html state (html-frames-noframes html)))
	  ((html-script-page? html)
	   (emit-html state (html-script-page-head html))
	   (emit-html-script state (html-script-page-script html))
	   (emit-html state (html-script-page-body html)))
	  (else (error "Illegal html top level:  " html))) ;; fixed by Shriram
    (display "</HTML>" port)
    (newline port)))

(define (emit-html-top-level html port)
  (display "Content-type: text/html" port)
  (newline port)
  (newline port)
  (raw-emit-html-top-level html port)
  )

(add-mime-handler! html-page? emit-html-top-level)

