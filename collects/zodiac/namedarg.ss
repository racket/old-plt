; $Id: namedarg.ss,v 1.10 1998/03/14 17:53:46 mflatt Exp $

(begin-construction-time (require-library "match.ss"))
(begin-construction-time (require-library "macro.ss"))

(begin-construction-time 
 (invoke-open-unit
  (require-relative-library "namedargr.ss")))

(define-macro define-argument-list
  (lambda args
    `(begin-construction-time
       (-:define-argument-list ,@(map (lambda (x) `',x) args)))))

(define-macro lambda/nal
  (begin-construction-time lambda/nal))

(define-macro call/nal
  (begin-construction-time call/nal))

#|

(define-argument-list m3-elaborator
  expr foo (kwd attributes: attr) (kwd vocabulary: vocab)
  (opt (kwd parameterization: params) (current-parameterization))
  (opt other 3))

(define-argument-list other
  expr foo (kwd attributes: attr) (kwd vocabulary: vocab)
  (opt (kwd parameterization: params) (current-parameterization))
  (opt other 3))

(define f (lambda/nal m3-elaborator
	    (values expr foo attr vocab params other)))

(call/nal m3-elaborator f
  (parameterization: 'pahram)
  (vocabulary: 'vohcab )
  'exper
  'fuh
  'udder
  (attributes: 'atter))

|#
