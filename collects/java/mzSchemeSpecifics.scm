;; Mario Latendrese, March 2001

;; The following were added due to incompatibility between
;; different Scheme systems. Definition for MzScheme.

(define my-directory-exists? directory-exists?)
(define my-with-output-to-file 
  (lambda (f p) (with-output-to-file f p 'replace)))
