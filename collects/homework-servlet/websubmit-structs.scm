(module websubmit-structs mzscheme
  (require "parsing.scm")
  (provide (struct full-assignment (dir size date))
           (struct upload-token (definitions testcases)))

  ;; a full-assignment is a structure
  ;; (make-full-assignment string (url | #f) date string)
  (define-struct (full-assignment homework-assignment) (dir size date))
  
  ;; an upload-token is a structure
  ;; (make-upload-token (string string)
  (define-struct upload-token (definitions testcases)))
  
  
