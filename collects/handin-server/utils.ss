(module utils mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide unpack-submission)

  (define (unpack-submission str)
    (let* ([base (make-object editor-stream-in-string-base% str)]
	   [stream (make-object editor-stream-in% base)]
	   [definitions-text (make-object text%)]
	   [interactions-text (make-object text%)])
      (read-editor-version stream base #t)
      (read-editor-global-header stream)
      (send definitions-text read-from-file stream)
      (send interactions-text read-from-file stream)
      (read-editor-global-footer stream)
      (values definitions-text interactions-text))))
