(define tmp-filename
  (build-path (collection-path "tests" "drscheme") "launcher-test-tmp.ss"))

(define (save-as filename)
  (let ([drs (wait-for-drscheme-frame)])
    (fw:test:menu-select "File" "Save Definitions As...")
    (let ([dlg (wait-for-new-frame drs)])
      (for-each
       fw:test:keystroke
       (string->list filename))
      (fw:test:button-push "OK"))))

(define drs (wait-for-drscheme-frame))
(define definitions-canvas (ivar drs definitions-canvas))
(send definitions-canvas focus)
"Full pathname"
(for-each fw:test:keystroke (string->list "(message-box \"1\" \"1\")"))
(save-as tmp-filename)
