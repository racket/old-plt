
(module keymap mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "mred-sig.ss" "mred"))

  (provide keymap@)
  (define keymap@
    (unit/sig (install-emacs-bindings)
      (import mred^)

      (define (install-emacs-bindings km)
	(define (map n f) (send km map-function n f))
	(define (map-meta key func)
	  (map (string-append "m:" key) func)
	  (map (string-append "esc;" key) func))
	
	(map "c:g" "ring-bell")
	(map-meta "c:g" "ring-bell")
	(map "c:x;c:g" "ring-bell")
	
	(map "c:p" "previous-line")
	(map "up" "previous-line")
	(map "s:c:p" "select-up")
	(map "s:up" "select-up")
	
	(map "c:n" "next-line")
	(map "down" "next-line")
	(map "s:c:n" "select-down")
	(map "s:down" "select-down")
	
	(map "c:e" "end-of-line")
	(map-meta "right" "end-of-line")
	(map "end" "end-of-line")
	(map-meta "s:right" "select-to-end-of-line")
	(map "s:end" "select-to-end-of-line")
	(map "s:c:e" "select-to-end-of-line")
	
	(map "c:a" "beginning-of-line")
	(map-meta "left" "beginning-of-line")
	(map "home" "beginning-of-line")
	(map-meta "s:left" "select-to-beginning-of-line")
	(map "s:home" "select-to-beginning-of-line")
	(map "s:c:a" "select-to-beginning-of-line")
	
	(map "c:f" "forward-character")
	(map "right" "forward-character")
	(map "s:c:f" "forward-select")
	(map "s:right" "forward-select")
	
	(map "c:b" "backward-character")
	(map "left" "backward-character")
	(map "s:c:b" "backward-select")
	(map "s:left" "backward-select")
	
	(map-meta "f" "forward-word")
	(map "a:right" "forward-word")
	(map "c:right" "forward-word")
	(map-meta "s:f" "forward-select-word")
	(map "a:s:right" "forward-select-word")
	(map "c:s:right" "forward-select-word")
	
	(map-meta "b" "backward-word")
	(map "a:left" "backward-word")
	
	(map "c:left" "backward-word")
	(map-meta "s:b" "backward-select-word")
	(map "a:s:left" "backward-select-word")
	(map "c:s:left" "backward-select-word")
	
	(map-meta "<" "beginning-of-file")
	(map-meta "up" "beginning-of-file")
	(map "c:home" "beginning-of-file")
	(map "s:c:home" "select-to-beginning-of-file")
	(map-meta "s:up" "select-to-beginning-of-file")
	
	(map-meta ">" "end-of-file")
	(map-meta "down" "end-of-file")
	(map "c:end" "end-of-file")
	(map "s:c:end" "select-to-end-of-file")
	(map "s:d:down" "select-to-end-of-file")
	
	(map "c:v" "next-page")
	(map "a:down" "next-page")
	(map "pagedown" "next-page")
	(map "c:down" "next-page")
	(map "s:c:v" "select-page-down")
	(map "a:s:down" "select-page-down")
	(map "s:pagedown" "select-page-down")
	(map "s:c:down" "select-page-down")
	
	(map-meta "v" "previous-page")
	(map "a:up" "previous-page")
	(map "pageup" "previous-page")
	(map "c:up" "previous-page")
	(map-meta "s:v" "select-page-up")
	(map "s:a:up" "select-page-up")
	(map "s:pageup" "select-page-up")
	(map "s:c:up" "select-page-up")
	
	(map "c:h" "delete-previous-character")
	(map "c:d" "delete-next-character")
	
	(map-meta "d" "kill-word")
	(map-meta "del" "backward-kill-word")
	(map-meta "c" "capitalize-word")
	(map-meta "u" "upcase-word")
	(map-meta "l" "downcase-word")
	
	(map "c:l" "center-view-on-line")
	
	(map "c:k" "delete-to-end-of-line")
	(map "c:y" "paste-clipboard")
	(map-meta "y" "paste-next")
	(map "c:_" "undo")
	(map (format "~a" (integer->char 31)) "undo") ; for Windows - strange
	(map "c:+" "redo")
	(map "c:x;u" "undo")
	(map "c:w" "cut-clipboard")
	(map-meta "w" "copy-clipboard")
	
	(map-meta "space" "collapse-space")
	(map-meta "\\" "remove-space")
	(map "c:x;c:o" "collapse-newline")
	(map "c:o" "open-line")
	(map "c:t" "transpose-chars")
	(map-meta "t" "transpose-words")
	
	(map "c:space" "toggle-anchor")
	
	(map "insert" "toggle-overwrite")
	(map-meta "o" "toggle-overwrite")
	
	(map-meta "g" "goto-line")
	(map-meta "p" "goto-position")
	
	(map "c:u" "command-repeat-0")
	(let loop ([n 9])
	  (unless (negative? n)
	    (let ([s (number->string n)])
	      (map-meta s (string-append "command-repeat-" s))
	      (loop (sub1 n)))))
	
	(map "c:x;e" "do-saved-macro")
	(map "c:x;(" "start-macro-record")
	(map "c:x;)" "end-macro-record")
	
	(map "leftbuttontriple" "mouse-select-click-line")
	(map "leftbuttondouble" "mouse-select-click-word")
	
	(map "rightbutton" "mouse-copy-click-region")
	(map "rightbuttondouble" "mouse-cut-click-region")
	(map "middlebutton" "mouse-paste-click-region")
	(map "c:rightbutton" "mouse-copy-clipboard")))))


