(unit/sig gui^
  (import framework^ gui-text^ gjc^ mred^ mzlib:function^)
  
  (define drjava-frame%
    (class frame:text-info-file% (filename)
      (inherit get-canvas% get-editor% get-area-container get-canvas get-menu-bar)
      (private
        (base-editor% text:info%);(get-editor%))
        (defs% (definitions-text-mixin base-editor%))
        (forwarding-mess (make-object forwarding-message this))
        (defs (make-object defs% forwarding-mess)))
      (override
        (make-editor (lambda () defs))
        (edit-menu:between-find-and-preferences void)
        (file-menu:new
         (lambda (item evt) (handler:edit-file #f (lambda _ (new-document #f)))))
        (file-menu:between-open-and-revert
         (let ([title "Set Library To..."])
           (lambda (file-menu)
             (make-object menu-item% title
               file-menu
               (lambda (menu-item control-event)
                 (let ([path (get-text-from-user title "Enter the extra library path:" #f "")])
                   (when path
                     (set-gjc-extension-path! path)
                     (send repl lib-changed))))
               #f
               "Use pre-compiled libraries"))))
        (file-menu:between-print-and-close void))
      (sequence
        (super-init filename)
        (fix-parens (send defs get-keymap)))
      (private
        (can% (get-canvas%))
        (repl% (repl-text-mixin base-editor%))
        (repl (make-object repl%))
        (new-can
         (lambda (parent ed)
           (send (make-object can% parent) set-editor ed)
           ed))
	(build-show-item
	 (lambda (show editor name key pos)
	   (let ([can (send editor get-canvas)]
		 [hide-str (format "Hide ~a" name)]
		 [show-str (format "Show ~a" name)])
             (make-object menu-item% hide-str show
               (lambda (item cntl)
                 (send (get-area-container)
                       change-children
                       (lambda (kids)
                         (if (memq can kids)
                             (begin
                               (send item set-label show-str)
                               (remq can kids))
                             (begin 
                               (send item set-label hide-str)
                               (cons (car kids) (pos can (cdr kids)))
                               (cons (car kids) (pos can (cdr kids))))))))
               key)))))
      (sequence
        (fix-parens (send repl get-keymap))
        (send defs set-file-format 'text)
        (add-parts
         (get-area-container)
         (lambda (parent mess)
           (send forwarding-mess set-forward-to! mess)
           defs)
         (lambda (parent) (new-can parent repl))
         (lambda (defs repl)
           (when filename
             (send defs load-file filename))
           defs))
        (send (get-area-container) change-children
              (lambda (l)
                (list* (cadr l) (car l) (cddr l))))
        (let ([show (make-object menu% "Show" (get-menu-bar))])
	  (build-show-item show defs "Definitions" #\d cons)
	  (build-show-item show repl "Interactions" #\e
			   (lambda (can tail) (cons (car tail) (cons can (cdr tail)))))))))
  
  
  (define forwarding-message
    (class object% (frame)
      (private
        (forward-to #f))
      (public
        (set-label
         (lambda (message)
           (when forward-to
             (send forward-to set-label message)
             (send frame set-label message))))
        (set-forward-to!
         (lambda (forwardee)
           (set! forward-to forwardee))))
      (sequence (super-init))))
  
  ;; fix-parens : Keymap% -> Void
  (define fix-parens
    (let ([fname "flash-paren-match"])
      (lambda (keymap)
        (send keymap add-function fname
              (lambda (edit event)
                (send edit on-default-char event)
                (let ([pos (paren:backward-match 
                            edit
                            (send edit get-start-position)
                            0
                            parens quotes comments #f #f)])
                  (when pos
                    (send edit flash-on pos (+ 1 pos))))
                #t))
        (map (lambda (x) (send keymap map-function (cdr x) fname))
             parens))))
  
  (define parens '(("(" . ")") ("[" . "]") ("{" . "}")))
  (define quotes '(("'" . "'") ("\"" . "\"") ("/*" . "*/")))
  (define comments '("//"))
  
  ;; new-document : (U String #f) -> Void
  (define (new-document name)
    (send (make-object drjava-frame% name) show #t))
  
  (handler:insert-format-handler "drjava" void new-document))
