; main.ss
; Defines main MrSpidey class
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------
; ported to MrEd 100 by Paul Steckler 

(define-struct fileinfo (filename frame thunk-port annotations))
;; annotations may also be (-> zodiac:parsed)

(define NUM-EXTRA-LINES 20)

(define-struct exn:flow-arrow-exists ())

(define (wrap-busy-cursor thunk)
  (dynamic-wind
    (lambda () (begin-busy-cursor))
    thunk
    (lambda () (end-busy-cursor))))

; ----------------------------------------------------------------------
(define tm (void))

(define MrSpidey%
  (class object% ()

    (public 

      [fileinfo* '()]
      [multiple-files
        (lambda ()
          (not (= (length fileinfo*) 1)))]
      ;; ------------------------------
      ;; Progress frame

      [progress-frame 'progress-frame]
      [progress-canvas 'progress-canvas]
      [progress-edit 'progress-edit]

      [init-progress-frame!
       (lambda ()
         (pretty-debug-gui `(init-progress-frame))
         (let* ( [f (parameterize
                      ([current-eventspace (make-eventspace)])
                      (make-object frame% (format "~a Progress" st:name)))]
                 [p (make-object vertical-panel% f)]
                 [c (make-object editor-canvas% p)]
                 [ph (make-object horizontal-panel% p)]
                 [_ (send ph stretchable-height #f)]
                 [_ (make-object horizontal-panel% ph)]
                 [b (make-object button% "Hide" ph 
                      (lambda _ (send f show #f)))]
                 [e (make-object text%)])
           (set! progress-frame f)
           (set! progress-canvas c)
           (set! progress-edit e)
           (send progress-frame resize 400 200)
           (send progress-frame show #f)))]

      [new-progress-frame!
       (lambda ()
         (pretty-debug-gui `(new-progress-frame))
         (set! progress-edit (make-object text%))
         (send progress-edit lock #t)
         (send progress-canvas set-editor progress-edit)
         (send progress-frame show #t)
         (send progress-frame iconize #f)
         (pretty-debug-gui `(new-progress-frame-done)))]

      [with-progress-frame
       (lambda (thunk)
           (let*
               ( [old-progress-handler (mrspidey:progress-handler)]
                 [current '()]
                 [current-start-time 0]
                 [width 6]
                 [total-width 18])
             (letrec
                 ([insert-numbers
                    (lambda (n)
                      (send progress-edit insert
                        (format "~a    ~ams" 
                          (padl 
                            (if (eq? n 'done) "" (format "~a" n))
                             width)
                          (padl (- (current-process-milliseconds) 
                                  (current-gc-milliseconds)
                                  current-start-time)
                            width))
                        (send progress-edit last-position)))]
		  [f (match-lambda*
                      [((? string? name) line)
                       (if (equal? name current)
                         (let ([end (send progress-edit last-position)])
                           (send progress-edit delete 
                             (- end total-width) end)
                           (insert-numbers line))
                         (begin
                           (f 'fresh-line)
                           (set! current name)
                           (set! current-start-time
                             (- (current-process-milliseconds) 
                               (current-gc-milliseconds)))
                           (send progress-edit insert 
                             (padr name 30))
                           (insert-numbers line)))]
                      [((? string? str)) 
                       (f 'fresh-line)
                       (send progress-edit insert str)
                       (f #\newline)]
                      [(#\newline)
                       (send progress-edit  insert (format "~n"))
                       (set! current '())]
                      [('fresh-line)
                       (unless (null? current) (f #\newline))])]
                  [g (lambda args
                       (send progress-edit lock #f)
                       (apply f args)
                       (send progress-edit lock #t))])
               (parameterize 
                 ([mrspidey:progress-handler 
                    (lambda args
                      ;;(apply old-progress-handler args)
                      (apply g args)
                      (flush-display))])
                 (send progress-frame show #t)
                 (begin0 
                   (thunk)
                   (mrspidey:progress
                     "=Done======================================"))))))]

      ;; ------------------------------

      [filename->fileinfo
       (lambda (file)
         (pretty-debug-gui `(filename->fileinfo ,file))
         (or
           (ormap
             (match-lambda
               [(and fi ($ fileinfo filename frame))
                 (and (string? file)
                   (string=? filename file)
                   fi)])
             fileinfo*)))]
      [filename->frame
       (lambda (file)
         (let ([x (filename->fileinfo file)])
           (and x 
		(fileinfo-frame x))))]

      [filename->edit
       (lambda (file)
         (let ([x (filename->frame file)])
           (and x (ivar x program-edit))))]

      [for-each-frame
       (lambda (f)
         (for-each
          (match-lambda
           [($ fileinfo filename frame)
            (when frame (f frame))])
          fileinfo*))]

      [focus-def
       (lambda (loc)
         (assert (zodiac:location? loc) 'focus-def loc)
         (let* ([file (zodiac:location-file loc)]
                [frame (filename->frame file)])
           (if frame
               (send frame focus-def (zodiac:location-offset loc))
               (message-box
                "Error"
                (format "File ~s is not loaded" (file-name-from-path file))
                '(ok)))))]

      ;; ------------------------------

      [open-analyzed-file-choice
        (lambda ()
          (let ([choice
                  (get-choices-from-user
                    "Open Unit"
                    "Select referenced unit file to open"
                    (quicksort (map fileinfo-filename fileinfo*) string<?)
                    #f '() '(single))])
            (when (string? choice)
              (open-fileinfo (filename->fileinfo choice) #t))))]

      [open-fileinfo
       (match-lambda*
        [((and fi ($ fileinfo filename frame)) show)
         (if frame
             (when show
               (send frame show #t)
               (send frame iconize #f))
             (add-frame fi show))])]

      [open-all
       (lambda (show) 
         (for-each 
           (lambda (fi) (open-fileinfo fi show))
           fileinfo*))]

      [add-frame
       (match-lambda*
        [( (and fi ($ fileinfo filename frame thunk-port thunk-expression))
           show
           . first-frame-locs)
         (pretty-debug-gui
           `(add-no-show-frame ,filename ,fi ,@first-frame-locs))
         (assert (not frame))
         (let ([summary-edit (make-object text%)])
           (initialize-summary summary-edit)
           (pretty-debug-gui `(summary-initialized))
           (parameterize
            ([mrspidey:add-summary-handler (add-summary summary-edit)])
            (with-progress-frame
             (lambda ()
               (pretty-debug-gui '(progress-frame-initialized))
               (mrspidey:progress
                 "===========================================")
               '(mrspidey:progress 
                  (format "Analyzing ~a" (file-name-from-path filename)))
               (let* ([annotations 
                        (calc-annotations (thunk-expression))]
                      [_ (set-fileinfo-annotations! fi annotations)]
                      [_ (pretty-debug-gui `(calling-Tframe ,filename))]
                      [frame (apply make-object spidey:frame% 
                               ;; only use margin
                               this 
                               filename summary-edit 
                               first-frame-locs)])
                 (pretty-debug-gui `(Have-Tframe ,filename))
                 (when show (send frame show #t))
                 (unless show (send frame show #t) (send frame show #f))
                 (set-fileinfo-frame! fi frame)
                 (update-arrows)
                 )))))])]

      [initialize-summary
        (lambda (edit)
          (let* ([delta (make-object style-delta% 
				     'change-family
				     'decorative)]
                  [click-delta (make-object style-delta%)])
            (send delta set-delta 'change-size 10)
            (send click-delta copy delta)
            (send click-delta set-delta-foreground "BLUE")
            (send click-delta set-delta 'change-underline #t)

            (let ( [insert
                     (lambda (s)
                       (let ([before (send edit get-end-position)])
                         (send edit insert s)
                         (let ([after (send edit get-end-position)])
                           (values before after))))])
              (let*-values
                ( [(s1 e1) (insert "Welcome to ")]
                  [(s2 e2) (insert "MrSpidey")]
                  [(s3 e3) (insert (format ", version ~a." (version:version)))]
                  [(s4 e4) (insert (format "~n"))])
                (send edit change-style delta s1 e1)
                (send edit change-style click-delta s2 e2)
                (send edit change-style delta s3 e3)
                (send edit set-clickback s2 e2
		  (lambda args
                    (make-object hyper-text%
		     (string->url
                      (string-append 
                        "file:"
                        (build-path
			  (collection-path "mrspidey") ; MATTHEW: got rid of plt-home
                          "about.html")))
		      #f))
                  click-delta)))))]

      [local-record-analyzed-file
        (lambda (filename thunk-port thunk-expression)
          (pretty-debug-gui
            `(local-record-analyzed-file ,filename 
               ,thunk-port
               ,thunk-expression))
          (set! fileinfo*
            (append fileinfo*
              (list (make-fileinfo filename #f 
                      thunk-port 
                      thunk-expression)))))]

      ;; ------------------------------

      [add-summary
       (lambda (summary-edit)
         (lambda line
           (send summary-edit lock #f)
           (match line
             [(str loc word-ofs)
               (cond
                 [(zodiac:zodiac? loc)
                   ((add-summary summary-edit) str 
                     (zodiac:zodiac-start loc) word-ofs)]
                 [(zodiac:location? loc)
                   (let* ( [click (lambda ignore (focus-def loc))]
                           [init-pos-box (box 0)]
                           [_ (send summary-edit get-position init-pos-box)]
                           [_ (send summary-edit insert str)]
                           [_ (send summary-edit insert 
                                (format " in file ~s line ~s"
                                  (file-name-from-path
                                    (zodiac:location-file loc))
                                  (zodiac:location-line loc)))]
                           [_ (send summary-edit insert #\newline)]
                           ;; Find start and end of word
                           [end (unbox init-pos-box)]
                           [_ 
                             (for i 0 (add1 word-ofs)
                               (set! end (scheme-paren:forward-match 
                                           summary-edit end 
                                           (send summary-edit last-position))))]
                           [start (scheme-paren:backward-match summary-edit end 0)])
             
                     '(pretty-debug-gui 
                        `(send summary-edit change-style check-link-delta
                           ,(send check-link-delta get-underlined-on)
                           ,start ,end))
                     ;; Paranoia - check have proper locations
                     (if (and start end)
                       (begin
                         (send summary-edit set-clickback start end click)
                         (send summary-edit change-style check-link-delta 
                           start end))
                       ;; Error
                       (pretty-print 
                         `(Error: annotate-summary 
                            ,str ,loc ,word-ofs ,start ,end))))]
                 [else
                   (begin
                     (printf 
                       "Bad location in main:add-summary-handler ~s~n"
                       loc)
                     (mrspidey:add-summary str))])]
             [(str)
              (send summary-edit insert str) 
              (send summary-edit insert #\newline)]
             [x (printf "add-summary, can't handle ~s~n" x)])
           (send summary-edit lock #t)))]

      ;; ------------------------------

      [on-frame-close
       (lambda (filename)
         (void))]

      [close-all-frames
       (lambda ()
         (send progress-frame show #f)
         (close-all-frames-except-progress)
         )]

      [close-all-frames-except-progress
       (lambda ()
         (for-each-frame (lambda (frame) (send frame on-close)))
         )]

      ;; ------------------------------
      
      [annotated-edit
        (lambda (mode filename canvas)
          ;; create edit buffer, load file and add annotations
          (pretty-debug-gui `(annotate-edit ,mode ,filename))
          (match (filename->fileinfo filename)
            [($ fileinfo filename frame thunk-port annotations)
	     (let ([port (thunk-port)]
		   [edit 
		    (make-object 
		     (mode-edit-class mode)
		     (if (multiple-files) "  " "")
		     this canvas)])
                (send edit set-filename filename)
                (send edit edit-sequence
                  (lambda ()
                    (pretty-debug-gui "loading!")
                    (let ([s (format "Loading   ~a: "
                               (file-name-from-path filename))])
                      (recur loop ([n 1])
                        (when (zero? (modulo n 50))
                          (mrspidey:progress s n))
                        (let ([r (read-line port)])
                          ;;(pretty-debug-gui `(inserting ,r))
                          (if (eof-object? r)
                            (mrspidey:progress s (sub1 n))
                            (begin
                              (send edit insert-line r)
                              (loop (add1 n))))))
                      (when (multiple-files)
                        (for i 0 NUM-EXTRA-LINES (send edit insert-line ""))))
                    (close-input-port port)
                    (pretty-debug-gui `(last-line ,(send edit last-line)))
                    (send edit change-style base-delta 
                      0 (send edit last-position))

                    (pretty-debug-gui "annotating!")
                    (annotate! filename edit mode annotations)

                    (send edit set-position 0)
                    (pretty-debug-gui `(annotate-buffer done))))

                edit)]))]

      [annotate!
        (lambda (filename edit mode annotations)
          (let ([s (format "Annotating ~a:" (file-name-from-path filename))])
            (mrspidey:progress s '...)
            (pretty-debug-gui `(annotate! ,filename ,edit ,mode ,annotations))
            (let ([annotation-fn-list (mode-annotation-fn-list mode)])
              (for-each
                (lambda (annotation-fn annotations)
                  (when annotation-fn
                    (for-each
                      (let ([fn (ivar/proc edit annotation-fn)])
                        (match-lambda
                          [(and annotation ($ annotation loc))
                            (when (string=? (zodiac:location-file loc)
                                    filename)
                              ;; Call the method named func to annotate etc.
                              (fn annotation))]))
                      annotations)))
                annotation-fn-list 
                (vector->list annotations)))
            (mrspidey:progress s 'done)))]
      
      ;; ------------------------------

      [shake-it
       '(lambda ()
          (let* ([n (random (vector-length fileinfo*))]
                 [fi (vector-ref fileinfo* n)]
                 [frame (fileinfo-frame fi)])
            (send frame shake-it)))]

      ;; ------------------------------
      ;; Arrows

      [list-flow-arrows '()]
      [add-flow-arrow 
        (lambda (src dest)
          (assert (and (FlowType? src) (FlowType? dest)))
          (pretty-debug-gui
            `(main:add-flow-arrow
               ,(FlowType->pretty src) ,(FlowType->pretty dest)))
          (with-handlers
            ([exn:flow-arrow-exists? (lambda (exn) (void))])
            (let ([flow-arrow (make-object flow-arrow% this src dest)])
              (when flow-arrow
                (set! list-flow-arrows (cons flow-arrow list-flow-arrows))))))]
      [draw-arrows
        (lambda ()
          (pretty-debug-gui `(main:draw-arrows))
          (for-each-frame
            (lambda (frame)
              (send (ivar frame program-edit) draw-arrows))))]
      [update-arrows
        (lambda ()
          (pretty-debug-gui `(main:update-arrows))
          (for-each 
            (lambda (arrow) (send arrow update))
            list-flow-arrows)
          (draw-arrows))]
      [delete-arrow
        (lambda (arrow)
          (send arrow delete-local)
          (set! list-flow-arrows (remv arrow list-flow-arrows)))]
      [delete-arrow-refresh
        (lambda (arrow)
          (delete-arrow arrow)
          (draw-arrows))]
      [delete-arrows
        (lambda ()
          (for-each 
            (lambda (arrow) (send arrow delete-local))
            list-flow-arrows)
          (set! list-flow-arrows '())
          (draw-arrows))]

      ;; ------------------------------

      [reanalyze 'reanalyze]

      [run-mrspidey

       (lambda (file . first-frame-locs)

         (pretty-debug-gui `(run-mrspidey ,file))
         (set! reanalyze 
             (lambda () (apply run-mrspidey file first-frame-locs)))

         (close-all-frames-except-progress)
         (pretty-debug-gui `(frames-closed))
         (set! fileinfo* '())
         (new-progress-frame!)

         (let ([file (normalize-path (normalize-path file))])
           ;; Following calls record-analyzed-file
           (st:analyze-and-make-annotations file)
           (apply add-frame (filename->fileinfo file) #t first-frame-locs)))])

    ;; ----------------------------------------------------------------------

    (sequence
      (super-init)
      (set! tm this)
      (record-analyzed-file-hook local-record-analyzed-file)
      (init-progress-frame!))))

(define spidey (make-object MrSpidey%))

;; ----------------------------------------------------------------------
;; mode says for each set of annotations,
;; either #f or fn to handle annotation.

(define-const-structure (mode name edit-class annotation-fn-list))

(define modes
  (list 
    (make-mode
      "Types and Checks" 
      flow-arrow:edit%
      (list 
        'add-type-annotation 
        'add-check-annotation
        'add-uncheck-annotation))
    (make-mode "Normal"
      spidey:static-edit%
      (list #f #f #f))))

;; ----------------------------------------------------------------------


