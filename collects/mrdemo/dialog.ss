
; Demonstrates how to create a simple input dialog. The dialog
;  asks the user to make one or more selections in a list box.

; get-favorite-colors returns #f or a list of strings
(define (get-favorite-colors)
  (let ([result #f])
    ; The callbacks for the `ok' and `cancel' buttons
    ; set `result', but the default should be the value for
    ; `cancel' in case the user closes the dialog via a close
    ; box or by typing ESC.
    (letrec ([dialog (make-object dialog% "Favorite Color")]
	     [list (make-object list-box% "Pick some colors:" 
				'("Red" "Green" "Blue" "Fucia")
				dialog
				; Procedure called whenever the
				;  user changes the selection or
				;   double-clicks:
				(lambda (l e)
				  (enable-ok)
                                  (when (eq? (send e get-event-type)
					     'list-box-dclick)
                                    (do-ok)))
				; 'multiple style => multiple selections 
                                '(multiple))]
	     [h-pane (make-object horizontal-pane% dialog)]
	     [ok (make-object button% "OK" h-pane
			      ; Procedure called when user clicks the
			      ; button:
			      (lambda (b e)
				(do-ok))
			      ; 'border style => Enter/Return = click
			      '(border))]
             [do-ok (lambda ()
		      ; Set `result' to the list of selected colors:
                      (set! result 
                            (map
                             (lambda (i) (send list get-string i))
                             (send list get-selections)))
                      (send dialog show #f))]
	     [enable-ok (lambda ()
			  ; Ok button should be enabled only when
			  ;  something is selected:
			  (send ok enable 
				(pair? (send list get-selections))))]
	     [cancel (make-object button% "Cancel" h-pane
				  (lambda (b e)
				    (send dislog show #f)))])
      (enable-ok)
      (send dialog show #t)
      ; `Show' for a dialog handles events and doesn't return until
      ; the dialog is closed. At that point, `result' is set:
      result)))

(get-favorite-colors)
