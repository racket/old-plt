
(module edit-main mzscheme
  (require (lib "edit.ss" "mred")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "string.ss")
	   (all-except "draw.ss" snip-class)
	   (all-except "graph.ss" snip-class))

  (let ([f (new-text-frame #f)])
    (let ([edit-menu (cadr (send (send f get-menu-bar) get-items))])

      (make-object menu-item% "Insert Plain Box" edit-menu
		   (lambda (item event)
		     (send (send f get-edit-target-object) 
			   insert (make-object draw-snip% 100 100))))

      (make-object menu-item% "Insert Graph..." edit-menu
		   (lambda (item event)
		     (let ([s (get-text-from-user "Formula"
						  "Formula to plot in [0,1]:"
						  f
						  "(lambda (x) x)")])
		       (when s
			 (let ([v (read-from-string s)])
			   (send (send f get-edit-target-object) insert 
				 (make-object graph-snip% v))))))))))



	   