(define mred@
  (let* ([debug/s@ (unit->unit/sig mred:debug@ () mred:debug^)]
	 [mred:plt-home-directory mred:plt-home-directory]
	 [mred:system-source-directory mred:system-source-directory]
	 [constants@ (unit/sig mred:constants^ (import)
		       (define plt-home-directory mred:plt-home-directory)
		       (define system-source-directory mred:system-source-directory))])
    (compound-unit/sig (import [core : mzlib:core^]
			       [trigger : mzlib:trigger^]
			       [application : mred:application^])
      (link [constants : mred:constants^ (constants@)]
	    [debug : mred:debug^ (debug/s@)]
	    [exn : mred:exn^ (mred:exn@ debug)]
	    [container : mred:container^
		       (mred:container@ debug (core function@))]
	    [exit : mred:exit^ (mred:exit@ debug preferences gui-utils)]
	    [preferences : mred:preferences^
			 (mred:preferences@ debug exn container exit gui-utils
					  edit (core function@))]
	    [autoload : mred:autoload^
		      (mred:autoload@ debug preferences (core file@))]
	    [autosave : mred:autosave^
		      (mred:autosave@ debug exit preferences)]
	    [mode : mred:mode^
		  (mred:mode@ debug keymap)]
	    [handler : mred:handler^
		     (mred:handler@ debug group editor-frame finder (core file@))] 
	    [keymap : mred:keymap^
		    (mred:keymap@ debug preferences exit finder handler
				find-string scheme-paren gui-utils)]
	    [match-cache : mred:match-cache^ (mred:match-cache@ debug)]
	    [scheme-paren : mred:scheme-paren^
			  (mred:scheme-paren@ debug paren)]
	    [paren : mred:paren^ (mred:paren@ debug)]
	    [path-utils : mred:path-utils^ (mred:path-utils@ debug)]
	    [gui-utils : mred:gui-utils^ (mred:gui-utils@ debug (core function@) trigger)]
	    [finder : mred:finder^
		    (mred:finder@ debug preferences (core string@)
				(core function@) (core file@))]
	    [icon : mred:icon^ (mred:icon@ debug constants)]
	    [menu : mred:menu^ (mred:menu@ debug (core function@))]
	    [edit : mred:edit^ 
		  (mred:edit@ debug finder path-utils mode scheme-paren
			    keymap (core function@))]
	    [group : mred:group^ 
		   (mred:group@ debug preferences editor-frame gui-utils
			      exit autosave handler (core function@))]
	    [frame : mred:frame^ 
		   (mred:frame@ debug preferences edit container canvas icon
			      menu group finder handler exit autosave
			      gui-utils (core function@) (core file@))]
	    [canvas : mred:canvas^ 
		    (mred:canvas@ debug container edit (core file@))]
	    [find-string : mred:find-string^ 
			 (mred:find-string@ debug container canvas edit frame)]
	    [editor-frame : mred:editor-frame^ 
			  (mred:editor-frame@ debug preferences edit frame container
					    canvas find-string icon menu group
					    finder handler exit autosave gui-utils
					    (core function@) (core file@))]
	    [project : mred:project^ 
		     (mred:project@ debug group container gui-utils exit finder
				  frame handler (core file@) (core function@))]
	    [console : mred:console^ 
		     (mred:console@ debug preferences edit frame find-string
				  exit finder handler gui-utils scheme-mode
				  scheme-paren icon (core function@) (core string@) (core pretty-print@) trigger)]
	    [scheme-mode : mred:scheme-mode^ 
			 (mred:scheme-mode@ debug preferences application container
					  mode match-cache paren scheme-paren icon
					  handler keymap (core string@))]
	    [hyper-dialog : mred:hyper-dialog^ 
			  (mred:hyper-dialog@ debug hyper-edit (core file@))]
	    [hyper-edit : mred:hyper-edit^ 
			(mred:hyper-edit@ debug edit hyper-dialog (core file@)
					(core string@))]
	    [hyper-frame : mred:hyper-frame^ 
			 (mred:hyper-frame@ debug hyper-edit hyper-dialog container
					  editor-frame canvas group handler)]
	    [html : mred:html-mode^ 
		  (mred:html-mode@ handler hyper-frame hyper-edit)])
      (export (unit debug)
	      (open constants)
	      (open (exn : mred:exn-external^))
	      (open container) (open preferences)
	      (open autoload) (open autosave) (open exit)
	      (open gui-utils) (open console) (open path-utils)
	      (open finder)
	      (open find-string) (open edit) (open canvas)
	      (open frame) (open editor-frame)
	      (open group) (open handler) (open icon) (open keymap)
	      (open match-cache) (open menu) (open mode) (open project)
	      (open scheme-paren) (open scheme-mode) (open paren)
	      (open hyper-edit) (open hyper-dialog) (open hyper-frame)))))
