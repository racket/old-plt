  (compound-unit/sig
    (import [core : mzlib:core^]
	    [trigger : mzlib:trigger^]
	    [application : mred:application^])
    (link [constants : mred:constants^ ((reference-unit/sig "constant.ss"))]
	  [version : mred:version^ ((reference-unit/sig "version.ss")
				    (core function@)
				    (core string@))]
	  [connections : mred:connections^
		       ((reference-unit/sig "connect.ss") (core function@))]
	  [exn : mred:exn^ ((reference-unit/sig "exn.ss"))]
	  [container : mred:container^
		     ((reference-unit/sig "containr.ss")
		      connections (core function@))]
	  [exit : mred:exit^ ((reference-unit/sig "exit.ss") preferences gui-utils)]
	  [preferences : mred:preferences^
		       ((reference-unit/sig "prefs.ss")
			exn container exit gui-utils
			edit (core pretty-print@)
			(core function@))]
	  [autoload : mred:autoload^
		    ((reference-unit/sig "autoload.ss") preferences (core file@))]
	  [autosave : mred:autosave^
		    ((reference-unit/sig "autosave.ss") exit preferences)]
	  [mode : mred:mode^
		((reference-unit/sig "mode.ss") keymap)]
	  [handler : mred:handler^
		   ((reference-unit/sig "handler.ss")
		    group gui-utils 
		    editor-frame finder hyper-frame
		    canvas container edit preferences
		    (core file@))] 
	  [keymap : mred:keymap^
		  ((reference-unit/sig "keys.ss")
		   preferences exit finder handler
		   find-string scheme-paren gui-utils)]
	  [match-cache : mred:match-cache^ ((reference-unit/sig "mcache.ss"))]
	  [scheme-paren : mred:scheme-paren^
			((reference-unit/sig "sparen.ss") paren)]
	  [paren : mred:paren^ ((reference-unit/sig "paren.ss"))]
	  [path-utils : mred:path-utils^ ((reference-unit/sig "fileutil.ss"))]
	  [icon : mred:icon^ ((reference-unit/sig "icon.ss") constants)]
	  [menu : mred:menu^ ((reference-unit/sig "menu.ss") (core function@))]
	  [edit : mred:edit^ 
		((reference-unit/sig "edit.ss")
		 connections finder path-utils mode
		 scheme-paren keymap icon preferences gui-utils
		 (core function@))]
	  [gui-utils : mred:gui-utils^
		     ((reference-unit/sig "guiutils.ss")
		      frame container canvas edit
		      (core function@) trigger)]
	  [finder : mred:finder^
		  ((reference-unit/sig "finder.ss")
		   container preferences
		   gui-utils edit canvas
		   (core string@) (core function@) (core file@))]
	  [group : mred:group^ 
		 ((reference-unit/sig "group.ss")
		  preferences editor-frame gui-utils
		  exit autosave handler application
		  (core function@))]
	  [canvas : mred:canvas^ 
		  ((reference-unit/sig "canvas.ss")
		   container edit (core file@))]
	  [panel : mred:panel^ ((reference-unit/sig "panel.ss")
				container canvas (core function@))]
	  [frame : mred:frame^ 
		 ((reference-unit/sig "frame.ss")
		  preferences edit container canvas icon
		  menu group finder find-string handler exit autosave
		  panel gui-utils application
		  (core function@) (core file@))]
	  [find-string : mred:find-string^ 
		       ((reference-unit/sig "findstr.ss")
			container canvas edit frame)]
	  [editor-frame : mred:editor-frame^ 
			((reference-unit/sig "edframe.ss")
			 preferences edit frame container
			 canvas find-string icon menu group
			 finder handler exit autosave gui-utils
			 (core function@) (core file@))]
	  [project : mred:project^ 
		   ((reference-unit/sig "project.ss")
		    group container gui-utils exit finder
		    frame handler (core file@) (core function@))]
	  [console : mred:console^ 
		   ((reference-unit/sig "console.ss")
		    preferences edit frame canvas find-string
		    exit finder handler gui-utils scheme-mode
		    scheme-paren icon hyper-frame version application
		    (core function@) (core string@) (core pretty-print@)
		    trigger)]
	  [scheme-mode : mred:scheme-mode^ 
		       ((reference-unit/sig "ssmode.ss")
			preferences application container
			mode match-cache paren scheme-paren icon
			handler keymap (core string@))]
	  [url : mred:url^ ((reference-unit/sig "url.ss")
			    (core function@) gui-utils
			    exn (core file@))]
	  [hyper-dialog : mred:hyper-dialog^ 
			((reference-unit/sig "hyprdial.ss")
			 hyper-edit (core file@))]
	  [hyper-edit : mred:hyper-edit^ 
		      ((reference-unit/sig "hypredit.ss")
		       edit hyper-dialog html url
		       (core file@) (core string@))]
	  [hyper-frame : mred:hyper-frame^ 
		       ((reference-unit/sig "hyprfram.ss")
			hyper-edit hyper-dialog container
			frame canvas group find-string
			preferences handler)]
	  [html : mred:html^ ((reference-unit/sig "html.ss")
			      url (core file@)
			      (core string@))])
    (export (open constants)
	    (open version)
	    (open (exn : mred:exn-external^))
	    (open connections) (open container) (open preferences)
	    (open autoload) (open autosave) (open exit)
	    (open gui-utils) (open console) (open path-utils)
	    (open finder)
	    (open find-string) (open edit) (open canvas)
	    (open frame) (open editor-frame)
	    (open group) (open handler) (open icon) (open keymap)
	    (open match-cache) (open menu) (open mode) 
	    (open panel) (open paren) (open project)
	    (open scheme-paren) (open scheme-mode) 
	    (open hyper-edit) (open hyper-dialog) (open hyper-frame)
	    (open url)))
