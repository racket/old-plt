(module framework-unit mzscheme
  (require "gui-utils-sig.ss"
	   "gui-utils-unit.ss"

           "test-sig.ss"
           "test-unit.ss"
           
           "sig.ss"
           "private/application.ss"
	   "private/version.ss"
	   "private/color-model.ss"
	   "private/exn.ss"
	   "private/exit.ss"
	   "private/menu.ss"
	   "private/preferences.ss"
	   "private/autosave.ss"
	   "private/handler.ss" 
	   "private/keymap.ss"
	   "private/match-cache.ss"
	   "private/paren.ss"
	   "private/scheme-paren.ss"
	   "private/path-utils.ss"
	   "private/icon.ss"
	   "private/editor.ss"
	   "private/pasteboard.ss"
	   "private/text.ss"
	   
	   "private/finder.ss"
	   "private/group.ss"
	   "private/canvas.ss"
	   "private/panel.ss"
	   "private/frame.ss"
	   "private/scheme.ss"
	   "private/main.ss")

  (provide framework@
	   framework-prefs@
	   framework-small-part@)

  (define framework@
    (compound-unit/sig
      (import)
      (link
       [pref-file : framework:prefs-file^ (prefs-file@)]
       [f : framework^ (framework-prefs@ pref-file)])
      (export
       (open f))))


  (define framework-prefs@
    (compound-unit/sig
      (import [pref-file : framework:prefs-file^])
      (link [keys : framework:keys^ (keys@)]
	    [test : framework:test^ (test@ keys)]
	    [f : frameworkc^ (framework-???@ keys test pref-file)])
      (export
       (unit keys)
       (unit test)
       (open f))))

  (define framework-small-part@
    (compound-unit/sig
      (import [keys : framework:keys^]
	      [test : framework:test^]
	      [pref-file : framework:prefs-file^])
      (link [application : framework:application^ (app@)]
	    [version : framework:version^ (version@)]
	    [color-model : framework:color-model^ (color-model@ )]
	    [exn : framework:exn^ (exn@)]
	    [exit : framework:exit^ (exit@ preferences gui-utils)]
	    [menu : framework:menu^ (menu@ preferences)]
	    [preferences : framework:preferences^ (prefs@ pref-file exn exit panel)]
	    [autosave : framework:autosave^ (autosave@ exit preferences)]
	    [handler : framework:handler^
		     (handler@ gui-utils finder group  text preferences frame)] 
	    [keymap : framework:keymap^
		    (keymap@ keys preferences finder handler scheme-paren frame)]
	    [match-cache : framework:match-cache^ (mcache@)]
	    [paren : framework:paren^ (paren@)]
	    [scheme-paren : framework:scheme-paren^ (sparen@ paren)]
	    [path-utils : framework:path-utils^ (fileutil@)]
	    [icon : framework:icon^ (icon@)]
	    [editor : framework:editor^
		    (editor@ autosave finder path-utils keymap icon
			     preferences text pasteboard frame gui-utils)]
	    [pasteboard : framework:pasteboard^ (pasteboard@ editor)]
	    [text : framework:text^
		  (text@ icon editor preferences keymap gui-utils color-model frame)]
	    [gui-utils : framework:gui-utils^ (gui-utils@)]
	    [finder : framework:finder^ (finder@ mred preferences gui-utils keymap)]
	    [group : framework:group^ (group@ application frame preferences)]
	    [canvas : framework:canvas^ (canvas@ preferences frame)]
	    [panel : framework:panel^ (panel@)]
	    [frame : framework:frame^ 
		   (frame@ group preferences icon handler application panel gui-utils
			   exit finder keymap text pasteboard editor canvas menu)]
	    [scheme : framework:scheme^ 
		    (scheme@ preferences match-cache paren
			     scheme-paren icon keymap text frame)]
	    [main : framework:main^ (main@ preferences exit group)])
      (export
       (unit menu)
       (unit application)
       (unit version)
       (unit color-model)
       (unit exn)
       (unit exit)
       (unit preferences)
       (unit autosave)
       (unit handler) 
       (unit keymap)
       (unit match-cache)
       (unit paren)
       (unit scheme-paren)
       (unit path-utils)
       (unit icon)
       (unit editor)
       (unit pasteboard)
       (unit text)
       (unit gui-utils)
       (unit finder)
       (unit group)
       (unit canvas)
       (unit panel)
       (unit frame)
       (unit scheme)
       (unit main)))))
