(module framework-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred"))

  (require "gui-utils-sig.ss"
	   "gui-utils-unit.ss"

           "test-sig.ss"
           "test-unit.ss"

	   "prefs-file-unit.ss"
	   "prefs-file-sig.ss"
           
           "framework-sig.ss"
	   "private/sig.ss"

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
	   framework-no-prefs@
	   frameworkc@)

  (define frameworkc@
    (compound-unit/sig
      (import [mred : mred^]
	      [test : framework:test^]
	      [pref-file : framework:prefs-file^]
	      [gui-utils : framework:gui-utils^])
      (link [application : framework:application^ (application@)]
	    [version : framework:version^ (version@)]
	    [color-model : framework:color-model^ (color-model@ )]
	    [exn : framework:exn^ (exn@)]
	    [exit : framework:exit^ (exit@ mred preferences gui-utils)]
	    [menu : framework:menu^ (menu@ mred preferences)]
	    [preferences : framework:preferences^
			 (preferences@ mred pref-file exn exit panel)]
	    [autosave : framework:autosave^ (autosave@ mred exit preferences)]
	    [handler : framework:handler^
		     (handler@ mred gui-utils finder group text preferences frame)] 
	    [keymap : framework:keymap^
		    (keymap@ mred preferences finder handler scheme-paren frame editor)]
	    [match-cache : framework:match-cache^ (match-cache@)]
	    [paren : framework:paren^ (paren@)]
	    [scheme-paren : framework:scheme-paren^ (scheme-paren@ paren)]
	    [path-utils : framework:path-utils^ (path-utils@)]
	    [icon : framework:icon^ (icon@ mred)]
	    [editor : framework:editor^
		    (editor@ mred autosave finder path-utils keymap icon
			     preferences text pasteboard frame gui-utils)]
	    [pasteboard : framework:pasteboard^ (pasteboard@ mred editor)]
	    [text : framework:text^
		  (text@ mred icon editor preferences keymap gui-utils color-model frame)]
	    [finder : framework:finder^ (finder@ mred preferences gui-utils keymap)]
	    [group : framework:group^ (group@ mred application frame preferences gui-utils text canvas)]
	    [canvas : framework:canvas^ (canvas@ mred preferences frame)]
	    [panel : framework:panel^ (panel@ icon mred)]
	    [frame : framework:frame^ 
		   (frame@ mred group preferences icon handler application panel gui-utils
			   exit finder keymap text pasteboard editor canvas menu)]
	    [scheme : framework:scheme^ 
		    (scheme@ mred preferences match-cache paren
			     scheme-paren icon keymap text editor frame)]
	    [main : framework:main^ (main@ mred preferences exit group)])
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
       (unit finder)
       (unit group)
       (unit canvas)
       (unit panel)
       (unit frame)
       (unit scheme)
       (unit main))))

  (define framework-no-prefs@
    (compound-unit/sig
      (import [mred : mred^]
	      [pref-file : framework:prefs-file^])
      (link [test : framework:test^ (framework:test@ mred)]
	    [gui-utils : framework:gui-utils^ (framework:gui-utils@ mred)]
	    [f : frameworkc^ (frameworkc@ mred test pref-file gui-utils)])
      (export
       (unit test)
       (unit gui-utils)
       (open f))))

  (define framework@
    (compound-unit/sig
      (import [mred : mred^])
      (link
       [prefs-file : framework:prefs-file^ (framework:prefs-file@)]
       [f : framework-no-prefs^ (framework-no-prefs@ mred prefs-file)])
      (export
       (unit prefs-file)
       (open f)))))
