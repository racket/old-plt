#include <X11/Intrinsic.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/Separator.h>
#include <X11/Xmu/Editres.h>
#include "gauge.h"

Widget toplevel, mainw, menubar;
Widget fileM, helpM, file, help, gauge;

void
QuitCB(Widget wid, XtPointer client_data, XtPointer call_data)
{
    exit(1);
}





void
main(int argc, char* argv[])
{
    XtAppContext app_context;
    Widget tmp;
    Arg args[16];
    int n;

    toplevel = XtAppInitialize(&app_context, "Gauge", 0, 0,
			       &argc, argv, 0, 0, 0);
/*    XtAddEventHandler(toplevel, 0, True, _XEditResCheckMessages, NULL); */

    mainw = XmCreateMainWindow(toplevel, "mainw", 0, 0);
    menubar = XmCreateMenuBar(mainw, "menubar", 0, 0);
    
    XtManageChild(tmp=XmCreateForm(mainw, "form", 0, 0));
    gauge = XtCreateManagedWidget("g1", xmGaugeWidgetClass, tmp, 0, 0);
    gauge = XtCreateManagedWidget("g2", xmGaugeWidgetClass, tmp, 0, 0);
    
    fileM = XmCreatePulldownMenu(menubar, "fileM", 0, 0);
    helpM = XmCreatePulldownMenu(menubar, "fileM", 0, 0);
    
    n = 0;
    XtSetArg(args[n], XmNsubMenuId, fileM); n++;
    XtManageChild(XmCreateCascadeButton(menubar, "file", args, n));
    
    n = 0;
    XtSetArg(args[n], XmNsubMenuId, helpM); n++;
    XtManageChild(tmp=XmCreateCascadeButton(menubar, "help", args, n));
    XtVaSetValues(menubar, 
		  XmNmenuHelpWidget, tmp,
		  NULL);


    XtManageChild(tmp=XmCreatePushButton(fileM, "open", 0, 0));
    XtManageChild(tmp=XmCreatePushButton(fileM, "save", 0, 0));
    XtManageChild(tmp=XmCreateSeparator(fileM, "s1", 0, 0));
    XtManageChild(tmp=XmCreatePushButton(fileM, "quit", 0, 0));
    XtAddCallback(tmp, XmNactivateCallback, QuitCB, 0);

    XtManageChild(tmp=XmCreatePushButton(helpM, "index", 0, 0));
    XtManageChild(tmp=XmCreatePushButton(helpM, "context", 0, 0));
    XtManageChild(tmp=XmCreateSeparator(helpM, "context", 0, 0));
    XtManageChild(tmp=XmCreatePushButton(helpM, "version", 0, 0));

    XtManageChild(menubar);
    XtManageChild(mainw);

    XtRealizeWidget(toplevel);
    XtAppMainLoop(app_context);
}
