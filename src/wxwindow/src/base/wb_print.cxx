/*
 * File:        wx_print.cc
 * Purpose:     Printer implementation (MSW)
 * Author:      Julian Smart
 * Created:     1995
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation "wx_print.h"
#endif

// For some reason, this must be defined for common dialogs to work.
#ifdef __WATCOMC__
#define INCLUDE_COMMDLG_H	1
#endif

#include "common.h"

#endif

#if USE_PRINTING_ARCHITECTURE
#if USE_COMMON_DIALOGS

#define WINDOWS_PRINTING    (wxTheApp->GetPrintMode() == wxPRINT_WINDOWS)

#if !defined(_MSC_VER)
# include "wx_utils.h"
# include "wx_dc.h"
# include "wx_main.h"
# include "wx_dialg.h"
# include "wx_cmdlg.h"
# include "wx_frame.h"
# include "wx_messg.h"
#endif

# include "wx_print.h"

#include <stdlib.h>

#ifdef wx_msw

#include "wx_pdf.h"

#include <commdlg.h>

#ifndef WIN32
#include <print.h>
#endif

#if !defined(APIENTRY)	// NT defines APIENTRY, 3.x not
#define APIENTRY FAR PASCAL
#endif
 
#ifdef WIN32
#define _EXPORT /**/
#else
#define _EXPORT _export
typedef signed short int SHORT ;
#endif
 
#if !defined(WIN32)	// 3.x uses FARPROC for dialogs
#define DLGPROC FARPROC
#endif

LONG APIENTRY _EXPORT wxAbortProc(HDC hPr, int Code);
#endif
 // End wx_msw

wxPrintPaperDatabase *wxThePrintPaperDatabase = NULL;

IMPLEMENT_DYNAMIC_CLASS(wxPrintDialog, wxDialogBox)

wxPrintDialog::wxPrintDialog(void):
 wxDialogBox()
{
  dialogParent = NULL;
  printerDC = NULL;
  destroyDC = TRUE;
  deviceName = NULL;
  driverName = NULL;
  portName = NULL;
}

wxPrintDialog::wxPrintDialog(wxWindow *p, wxPrintData *data):
 wxDialogBox()
{
  Create(p, data);
}

Bool wxPrintDialog::Create(wxWindow *p, wxPrintData *data)
{
  dialogParent = p;
  printerDC = NULL;
  destroyDC = TRUE;
  deviceName = NULL;
  driverName = NULL;
  portName = NULL;

  if (data)
    printData = (*data);

#ifdef wx_msw
  if (WINDOWS_PRINTING)
    ((PRINTDLG *)printData.printData)->hwndOwner=p ? p->GetHWND() : (HANDLE)NULL;
#endif
  return TRUE;
}

wxPrintDialog::~wxPrintDialog(void)
{
  if (destroyDC && printerDC)
    delete printerDC;
  if (deviceName) delete[] deviceName;
  if (driverName) delete[] driverName;
  if (portName) delete[] portName;
}

#ifdef wx_msw
static BOOL do_print(void *data, HWND parent)
{
  PRINTDLG *p = (PRINTDLG *)data;
  p->hwndOwner = parent;

  return PrintDlg(p);
}
#endif

Bool wxPrintDialog::Show(Bool flag)
{
  if (!flag)
    return FALSE;
#ifdef wx_msw    
  if (WINDOWS_PRINTING)
  {
    if (wxPrimitiveDialog(do_print, printData.printData , 0) != 0 )
    {
      wxPrinterDC *pdc = new wxPrinterDC(((PRINTDLG *)printData.printData)->hDC);
      printerDC = pdc;
      return TRUE;
    }
    else
      return FALSE;
  }
#endif

  return FALSE;
}

wxDC *wxPrintDialog::GetPrintDC(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    if (printerDC)
    {
      destroyDC = FALSE;
      return printerDC;
    }
    else
      return NULL;
  }
  else
#endif
  {
    return new wxPostScriptDC(wxGetThePrintSetupData()->GetPrinterFile(), FALSE, NULL);
  }
}

/*
 * Print data
 */

IMPLEMENT_DYNAMIC_CLASS(wxPrintData, wxObject)

wxPrintData::wxPrintData(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = new PRINTDLG;
    printData = (void *)pd;
  
    pd->Flags=PD_RETURNDC | PD_NOSELECTION | PD_NOPAGENUMS;
    pd->lStructSize = sizeof( PRINTDLG );
    pd->hwndOwner=(HANDLE)NULL;
    pd->hDevMode=(HANDLE)NULL;
    pd->hDevNames=(HANDLE)NULL;
    pd->nFromPage=0;
    pd->nToPage=0;
    pd->nMinPage=0;
    pd->nMaxPage=0;
    pd->nCopies=1;
    pd->hInstance=(HINSTANCE)NULL;
  }
  else
#endif
  {
    printFromPage = 0;
    printToPage = 0;
    printMinPage = 0;
    printMaxPage = 0;
    printNoCopies = 1;
    printAllPages = FALSE;
    printCollate = FALSE;
    printToFile = FALSE;
    printEnableSelection = FALSE;
    printEnablePageNumbers = TRUE;
    printEnablePrintToFile = TRUE;
    printEnableHelp = FALSE;
    printSetupDialog = FALSE;
  }
}

wxPrintData::~wxPrintData(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    delete pd;
  }
  else
#endif
  {
  }
}

int wxPrintData::GetFromPage(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
	if (!(pd->Flags & (PD_PAGENUMS | PD_SELECTION)))
	  return 1;
	else
      return pd->nFromPage;
  }
  else
#endif
  {
    return printFromPage;
  }
}

int wxPrintData::GetToPage(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (!(pd->Flags & (PD_PAGENUMS | PD_SELECTION)))
	  return 32000;
	else
      return pd->nToPage;
  }
  else
#endif
  {
    return printToPage;
  }
}

int wxPrintData::GetMinPage(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return pd->nMinPage;
  }
  else
#endif
  {
    return printMinPage;
  }
}

int wxPrintData::GetMaxPage(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return pd->nMaxPage;
  }
  else
#endif
  {
    return printMaxPage;
  }
}

int wxPrintData::GetNoCopies(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return pd->nCopies;
  }
  else
#endif
  {
    return printNoCopies;
  }
}

Bool wxPrintData::GetAllPages(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_ALLPAGES) == PD_ALLPAGES);
  }
  else
#endif
  {
    return printAllPages;
  }
}

Bool wxPrintData::GetPrintToFile(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_PRINTTOFILE) == PD_PRINTTOFILE);
  }
  else
#endif
  {
    return printToFile;
  }
}

Bool wxPrintData::GetCollate(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_COLLATE) == PD_ALLPAGES);
  }
  else
#endif
  {
    return printCollate;
  }
}

/*
char *wxPrintData::GetDeviceName(void)
{
  return NULL;
}

char *wxPrintData::GetDriverName(void)
{
  return NULL;
}

char *wxPrintData::GetPortName(void)
{
  return NULL;
}

void wxPrintData::SetDeviceName(char *)
{
}

void wxPrintData::SetDriverName(char *)
{
}

void wxPrintData::SetPortName(char *)
{
}

*/

void wxPrintData::SetFromPage(int p)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    pd->nFromPage = (UINT)p;
  }
  else
#endif
  {
    printFromPage = p;
  }
}

void wxPrintData::SetToPage(int p)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    pd->nToPage = (UINT)p;
  }
  else
#endif
  {
    printToPage = p;
  }
}

void wxPrintData::SetMinPage(int p)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    pd->nMinPage = (UINT)p;
  }
  else
#endif
  {
    printMinPage = p;
  }
}

void wxPrintData::SetMaxPage(int p)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    pd->nMaxPage = (UINT)p;
  }
  else
#endif
  {
    printMaxPage = p;
  }
}

void wxPrintData::SetNoCopies(int c)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    pd->nCopies = (UINT)c;
  }
  else
#endif
  {
    printNoCopies = c;
  }
}

void wxPrintData::SetAllPages(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
      pd->Flags |= PD_ALLPAGES;
    }
    else
    {
      if (pd->Flags & PD_ALLPAGES)
        pd->Flags -= PD_ALLPAGES;
    }
  }
  else
#endif
  {
    printAllPages = flag;
  }
}

void wxPrintData::SetCollate(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
      pd->Flags |= PD_COLLATE;
    }
    else
    {
      if (pd->Flags & PD_COLLATE)
        pd->Flags -= PD_COLLATE;
    }
  }
  else
#endif
  {
    printCollate = flag;
  }
}

void wxPrintData::SetPrintToFile(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
      pd->Flags |= PD_PRINTTOFILE;
    }
    else
    {
      if (pd->Flags & PD_PRINTTOFILE)
        pd->Flags -= PD_PRINTTOFILE;
    }
  }
  else
#endif
  {
    printToFile = flag;
  }
}

void wxPrintData::EnablePrintToFile(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
      if (pd->Flags & PD_DISABLEPRINTTOFILE)
        pd->Flags -= PD_DISABLEPRINTTOFILE;
    }
    else
      pd->Flags |= PD_DISABLEPRINTTOFILE;
  }
  else
#endif
  {
    printEnablePrintToFile = flag;
  }
}

Bool wxPrintData::GetEnablePrintToFile(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_DISABLEPRINTTOFILE) != PD_DISABLEPRINTTOFILE);
  }
  else
#endif
  {
    return printEnablePrintToFile;
  }
}

void wxPrintData::EnableSelection(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
      if (pd->Flags & PD_NOSELECTION)
        pd->Flags -= PD_NOSELECTION;
    }
    else
    {
      pd->Flags |= PD_NOSELECTION;
    }
  }
  else
#endif
  {
    printEnableSelection = flag;
  }
}

Bool wxPrintData::GetEnableSelection(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_NOSELECTION) != PD_NOSELECTION);
  }
  else
#endif
  {
    return printEnableSelection;
  }
}

void wxPrintData::EnablePageNumbers(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
     if (pd->Flags & PD_NOPAGENUMS)
       pd->Flags -= PD_NOPAGENUMS;
    }
    else
    {
      pd->Flags |= PD_NOPAGENUMS;
    }
  }
  else
#endif
  {
    printEnablePageNumbers = flag;
  }
}

Bool wxPrintData::GetEnablePageNumbers(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_NOPAGENUMS) != PD_NOPAGENUMS);
  }
  else
#endif
  {
    return printEnablePageNumbers;
  }
}

void wxPrintData::EnableHelp(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
      pd->Flags |= PD_SHOWHELP;
    }
    else if (pd->Flags & PD_SHOWHELP)
      pd->Flags -= PD_SHOWHELP;
  }
  else
#endif
  {
    printEnableHelp = flag;
  }
}

Bool wxPrintData::GetEnableHelp(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_SHOWHELP) == PD_SHOWHELP);
  }
  else
#endif
  {
    return printEnableHelp;
  }
}

void wxPrintData::SetSetupDialog(Bool flag)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    if (flag)
    {
      pd->Flags |= PD_PRINTSETUP;
    }
    else if (pd->Flags & PD_PRINTSETUP)
    {
      pd->Flags -= PD_PRINTSETUP;
    }
  }
  else
#endif
  {
    printSetupDialog = flag;
  }
}

Bool wxPrintData::GetSetupDialog(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    return ((pd->Flags & PD_PRINTSETUP) == PD_PRINTSETUP);
  }
  else
#endif
  {
    return printSetupDialog;
  }
}

void wxPrintData::operator=(const wxPrintData& data)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    PRINTDLG *pd = (PRINTDLG *)printData;
    pd->lStructSize = ((PRINTDLG *)data.printData)->lStructSize;
    pd->hwndOwner = ((PRINTDLG *)data.printData)->hwndOwner;
    pd->hDevMode = ((PRINTDLG *)data.printData)->hDevMode;
    pd->Flags = ((PRINTDLG *)data.printData)->Flags;
    pd->nFromPage = ((PRINTDLG *)data.printData)->nFromPage;
    pd->nToPage = ((PRINTDLG *)data.printData)->nToPage;
    pd->nMinPage = ((PRINTDLG *)data.printData)->nMinPage;
    pd->nMaxPage = ((PRINTDLG *)data.printData)->nMaxPage;
    pd->nCopies = ((PRINTDLG *)data.printData)->nCopies;
    pd->hInstance = ((PRINTDLG *)data.printData)->hInstance;
  }
  else
#endif
  {
    printFromPage = data.printFromPage;
    printToPage = data.printToPage;
    printMinPage = data.printMinPage;
    printMaxPage = data.printMaxPage;
    printNoCopies = data.printNoCopies;
    printAllPages = data.printAllPages;
    printCollate = data.printCollate;
    printToFile = data.printToFile;
    printEnableSelection = data.printEnableSelection;
    printEnablePageNumbers = data.printEnablePageNumbers;
    printEnableHelp = data.printEnableHelp;
    printEnablePrintToFile = data.printEnablePrintToFile;
    printSetupDialog = data.printSetupDialog;
  }
}

/*
 * Printer
 */
 
IMPLEMENT_DYNAMIC_CLASS(wxPrinter, wxObject)

wxPrinter::wxPrinter(wxPrintData *data)
{
  currentPrintout = NULL;
  abortWindow = NULL;
  abortIt = FALSE;
  if (data)
    printData = (*data);
#ifdef wx_msw
  lpAbortProc = MakeProcInstance((FARPROC) wxAbortProc, wxhInstance);
#endif
}

wxWindow *wxPrinter::abortWindow = NULL;
Bool wxPrinter::abortIt = FALSE;

wxPrinter::~wxPrinter(void)
{
#ifdef wx_msw
  FreeProcInstance(lpAbortProc);
#endif
}

Bool wxPrinter::Print(wxWindow *parent, wxPrintout *printout, Bool prompt)
{
  abortIt = FALSE;
  abortWindow = NULL;

  if (!printout)
    return FALSE;
    
  printout->SetIsPreview(FALSE);
  printout->OnPreparePrinting();

  // Get some parameters from the printout, if defined
  int fromPage, toPage;
  int minPage, maxPage;
  printout->GetPageInfo(&minPage, &maxPage, &fromPage, &toPage);

  if (maxPage == 0)
    return FALSE;

  printData.SetMinPage(minPage);
  printData.SetMaxPage(maxPage);
  if (fromPage != 0)
    printData.SetFromPage(fromPage);
  if (toPage != 0)
    printData.SetToPage(toPage);

  if (minPage != 0)
  {
    printData.EnablePageNumbers(TRUE);
    if (printData.GetFromPage() < printData.GetMinPage())
      printData.SetFromPage(printData.GetMinPage());
    else if (printData.GetFromPage() > printData.GetMaxPage())
      printData.SetFromPage(printData.GetMaxPage());
    if (printData.GetToPage() > printData.GetMaxPage())
      printData.SetToPage(printData.GetMaxPage());
    else if (printData.GetToPage() < printData.GetMinPage())
      printData.SetToPage(printData.GetMinPage());
  }
  else
    printData.EnablePageNumbers(FALSE);
  
  // Create a suitable device context  
  wxDC *dc = NULL;
  if (prompt)
  {
    wxPrintDialog  *dialog = new wxPrintDialog(parent, &printData);
    if (dialog->Show(TRUE))
    {
      dc = dialog->GetPrintDC();
      printData = dialog->GetPrintData();
    }
  }
  else
  {
#ifdef wx_msw
    if (WINDOWS_PRINTING)
      dc = new wxPrinterDC(NULL, NULL, NULL, FALSE);
    else
#endif
    {
      dc = new wxPostScriptDC(wxGetThePrintSetupData()->GetPrinterFile(), FALSE, NULL);
    }
  }

  // May have pressed cancel.
  if (!dc || !dc->Ok())
  {
    if (dc) delete dc;
    return FALSE;
  }
  
  int logPPIScreenX = 0;
  int logPPIScreenY = 0;
  int logPPIPrinterX = 0;
  int logPPIPrinterY = 0;

#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    HDC hdc = ::GetDC(NULL);
    logPPIScreenX = ::GetDeviceCaps(hdc, LOGPIXELSX);
    logPPIScreenY = ::GetDeviceCaps(hdc, LOGPIXELSY);
    ::ReleaseDC(NULL, hdc);

    logPPIPrinterX = ::GetDeviceCaps(dc->cdc, LOGPIXELSX);
    logPPIPrinterY = ::GetDeviceCaps(dc->cdc, LOGPIXELSY);
  }
  else
#endif
  {
    // Correct values for X/PostScript?
    logPPIScreenX = 100;
    logPPIScreenY = 100;
    logPPIPrinterX = 100;
    logPPIPrinterY = 100;
  }

  printout->SetPPIScreen(logPPIScreenX, logPPIScreenY);
  printout->SetPPIPrinter(logPPIPrinterX, logPPIPrinterY);

  // Set printout parameters  
  printout->SetDC(dc);

  float w, h;
  dc->GetSize(&w, &h);
  printout->SetPageSizePixels((int)w, (int)h);
  dc->GetSizeMM(&w, &h);
  printout->SetPageSizeMM((int)w, (int)h);

  // Create an abort window
  wxBeginBusyCursor();

#ifdef wx_msw
  wxWindow *win = CreateAbortWindow(parent, printout);
  wxYield();
  ::SetAbortProc(dc->cdc, lpAbortProc);
  
  if (!win)
  {
    wxEndBusyCursor();
    wxMessageBox("Sorry, could not create an abort dialog.", "Print Error", wxOK, parent);
    delete dc;
  }
  abortWindow = win;
  abortWindow->Show(TRUE);
  wxYield();
#endif

  printout->OnBeginPrinting();
  
  Bool keepGoing = TRUE;

  int copyCount;
  for (copyCount = 1; copyCount <= printData.GetNoCopies(); copyCount ++)
  {
    if (!printout->OnBeginDocument(printData.GetFromPage(), printData.GetToPage()))
    {
      wxEndBusyCursor();
      wxMessageBox("Could not start printing.", "Print Error", wxOK, parent);
      break;
    }
    if (abortIt)
      break;

    int pn;
	int is_down = (printData.GetFromPage() > printData.GetToPage());
	int endpage = printData.GetToPage() + (is_down ? -1 : 1);
    for (pn = printData.GetFromPage(); keepGoing && (pn != endpage);
	     pn = (is_down ? pn - 1 : pn + 1))
    {
	  if (!printout->HasPage(pn)) {
		  if (!is_down)
			  break;
	  } else {
      if (abortIt)
      {
        keepGoing = FALSE;
        break;
      }
      else
      {
#ifdef wx_msw
//        int dcID = ::SaveDC(dc->cdc);
#endif
        dc->StartPage();
        printout->OnPrintPage(pn);
        dc->EndPage();
#ifdef wx_msw
//        ::RestoreDC(dc->cdc, dcID);
#endif
      }
	  }
    }
    printout->OnEndDocument();
  }

  printout->OnEndPrinting();

#ifdef wx_msw
  if (abortWindow)
  {
    abortWindow->Show(FALSE);
    delete abortWindow;
    abortWindow = NULL;
  }
#endif
  
  wxEndBusyCursor();

  delete dc;
  
  return TRUE;
}

Bool wxPrinter::PrintDialog(wxWindow *parent)
{
  wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
  return dialog->Show(TRUE);
}

static void wxAbortWindowCancel(wxButton& WXUNUSED(but), wxCommandEvent& WXUNUSED(event))
{
  wxPrinter::abortIt = TRUE;
  wxPrinter::abortWindow->Show(FALSE);
  delete wxPrinter::abortWindow;
  wxPrinter::abortWindow = NULL;
}

wxWindow *wxPrinter::CreateAbortWindow(wxWindow *parent, wxPrintout *WXUNUSED(printout))
{
  wxDialogBox *dialog = new wxDialogBox(parent, "Printing", 0, 0, 400, 400);
  (void) new wxMessage(dialog, "Please wait, printing...");
  dialog->NewLine();
  wxButton *button = new wxButton(dialog, (wxFunction) wxAbortWindowCancel, "Cancel");
  
  dialog->Fit();
  button->Centre(wxHORIZONTAL);

  dialog->Centre();
  return dialog;
}

Bool wxPrinter::Setup(wxWindow *parent)
{
  wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
  dialog->GetPrintData().SetSetupDialog(TRUE);
  return dialog->Show(TRUE);
}

void wxPrinter::ReportError(wxWindow *parent, wxPrintout *WXUNUSED(printout), char *message)
{
  wxMessageBox(message, "Printing Error", wxOK, parent);
}

wxPrintData &wxPrinter::GetPrintData(void)
{
  return printData;
}

/*
 * Printout class
 */
 
IMPLEMENT_ABSTRACT_CLASS(wxPrintout, wxObject)

wxPrintout::wxPrintout(char *title)
{
  printoutTitle = title ? copystring(title) : (char *)NULL;
  printoutDC = NULL;
  pageWidthMM = 0;
  pageHeightMM = 0;
  pageWidthPixels = 0;
  pageHeightPixels = 0;
  PPIScreenX = 0;
  PPIScreenY = 0;
  PPIPrinterX = 0;
  PPIPrinterY = 0;
  isPreview = FALSE;
}

wxPrintout::~wxPrintout(void)
{
  if (printoutTitle)
    delete[] printoutTitle;
}

Bool wxPrintout::OnBeginDocument(int WXUNUSED(startPage), int WXUNUSED(endPage))
{
  return GetDC()->StartDoc("Printing");
}

void wxPrintout::OnEndDocument(void)
{
  GetDC()->EndDoc();
}

void wxPrintout::OnBeginPrinting(void)
{
}

void wxPrintout::OnEndPrinting(void)
{
}

Bool wxPrintout::HasPage(int page)
{
  return (page == 1);
}

void wxPrintout::GetPageInfo(int *minPage, int *maxPage, int *fromPage, int *toPage)
{
  *minPage = 1;
  *maxPage = 32000;
  *fromPage = 1;
  *toPage = 1;
}

#ifdef wx_msw

/****************************************************************************

    FUNCTION: wxAbortProc()

    PURPOSE:  Processes messages for the Abort Dialog box

****************************************************************************/

LONG APIENTRY _EXPORT wxAbortProc(HDC WXUNUSED(hPr), int WXUNUSED(Code))
{
    MSG msg;

    if (!wxPrinter::abortWindow)              /* If the abort dialog isn't up yet */
        return(TRUE);

    /* Process messages intended for the abort dialog box */

    while (!wxPrinter::abortIt && PeekMessage(&msg, NULL, NULL, NULL, TRUE))
        if (!IsDialogMessage(wxPrinter::abortWindow->GetHWND(), &msg)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }

    /* bAbort is TRUE (return is FALSE) if the user has aborted */

    return (!wxPrinter::abortIt);
}
#endif

#endif
  // USE_COMMON_DIALOGS
#endif
  // End USE_PRINTING_ARCHITECTURE
