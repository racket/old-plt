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
  else
#endif
  {
    if (printData.GetSetupDialog())
    {
      wxGenericPrintSetupDialog *genericPrintSetupDialog =
          new wxGenericPrintSetupDialog(dialogParent, *wxThePrintSetupData);
      genericPrintSetupDialog->Show(TRUE);
      if (wxGenericPrintSetupDialog::printSetupDialogCancelled)
      {
        return FALSE;
      }
      else
      {
        *wxThePrintSetupData = genericPrintSetupDialog->printData;

        // Dialog not deleted if OK pressed, to give us the opportunity
        // to copy the printData.

        delete genericPrintSetupDialog;
        return FALSE;
      }
    }
    else
    {
      wxGenericPrintDialog *genericPrintDialog = new wxGenericPrintDialog(dialogParent, printData);
      genericPrintDialog->Show(TRUE);
      if (wxGenericPrintDialog::printDialogCancelled)
      {
        return FALSE;
      }
      else
      {
        printData = genericPrintDialog->printData;
        delete genericPrintDialog;
        return TRUE;
      }
    }
  }
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
    return new wxPostScriptDC(wxThePrintSetupData->GetPrinterFile(), FALSE, NULL);
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
      dc = new wxPostScriptDC(wxThePrintSetupData->GetPrinterFile(), FALSE, NULL);
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

#if 0

/*
 * Preview canvas
 */
 
IMPLEMENT_CLASS(wxPreviewCanvas, wxCanvas)

wxPreviewCanvas::wxPreviewCanvas(wxPrintPreview *preview, wxWindow *parent, int x, int y, int w, int h,
    long style, char *name):
 wxCanvas(parent, x, y, w, h, style, name)
{
  printPreview = preview;
  if (!previewBackgroundBrush)
    previewBackgroundBrush = wxTheBrushList->FindOrCreateBrush("THISTLE", wxSOLID);
  SetBackground(previewBackgroundBrush);
  SetScrollbars(40, 40, 50, 50, 8, 8);
}

wxBrush *wxPreviewCanvas::previewBackgroundBrush = NULL;

wxPreviewCanvas::~wxPreviewCanvas(void)
{
}

void wxPreviewCanvas::OnPaint(void)
{
  wxCanvas::OnPaint();

  if (printPreview)
    printPreview->PaintPage(this);
}

void wxPreviewCanvas::OnEvent(wxMouseEvent& WXUNUSED(event))
{
}

void wxPreviewCanvas::OnChar(wxKeyEvent& WXUNUSED(event))
{
}

/*
 * Preview control bar
 */
 
IMPLEMENT_CLASS(wxPreviewControlBar, wxPanel)

wxPreviewControlBar::wxPreviewControlBar(wxPrintPreview *preview, long buttons,
    wxWindow *parent, int x, int y, int w, int h,
    long style, char *name):
  wxPanel(parent, x, y, w, h, style, name)
{
  printPreview = preview;
  closeButton = NULL;
  nextPageButton = NULL;
  previousPageButton = NULL;
  printButton = NULL;
  zoomControl = NULL;
  buttonFlags = buttons;
}

wxFont *wxPreviewControlBar::buttonFont = NULL;

wxPreviewControlBar::~wxPreviewControlBar(void)
{
}

void wxPreviewControlBar::OnPaint(void)
{
  wxPanel::OnPaint();

  int w, h;
  GetSize(&w, &h);
  wxDC *dc = GetDC();
  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxTRANSPARENT_BRUSH);
  DrawLine(0, h-1, w, h-1);
}

static void wxPreviewCloseFunc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPreviewFrame *frame = (wxPreviewFrame *)controlBar->GetParent();
  if (frame->GetEventHandler()->OnClose())
    delete frame;
}

static void wxPreviewPrintFunc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPrintPreview *preview = controlBar->GetPrintPreview();
  preview->Print(TRUE);
}

static void wxPreviewNextFunc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPrintPreview *preview = controlBar->GetPrintPreview();
  if (preview)
  {
    int currentPage = preview->GetCurrentPage();
    if ((preview->GetMaxPage() > 0) &&
        (currentPage < preview->GetMaxPage()) &&
        preview->GetPrintout()->HasPage(currentPage + 1))
    {
      preview->SetCurrentPage(currentPage + 1);
    }
  }
}

static void wxPreviewPreviousFunc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPrintPreview *preview = controlBar->GetPrintPreview();
  if (preview)
  {
    int currentPage = preview->GetCurrentPage();
    if ((preview->GetMinPage() > 0) &&
        (currentPage > preview->GetMinPage()) &&
        preview->GetPrintout()->HasPage(currentPage - 1))
    {
      preview->SetCurrentPage(currentPage - 1);
    }
  }
}

static void wxPreviewZoomFunc(wxChoice& but, wxCommandEvent& WXUNUSED(event))
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  int zoom = controlBar->GetZoomControl();
  if (controlBar->GetPrintPreview())
    controlBar->GetPrintPreview()->SetZoom(zoom);
}

void wxPreviewControlBar::CreateButtons(void)
{
  SetSize(0, 0, 400, 40);

  SetLabelPosition(wxVERTICAL);
  if (!buttonFont)
    buttonFont = wxTheFontList->FindOrCreateFont(11, wxSWISS, wxNORMAL, wxBOLD);
  SetButtonFont(buttonFont);

  int buttonWidth = 65;
  int buttonHeight = 25;

  closeButton = new wxButton(this, (wxFunction)wxPreviewCloseFunc, "Close", -1, -1, buttonWidth, buttonHeight);
  
  if (buttonFlags & wxPREVIEW_PRINT)
    printButton =  new wxButton(this, (wxFunction)wxPreviewPrintFunc, "Print...", -1, -1, buttonWidth, buttonHeight);
  if (buttonFlags & wxPREVIEW_PREVIOUS)
    previousPageButton = new wxButton(this, (wxFunction)wxPreviewPreviousFunc, "<<", -1, -1, buttonWidth, buttonHeight);
  if (buttonFlags & wxPREVIEW_NEXT)
    nextPageButton = new wxButton(this, (wxFunction)wxPreviewNextFunc, ">>", -1, -1, buttonWidth, buttonHeight);

  char *choices[] = { "10%", "20%", "25%", "30%", "35%", "40%", "45%", "50%", "55%", "60%",
    "65%", "70%", "75%", "80%", "85%", "90%", "95%", "100%", "110%", "120%", "150%", "200%" };
  int n = 22;
  if (buttonFlags & wxPREVIEW_ZOOM)
  {
    zoomControl = new wxChoice(this, (wxFunction)wxPreviewZoomFunc, NULL, -1, -1, 100, -1, n, (char **)choices); /* MATTHEW: BC */
    SetZoomControl(printPreview->GetZoom());
  }

  closeButton->SetDefault();
}

void wxPreviewControlBar::SetZoomControl(int zoom)
{
  char buf[20];
  sprintf(buf, "%d%%", zoom);
  if (zoomControl)
    zoomControl->SetStringSelection(buf);
}

int wxPreviewControlBar::GetZoomControl(void)
{
  char buf[20];
  if (zoomControl && zoomControl->GetStringSelection())
  {
    strcpy(buf, zoomControl->GetStringSelection());
    buf[strlen(buf) - 1] = 0;
    return (int)atoi(buf);
  }
  else return 0;
}

/*
 * Preview frame
 */

IMPLEMENT_CLASS(wxPreviewFrame, wxFrame)

wxPreviewFrame::wxPreviewFrame(wxPrintPreview *preview, wxFrame *parent, char *title,
    int x, int y, int w, int h, long style, char *name):
 wxFrame(parent, title, x, y, w, h, style, name)
{
  printPreview = preview;
  controlBar = NULL;
  previewCanvas = NULL;
}

wxPreviewFrame::~wxPreviewFrame(void)
{
}

Bool wxPreviewFrame::OnClose(void)
{
  MakeModal(FALSE);
  
  // Need to delete the printout and the print preview
  wxPrintout *printout = printPreview->GetPrintout();
  if (printout)
  {
    delete printout;
    printPreview->SetPrintout(NULL);
    printPreview->SetCanvas(NULL);
    printPreview->SetFrame(NULL);
  }
  delete printPreview;
  return TRUE;
}

void wxPreviewFrame::Initialize(void)
{
  CreateStatusLine();
  
  CreateCanvas();
  CreateControlBar();

  printPreview->SetCanvas(previewCanvas);
  printPreview->SetFrame(this);

  // Set layout constraints here

  // Control bar constraints
  wxLayoutConstraints *c1 = new wxLayoutConstraints;
//  int w, h;
//  controlBar->GetSize(&w, &h);
  int h;
#ifdef wx_msw
  h = 40;
#else
  h = 60;
#endif

  c1->left.SameAs       (this, wxLeft);
  c1->top.SameAs        (this, wxTop);
  c1->right.SameAs      (this, wxRight);
  c1->height.Absolute   (h);

  controlBar->SetConstraints(c1);

  // Canvas constraints
  wxLayoutConstraints *c2 = new wxLayoutConstraints;

  c2->left.SameAs       (this, wxLeft);
  c2->top.Below         (controlBar);
  c2->right.SameAs      (this, wxRight);
  c2->bottom.SameAs     (this, wxBottom);

  previewCanvas->SetConstraints(c2);

  SetAutoLayout(TRUE);

  MakeModal(TRUE);

#ifdef wx_x
  Layout();
#endif
}

void wxPreviewFrame::CreateCanvas(void)
{
  previewCanvas = new wxPreviewCanvas(printPreview, this);
}

void wxPreviewFrame::CreateControlBar(void)
{
  long buttons = wxPREVIEW_DEFAULT;
  if (printPreview->GetPrintoutForPrinting())
    buttons |= wxPREVIEW_PRINT;
    
  controlBar = new wxPreviewControlBar(printPreview, buttons, this, 0, 0, 400, 40);
  controlBar->CreateButtons();
}
 
/*
 * Print preview
 */

IMPLEMENT_CLASS(wxPrintPreview, wxObject)

wxPrintPreview::wxPrintPreview(wxPrintout *printout, wxPrintout *printoutForPrinting, wxPrintData *data)
{
  previewPrintout = printout;
  if (previewPrintout)
    previewPrintout->SetIsPreview(TRUE);
    
  printPrintout = printoutForPrinting;
  if (data)
    printData = (*data);

  previewCanvas = NULL;
  previewFrame = NULL;
  previewBitmap = NULL;
  currentPage = 1;
  currentZoom = 30;
  topMargin = 40;
  leftMargin = 40;
  pageWidth = 0;
  pageHeight = 0;

  printout->OnPreparePrinting();

  // Get some parameters from the printout, if defined
  int selFrom, selTo;
  printout->GetPageInfo(&minPage, &maxPage, &selFrom, &selTo);

  DetermineScaling();
}

wxPrintPreview::~wxPrintPreview(void)
{
  if (previewPrintout)
    delete previewPrintout;
  if (previewBitmap)
    delete previewBitmap;
  if (printPrintout)
    delete printPrintout;
}

Bool wxPrintPreview::SetCurrentPage(int pageNum)
{
  if (currentPage == pageNum)
    return TRUE;

  currentPage = pageNum;
  if (previewBitmap)
  {
    delete previewBitmap;
    previewBitmap = NULL;
  }

  if (previewCanvas)
  {
    RenderPage(pageNum);
    previewCanvas->Refresh();
  }
  return TRUE;
}

void wxPrintPreview::SetPrintout(wxPrintout *printout)
{
  previewPrintout = printout;
}

void wxPrintPreview::SetFrame(wxFrame *frame)
{
  previewFrame = frame;
}
  
void wxPrintPreview::SetCanvas(wxCanvas *canvas)
{
  previewCanvas = canvas;
}

Bool wxPrintPreview::PaintPage(wxCanvas *canvas)
{
  DrawBlankPage(canvas);

  if (!previewBitmap)
    RenderPage(currentPage);
    
  if (!previewBitmap)
    return FALSE;

  if (!canvas)
    return FALSE;

  int canvasWidth, canvasHeight;
  canvas->GetSize(&canvasWidth, &canvasHeight);
  
  float zoomScale = (float)((float)currentZoom/(float)100);
  float actualWidth = (float)(zoomScale*pageWidth*previewScale);
//  float actualHeight = (float)(zoomScale*pageHeight*previewScale);

  float x = (float)((canvasWidth - actualWidth)/2.0);
  if (x < leftMargin)
    x = leftMargin;
  float y = topMargin;

  wxMemoryDC temp_dc;
  temp_dc.SelectObject(previewBitmap);
  canvas->GetDC()->Blit(x, y, previewBitmap->GetWidth(), previewBitmap->GetHeight(), &temp_dc, 0, 0);

  temp_dc.SelectObject(NULL);

  return TRUE;
}

Bool wxPrintPreview::RenderPage(int pageNum)
{
  int canvasWidth, canvasHeight;
  if (!previewCanvas)
  {
    wxMessageBox("wxPrintPreview::RenderPage: must use wxPrintPreview::SetCanvas to let me know about the canvas!",
      "Print Preview Failure", wxOK);
    return FALSE;
  }
  previewCanvas->GetSize(&canvasWidth, &canvasHeight);
  
  float zoomScale = (float)((float)currentZoom/(float)100);
  float actualWidth = (float)(zoomScale*pageWidth*previewScale);
  float actualHeight = (float)(zoomScale*pageHeight*previewScale);

  float x = (float)((canvasWidth - actualWidth)/2.0);
  if (x < leftMargin)
    x = leftMargin;
//  float y = topMargin;

  if (!previewBitmap)
  {
    previewBitmap = new wxBitmap((int)actualWidth, (int)actualHeight);
    if (!previewBitmap || !previewBitmap->Ok())
    {
      if (previewBitmap)
        delete previewBitmap;
      wxMessageBox("Sorry, not enough memory to create a preview.", "Print Preview Failure", wxOK);
      return FALSE;
    }
  }
  wxMemoryDC memoryDC;
  memoryDC.SelectObject(previewBitmap);

  memoryDC.Clear();

  previewPrintout->SetDC(&memoryDC);
  previewPrintout->SetPageSizePixels(pageWidth, pageHeight);
  previewPrintout->OnBeginPrinting();

  if (!previewPrintout->OnBeginDocument(printData.GetFromPage(), printData.GetToPage()))
  {
    wxMessageBox("Could not start document preview.", "Print Preview Failure", wxOK);
    memoryDC.SelectObject(NULL);
    delete previewBitmap;
    return FALSE;
  }
  previewPrintout->OnPrintPage(pageNum);
  previewPrintout->OnEndDocument();
  previewPrintout->OnEndPrinting();
  previewPrintout->SetDC(NULL);
  memoryDC.SelectObject(NULL);

  char buf[200];
  if (maxPage != 0)
    sprintf(buf, "Page %d of %d", pageNum, maxPage);
  else
    sprintf(buf, "Page %d", pageNum);

  if (previewFrame)
    previewFrame->SetStatusText(buf);

  return TRUE;
}

Bool wxPrintPreview::DrawBlankPage(wxCanvas *canvas)
{
  int canvasWidth, canvasHeight;
  canvas->GetSize(&canvasWidth, &canvasHeight);
  
  float zoomScale = (float)((float)currentZoom/(float)100);
  float actualWidth = zoomScale*pageWidth*previewScale;
  float actualHeight = zoomScale*pageHeight*previewScale;

  float x = (float)((canvasWidth - actualWidth)/2.0);
  if (x < leftMargin)
    x = leftMargin;
  float y = topMargin;

  wxDC *dc = canvas->GetDC();

  // Draw shadow, allowing for 1-pixel border AROUND the actual page
  int shadowOffset = 4;
  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxBLACK_BRUSH);
  dc->DrawRectangle(x-1 + shadowOffset, y-1 + shadowOffset, actualWidth+2, actualHeight+2);

  // Draw blank page allowing for 1-pixel border AROUND the actual page
  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxWHITE_BRUSH);
  dc->DrawRectangle(x-1, y-1, actualWidth+2, actualHeight+2);
  return TRUE;
}

wxPrintData &wxPrintPreview::GetPrintData(void)
{
  return printData;
}

void wxPrintPreview::SetZoom(int percent)
{
  if (currentZoom == percent)
    return;
    
  currentZoom = percent;
  if (previewBitmap)
  {
    delete previewBitmap;
    previewBitmap = NULL;
  }
  RenderPage(currentPage);
  
  if (previewCanvas)
  {
    previewCanvas->Clear();
    previewCanvas->Refresh();
  }
}

int wxPrintPreview::GetZoom(void)
{
  return currentZoom;
}

Bool wxPrintPreview::Print(Bool interactive)
{
  if (!printPrintout)
    return FALSE;
  wxPrinter printer(&printData);
  return printer.Print(previewFrame, printPrintout, interactive);
}

void wxPrintPreview::DetermineScaling(void)
{
#ifdef wx_msw
  if (WINDOWS_PRINTING)
  {
    HDC dc = ::GetDC(NULL);
    int screenWidth = ::GetDeviceCaps(dc, HORZSIZE);
    int screenHeight = ::GetDeviceCaps(dc, VERTSIZE);
    int screenXRes = ::GetDeviceCaps(dc, HORZRES);
    int screenYRes = ::GetDeviceCaps(dc, VERTRES);
    int logPPIScreenX = ::GetDeviceCaps(dc, LOGPIXELSX);
    int logPPIScreenY = ::GetDeviceCaps(dc, LOGPIXELSY);

    previewPrintout->SetPPIScreen(logPPIScreenX, logPPIScreenY);

    ::ReleaseDC(NULL, dc);

    // Get a device context for the currently selected printer
    wxPrinterDC printerDC(NULL, NULL, NULL, FALSE);

    int printerWidth = 150;
    int printerHeight = 250;
    int printerXRes = 1500;
    int printerYRes = 2500;

    if (printerDC.cdc)
    {
      printerWidth = ::GetDeviceCaps(printerDC.cdc, HORZSIZE);
      printerHeight = ::GetDeviceCaps(printerDC.cdc, VERTSIZE);
      printerXRes = ::GetDeviceCaps(printerDC.cdc, HORZRES);
      printerYRes = ::GetDeviceCaps(printerDC.cdc, VERTRES);

      int logPPIPrinterX = ::GetDeviceCaps(printerDC.cdc, LOGPIXELSX);
      int logPPIPrinterY = ::GetDeviceCaps(printerDC.cdc, LOGPIXELSY);

      previewPrintout->SetPPIPrinter(logPPIPrinterX, logPPIPrinterY);
      previewPrintout->SetPageSizeMM(printerWidth, printerHeight);
    }

    pageWidth = printerXRes;
    pageHeight = printerYRes;

    // At 100%, the page should look about page-size on the screen.
    previewScale = (float)((float)screenWidth/(float)printerWidth);
    previewScale = previewScale * (float)((float)screenXRes/(float)printerYRes);
  }
  else
#endif
  {
    char *paperType = wxThePrintSetupData->GetPaperName();
    if (!paperType)
      paperType = "A4 210 x 297 mm";

    wxPrintPaperType *paper = wxThePrintPaperDatabase->FindPaperType(paperType);
    if (!paper)
      paper = wxThePrintPaperDatabase->FindPaperType("A4 210 x 297 mm");
    if (paper)
    {
      previewPrintout->SetPPIScreen(100, 100);
      previewPrintout->SetPPIPrinter(100, 100);
      pageWidth = paper->widthPixels;
      pageHeight = paper->heightPixels;
      previewPrintout->SetPageSizeMM(paper->widthMM, paper->heightMM);
      previewPrintout->SetPageSizePixels(paper->widthPixels, paper->heightPixels);

      // At 100%, the page should look about page-size on the screen.
      previewScale = 0.8;
//      previewScale = (float)((float)screenWidth/(float)printerWidth);
//      previewScale = previewScale * (float)((float)screenXRes/(float)printerYRes);
    }
  }
}

#endif

/*
 * Generic print dialog for non-Windows printing use.
 */

void wxGenericPrintOkProc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericPrintDialog *dialog = (wxGenericPrintDialog *)but.GetParent();
  dialog->DialogToValues();

  // There are some interactions between the global setup data
  // and the standard print dialog. The global printing 'mode'
  // is determined by whether the user checks Print to file
  // or not.
  if (dialog->printData.GetPrintToFile())
  {
    wxThePrintSetupData->SetPrinterMode(PS_FILE);

    char *f = wxFileSelector("PostScript file",
        wxPathOnly(wxThePrintSetupData->GetPrinterFile()),
        wxFileNameFromPath(wxThePrintSetupData->GetPrinterFile()),
        "ps", "*.ps", 0, dialog);
    if (f)
      wxThePrintSetupData->SetPrinterFile(f);
    else
      return;
  }
  else
    wxThePrintSetupData->SetPrinterMode(PS_PRINTER);
  
  dialog->printDialogCancelled = FALSE;
  dialog->Show(FALSE);
}

void wxGenericPrintCancelProc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericPrintDialog *dialog = (wxGenericPrintDialog *)but.GetParent();
  dialog->printDialogCancelled = TRUE;
  dialog->Show(FALSE);
  delete dialog;
}

void wxGenericPrintSetupProc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericPrintDialog *dialog = (wxGenericPrintDialog *)but.GetParent();
  dialog->OnSetup();
}

void wxGenericPrintRangeProc(wxRadioBox& rbox, wxCommandEvent& event)
{
  wxGenericPrintDialog *dialog = (wxGenericPrintDialog *)rbox.GetParent();
  if (event.commandInt == 0)
  {
    dialog->fromText->Enable(FALSE);
    dialog->toText->Enable(FALSE);
  }
  else if (event.commandInt == 1)
  {
    dialog->fromText->Enable(TRUE);
    dialog->toText->Enable(TRUE);
  }
}

IMPLEMENT_CLASS(wxGenericPrintDialog, wxDialogBox)

wxGenericPrintDialog::wxGenericPrintDialog(wxWindow *parent, wxPrintData& data):
  wxDialogBox(parent, "Print", TRUE, 0, 0, 600, 600)
{
  printData = data;
  printDialogCancelled = FALSE;
  
  SetLabelPosition(wxHORIZONTAL);

  int buttonWidth = 65;
  int buttonHeight = 25;

  wxButton *okButton = new wxButton(this, (wxFunction)wxGenericPrintOkProc, "OK", -1, -1, buttonWidth, buttonHeight);
  (void) new wxButton(this, (wxFunction)wxGenericPrintCancelProc, "Cancel", -1, -1, buttonWidth, buttonHeight);

//  if (printData.GetEnableHelp())
//  wxButton *helpButton = new wxButton(this, (wxFunction)wxGenericPrintHelpProc, "Help", -1, -1, buttonWidth, buttonHeight);

  setupButton = new wxButton(this, (wxFunction)wxGenericPrintSetupProc, "Setup...", -1, -1, buttonWidth, buttonHeight);

  NewLine();
  okButton->SetDefault();
  okButton->SetFocus();

  printerMessage = new wxMessage(this, "PostScript printer");
  NewLine();
  char *choices[2];
  choices[0] = "All";
  choices[1] = "Pages";
  rangeRadioBox = new wxRadioBox(this, (wxFunction)wxGenericPrintRangeProc, "Print Range",
    -1, -1, -1, -1, 2, (char **)choices, 2); /* MATTHEW: BC */
  rangeRadioBox->SetSelection(1);

  NewLine();

  fromText = new wxText(this, (wxFunction)NULL, "From:", "", -1, -1, 100, -1);
  toText = new wxText(this, (wxFunction)NULL, "To:", "", -1, -1, 100, -1);
  noCopiesText = new wxText(this, (wxFunction)NULL, "Copies:", "", -1, -1, 100, -1);
    
  NewLine();

  printToFileCheckBox = new wxCheckBox(this, (wxFunction)NULL, "Print to File");
//  collateCopiesCheckBox = new wxCheckBox(this, (wxFunction)NULL, "Collate Copies");
  
  Fit();
  Centre(wxBOTH);

  ValuesToDialog();
}

Bool wxGenericPrintDialog::printDialogCancelled = FALSE;

wxGenericPrintDialog::~wxGenericPrintDialog(void)
{
}

Bool wxGenericPrintDialog::OnClose(void)
{
  printDialogCancelled = TRUE;
  return TRUE;
}

void wxGenericPrintDialog::OnSetup(void)
{
  wxGenericPrintSetupDialog *genericPrintSetupDialog =
          new wxGenericPrintSetupDialog(this, *wxThePrintSetupData);
  genericPrintSetupDialog->Show(TRUE);
  if (wxGenericPrintSetupDialog::printSetupDialogCancelled)
  {
  }
  else
  {
    *wxThePrintSetupData = genericPrintSetupDialog->printData;

    // Dialog not deleted if OK pressed, to give us the opportunity
    // to copy the printData.

    delete genericPrintSetupDialog;
  }
}

void wxGenericPrintDialog::ValuesToDialog(void)
{
  char buf[10];
  if (printData.GetEnablePageNumbers())
  {
    fromText->Enable(TRUE);
    toText->Enable(TRUE);

    sprintf(buf, "%d", printData.GetFromPage());
    fromText->SetValue(buf);
    sprintf(buf, "%d", printData.GetToPage());
    toText->SetValue(buf);
    
    if (printData.GetAllPages())
      rangeRadioBox->SetSelection(0);
    else
      rangeRadioBox->SetSelection(1);
  }
  else
  {
    fromText->Enable(FALSE);
    toText->Enable(FALSE);
    rangeRadioBox->SetSelection(0);
    rangeRadioBox->wxRadioBox::Enable(1, FALSE);
  }
  sprintf(buf, "%d", printData.GetNoCopies());
  noCopiesText->SetValue(buf);

  printToFileCheckBox->SetValue(printData.GetPrintToFile());
  printToFileCheckBox->Enable(printData.GetEnablePrintToFile());

//  collateCheckBox->SetValue(printData.GetCollate());
//  collateCheckBox->Enable(printData.GetEnableCollate());

}

void wxGenericPrintDialog::DialogToValues(void)
{
  if (printData.GetEnablePageNumbers())
  {
    printData.SetFromPage(atoi(fromText->GetValue()));
    printData.SetToPage(atoi(fromText->GetValue()));
  }
  if (rangeRadioBox->GetSelection() == 0)
    printData.SetAllPages(TRUE);
  else
    printData.SetAllPages(FALSE);
  printData.SetNoCopies(atoi(noCopiesText->GetValue()));
  printData.SetPrintToFile(printToFileCheckBox->GetValue());
}

/*
 * Generic print setup dialog
 */

void wxGenericPrintSetupOkProc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericPrintSetupDialog *dialog = (wxGenericPrintSetupDialog *)but.GetParent();
  dialog->printSetupDialogCancelled = FALSE;
  dialog->DialogToValues();
  dialog->Show(FALSE);
}

void wxGenericPrintSetupCancelProc(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericPrintSetupDialog *dialog = (wxGenericPrintSetupDialog *)but.GetParent();
  dialog->printSetupDialogCancelled = TRUE;
  dialog->Show(FALSE);
  delete dialog;
}

IMPLEMENT_CLASS(wxGenericPrintSetupDialog, wxDialogBox)

wxGenericPrintSetupDialog::wxGenericPrintSetupDialog(wxWindow *parent, wxPrintSetupData& data):
  wxDialogBox(parent, "Print Setup", TRUE, 0, 0, 600, 600)
{
  printData = data;
  printSetupDialogCancelled = FALSE;
  
  SetLabelPosition(wxHORIZONTAL);

  int buttonWidth = 65;
  int buttonHeight = 25;

  wxButton *okButton = new wxButton(this, (wxFunction)wxGenericPrintSetupOkProc, "OK", -1, -1, buttonWidth, buttonHeight);
  (void) new wxButton(this, (wxFunction)wxGenericPrintSetupCancelProc, "Cancel", -1, -1, buttonWidth, buttonHeight);

//  if (printData.GetEnableHelp())
//  wxButton *helpButton = new wxButton(this, (wxFunction)wxGenericPrintSetupHelpProc, "Help", -1, -1, buttonWidth, buttonHeight);

  NewLine();
  okButton->SetDefault();
  okButton->SetFocus();

  printerMessage = new wxMessage(this, "PostScript print setup");
  NewLine();
  paperTypeChoice = CreatePaperTypeChoice();
  colourCheckBox = new wxCheckBox(this, (wxFunction)NULL, "Print in colour");

  NewLine();

  char *choices[2];
  choices[0] = "Portrait";
  choices[1] = "Landscape";
  orientationRadioBox = new wxRadioBox(this, (wxFunction)NULL, "Orientation",
    -1, -1, -1, -1, 2, (char **)choices, 2); /* MATTHEW: BC */
  orientationRadioBox->SetSelection(0);

  NewLine();

  printerCommandText = new wxText(this, (wxFunction)NULL, "Printer command:", "", -1, -1, 220, -1);
  printerOptionsText = new wxText(this, (wxFunction)NULL, "Printer options:", "", -1, -1, 220, -1);
  NewLine();

  Fit();
  Centre(wxBOTH);

  ValuesToDialog();
}

Bool wxGenericPrintSetupDialog::printSetupDialogCancelled = FALSE;

wxGenericPrintSetupDialog::~wxGenericPrintSetupDialog(void)
{
}

Bool wxGenericPrintSetupDialog::OnClose(void)
{
  printSetupDialogCancelled = TRUE;
  return TRUE;
}

void wxGenericPrintSetupDialog::ValuesToDialog(void)
{
  if (printerCommandText && printData.GetPrinterCommand())
    printerCommandText->SetValue(printData.GetPrinterCommand());
  if (printerOptionsText && printData.GetPrinterOptions())
    printerOptionsText->SetValue(printData.GetPrinterOptions());
  if (colourCheckBox)
    colourCheckBox->SetValue(printData.GetColour());
  if (orientationRadioBox)
  {
    if (printData.GetPrinterOrientation() == PS_PORTRAIT)
      orientationRadioBox->SetSelection(0);
    else
      orientationRadioBox->SetSelection(1);
  }
}

void wxGenericPrintSetupDialog::DialogToValues(void)
{
  if (printerCommandText)
    printData.SetPrinterCommand(printerCommandText->GetValue());
  if (printerOptionsText)
    printData.SetPrinterOptions(printerOptionsText->GetValue());
  if (colourCheckBox)
    printData.SetColour(colourCheckBox->GetValue());
  if (orientationRadioBox)
  {
    int sel = orientationRadioBox->GetSelection();
    if (sel == 0)
      printData.SetPrinterOrientation(PS_PORTRAIT);
    else
      printData.SetPrinterOrientation(PS_LANDSCAPE);
  }
  if (paperTypeChoice)
  {
    char *val = paperTypeChoice->GetStringSelection();
    if (val && *val)
      printData.SetPaperName(val);
  }
}

wxChoice *wxGenericPrintSetupDialog::CreatePaperTypeChoice(void)
{
  if (!wxThePrintPaperDatabase)
  {
    wxThePrintPaperDatabase = new wxPrintPaperDatabase;
    wxThePrintPaperDatabase->CreateDatabase();
  }
  int n = wxThePrintPaperDatabase->Number();
  char **choices = new char *[n];
  int sel = 0;
  int i;
  for (i = 0; i < n; i++)
  {
    wxPrintPaperType *paper = (wxPrintPaperType *)wxThePrintPaperDatabase->Nth(i)->Data();
    choices[i] = (char *)paper->pageName;
    if (printData.GetPaperName() && strcmp(choices[i], printData.GetPaperName()) == 0)
      sel = i;
  }
  wxChoice *choice = new wxChoice(this, (wxFunction)NULL, "Paper type", -1, -1, 300, -1, n,
    choices);
  delete[] choices;

  choice->SetSelection(sel);
  return choice;
}

#endif
  // USE_COMMON_DIALOGS
#endif
  // End USE_PRINTING_ARCHITECTURE
