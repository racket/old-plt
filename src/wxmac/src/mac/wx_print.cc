/*
 * File:        wx_print.cc
 * Purpose:     Printer implementation (mac)
 * Author:      Lj Birk (original msw by Julian Smart
 * Created:     1995
 * Updated:	October 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
#ifndef wx_mac
#pragma hdrstop
#endif

#ifdef testbmp
#include <tagobj.h>
#endif

#include <FixMath.h>

#include "common.h"

#if USE_PRINTING_ARCHITECTURE
#if USE_COMMON_DIALOGS

#include "wx_utils.h"
#include "wx_print.h"
#include "wx_dc.h"
#include "wx_dcpr.h"
#include "wx_main.h"
#include "wx_frame.h"
#include "wx_buttn.h"
#include "wx_dcmem.h"
#include "wx_messg.h"

#include <stdlib.h>
#ifndef wx_mac
#include <commdlg.h>
#endif

#ifdef wx_mac
#include "wx_dcps.h"
#endif

static int popen_p;
static Bool initialized = FALSE;

#ifdef OS_X
void wxPMBegin(void)
{
  if (!initialized) {
    PMBegin();
    return PMError();
  }
  return noErr;
}
#else
void wxPrOpen(void)
{
  if (! initialized) {
    PrOpen();
    return PrError();
  }
  return noErr;
}
#endif

#ifdef OS_X
void wxPMEnd(void)
{
}
#else
void wxPrClose(void)
{
}
#endif

wxPrintDialog::wxPrintDialog(wxWindow *p, wxPrintData *data):
 wxDialogBox((wxFrame *)p, "Printer Dialog")
{
  dialogParent = p;

  printData = data;

}

wxPrintDialog::~wxPrintDialog(void)
{
}

Bool wxPrintDialog::Show(Bool flag)
{
  Bool prtJob = FALSE;

#ifdef OS_X
  PMPrintDialog(printData.cPrintSettings,printData.cPageFormat,&prtJob);
#else
  prtJob = PrJobDialog(printData.macPrData);
#endif

  return prtJob;
}

/*
 * Print data
 */

wxPrintData::wxPrintData(void)
{
#ifdef OS_X
  if (PMCreatePrintSettings(&cPrintSettings) != noErr)
    return;
    
  if (PMDefaultPrintSettings(cPrintSettings) != noErr) {
    PMDisposePrintSettings(cPrintSettings);
    return;
  }
  
  if (PMCreatePageFormat(&cPageFormat) != noErr) {
    PMDisposePrintSettings(cPrintSettings);
    return;
  }
  
  if (PMDefaultPageFormat(cPageFormat) != noErr) {
    PMDisposePrintSettings(cPrintSettings);
    PMDisposePageFormat(cPageFormat);
    return;
  }

#else
  THPrint pd = (THPrint)NewHandleClear(sizeof(TPrint));
  CheckMemOK(pd);
  macPrData = pd;
  
  /* MATTHEW: [6] */
  PrintDefault(macPrData);
}

wxPrintData::~wxPrintData(void)
{
#ifdef OS_X
  if (cPrintSettings)
    PMDisposePrintSettings(cPrintSettings);

  if (cPageFormat)
    PMDisposePageFormat(cPageFormat);
#else
  if (macPrData)
    DisposeHandle((Handle)macPrData);
#endif
}

#ifndef OS_X
// can't implement this function under OS X
void wxPrintData::SetAbortFlag()
{
	  (**macPrData).prJob.fFromUsr=TRUE;
}
#endif

int wxPrintData::GetFromPage(void)
{
#ifdef OS_X
  UInt32 result;
  PMGetFirstPage(cPrintSettings,&result);
  return result;
#else
  return (**macPrData).prJob.iFstPage;
#endif
}

int wxPrintData::GetToPage(void)
{
#ifdef OS_X
  UInt32 result;
  PMGetLastPage(cPrintSettings,&result);
  return result;
#else
  return (**macPrData).prJob.iLstPage;
#endif
}

int wxPrintData::GetMinPage(void)
{
  return GetFromPage();
}

int wxPrintData::GetMaxPage(void)
{
  return GetToPage();
}

int wxPrintData::GetNoCopies(void)
{
#ifdef OS_X
  UInt32 result;
  PMGetCopies(cPrintSettings,&result);
  return result;
#else
  return (**macPrData).prJob.iCopies;
#endif
}

Bool wxPrintData::GetAllPages(void)
{
  // here's the original (nonsensical) line:
  // return ( GetMaxPage() == (**macPrData).prJob.iLstPage);
  return TRUE;
}

Bool wxPrintData::GetCollate(void)
{
  return FALSE;
}

/*
wxDC *wxPrintData::GetDC(void)
{
  if (printerDC)
  {
    destroyDC = FALSE;
    return printerDC;
  }
  else
    return NULL;
}
*/

void wxPrintData::SetFromPage(int p)
{
#ifdef OS_X
  PMSetFirstPage(cPrintSettings,p,false);
#else
  (**macPrData).prJob.iFstPage = p;
#endif
}

void wxPrintData::SetToPage(int p)
{
#ifdef OS_X
  PMSetLastPage(cPrintSettings,p,false);
#else
  (**macPrData).prJob.iLstPage = p;
#endif
}

void wxPrintData::SetMinPage(int p)
{
  SetFromPage(p);
}

void wxPrintData::SetMaxPage(int p)
{
  SetToPage(p);
}

void wxPrintData::SetNoCopies(int c)
{
#ifdef OS_X
  PMSetCopies(cPrintSettings,c,false);
#else
  (**macPrData).prJob.iCopies = c;
#endif
}

void wxPrintData::SetAllPages(Bool flag)
{
#ifdef OS_X
  PMSetPageRange(cPrintSettings,0,kPMPrintAllPages);
#endif
}

void wxPrintData::SetCollate(Bool flag)
{
}

void wxPrintData::SetPrintToFile(Bool flag)
{
  //(**macPrData).
}

void wxPrintData::EnablePrintToFile(Bool flag)
{
}

void wxPrintData::EnableSelection(Bool flag)
{
}

void wxPrintData::EnablePageNumbers(Bool flag)
{
}

void wxPrintData::EnableHelp(Bool flag)
{
}

void wxPrintData::SetSetupDialog(Bool flag)
{
// page setup menu item
//  PrOpen();
//  PrStlDialog(printData.macPrData);
//  PrClose();
}

#ifndef OS_X
void wxPrintData::operator=(const wxPrintData& data)
{
   this->macPrData = data.macPrData;
}
#endif

/*
 * Printer
 */
 
wxPrinter::wxPrinter(wxPrintData *data)
{
  currentPrintout = NULL;
  abortWindow = NULL;
  abortIt = FALSE;
  if (data)
    printData = (*data);
  //lpAbortProc = MakeProcInstance((FARPROC) wxAbortProc, wxhInstance);
}

wxWindow *wxPrinter::abortWindow = NULL;
Bool wxPrinter::abortIt = FALSE;

wxPrinter::~wxPrinter(void)
{
  //FreeProcInstance(lpAbortProc);
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

#ifdef OS_X
  if (! printData) {
    printData = new wxPrintData;
  }
#else
  wxPrOpen();
  if (PrError() != fnfErr) {
    PrintDefault(printData.macPrData);
  }
#endif  

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
    // create a mac dc with a printer port
    // may need to create a dccan in order to draw
    // and set the dc as a print dc
    wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
    dialog->Show(TRUE);
    delete dialog;
    //printData = dialog.GetPrintData();
  }

  // sanity check  
  if (printData.GetFromPage() <= 0 || 
      printData.GetToPage() <= 0)
  {
    wxPrClose();
    return FALSE;
  }
  
    //dc = new wxPrinterDC(NULL, NULL, NULL, FALSE);
  dc = new wxPrinterDC(printData.macPrData); 

  // May have pressed cancel.
  if (!dc || !dc->Ok())
  {
    if (dc) delete dc; // PrSetError
    wxPrClose();
    return FALSE;
  }

  GDHandle gThisGDevice = GetMainDevice();
  int logPPIScreenX = (int)Fix2Long((**(**gThisGDevice).gdPMap).hRes);
  int logPPIScreenY = (int)Fix2Long((**(**gThisGDevice).gdPMap).vRes);

  printout->SetPPIScreen(logPPIScreenX, logPPIScreenY);

  int logPPIPrinterX = (**(printData.macPrData)).prInfo.iHRes;  //::GetDeviceCaps(dc->cdc, LOGPIXELSX);
  int logPPIPrinterY = (**(printData.macPrData)).prInfo.iVRes;  //::GetDeviceCaps(dc->cdc, LOGPIXELSY);

  printout->SetPPIPrinter(logPPIPrinterX, logPPIPrinterY);

  // Set printout parameters  
  printout->SetDC(dc);

///// TODO figure the equivalent
  float w, h;
  dc->GetSize(&w, &h);
  printout->SetPageSizePixels((int)w, (int)h);
  //dc->GetSizeMM(&w, &h);
  dc->GetSize(&w, &h);
  printout->SetPageSizeMM((int)w, (int)h);

  // Create an abort window
  //wxBeginBusyCursor();
  wxWindow *win = CreateAbortWindow(parent, printout);
  //wxYield();
  //::SetAbortProc(dc->cdc, lpAbortProc);
  
  if (!win)
  {
 //   wxEndBusyCursor();
    wxMessageBox("Sorry, could not create an abort dialog.", "Print Error", wxOK, (wxFrame *)parent);
    delete dc;
    wxPrClose();
	return FALSE;
  }
  abortWindow = win;
  abortWindow->Show(TRUE);
 // wxYield();

  //(**printData.macPrData).prJob.pIdleProc = printIdleUPP;

  printout->OnBeginPrinting();
  
  Bool keepGoing = TRUE;

  for (int copyCount = 1; copyCount <= printData.GetNoCopies(); copyCount ++)
  {
    if (!printout->OnBeginDocument(printData.GetFromPage(), printData.GetToPage()))
    {
      //wxEndBusyCursor();
      wxMessageBox("Could not start printing.", "Print Error");
      break;
    }
    if (abortIt) {
      printData.SetAbortFlag();
      wxDialogBox *dialog = new wxDialogBox(parent, "Print Aborted", 0, 0, 400, 400);
      break;
    }

    for (int pn = printData.GetFromPage(); 
             keepGoing && 
             (pn <= printData.GetToPage()) && printout->HasPage(pn);
         pn++)
    {
      if (abortIt)
      {
        printData.SetAbortFlag();
        wxDialogBox *dialog = new wxDialogBox(parent, "Print Aborted", 0, 0, 400, 400);
        keepGoing = FALSE;
        break;
      }
      else
      {
        dc->StartPage();
        printout->OnPrintPage(pn);
        dc->EndPage();
      }
    }
    printout->OnEndDocument();
  }

  printout->OnEndPrinting();

  if (abortWindow)
  {
    abortWindow->Show(FALSE);
    delete abortWindow;
    abortWindow = NULL;
  }
  
  //wxEndBusyCursor();

  delete dc;
  
#ifdef OS_X
  wxPrClose();
#endif

  return TRUE;
}

Bool wxPrinter::PrintDialog(wxWindow *parent)
{
  wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
  dialog->Show(TRUE);
  delete dialog;
  return 0;
}
// TODO make this a UPP and stuf in THPrintPtr
static void wxAbortWindowCancel(wxButton& but, wxCommandEvent& event)
{
  wxPrinter::abortIt = TRUE;
  wxPrinter::abortWindow->Show(FALSE);
  delete wxPrinter::abortWindow;
  wxPrinter::abortWindow = NULL;
}

wxWindow *wxPrinter::CreateAbortWindow(wxWindow *parent, wxPrintout *printout)
{
    //wxMessageBox("Printing, please wait.", "Printing", wxOK, (wxFrame *)parent);

  wxDialogBox *dialog = new wxDialogBox(parent, "Printing", FALSE, 0, 0, 400, 400);
  wxMessage *message = new wxMessage(dialog, "Press Cmd-. to stop printing");
  dialog->Fit();

  dialog->Centre(wxBOTH);
  
  return dialog;
}

Bool wxPrinter::Setup(wxWindow *parent)
{
  wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
  dialog->GetPrintData().SetSetupDialog(TRUE);
  dialog->Show(TRUE);
  delete dialog;
  return 0;
}

void wxPrinter::ReportError(wxWindow *parent, wxPrintout *printout, char *message)
{
  wxMessageBox(message, "Printing Error", wxOK, (wxFrame *)parent);
}

wxPrintData &wxPrinter::GetPrintData(void)
{
  return printData;
}

/*
 * Printout class
 */
 
wxPrintout::wxPrintout(char *title)
{
  printoutTitle = title ? copystring(title) : NULL;
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

Bool wxPrintout::OnBeginDocument(int startPage, int endPage)
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

/****************************************************************************

    FUNCTION: wxAbortProc()

    PURPOSE:  Processes messages for the Abort Dialog box

****************************************************************************/

void printIdle(void)
{
	EventRecord theEvent;
	while (GetNextEvent(24,&theEvent)) {
		if ((theEvent.modifiers & cmdKey) &&
			(theEvent.message & 0xff) =='.') 
		    wxPrinter::abortIt = TRUE;
		}
}

#endif
  // USE_COMMON_DIALOGS
#endif
  // End USE_PRINTING_ARCHITECTURE


