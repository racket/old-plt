/*
 * File:	wx_print.h
 * Purpose:	Printing-related classes
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

#ifndef wx_printh
#define wx_printh

#include "common.h"
#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_dialg.h"
#include "wx_choic.h"

#ifdef wx_mac
#include <Printing.h>
#endif

#ifdef IN_CPROTO
typedef       void    *wxPrintDialog ;
typedef       void    *wxPrinter ;
typedef       void    *wxPrintout ;
#else

class wxDC;
class wxPrintout;
class wxPrinter;
class wxPrintDialog;

/*
 * wxPrintData
 * Encapsulates information displayed and edited in the printer dialog box.
 */
 
class wxPrintData: public wxObject
{
 public:
  // macintosh
  THPrint macPrData;

  wxPrintData(void);
  ~wxPrintData(void);

  void SetAbortFlag(void); // set mac abort flag
  int GetFromPage(void);
  int GetToPage(void);
  int GetMinPage(void);
  int GetMaxPage(void);
  int GetNoCopies(void);
  Bool GetAllPages(void);
  Bool GetCollate(void);

  void SetFromPage(int);
  void SetToPage(int);
  void SetMinPage(int);
  void SetMaxPage(int);
  void SetNoCopies(int);
  void SetAllPages(Bool);
  void SetCollate(Bool);
  void SetPrintToFile(Bool);
  void SetSetupDialog(Bool);

  void EnablePrintToFile(Bool);
  void EnableSelection(Bool);
  void EnablePageNumbers(Bool);
  void EnableHelp(Bool);

  void operator=(const wxPrintData& data);
};

/*
 * wxPrinterDialog
 * The common dialog for printing.
 */
 
class wxPrintDialog: public wxDialogBox
{
 private:
  wxPrintData printData;
  wxDC *printerDC;
  Bool destroyDC;
  char *deviceName;
  char *driverName;
  char *portName;
  wxWindow *dialogParent;
 public:
  wxPrintDialog(wxWindow *parent, wxPrintData *data = NULL);
  ~wxPrintDialog(void);

#ifdef wx_mac
  virtual void Show(Bool flag);
#else
  virtual void Show(Bool flag);
#endif

  virtual wxPrintData& GetPrintData(void) { return printData; }
  virtual wxDC *GetPrintDC(void);
};

/*
 * Represents the printer: manages printing a wxPrintout object
 */
 
class wxPrinter: public wxObject
{
 private:
  wxPrintData printData;
  wxPrintout *currentPrintout;
#ifndef wx_mac
  FARPROC lpAbortProc;
#endif
 public:
  static wxWindow *abortWindow;
  static Bool abortIt;

  wxPrinter(wxPrintData *data = NULL);
  ~wxPrinter(void);

  virtual Bool Print(wxWindow *parent, wxPrintout *printout, Bool prompt = TRUE);
  virtual Bool PrintDialog(wxWindow *parent);
  virtual wxWindow *CreateAbortWindow(wxWindow *parent, wxPrintout *printout);
  virtual Bool Setup(wxWindow *parent);
  virtual void ReportError(wxWindow *parent, wxPrintout *printout, char *message);
  virtual wxPrintData &GetPrintData(void);
  virtual inline Bool Abort(void) { return abortIt; }
};

/*
 * wxPrintout
 * Represents an object via which a document may be printed.
 * The programmer derives from this, overrides (at least) OnPrintPage,
 * and passes it to a wxPrinter object for printing, or a wxPrintPreview
 * object for previewing.
 */
 
class wxPrintout: public wxObject
{
 private:
   char *printoutTitle;
   wxDC *printoutDC;

   int pageWidthPixels;
   int pageHeightPixels;

   int pageWidthMM;
   int pageHeightMM;

   int PPIScreenX;
   int PPIScreenY;
   int PPIPrinterX;
   int PPIPrinterY;

   Bool isPreview;
 public:
  wxPrintout(char *title = "Printout");
  ~wxPrintout(void);

  virtual Bool OnBeginDocument(int startPage, int endPage);
  virtual void OnEndDocument(void);
  virtual void OnBeginPrinting(void);
  virtual void OnEndPrinting(void);

  // Guaranteed to be before any other functions are called
  inline virtual void OnPreparePrinting(void) { }

  virtual Bool HasPage(int page);
  virtual Bool OnPrintPage(int page) = 0;
  virtual void GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo);

  inline virtual char *GetTitle(void) { return printoutTitle; }

  inline wxDC *GetDC(void) { return printoutDC; }
  inline void SetDC(wxDC *dc) { printoutDC = dc; }
  inline void SetPageSizePixels(int w, int  h) { pageWidthPixels = w; pageHeightPixels = h; }
  inline void GetPageSizePixels(int *w, int  *h) { *w = pageWidthPixels; *h = pageHeightPixels; }
  inline void SetPageSizeMM(int w, int  h) { pageWidthMM = w; pageHeightMM = h; }
  inline void GetPageSizeMM(int *w, int  *h) { *w = pageWidthMM; *h = pageHeightMM; }

  inline void SetPPIScreen(int x, int y) { PPIScreenX = x; PPIScreenY = y; }
  inline void GetPPIScreen(int *x, int *y) { *x = PPIScreenX; *y = PPIScreenY; }
  inline void SetPPIPrinter(int x, int y) { PPIPrinterX = x; PPIPrinterY = y; }
  inline void GetPPIPrinter(int *x, int *y) { *x = PPIPrinterX; *y = PPIPrinterY; }

  inline virtual Bool IsPreview(void) { return isPreview; }

  inline virtual void SetIsPreview(Bool p) { isPreview = p; }
};

void wxPrOpen(void);
void wxPrClose(void);

#endif // IN_CPROTO
#endif // wx_printh
