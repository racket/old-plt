/*
 * File:	wx_doc.h
 * Purpose:	Document class header file
 * Author:	Julian Smart
 * After:       Carlo Alberto Avizzano
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

#ifndef wx_doch
#define wx_doch

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_list.h"

#if !USE_DYNAMIC_CLASSES
#error To use doc/view, you must switch USE_DYNAMIC_CLASSES on.
#endif

#if USE_PRINTING_ARCHITECTURE
#include "wx_print.h"
#endif

class wxWindow;
// class wxDocItem;
class wxDocument;
class wxView;
class wxDocTemplate;
class wxDocManager;
class wxPrintInfo;
class wxCommand;
class wxCommandProcessor;
class wxFileHistory;

class ostream;
class istream;

// Document manager flags
#define wxDOC_SDI       1
#define wxDOC_MDI       2
#define wxDOC_NEW       4
#define wxDOC_SILENT    8
#define wxDEFAULT_DOCMAN_FLAGS      wxDOC_SDI

// Document template flags
#define wxTEMPLATE_VISIBLE      1
#define wxTEMPLATE_INVISIBLE    2
#define wxDEFAULT_TEMPLATE_FLAGS    wxTEMPLATE_VISIBLE

#define wxMAX_FILE_HISTORY      9

class wxDocument : public wxEvtHandler
{
  DECLARE_ABSTRACT_CLASS(wxDocument)
 protected:
  wxList documentViews;
  wxList documentItems;
  char *documentFile;
  char *documentTitle;
  char *documentTypeName;
  wxDocTemplate *documentTemplate;
  Bool documentModified;
  wxDocument *documentParent;
  wxCommandProcessor *commandProcessor;
 public:
  wxDocument(wxDocument *parent = NULL);
  ~wxDocument(void);

  void SetFilename(char *f);
  inline char *GetFilename(void) { return documentFile; }
  void SetTitle(char *t);
  inline char *GetTitle(void) { return documentTitle; }
  void SetDocumentName(char *name);
  inline char *GetDocumentName(void) { return documentTypeName; }

  virtual Bool Close(void);
  virtual Bool Save(void);
  virtual Bool SaveAs(void);
  virtual Bool Revert(void);

  virtual ostream& SaveObject(ostream& stream);
  virtual istream& LoadObject(istream& stream);

  // Called by wxWindows
  virtual Bool OnSaveDocument(char *filename);
  virtual Bool OnOpenDocument(char *filename);
  virtual Bool OnNewDocument(void);
  virtual Bool OnCloseDocument(void);

  // Prompts for saving if about to close a modified document.
  // Returns TRUE if ok to close the document (may have saved in the
  // meantime, or set modified to FALSE)
  virtual Bool OnSaveModified(void);

  // Called by framework if created automatically by the
  // default document manager: gives document a chance to
  // initialise and (usually) create a view
  virtual Bool OnCreate(char *path, long flags);

  // By default, creates a base wxCommandProcessor.
  virtual wxCommandProcessor *OnCreateCommandProcessor(void);
  virtual inline wxCommandProcessor *GetCommandProcessor(void) { return commandProcessor; }
  virtual inline void SetCommandProcessor(wxCommandProcessor *proc) { commandProcessor = proc; }

  // Called after a view is added or removed.
  // The default implementation deletes the document if this
  // is there are no more views.
  virtual void OnChangedViewList(void);

  virtual Bool DeleteContents(void);

  virtual Bool Draw(wxDC&);
  virtual inline Bool IsModified(void) { return documentModified; }
  virtual inline void Modify(Bool mod) { documentModified = mod; }

  virtual Bool AddView(wxView *view);
  virtual Bool RemoveView(wxView *view);
  inline wxList& GetViews(void) { return documentViews; }
  wxView *GetFirstView(void);

/*
  virtual   Bool Insert(wxDocItem *obj);
  virtual   Bool Append(wxDocItem *obj);
  virtual   Bool Delete(wxDocItem *obj);
  virtual   Bool Exists(wxDocItem *obj);
*/

  virtual void UpdateAllViews(wxView *sender = NULL, wxObject *hint = NULL);

  // Remove all views (because we're closing the document)
  virtual Bool DeleteAllViews(void);

  // Other stuff
  virtual wxDocManager *GetDocumentManager(void);
  virtual inline wxDocTemplate *GetDocumentTemplate(void) { return documentTemplate; }
  virtual inline void SetDocumentTemplate(wxDocTemplate *temp) { documentTemplate = temp; }

  // Get title, or filename if no title, else [unnamed]
  virtual Bool GetPrintableName(char *buf);

  // Returns a window that can be used as a parent for document-related
  // dialogs. Override if necessary.
  virtual wxWindow *GetDocumentWindow(void);
};

class wxView: public wxEvtHandler
{
  DECLARE_ABSTRACT_CLASS(wxView)
 protected:
  wxDocument *viewDocument;
  char *viewTypeName;
  wxFrame *viewFrame;
 public:
  wxView(wxDocument *doc = NULL);
  ~wxView(void);

  inline wxDocument *GetDocument(void) { return viewDocument; }
  void SetDocument(wxDocument *doc);

  inline char *GetViewName(void) { return viewTypeName; }
  void SetViewName(char *name);

  inline wxFrame *GetFrame(void) { return viewFrame ; }
  inline void SetFrame(wxFrame *frame) { viewFrame = frame; }

  // Is the item in this view selected?
//  virtual Bool IsSelected(wxDocItem *item);

  virtual void OnActivateView(Bool activate, wxView *activeView, wxView *deactiveView);
  virtual void OnDraw(wxDC *dc) = 0;
  virtual void OnPrint(wxDC *dc, wxPrintInfo *info);
  virtual void OnUpdate(wxView *sender, wxObject *hint = NULL);
  virtual void OnChangeFilename(void);

  // Called by framework if created automatically by the
  // default document manager class: gives view a chance to
  // initialise
  virtual Bool OnCreate(wxDocument *WXUNUSED(doc), long WXUNUSED(flags)) { return TRUE; };

  // Checks if the view is the last one for the document; if so,
  // asks user to confirm save data (if modified). If ok,
  // deletes itself and returns TRUE.
  virtual Bool Close(Bool deleteWindow = TRUE);

  // Override to do cleanup/veto close
  virtual Bool OnClose(Bool deleteWindow);
  // Defeat compiler warning
  inline Bool OnClose(void) { return wxEvtHandler::OnClose(); }

  // A view's window can call this to notify the view it is (in)active.
  // The function then notifies the document manager.
  virtual void Activate(Bool activate);

  virtual inline wxDocManager *GetDocumentManager(void) { return viewDocument->GetDocumentManager(); }

#if USE_PRINTING_ARCHITECTURE
  virtual wxPrintout *OnCreatePrintout(void);
#endif
};

/*

// Not yet sure how to deal with doc items, and what extra functionality
// it can give us. It might allow us to abstract another level of
// functionality such as cut and paste. However, not all applications will
// wish to conform to a central model at this level of detail.
class wxDocItem : public wxObject
{
  DECLARE_ABSTRACT_CLASS(wxDocItem)
 public:
  wxDocItem(void);
  ~wxDocItem(void);
  virtual ostream& SaveObject(ostream& stream) { return stream; };
  virtual istream& LoadObject(istream& stream) { return stream; };
  virtual Bool Draw(wxDC& );
  virtual Bool OnDelete(void) { return TRUE; }
};
*/

// Represents user interface (and other) properties of documents and views
class wxDocTemplate: public wxObject
{
  DECLARE_CLASS(wxDocTemplate)
 protected:
  long tFlags;
  char *tFileFilter;
  char *tDirectory;
  char *tDescription;
  char *tDefaultExt;
  char *tDocTypeName;
  char *tViewTypeName;
  wxDocManager *tDocumentManager;

  // For dynamic creation of appropriate instances.
  wxClassInfo *tDocClassInfo;
  wxClassInfo *tViewClassInfo;

 public:

  // Associate document and view types.
  // They're for identifying what view is associated with what
  // template/document type
  wxDocTemplate(wxDocManager *manager, char *descr, char *filter, char *dir, char *ext,
     char *docTypeName, char *viewTypeName,
     wxClassInfo *docClassInfo = NULL, wxClassInfo *viewClassInfo = NULL,
     long flags = wxDEFAULT_TEMPLATE_FLAGS);

  ~wxDocTemplate(void);

  // By default, these two member functions dynamically creates document
  // and view using dynamic instance construction.
  // Override these if you need a different method of construction.
  virtual wxDocument *CreateDocument(char *path, long flags = 0);
  virtual wxView *CreateView(wxDocument *doc, long flags = 0);

  inline char *GetDefaultExtension(void) { return tDefaultExt; };
  inline char *GetDescription(void) { return tDescription; }
  inline char *GetDirectory(void) { return tDirectory; };
  inline wxDocManager *GetDocumentManager(void) { return tDocumentManager; }
  inline void SetDocumentManager(wxDocManager *manager) { tDocumentManager = manager; }
  inline char *GetFileFilter(void) { return tFileFilter; };
  inline long GetFlags(void) { return tFlags; };
  virtual char *GetViewName(void) { return tViewTypeName; }
  virtual char *GetDocumentName(void) { return tDocTypeName; }

  void SetFileFilter(char *filter);
  void SetDirectory(char *dir);
  void SetDescription(char *descr);
  void SetDefaultExtension(char *ext);
  void SetFlags(long flags);

  inline Bool IsVisible(void) { return ((tFlags & wxTEMPLATE_VISIBLE) == wxTEMPLATE_VISIBLE); }

};

// One object of this class may be created in an application,
// to manage all the templates and documents.
class wxDocManager: public wxEvtHandler
{
  DECLARE_DYNAMIC_CLASS(wxDocManager)
 protected:
  long mnFlags;
  int defaultDocumentNameCounter;
  int maxDocsOpen;
  wxList mnDocs;
  wxList mnTemplates;
  wxView *currentView;
  wxFileHistory *fileHistory;

 public:
  wxDocManager(long flags = wxDEFAULT_DOCMAN_FLAGS, Bool initialize = TRUE);
  ~wxDocManager(void);

  virtual Bool Initialize(void);

  // Handlers for common user commands
  virtual void OnMenuCommand(int command);
  virtual void OnFileClose(void);
  virtual void OnFileNew(void);
  virtual void OnFileOpen(void);
  virtual void OnFileRevert(void);
  virtual void OnFileSave(void);
  virtual void OnFileSaveAs(void);
  virtual void OnPrint(void);
  virtual void OnPrintSetup(void);
  virtual void OnPreview(void);
  virtual void OnUndo(void);
  virtual void OnRedo(void);

  virtual wxDocument *CreateDocument(char *path, long flags = 0);
  virtual wxView *CreateView(wxDocument *doc, long flags = 0);
  virtual void DeleteTemplate(wxDocTemplate *temp, long flags = 0);
  virtual Bool FlushDoc(wxDocument *doc);
  virtual wxDocTemplate *MatchTemplate(char *path);
  virtual wxDocTemplate *SelectDocumentPath(wxDocTemplate **templates,
    int noTemplates, char *path, int bufSize, long flags, Bool save = FALSE);
  virtual wxDocTemplate *SelectDocumentType(wxDocTemplate **templates,
    int noTemplates);
  virtual wxDocTemplate *SelectViewType(wxDocTemplate **templates,
    int noTemplates);
  virtual wxDocTemplate *FindTemplateForPath(char *path);

  void AssociateTemplate(wxDocTemplate *temp);
  void DisassociateTemplate(wxDocTemplate *temp);

  wxDocument *GetCurrentDocument(void);

  inline void SetMaxDocsOpen(int n) { maxDocsOpen = n; }
  inline int GetMaxDocsOpen(void) { return maxDocsOpen; }

  // Add and remove a document from the manager's list
  void AddDocument(wxDocument *doc);
  void RemoveDocument(wxDocument *doc);

  // Clear remaining documents and templates
  Bool Clear(Bool force = TRUE);

  // Views or windows should inform the document manager
  // when a view is going in or out of focus
  virtual void ActivateView(wxView *view, Bool activate = TRUE, Bool deleting = FALSE);
  virtual inline wxView *GetCurrentView(void) { return currentView; }

  virtual inline wxList& GetDocuments(void) { return mnDocs; }

  // Make a default document name
  virtual Bool MakeDefaultName(char *buf);

  virtual wxFileHistory *OnCreateFileHistory(void);
  virtual inline wxFileHistory *GetFileHistory(void) { return fileHistory; }

  // File history management
  virtual void AddFileToHistory(char *file);
  virtual int GetNoHistoryFiles(void);
  virtual char *GetHistoryFile(int i);
  virtual void FileHistoryUseMenu(wxMenu *menu);
  virtual void FileHistoryLoad(char *resourceFile, char *section);
  virtual void FileHistorySave(char *resourceFile, char *section);
};

/*
 * A default child frame
 */

class wxDocChildFrame: public wxFrame
{
  DECLARE_CLASS(wxDocChildFrame)

 protected:

  wxDocument *childDocument;
  wxView *childView;

 public:
 
  wxDocChildFrame(wxDocument *doc, wxView *view, wxFrame *frame, char *title, int x = 100, int y = 100, int w = 600, int h = 600,
    long type = wxDEFAULT_FRAME | wxSDI, char *name = "frame");
  ~wxDocChildFrame(void);

  Bool OnClose(void);
  void OnMenuCommand(int id);
  void OnActivate(Bool active);

  inline wxDocument *GetDocument(void) { return childDocument; }
  inline wxView *GetView(void) { return childView; }
  inline void SetDocument(wxDocument *doc) { childDocument = doc; }
  inline void SetView(wxView *view) { childView = view; }
};

/*
 * A default parent frame
 */
 
class wxDocParentFrame: public wxFrame
{
  DECLARE_CLASS(wxDocParentFrame)
 protected:
  wxDocManager *docManager;
 public:
  wxDocParentFrame(wxDocManager *manager, wxFrame *frame, char *title, int x = 100, int y = 100, int w = 600, int h = 600,
    long type = wxDEFAULT_FRAME | wxSDI, char *name = "frame");
  Bool OnClose(void);
  void OnMenuCommand(int id);
  wxDocManager *GetDocumentManager(void) { return docManager; }
};

/*
 * Provide simple default printing facilities
 */

#if USE_PRINTING_ARCHITECTURE
class wxDocPrintout: public wxPrintout
{
  DECLARE_DYNAMIC_CLASS(wxDocPrintout)
 protected:
  wxView *printoutView;
 public:
  wxDocPrintout(wxView *view = NULL, char *title = "Printout");
  Bool OnPrintPage(int page);
  Bool HasPage(int page);
  Bool OnBeginDocument(int startPage, int endPage);
  void GetPageInfo(int *minPage, int *maxPage, int *selPageFrom, int *selPageTo);

  virtual inline wxView *GetView(void) { return printoutView; }
};
#endif

/*
 * Command processing framework
 */

class wxCommand: public wxObject
{
  DECLARE_CLASS(wxCommand)
 protected:
  Bool canUndo;
  char *commandName;
 public:
  wxCommand(Bool canUndoIt = FALSE, char *name = NULL);
  ~wxCommand(void);

  // Override this to perform a command
  virtual Bool Do(void) = 0;

  // Override this to undo a command
  virtual Bool Undo(void) = 0;

  virtual inline Bool CanUndo(void) { return canUndo; }
  virtual inline char *GetName(void) { return commandName; }
};

class wxCommandProcessor: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxCommandProcessor)
 protected:
  int maxNoCommands;
  wxList commands;
  wxNode *currentCommand;
  wxMenu *commandEditMenu;
 public:
  wxCommandProcessor(int maxCommands = 100);
  ~wxCommandProcessor(void);

  // Pass a command to the processor. The processor calls Do();
  // if successful, is appended to the command history unless
  // storeIt is FALSE.
  virtual Bool Submit(wxCommand *command, Bool storeIt = TRUE);
  virtual Bool Undo(void);
  virtual Bool Redo(void);
  virtual Bool CanUndo(void);

  // Call this to manage an edit menu.
  virtual inline void SetEditMenu(wxMenu *menu) { commandEditMenu = menu; }
  virtual inline wxMenu *GetEditMenu(void) { return commandEditMenu; }
  virtual void SetMenuStrings(void);
  virtual void Initialize(void);

  virtual inline wxList& GetCommands(void) { return commands; }
  virtual inline int GetMaxCommands(void) { return maxNoCommands; }
  virtual void ClearCommands(void);
};

class wxFileHistory: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxFileHistory)
 protected:
  // Last n files
  char **fileHistory;
  // Number of files saved
  int fileHistoryN;
  // Menu to maintain
  wxMenu *fileMenu;
  // Max files to maintain
  int fileMaxFiles;
 public:
  wxFileHistory(int maxFiles = 9);
  ~wxFileHistory(void);

  // File history management
  virtual void AddFileToHistory(char *file);
  inline virtual int GetNoHistoryFiles(void) { return fileHistoryN; }
  virtual char *GetHistoryFile(int i);
  virtual int GetMaxFiles(void) { return fileMaxFiles; }
  virtual void FileHistoryUseMenu(wxMenu *menu);
  virtual void FileHistoryLoad(char *resourceFile, char *section);
  virtual void FileHistorySave(char *resourceFile, char *section);
};

// Not yet used
class wxPrintInfo: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxPrintInfo)
 public:
  long pageNumber;

  wxPrintInfo(void);
  ~wxPrintInfo(void);
};

// For compatibility with existing file formats:
// converts from/to a stream to/from a temporary file.
Bool wxTransferFileToStream(char *filename, ostream& stream);
Bool wxTransferStreamToFile(istream& stream, char *filename);


#endif
