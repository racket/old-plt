/*
 * File:	wx_doc.cc
 * Purpose:	Document class implementation
 * Author:	Julian Smart
 * After:       Carlo Alberto Avizzano
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"

#endif

#if USE_DOC_VIEW_ARCHITECTURE

#include "wx_win.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_dc.h"
#include "wx_dialg.h"
#include "wx_menu.h"
#include "wx_list.h"
#include "wx_doc.h"
#include "wx_cmdlg.h"

#include <stdio.h>
#include <iostream.h>
#include <fstream.h>

// wxDocManager *wxTheDocManager = NULL;

/*
 * wxDocItem: to be implemented
 */

/*

IMPLEMENT_ABSTRACT_CLASS(wxDocItem, wxObject)

wxDocItem::wxDocItem(void)
 :wxObject()
{
};

wxDocItem::~wxDocItem(void)
{
};

Bool wxDocItem::Draw(wxDC& context)
{
  return TRUE;
};
*/

/*
 * Definition of wxDocument
 */

IMPLEMENT_ABSTRACT_CLASS(wxDocument, wxEvtHandler)

wxDocument::wxDocument(wxDocument *parent)
{
  documentModified=FALSE;
  documentFile=NULL;
  documentTitle=NULL;
  documentParent=parent;
  documentTemplate = NULL;
  documentTypeName = NULL;

//  GetDocumentManager()->AddDocument(this);
}

Bool wxDocument::DeleteContents(void)
{
/*
  wxNode *node = documentItems.First();
  while (node)
  {
    wxDocItem *item = (wxDocItem *)node->Next();
    if (!item->OnDelete())
      return FALSE;
    else
    {
      wxNode *next = node->Next();
      delete item;
      delete node;
      node = next;
      Modify(TRUE);
    }
  }
*/
  return TRUE;
}

wxDocument::~wxDocument(void)
{
  DeleteContents();
  SetFilename(NULL);
  SetTitle(NULL);
  SetDocumentName(NULL);

  if (commandProcessor)
    delete commandProcessor;
    
  GetDocumentManager()->RemoveDocument(this);

  // Not safe to do here, since it'll
  // invoke virtual view functions expecting to see
  // valid derived objects: and by the time we get
  // here, we've called destructors higher up.
//  DeleteAllViews();
}
	
Bool wxDocument::Close(void)
{
  if (OnSaveModified())
    return OnCloseDocument();
  else
    return FALSE;
}
	
Bool wxDocument::OnCloseDocument(void)
{
  DeleteContents();
  SetFilename(NULL);
  SetTitle(NULL);
  Modify(FALSE);
  return TRUE;
}

// Note that this implicitly deletes the document when
// the last view is deleted.
Bool wxDocument::DeleteAllViews(void)
{
  wxNode *node = documentViews.First();
  while (node)
  {
    wxView *view = (wxView *)node->Data();
    if (!view->Close())
      return FALSE;

    wxNode *next = node->Next();
      
    delete view; // Deletes node implicitly
    node = next;
  }
  return TRUE;
}

wxView *wxDocument::GetFirstView(void)
{
  if (documentViews.Number() == 0)
    return NULL;
  return (wxView *)documentViews.First()->Data();
}

wxDocManager *wxDocument::GetDocumentManager(void)
{
  return documentTemplate->GetDocumentManager();
}

Bool wxDocument::OnNewDocument(void)
{
  if (!OnSaveModified())
    return FALSE;
    
  if (OnCloseDocument()==FALSE) return FALSE;
  DeleteContents();
  Modify(FALSE);
  SetFilename(NULL);

  char name[256];
  GetDocumentManager()->MakeDefaultName(name);
  SetTitle(name);

  // Notify the views that the filename has changed
  wxNode *node = documentViews.First();
  while (node)
  {
    wxView *view = (wxView *)node->Data();
    view->OnChangeFilename();
    node = node->Next();
  }

  return TRUE;
}
	
Bool wxDocument::Save(void)
{
  if (!IsModified()) return TRUE;
  if (!documentFile)
    return SaveAs();
  else
    return OnSaveDocument(documentFile);
}
	
Bool wxDocument::SaveAs(void)
{
  wxDocTemplate *docTemplate = GetDocumentTemplate();
  if (!docTemplate)
    return FALSE;
  
  char *tmp = wxFileSelector("Save as", docTemplate->GetDirectory(), NULL,
    docTemplate->GetDefaultExtension(), docTemplate->GetFileFilter(),
    0, GetDocumentWindow());
    
  if (!tmp)
    return FALSE;
  else
  {
    SetFilename(tmp);
    SetTitle(wxFileNameFromPath(tmp));
    
    GetDocumentManager()->AddFileToHistory(tmp);

    // Notify the views that the filename has changed
    wxNode *node = documentViews.First();
    while (node)
    {
      wxView *view = (wxView *)node->Data();
      view->OnChangeFilename();
      node = node->Next();
    }
  }
  return OnSaveDocument(documentFile);
}
	
Bool wxDocument::OnSaveDocument(char *file)
{
  if (!file)
    return FALSE;

  char *msgTitle = wxTheApp->GetAppName() ? wxTheApp->GetAppName() : "File error";

  ofstream store(file);
  if (store.fail() || store.bad())
  {
    (void)wxMessageBox("Sorry, could not open this file for saving.", msgTitle, wxOK | wxICON_EXCLAMATION,
      GetDocumentWindow());
    // Saving error
    return FALSE;
  }
  if (SaveObject(store)==FALSE)
  {
    (void)wxMessageBox("Sorry, could not save this file.", msgTitle, wxOK | wxICON_EXCLAMATION,
      GetDocumentWindow());
    // Saving error
    return FALSE;
  }
  Modify(FALSE);
  SetFilename(file);
  return TRUE;
}
	
Bool wxDocument::OnOpenDocument(char *file)
{
  if (!OnSaveModified())
    return FALSE;

  char *msgTitle = wxTheApp->GetAppName() ? wxTheApp->GetAppName() : "File error";

  ifstream store(file);
  if (store.fail() || store.bad())
  {
    (void)wxMessageBox("Sorry, could not open this file.", msgTitle, wxOK|wxICON_EXCLAMATION,
     GetDocumentWindow());
    return FALSE;
  }
  if (LoadObject(store)==FALSE)
  {
    (void)wxMessageBox("Sorry, could not open this file.", msgTitle, wxOK|wxICON_EXCLAMATION,
      GetDocumentWindow());
    return FALSE;
  }
  SetFilename(file);
  Modify(FALSE);

  // Notify the views that the filename has changed
  wxNode *node = GetViews().First();
  while (node)
  {
    wxView *view = (wxView *)node->Data();
    view->OnChangeFilename();
    node = node->Next();
  }
  UpdateAllViews();
  
  return TRUE;
}
	
istream& wxDocument::LoadObject(istream& stream)
{
  wxObject::LoadObject(stream);

  return stream;
}

ostream& wxDocument::SaveObject(ostream& stream)
{
  wxObject::SaveObject(stream);
  
  return stream;
}

Bool wxDocument::Revert(void)
{
  return FALSE;
}

void wxDocument::SetFilename(char *f)
{
  // Return if f == documentFilename
  if (f == documentFile)
    return;
  
  if (documentFile)
    delete[] documentFile;
  documentFile = NULL;
  if (f)
  {
    documentFile = copystring(f);
  }
}

void wxDocument::SetTitle(char *t)
{
  // Return if case f == documentFile
  if (t == documentTitle)
    return;

  if (documentTitle)
    delete[] documentTitle;
  documentTitle = NULL;
  if (t)
  {
    documentTitle = copystring(t);
  }
}

void wxDocument::SetDocumentName(char *name)
{
  if (name == documentTypeName)
    return;
    
  if (documentTypeName)
    delete[] documentTypeName;
  documentTypeName = NULL;
  if (name)
    documentTypeName = copystring(name);
}

// Get title, or filename if no title, else unnamed
Bool wxDocument::GetPrintableName(char *buf)
{
  if (documentTitle)
  {
    strcpy(buf, documentTitle);
    return TRUE;
  }
  else if (documentFile)
  {
    strcpy(buf, wxFileNameFromPath(documentFile));
    return TRUE;
  }
  else
  {
    strcpy(buf, "unnamed");
    return TRUE;
  }
}

wxWindow *wxDocument::GetDocumentWindow(void)
{
  wxView *view = GetFirstView();
  if (view)
    return view->GetFrame();
  else
    return wxTheApp->GetTopWindow();
}

wxCommandProcessor *wxDocument::OnCreateCommandProcessor(void)
{
  return new wxCommandProcessor;
}

// TRUE if safe to close
Bool wxDocument::OnSaveModified(void)
{
  if (IsModified())
  {
    char buf[300];
    char title[300];
    GetPrintableName(title);

    char *msgTitle = wxTheApp->GetAppName() ? wxTheApp->GetAppName() : "Warning";

    sprintf(buf, "Do you want to save changes to document %s?", title);
    int res = wxMessageBox(buf, msgTitle, wxYES_NO|wxCANCEL|wxICON_QUESTION,
      GetDocumentWindow());
    if (res == wxNO)
    {
      Modify(FALSE);
      return TRUE;
    }
    else if (res == wxYES)
      return Save();
    else if (res == wxCANCEL)
      return FALSE;
  }
  return TRUE;
}

Bool wxDocument::Draw(wxDC& WXUNUSED(context))
{
/*
  wxNode *current = documentItems.First();
  wxDocItem *obj;
  while (current)
  {
    wxNode *next = current->Next();
    obj=(wxDocItem *)(current->Data()); 
    if (obj->Draw(context)==FALSE) return FALSE;
    current = next;
  }
*/
  return TRUE;
}

/*
Bool wxDocument::Insert(wxDocItem *obj)
{
  Modify(TRUE);
  documentItems.Insert(obj);
  return TRUE;
}

Bool wxDocument::Append(wxDocItem *obj)
{
  Modify(TRUE);
  documentItems.Append(obj);
  return TRUE;
}

Bool wxDocument::Delete(wxDocItem *obj)
{
  Modify(TRUE);
  return documentItems.DeleteObject(obj);
}

Bool wxDocument::Exists(wxDocItem *obj)
{
  Modify(TRUE);
  return (documentItems.Member(obj) != NULL);
}
*/

Bool wxDocument::AddView(wxView *view)
{
  if (!documentViews.Member(view))
  {
    documentViews.Append(view);
    OnChangedViewList();
  }
  return TRUE;
}

Bool wxDocument::RemoveView(wxView *view)
{
  (void)documentViews.DeleteObject(view);
  OnChangedViewList();
  return TRUE;
}

Bool wxDocument::OnCreate(char *WXUNUSED(path), long flags)
{
  if (GetDocumentTemplate()->CreateView(this, flags))
    return TRUE;
  else
    return FALSE;
}

// Called after a view is added or removed.
// The default implementation deletes the document if
// there are no more views.
void wxDocument::OnChangedViewList(void)
{
  if (documentViews.Number() == 0)
  {
    if (OnSaveModified())
    {
      delete this;
    }
  }
}

void wxDocument::UpdateAllViews(wxView *sender, wxObject *hint)
{
  wxNode *node = documentViews.First();
  while (node)
  {
    wxView *view = (wxView *)node->Data();
    view->OnUpdate(sender, hint);
    node = node->Next();
  }
}

/*

// Definition of the class wxDocCad
// It provides facilites for the EDIT MENU
// and the FILE MENU

wxWinDoc::wxWinDoc(void): wxDocument() { buffer= NULL; }

wxWinDoc::~wxWinDoc(void) {}


//We have to make sure of not using free store.
// If our wxDocItem types use free store, we
// have to append a copy method to the class, and
// to rewrite constructor and destructor.


// With cut I can reuse the original copy    
Bool wxWinDoc::Cut(char *name)
{
  wxDocItem *tmp;
  tmp=Remove(name);
  if (tmp==NULL) return FALSE;
  if (buffer !=NULL) delete buffer;
  buffer=tmp;
  return TRUE;
}

// Here I have to make a copy and insert it in the buffer.
// If the buffer is already full, I've to clean it before
		
Bool wxWinDoc::Copy(char *name)
{
  wxDocItem *tmp;
  tmp = Exists(name);
  if (tmp== NULL) return FALSE; // No name matchs
  if (buffer !=NULL) delete buffer;
  buffer= wxDocItem::New(tmp->__type);
  *buffer= *tmp;
  return TRUE;
}


// Here I have to make a COPY OF THE BUFFER AND INSERT 
// THIS ONE
	
wxWinDoc::Paste(char *name)
{
  if (buffer==NULL) return FALSE;
  wxDocItem *tmp;
  tmp = wxDocItem::New(buffer->__type);
  *tmp= *buffer;
  Document.Insert(tmp);
  return TRUE;
}	
	
Bool wxWinDoc::Print(void)
{
  return FALSE;
}
*/

/*
 * Document view
 */
 
IMPLEMENT_ABSTRACT_CLASS(wxView, wxEvtHandler)

wxView::wxView(wxDocument *doc)
{
  SetDocument(doc);
  
  viewTypeName = NULL;
  viewFrame = NULL;
}

wxView::~wxView(void)
{
  GetDocumentManager()->ActivateView(this, FALSE, TRUE);
  viewDocument->RemoveView(this);
  SetViewName(NULL);
}

/*
Bool wxView::IsSelected(wxDocItem *item)
{
  return FALSE;
}
*/

void wxView::OnActivateView(Bool WXUNUSED(activate), wxView *WXUNUSED(activeView), wxView *WXUNUSED(deactiveView))
{
}

void wxView::OnPrint(wxDC *dc, wxPrintInfo *WXUNUSED(info))
{
  OnDraw(dc);
}

void wxView::OnUpdate(wxView *WXUNUSED(sender), wxObject *WXUNUSED(hint))
{
}

void wxView::OnChangeFilename(void)
{
  if (GetFrame() && GetDocument())
  {
    char name[256];
    GetDocument()->GetPrintableName(name);

    // If the frame is an MDI child, just set the title
    // to the name.
    // Otherwise, append the document name to the name of the application
    if ((GetFrame()->GetWindowStyleFlag() & wxMDI_CHILD) == wxMDI_CHILD)
    {
      GetFrame()->SetTitle(name);
    }
    else
    {
      char *s = wxTheApp->GetAppName();
      if (s)
      {
        char buf[400];
        sprintf(buf, "%s - %s", s, name);
        GetFrame()->SetTitle(buf);
      }
      else
        GetFrame()->SetTitle(name);
    }
  }
}

void wxView::SetViewName(char *name)
{
  if (viewTypeName)
    delete[] viewTypeName;
  viewTypeName = NULL;
  if (name)
    viewTypeName = copystring(name);
}

void wxView::SetDocument(wxDocument *doc)
{
  viewDocument = doc;
  if (doc)
    doc->AddView(this);
}

Bool wxView::Close(Bool deleteWindow)
{
  if (OnClose(deleteWindow))
    return TRUE;
  else
    return FALSE;
}

void wxView::Activate(Bool activate)
{
  if (GetDocumentManager())
  {
    OnActivateView(activate, this, GetDocumentManager()->GetCurrentView());
    GetDocumentManager()->ActivateView(this, activate);
  }
}

Bool wxView::OnClose(Bool WXUNUSED(deleteWindow))
{
  return GetDocument() ? GetDocument()->Close() : TRUE;
}

#if USE_PRINTING_ARCHITECTURE
wxPrintout *wxView::OnCreatePrintout(void)
{
  return new wxDocPrintout(this);
}
#endif


/*
 * wxDocTemplate
 */

IMPLEMENT_ABSTRACT_CLASS(wxDocTemplate, wxObject)

wxDocTemplate::wxDocTemplate(wxDocManager *manager, char *descr, char *filter, char *dir, char *ext,
  char *docTypeName, char *viewTypeName, wxClassInfo *docClassInfo, wxClassInfo *viewClassInfo,
  long flags)
{
  tDocumentManager = manager;
  tFlags = flags;
  if (descr)
    tDescription = copystring(descr);
  else
    tDescription = NULL;
  if (dir)
    tDirectory = copystring(dir);
  else
    tDirectory = NULL;
  if (ext)
    tDefaultExt = copystring(ext);
  else
    tDescription = NULL;
  if (filter)
    tFileFilter = copystring(filter);
  else
    tFileFilter = NULL;
  tFlags = flags;

  if (docTypeName)
    tDocTypeName = copystring(docTypeName);
  else
    tDocTypeName = NULL;

  if (viewTypeName)
    tViewTypeName = copystring(viewTypeName);
  else
    tViewTypeName = NULL;

  tDocumentManager->AssociateTemplate(this);

  tDocClassInfo = docClassInfo;
  tViewClassInfo = viewClassInfo;
}

wxDocTemplate::~wxDocTemplate(void)
{
  if (tFileFilter)
    delete[] tFileFilter;
  if (tDirectory)
    delete[] tDirectory;
  if (tDescription)
    delete[] tDescription;
  if (tDefaultExt)
    delete[] tDefaultExt;
  if (tDocTypeName)
    delete[] tDocTypeName;
  if (tViewTypeName)
    delete[] tViewTypeName;

  tDocumentManager->DisassociateTemplate(this);
}
  
void wxDocTemplate::SetFileFilter(char *filter)
{
  if (filter == tFileFilter)
    return;
    
  if (tFileFilter)
    delete[] tFileFilter;
  if (filter)
    tFileFilter = copystring(filter);
  else
    tFileFilter = NULL;
}

void wxDocTemplate::SetDirectory(char *dir)
{
  if (dir == tDirectory)
    return;
    
  if (tDirectory)
    delete[] tDirectory;
  if (dir)
    tDirectory = copystring(dir);
  else
    tDirectory = NULL;
}

void wxDocTemplate::SetDescription(char *descr)
{
  if (descr == tDescription)
    return;
    
  if (tDescription)
    delete[] tDescription;
  if (descr)
    tDescription = copystring(descr);
  else
    tDescription = NULL;
}

void wxDocTemplate::SetDefaultExtension(char *ext)
{
  if (ext == tDefaultExt)
    return;
    
  if (tDefaultExt)
    delete[] tDefaultExt;
  if (ext)
    tDefaultExt = copystring(ext);
  else
    tDefaultExt = NULL;
}

void wxDocTemplate::SetFlags(long flags)
{
  tFlags = flags;
}

// Tries to dynamically construct an object of the right
// class.
wxDocument *wxDocTemplate::CreateDocument(char *path, long flags)
{
  if (!tDocClassInfo)
    return NULL;
  wxDocument *doc = (wxDocument *)tDocClassInfo->CreateObject();
  doc->SetFilename((char *)path);
  doc->SetDocumentTemplate(this);
  GetDocumentManager()->AddDocument(doc);
  doc->SetCommandProcessor(doc->OnCreateCommandProcessor());
  
  if (doc->OnCreate(path, flags))
    return doc;
  else
  {
    delete doc;
    return NULL;
  }
}

wxView *wxDocTemplate::CreateView(wxDocument *doc, long flags)
{
  if (!tViewClassInfo)
    return NULL;
  wxView *view = (wxView *)tViewClassInfo->CreateObject();
  view->SetDocument(doc);
  if (view->OnCreate(doc, flags))
  {
    return view;
  }
  else
  {
    delete view;
    return NULL;
  }
}

IMPLEMENT_DYNAMIC_CLASS(wxDocManager, wxEvtHandler)

wxDocManager::wxDocManager(long flags, Bool initialize)
{
  defaultDocumentNameCounter = 1;
  mnFlags = flags;
  currentView = NULL;
  maxDocsOpen = 10000;
  fileHistory = NULL;
  if (initialize)
    Initialize();
}

wxDocManager::~wxDocManager(void)
{
  Clear();
  if (fileHistory)
    delete fileHistory;
}

Bool wxDocManager::Clear(Bool force)
{
  wxNode *node = mnDocs.First();
  while (node)
  {
    wxDocument *doc = (wxDocument *)node->Data();
    wxNode *next = node->Next();

    if (!doc->Close() && !force)
      return FALSE;

    // Implicitly deletes the document when the last
    // view is removed (deleted)
    doc->DeleteAllViews();

    // Check document is deleted
    if (mnDocs.Member(doc))
      delete doc;

    // This assumes that documents are not connected in
    // any way, i.e. deleting one document does NOT
    // delete another.
    node = next;
  }
  return TRUE;
}

Bool wxDocManager::Initialize(void)
{
  fileHistory = OnCreateFileHistory();
  return TRUE;
}

wxFileHistory *wxDocManager::OnCreateFileHistory(void)
{
  return new wxFileHistory;
}

// Handlers for common user commands
void wxDocManager::OnMenuCommand(int command)
{
  switch (command)
  {
    case wxID_OPEN:
      OnFileOpen();
      break;
    case wxID_CLOSE:
      OnFileClose();
      break;
    case wxID_NEW:
      OnFileNew();
      break;
    case wxID_SAVE:
      OnFileSave();
      break;
    case wxID_SAVEAS:
      OnFileSaveAs();
      break;
    case wxID_REVERT:
      OnFileRevert();
      break;
    case wxID_PRINT:
      OnPrint();
      break;
    case wxID_PRINT_SETUP:
      OnPrintSetup();
      break;
    case wxID_PREVIEW:
      OnPreview(); 
      break;
    case wxID_UNDO:
      OnUndo(); 
      break;
    case wxID_REDO:
      OnRedo(); 
      break;
    default:
    {
      wxView *view = GetCurrentView();
      if (view)
        view->OnMenuCommand(command);
      break;
    }
  }
}

void wxDocManager::OnFileClose(void)
{
  wxDocument *doc = GetCurrentDocument();
  if (!doc)
    return;
  if (doc->Close())
  {
    doc->DeleteAllViews();
    if (mnDocs.Member(doc))
      delete doc;
  }
}

void wxDocManager::OnFileNew(void)
{
  CreateDocument(NULL, wxDOC_NEW);
}

void wxDocManager::OnFileOpen(void)
{
  CreateDocument(NULL, 0);
}

void wxDocManager::OnFileRevert(void)
{
  wxDocument *doc = GetCurrentDocument();
  if (!doc)
    return;
  doc->Revert();
}

void wxDocManager::OnFileSave(void)
{
  wxDocument *doc = GetCurrentDocument();
  if (!doc)
    return;
  doc->Save();
}

void wxDocManager::OnFileSaveAs(void)
{
  wxDocument *doc = GetCurrentDocument();
  if (!doc)
    return;
  doc->SaveAs();
}

void wxDocManager::OnPrint(void)
{
  wxView *view = GetCurrentView();
  if (!view)
    return;
#if USE_PRINTING_ARCHITECTURE
  wxPrintout *printout = view->OnCreatePrintout();
  if (printout)
  {
    wxPrinter printer;
    printer.Print(view->GetFrame(), printout, TRUE);
    delete printout;
  }
#endif
}

void wxDocManager::OnPrintSetup(void)
{
  wxWindow *parentWin = wxTheApp->GetTopWindow();
  wxView *view = GetCurrentView();
  if (view)
    parentWin = view->GetFrame();
#if USE_PRINTING_ARCHITECTURE
  wxPrintDialog printerDialog(parentWin);
  printerDialog.GetPrintData().SetSetupDialog(TRUE);
  printerDialog.Show(TRUE);
#endif
}

void wxDocManager::OnPreview(void)
{
  wxView *view = GetCurrentView();
  if (!view)
    return;
#if USE_PRINTING_ARCHITECTURE
  wxPrintout *printout = view->OnCreatePrintout();
  if (printout)
  {
    // Pass two printout objects: for preview, and possible printing.
    wxPrintPreview *preview = new wxPrintPreview(printout, view->OnCreatePrintout());
    wxPreviewFrame *frame = new wxPreviewFrame(preview, (wxFrame *)wxTheApp->GetTopWindow(), "Print Preview", 100, 100, 600, 650);
    frame->Centre(wxBOTH);
    frame->Initialize();
    frame->Show(TRUE);
  }
#endif
}

void wxDocManager::OnUndo(void)
{
  wxDocument *doc = GetCurrentDocument();
  if (!doc)
    return;
  if (doc->GetCommandProcessor())
    doc->GetCommandProcessor()->Undo();
}

void wxDocManager::OnRedo(void)
{
  wxDocument *doc = GetCurrentDocument();
  if (!doc)
    return;
  if (doc->GetCommandProcessor())
    doc->GetCommandProcessor()->Redo();
}

wxDocument *wxDocManager::CreateDocument(char *path, long flags)
{
  wxDocTemplate **templates = new wxDocTemplate *[mnTemplates.Number()];
  int i;
  int n = 0;
  for (i = 0; i < mnTemplates.Number(); i++)
  {
    wxDocTemplate *temp = (wxDocTemplate *)(mnTemplates.Nth(i)->Data());
    if (temp->IsVisible())
    {
      templates[n] = temp;
      n ++;
    }
  }
  if (n == 0)
  {
    delete[] templates;
    return NULL;
  }

  // If we've reached the max number of docs, close the
  // first one.
  if (GetDocuments().Number() >= maxDocsOpen)
  {
    wxDocument *doc = (wxDocument *)GetDocuments().First()->Data();
    if (doc->Close())
    {
      // Implicitly deletes the document when
      // the last view is deleted
      doc->DeleteAllViews();

      // Check we're really deleted
      if (mnDocs.Member(doc))
        delete doc;
    }
    else
      return NULL;
  }
  
  // New document: user chooses a template, unless there's only one.
  if (flags & wxDOC_NEW)
  {
    if (n == 1)
    {
      wxDocTemplate *temp = templates[0];
      delete[] templates;
      wxDocument *newDoc = temp->CreateDocument(path, flags);
      if (newDoc)
      {
        newDoc->SetDocumentName(temp->GetDocumentName());
        newDoc->SetDocumentTemplate(temp);
        newDoc->OnNewDocument();
      }
      return newDoc;
    }

    wxDocTemplate *temp = SelectDocumentType(templates, n);
    delete[] templates;
    if (temp)
    {
      wxDocument *newDoc = temp->CreateDocument(path, flags);
      if (newDoc)
      {
        newDoc->SetDocumentName(temp->GetDocumentName());
        newDoc->SetDocumentTemplate(temp);
        newDoc->OnNewDocument();
      }
      return newDoc;
    }
    else
      return NULL;
  }

  // Existing document
  wxDocTemplate *temp = NULL;

  char path2[400];
  if (path)
    strcpy(path2, path);
  else
    path2[0] = 0;

  if (flags & wxDOC_SILENT)
    temp = FindTemplateForPath(path2);
  else
    temp = SelectDocumentPath(templates, n, path2, sizeof(path2), flags);

  delete[] templates;

  if (temp)
  {
    wxDocument *newDoc = temp->CreateDocument(path2, flags);
    if (newDoc)
    {
      newDoc->SetDocumentName(temp->GetDocumentName());
      newDoc->SetDocumentTemplate(temp);
      if (!newDoc->OnOpenDocument((char *)path2))
      {
        delete newDoc;
        return NULL;
      }
      AddFileToHistory((char *)path2);
    }
    return newDoc;
  }
  else
    return NULL;
}

wxView *wxDocManager::CreateView(wxDocument *doc, long flags)
{
  wxDocTemplate **templates = new wxDocTemplate *[mnTemplates.Number()];
  int n =0;
  int i;
  for (i = 0; i < mnTemplates.Number(); i++)
  {
    wxDocTemplate *temp = (wxDocTemplate *)(mnTemplates.Nth(i)->Data());
    if (temp->IsVisible())
    {
      if (strcmp(temp->GetDocumentName(), doc->GetDocumentName()) == 0)
      {
        templates[n] = temp;
        n ++;
      }
    }
  }
  if (n == 0)
  {
    delete[] templates;
    return NULL;
  }
  if (n == 1)
  {
    wxDocTemplate *temp = templates[0];
    delete[] templates;
    wxView *view = temp->CreateView(doc, flags);
    if (view)
      view->SetViewName(temp->GetViewName());
    return view;
  }
  
  wxDocTemplate *temp = SelectViewType(templates, n);
  delete[] templates;
  if (temp)
  {
    wxView *view = temp->CreateView(doc, flags);
    if (view)
      view->SetViewName(temp->GetViewName());
    return view;
  }
  else
    return NULL;
}

// Not yet implemented
void wxDocManager::DeleteTemplate(wxDocTemplate *WXUNUSED(temp), long WXUNUSED(flags))
{
}

// Not yet implemented
Bool wxDocManager::FlushDoc(wxDocument *WXUNUSED(doc))
{
  return FALSE;
}

wxDocument *wxDocManager::GetCurrentDocument(void)
{
  if (currentView)
    return currentView->GetDocument();
  else
    return NULL;
}

// Make a default document name
Bool wxDocManager::MakeDefaultName(char *buf)
{
  sprintf(buf, "unnamed%d", defaultDocumentNameCounter);
  defaultDocumentNameCounter ++;
  return TRUE;
}

// Not yet implemented
wxDocTemplate *wxDocManager::MatchTemplate(char *WXUNUSED(path))
{
  return NULL;
}

// File history management
void wxDocManager::AddFileToHistory(char *file)
{
  if (fileHistory)
    fileHistory->AddFileToHistory(file);
}

char *wxDocManager::GetHistoryFile(int i)
{
  if (fileHistory)
    return fileHistory->GetHistoryFile(i);
  else
    return NULL;
}

void wxDocManager::FileHistoryUseMenu(wxMenu *menu)
{
  if (fileHistory)
    fileHistory->FileHistoryUseMenu(menu);
}

void wxDocManager::FileHistoryLoad(char *resourceFile, char *section)
{
  if (fileHistory)
    fileHistory->FileHistoryLoad(resourceFile, section);
}

void wxDocManager::FileHistorySave(char *resourceFile, char *section)
{
  if (fileHistory)
    fileHistory->FileHistorySave(resourceFile, section);
}

int wxDocManager::GetNoHistoryFiles(void)
{
  if (fileHistory)
    return fileHistory->GetNoHistoryFiles();
  else
    return 0;
}

static char *FindExtension(char *path)
{
  static char ext[10];
  int len = strlen(path);
  if (path)
  {
    int i = 0;
    for (i = (len-1); i > 0; i --)
      if (path[i] == '.')
        break;
    if (path[i] == '.')
    {
      int j;
      for (j = i+1; j < len; j++)
        ext[(int)(j-(i+1))] = (char)wxToLower(path[j]); // NOTE Should not use tolower under UNIX
      ext[j-(i+1)] = 0;
      return ext;
    }
    else
      return NULL;
  }
  else return NULL;
}


// Given a path, try to find a matching template. Won't
// always work, of course.
wxDocTemplate *wxDocManager::FindTemplateForPath(char *path)
{
  char *theExt = FindExtension(path);
  if (!theExt)
    return NULL;
  wxDocTemplate *theTemplate = NULL;

  if (mnTemplates.Number() == 1)
    return (wxDocTemplate *)mnTemplates.First()->Data();
    
  // Find the template which this extension corresponds to
  int i;
  for (i = 0; i < mnTemplates.Number(); i++)
  {
    wxDocTemplate *temp = (wxDocTemplate *)mnTemplates.Nth(i)->Data();
    if (strcmp(temp->GetDefaultExtension(), theExt) == 0)
    {
      theTemplate = temp;
      break;
    }
  }
  return theTemplate;
}

// Prompts user to open a file, using file specs in templates.
// How to implement in wxWindows? Must extend the file selector
// dialog or implement own; OR match the extension to the
// template extension.
wxDocTemplate *wxDocManager::SelectDocumentPath(wxDocTemplate **templates,
    int noTemplates, char *path, int WXUNUSED(bufSize), long WXUNUSED(flags), Bool WXUNUSED(save))
{
  // We can only have multiple filters in Windows
#ifdef wx_msw
  char *descrBuf = new char[1000];
  descrBuf[0] = 0;
  int i;
  for (i = 0; i < noTemplates; i++)
  {
    if (templates[i]->IsVisible())
    {
      strcat(descrBuf, templates[i]->GetDescription());
      strcat(descrBuf, " (");
      strcat(descrBuf, templates[i]->GetFileFilter());
      strcat(descrBuf, ") ");
      strcat(descrBuf, "|");
      strcat(descrBuf, templates[i]->GetFileFilter());
      strcat(descrBuf, "|");
    }
  }
  int len = strlen(descrBuf);
  if (len > 0)
    // Omit final "|"
    descrBuf[len-1] = 0;

  char *pathTmp = wxFileSelector("Select a file", NULL, NULL, NULL, descrBuf, 0, wxTheApp->GetTopWindow());
  delete[] descrBuf;
  if (pathTmp)
  {
    strcpy(path, pathTmp);
    char *theExt = FindExtension(path);
    if (!theExt)
      return NULL;

    // This is dodgy in that we're selecting the template on the
    // basis of the file extension, which may not be a standard
    // one. We really want to know exactly which template was
    // chosen by using a more advanced file selector.
    wxDocTemplate *theTemplate = FindTemplateForPath(path);
    return theTemplate;
  }
  else
  {
    path[0] = 0;
    return NULL;
  }
#else
  // In all other windowing systems, until we have more advanced
  // file selectors, we must select the document type (template) first, and
  // _then_ pop up the file selector.
  wxDocTemplate *temp = SelectDocumentType(templates, noTemplates);
  if (!temp)
    return NULL;

  char *pathTmp = wxFileSelector("Select a file", NULL, NULL,
     temp->GetDefaultExtension(),
     temp->GetFileFilter(),
     0, wxTheApp->GetTopWindow());

  if (pathTmp)
  {
    strcpy(path, pathTmp);
    return temp;
  }
  else
    return NULL;
#endif
}

wxDocTemplate *wxDocManager::SelectDocumentType(wxDocTemplate **templates,
    int noTemplates)
{
  char **strings = new char *[noTemplates];
  char **data = new char *[noTemplates];
  int i;
  int n = 0;
  for (i = 0; i < noTemplates; i++)
  {
    if (templates[i]->IsVisible())
    {
      strings[n] = templates[i]->GetDescription();
      data[n] = (char *)templates[i];
      n ++;
    }
  }
  if (n == 0)
  {
    delete[] strings;
    delete[] data;
    return NULL;
  }
  else if (n == 1)
  {
    wxDocTemplate *temp = (wxDocTemplate *)data[0];
    delete[] strings;
    delete[] data;
    return temp;
  }
  
  wxDocTemplate *theTemplate = (wxDocTemplate *)wxGetSingleChoiceData("Select a document template", "Templates", n,
    strings, data);
  delete[] strings;
  delete[] data;
  return theTemplate;
}

wxDocTemplate *wxDocManager::SelectViewType(wxDocTemplate **templates,
    int noTemplates)
{
  char **strings = new char *[noTemplates];
  char **data = new char *[noTemplates];
  int i;
  int n = 0;
  for (i = 0; i < noTemplates; i++)
  {
    if (templates[i]->IsVisible() && templates[i]->GetViewName())
    {
      strings[n] = templates[i]->GetViewName();
      data[n] = (char *)templates[i];
    }
  }
  wxDocTemplate *theTemplate = (wxDocTemplate *)wxGetSingleChoiceData("Select a document view", "Views", n,
    strings, data);
  delete[] strings;
  delete[] data;
  return theTemplate;
}

void wxDocManager::AssociateTemplate(wxDocTemplate *temp)
{
  if (!mnTemplates.Member(temp))
    mnTemplates.Append(temp);
}

void wxDocManager::DisassociateTemplate(wxDocTemplate *temp)
{
  mnTemplates.DeleteObject(temp);
}

// Add and remove a document from the manager's list
void wxDocManager::AddDocument(wxDocument *doc)
{
  if (!mnDocs.Member(doc))
    mnDocs.Append(doc);
}

void wxDocManager::RemoveDocument(wxDocument *doc)
{
  mnDocs.DeleteObject(doc);
}

// Views or windows should inform the document manager
// when a view is going in or out of focus
void wxDocManager::ActivateView(wxView *view, Bool activate, Bool WXUNUSED(deleting))
{
  // If we're deactiving, and if we're not actually deleting the view, then
  // don't reset the current view because we may be going to
  // a window without a view.
  // WHAT DID I MEAN BY THAT EXACTLY?
/*
  if (deleting)
  {
    if (currentView == view)
      currentView = NULL;
  }
  else
*/
  {
    if (activate)
      currentView = view;
    else
      currentView = NULL;
  }
}

/*
 * Default document child frame
 */
 
IMPLEMENT_CLASS(wxDocChildFrame, wxFrame)

wxDocChildFrame::wxDocChildFrame(wxDocument *doc, wxView *view, wxFrame *frame, char *title,
  int x, int y, int w, int h, long style, char *name):
    wxFrame(frame, title, x, y, w, h, style, name)
{
  childDocument = doc;
  childView = view;
  if (view)
    view->SetFrame(this);
}

wxDocChildFrame::~wxDocChildFrame(void)
{
}

// Intercept menu commands
void wxDocChildFrame::OnMenuCommand(int id)
{
  if (childView)
    childView->Activate(TRUE);

  if (GetParent())
    ((wxFrame *)GetParent())->OnMenuCommand(id);
}

void wxDocChildFrame::OnActivate(Bool active)
{
  wxFrame::OnActivate(active);

  if (childView)
    childView->Activate(active);
}

Bool wxDocChildFrame::OnClose(void)
{
  // Close view but don't delete the frame while doing so!
  // ...since it will be deleted by wxWindows if we return TRUE.
  if (childView)
  {
    Bool ans = childView->Close(FALSE); // FALSE means don't delete associated window
    if (ans)
    {
      childView->Activate(FALSE);
      delete childView;
      childView = NULL;
      childDocument = NULL;
    }
    
    return ans;
  }
  else return TRUE;
}

/*
 * Default parent frame
 */

IMPLEMENT_CLASS(wxDocParentFrame, wxFrame)
 
wxDocParentFrame::wxDocParentFrame(wxDocManager *manager, wxFrame *frame, char *title, int x, int y, int w, int h, long type, char *name):
  wxFrame(frame, title, x, y, w, h, type, name)
{
  docManager = manager;
}

// Intercept menu commands
void wxDocParentFrame::OnMenuCommand(int id)
{
  switch (id)
  {
    case wxID_EXIT:
    {
      if (GetEventHandler()->OnClose())
        delete this;
      break;
    }
    case wxID_FILE1:
    case wxID_FILE2:
    case wxID_FILE3:
    case wxID_FILE4:
    case wxID_FILE5:
    case wxID_FILE6:
    case wxID_FILE7:
    case wxID_FILE8:
    case wxID_FILE9:
    {
      char *f = docManager->GetHistoryFile(id-wxID_FILE1);
      if (f)
        (void)docManager->CreateDocument(f, wxDOC_SILENT);
      break;
    }
    default:
    {
      docManager->OnMenuCommand(id);
    }
  }
}

// Define the behaviour for the frame closing
// - must delete all frames except for the main one.
Bool wxDocParentFrame::OnClose(void)
{
  return docManager->Clear(FALSE);
}

#if USE_PRINTING_ARCHITECTURE

IMPLEMENT_DYNAMIC_CLASS(wxDocPrintout, wxPrintout)

wxDocPrintout::wxDocPrintout(wxView *view, char *title):
  wxPrintout(title)
{
  printoutView = view;
}

Bool wxDocPrintout::OnPrintPage(int WXUNUSED(page))
{
  wxDC *dc = GetDC();
  
  // Get the logical pixels per inch of screen and printer
  int ppiScreenX, ppiScreenY;
  GetPPIScreen(&ppiScreenX, &ppiScreenY);
  int ppiPrinterX, ppiPrinterY;
  GetPPIPrinter(&ppiPrinterX, &ppiPrinterY);

  // This scales the DC so that the printout roughly represents the
  // the screen scaling. The text point size _should_ be the right size
  // but in fact is too small for some reason. This is a detail that will
  // need to be addressed at some point but can be fudged for the
  // moment.
  float scale = (float)((float)ppiPrinterX/(float)ppiScreenX);

  // Now we have to check in case our real page size is reduced
  // (e.g. because we're drawing to a print preview memory DC)
  int pageWidth, pageHeight;
  float w, h;
  dc->GetSize(&w, &h);
  GetPageSizePixels(&pageWidth, &pageHeight);

  // If printer pageWidth == current DC width, then this doesn't
  // change. But w might be the preview bitmap width, so scale down.
  float overallScale = scale * (float)(w/(float)pageWidth);
  dc->SetUserScale(overallScale, overallScale);

  if (printoutView)
  {
    printoutView->OnDraw(dc);
  }
  return TRUE;
}

Bool wxDocPrintout::HasPage(int pageNum)
{
  return (pageNum == 1);
}

Bool wxDocPrintout::OnBeginDocument(int startPage, int endPage)
{
  if (!wxPrintout::OnBeginDocument(startPage, endPage))
    return FALSE;

  return TRUE;
}

void wxDocPrintout::GetPageInfo(int *minPage, int *maxPage, int *selPageFrom, int *selPageTo)
{
  *minPage = 1;
  *maxPage = 1;
  *selPageFrom = 1;
  *selPageTo = 1;
}

#endif

/*
 * Command processing framework
 */

IMPLEMENT_CLASS(wxCommand, wxObject)

wxCommand::wxCommand(Bool canUndoIt, char *name)
{
  canUndo = canUndoIt;
  if (name)
    commandName = copystring(name);
  else
    commandName = NULL;
}

wxCommand::~wxCommand(void)
{
  if (commandName)
    delete[] commandName;
}

// Command processor
IMPLEMENT_DYNAMIC_CLASS(wxCommandProcessor, wxObject)

wxCommandProcessor::wxCommandProcessor(int maxCommands)
{
  maxNoCommands = maxCommands;
  currentCommand = NULL;
  commandEditMenu = NULL;
}

wxCommandProcessor::~wxCommandProcessor(void)
{
  ClearCommands();
}

// Pass a command to the processor. The processor calls Do();
// if successful, is appended to the command history unless
// storeIt is FALSE.
Bool wxCommandProcessor::Submit(wxCommand *command, Bool storeIt)
{
  Bool success = command->Do();
  if (success && storeIt)
  {
    if (commands.Number() == maxNoCommands)
    {
      wxNode *firstNode = commands.First();
      wxCommand *firstCommand = (wxCommand *)firstNode->Data();
      delete firstCommand;
      delete firstNode;
    }
    
    commands.Append(command);
    currentCommand = commands.Last();
    SetMenuStrings();
  }
  return success;
}

Bool wxCommandProcessor::Undo(void)
{
  if (currentCommand)
  {
    wxCommand *command = (wxCommand *)currentCommand->Data();
    if (command->CanUndo())
    {
      Bool success = command->Undo();
      if (success)
      {
        currentCommand = currentCommand->Previous();
        SetMenuStrings();
        return TRUE;
      }
    }
  }
  return FALSE;
}

Bool wxCommandProcessor::Redo(void)
{
  wxCommand *redoCommand = NULL;
  wxNode *redoNode = NULL;
  if (currentCommand && currentCommand->Next())
  {
    redoCommand = (wxCommand *)currentCommand->Next()->Data();
    redoNode = currentCommand->Next();
  }
  else
  {
    if (commands.Number() > 0)
    {
      redoCommand = (wxCommand *)commands.First()->Data();
      redoNode = commands.First();
    }
  }

  if (redoCommand)
  {
    Bool success = redoCommand->Do();
    if (success)
    {
      currentCommand = redoNode;
      SetMenuStrings();
      return TRUE;
    }
  }
  return FALSE;
}

Bool wxCommandProcessor::CanUndo(void)
{
  if (currentCommand)
    return ((wxCommand *)currentCommand->Data())->CanUndo();
  return FALSE;
}

void wxCommandProcessor::Initialize(void)
{
  currentCommand = commands.Last();
  SetMenuStrings();
}

void wxCommandProcessor::SetMenuStrings(void)
{
  if (commandEditMenu)
  {
    char buf[256];
    if (currentCommand)
    {
      wxCommand *command = (wxCommand *)currentCommand->Data();
      char *commandName = command->GetName();
      if (!commandName) commandName = "Unnamed command";
      Bool canUndo = command->CanUndo();
      if (canUndo)
        sprintf(buf, "&Undo %s", commandName);
      else
        sprintf(buf, "Can't &Undo %s", commandName);
        
      commandEditMenu->SetLabel(wxID_UNDO, buf);
      commandEditMenu->Enable(wxID_UNDO, canUndo);
      
      // We can redo, if we're not at the end of the history.
      if (currentCommand->Next())
      {
        wxCommand *redoCommand = (wxCommand *)currentCommand->Next()->Data();
        char *redoCommandName = redoCommand->GetName();
        if (!redoCommandName) redoCommandName = "Unnamed command";
        sprintf(buf, "&Redo %s", redoCommandName);
        commandEditMenu->SetLabel(wxID_REDO, buf);
        commandEditMenu->Enable(wxID_REDO, TRUE);
      }
      else
      {
        commandEditMenu->SetLabel(wxID_REDO, "&Redo");
        commandEditMenu->Enable(wxID_REDO, FALSE);
      }
    }
    else
    {
      commandEditMenu->SetLabel(wxID_UNDO, "&Undo");
      commandEditMenu->Enable(wxID_UNDO, FALSE);

      if (commands.Number() == 0)
      {
        commandEditMenu->SetLabel(wxID_REDO, "&Redo");
        commandEditMenu->Enable(wxID_REDO, FALSE);
      }
      else
      {
        // currentCommand is NULL but there are commands: this means that
        // we've undone to the start of the list, but can redo the first.
        wxCommand *redoCommand = (wxCommand *)commands.First()->Data();
        char *redoCommandName = redoCommand->GetName();
        if (!redoCommandName) redoCommandName = "Unnamed command";
        sprintf(buf, "&Redo %s", redoCommandName);
        commandEditMenu->SetLabel(wxID_REDO, buf);
        commandEditMenu->Enable(wxID_REDO, TRUE);
      }
    }
  }
}
void wxCommandProcessor::ClearCommands(void)
{
  wxNode *node = commands.First();
  while (node)
  {
    wxCommand *command = (wxCommand *)node->Data();
    delete command;
    delete node;
    node = commands.First();
  }
  currentCommand = NULL;
}


/*
 * File history processor
 */

IMPLEMENT_DYNAMIC_CLASS(wxFileHistory, wxObject)

wxFileHistory::wxFileHistory(int maxFiles)
{
  fileMaxFiles = maxFiles;
  fileMenu = NULL;
  fileHistoryN = 0;
  fileHistory = new char *[fileMaxFiles];
}

wxFileHistory::~wxFileHistory(void)
{
  int i;
  for (i = 0; i < fileHistoryN; i++)
    delete[] fileHistory[i];
  delete[] fileHistory;
}

// File history management
void wxFileHistory::AddFileToHistory(char *file)
{
  if (!fileMenu)
    return;
    
  int i;

  // Check we don't already have this file
  for (i = 0; i < fileHistoryN; i++)
  {
    if (fileHistory[i] && (strcmp(fileHistory[i], file) == 0))
      return;
  }
  
  // Add to the project file history:
  // Move existing files (if any) down so we can insert file at beginning.
  
  // First delete filename that has popped off the end of the array (if any)
  if (fileHistoryN == fileMaxFiles)
  {
    delete[] fileHistory[fileMaxFiles-1];
    fileHistory[fileMaxFiles-1] = NULL;
  }
  if (fileHistoryN < fileMaxFiles)
  {
    if (fileHistoryN == 0)
      fileMenu->AppendSeparator();
    fileMenu->Append(wxID_FILE1+fileHistoryN, "[EMPTY]");
    fileHistoryN ++;
  }
  // Shuffle filenames down
  for (i = (fileHistoryN-1); i > 0; i--)
  {
    fileHistory[i] = fileHistory[i-1];
  }
  fileHistory[0] = copystring(file);
  
  for (i = 0; i < fileHistoryN; i++)
    if (fileHistory[i])
    {
      char buf[400];
      sprintf(buf, "&%d %s", i+1, fileHistory[i]);
      fileMenu->SetLabel(wxID_FILE1+i, buf);
    }
}

char *wxFileHistory::GetHistoryFile(int i)
{
  if (i < fileHistoryN)
    return fileHistory[i];
  else
    return NULL;
}

void wxFileHistory::FileHistoryUseMenu(wxMenu *menu)
{
  fileMenu = menu;
}

void wxFileHistory::FileHistoryLoad(char *resourceFile, char *section)
{
#if USE_RESOURCES
  fileHistoryN = 0;
  char buf[400];
  sprintf(buf, "file%d", fileHistoryN+1);
  char *historyFile = NULL;
  while ((fileHistoryN <= fileMaxFiles) && wxGetResource(section, buf, &historyFile, resourceFile) && historyFile)
  {
    // wxGetResource allocates memory so this is o.k.
    fileHistory[fileHistoryN] = historyFile;
    fileHistoryN ++;
    sprintf(buf, "file%d", fileHistoryN+1);
    historyFile = NULL;
  }
#endif
}

void wxFileHistory::FileHistorySave(char *resourceFile, char *section)
{
#if USE_RESOURCES
  char buf[400];
  int i;
  for (i = 0; i < fileHistoryN; i++)
  {
    sprintf(buf, "file%d", i+1);
    wxWriteResource(section, buf, fileHistory[i], resourceFile);
  }
#endif
}
  
/*
 * wxPrintInfo
 */

IMPLEMENT_DYNAMIC_CLASS(wxPrintInfo, wxObject)

wxPrintInfo::wxPrintInfo(void)
{
  pageNumber = 1;
}

wxPrintInfo::~wxPrintInfo(void)
{
}

/*
 * Permits compatibility with existing file formats and functions
 * that manipulate files directly
 */
 
Bool wxTransferFileToStream(char *filename, ostream& stream)
{
  FILE *fd1;
  int ch;

  if ((fd1 = fopen (filename, "rb")) == NULL)
    return FALSE;

  while ((ch = getc (fd1)) != EOF)
    stream << (unsigned char)ch;

  fclose (fd1);
  return TRUE;
}

Bool wxTransferStreamToFile(istream& stream, char *filename)
{
  FILE *fd1;
  int ch;

  if ((fd1 = fopen (filename, "wb")) == NULL)
    {
      return FALSE;
    }

  while (!stream.eof())
  {
    ch = stream.get();
    if (!stream.eof())
      putc (ch, fd1);
  }
  fclose (fd1);
  return TRUE;
}

#endif
  // End USE_DOC_VIEW_ARCHITECTURE
