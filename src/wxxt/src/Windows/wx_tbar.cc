/*
 * File:	wx_tbar.cc
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "%W% %G%";

#ifdef __GNUG__
#pragma implementation "wx_tbar.h"
#endif

#include "common.h"

#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#if USE_TOOLBAR

#include "wx_tbar.h"

IMPLEMENT_DYNAMIC_CLASS(wxToolBarTool, wxObject)

IMPLEMENT_DYNAMIC_CLASS(wxToolBar, wxCanvas)

static wxPen *thickBlackPen = NULL;

wxToolBarTool::wxToolBarTool(int theIndex,
                    wxBitmap *theBitmap1, wxBitmap *theBitmap2, Bool toggle, wxCanvas *WXUNUSED(canvas),
                    float WXUNUSED(xPos), float WXUNUSED(yPos))
{
  if (!theBitmap1->Ok() || theBitmap1->selectedIntoDC)
    theBitmap1 = NULL;
  if (theBitmap2 && (!theBitmap2->Ok() || theBitmap2->selectedIntoDC))
    theBitmap2 = NULL;

  clientData = NULL;
  index = theIndex;
  isToggle = toggle;
  toggleState = FALSE;
  enabled = TRUE;
  bitmap1 = theBitmap1;
  width = 0.0;
  height = 0.0;
  deleteSecondBitmap = FALSE;
  if (theBitmap1)
  {
    width = (float)theBitmap1->GetWidth();
    height = (float)theBitmap1->GetHeight();
  }

  if (theBitmap2)
    bitmap2 = theBitmap2;
  else
    bitmap2 = NULL;
}

wxToolBarTool::~wxToolBarTool(void)
{
  if (bitmap1)
    bitmap1->selectedIntoDC = FALSE;
  if (bitmap2)
    bitmap2->selectedIntoDC = FALSE;

  if (deleteSecondBitmap && bitmap2)
    delete bitmap2;
}

wxToolBar::wxToolBar(wxFrame *parent, int x, int y, int w, int h, long style,
                     int direction, int RowsOrColumns, char *name):
  wxCanvas(parent, x, y, w, h, style, name), tools(wxKEY_INTEGER)
{
  __type = wxTYPE_TOOLBAR;
  tilingDirection = direction;
  rowsOrColumns = RowsOrColumns;
  maxWidth = 0.0;
  maxHeight = 0.0;
  xMargin = 0.0;
  yMargin = 0.0;
  currentTool = -1;
}

wxToolBar::~wxToolBar(void)
{
  wxNode *node = tools.First();
  while (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    delete tool;
    node = node->Next();
  }
}

static int wxOnPaintCount = 0;
void wxToolBar::OnPaint(void)
{
  // Prevent reentry of OnPaint which would cause
  // wxMemoryDC errors.
  if (wxOnPaintCount > 0)
    return;
  wxOnPaintCount ++;

  wxMemoryDC dc2(GetDC());

  wxNode *node = tools.First();
  while (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    DrawTool(dc2, tool);
    node = node->Next();
  }
  wxOnPaintCount --;
}

void wxToolBar::OnSize(int w, int h)
{
  wxCanvas::OnSize(w, h);
}

void wxToolBar::OnKillFocus(void)
{
  currentTool = -1;
  OnMouseEnter(-1);
}

void wxToolBar::OnEvent(wxMouseEvent& event)
{
/*
  if (event.Leaving())
  {
    currentTool = -1;
    OnMouseEnter(-1);
    return;
  }

  float x, y;
  event.Position(&x, &y);
  wxToolBarTool *tool = FindToolForPosition(x, y);

  if (!tool)
  {
    if (currentTool > -1)
    {
      currentTool = -1;
      OnMouseEnter(-1);
    }
    return;
  }
  
  if (!event.Dragging() && !event.IsButton())
  {
    if (tool->index != currentTool)
    {
      OnMouseEnter(tool->index);
      currentTool = tool->index;
      return;
    }
  }
  if (event.LeftDown() && tool->enabled)
  {
    if (tool->isToggle)
    {
      tool->toggleState = !tool->toggleState;
      if (OnLeftClick(tool->index, tool->toggleState))
      {
        wxMemoryDC *dc2 = new wxMemoryDC(GetDC());
        DrawTool(*dc2, tool);
        delete dc2;
      }
      else tool->toggleState = !tool->toggleState;
    }
    else OnLeftClick(tool->index, FALSE);
  }
  else if (event.RightDown())
  {
    OnRightClick(tool->index, x, y);
  }
*/
  if (event.Leaving())
  {
    if (currentTool > -1)
    {
      SpringUpButton(currentTool);
      currentTool = -1;
      OnMouseEnter(-1);
    }
    return;
  }

  float x, y;
  event.Position(&x, &y);
  wxToolBarTool *tool = FindToolForPosition(x, y);

  if (!tool)
  {
    if (currentTool > -1)
    {
      SpringUpButton(currentTool);
      currentTool = -1;
      OnMouseEnter(-1);
    }
    return;
  }

  if (!event.IsButton())
  {
    if (tool->index != currentTool)
    {
      SpringUpButton(currentTool);
      OnMouseEnter(tool->index);
      currentTool = tool->index;
      return;
    }
  }
  // Left buton pressed.
  // If the tool is enabled and it is not a toggle tool, then depress the
  // button. If the tool is enabled and it is a toggle tool, then Call
  // OnLeftClick to determine if we can toggle it.
  //
  if (event.LeftDown() && tool->enabled)
  {
    if (tool->isToggle)
    {
      Bool tempToggleState = tool->toggleState;
      if (OnLeftClick(tool->index, !tool->toggleState))
      {
        tool->toggleState = !tempToggleState;
        wxMemoryDC *dc2 = new wxMemoryDC(GetDC());
        DrawTool(*dc2, tool);
        delete dc2;
      }
    }
    else {
      if (!tool->toggleState){
        tool->toggleState = TRUE;
        wxMemoryDC *dc2 = new wxMemoryDC(GetDC());
        DrawTool(*dc2, tool);
        delete dc2;
      }
    }
  }
  else if (event.RightDown())
  {
    OnRightClick(tool->index, x, y);
  }
  // Left Button Released.
  // If the button is enabled and it is not a toggle tool and it is
  // in the pressed state, then raise the button and call OnLeftClick.
  //
  if (event.LeftUp() && tool->enabled && !tool->isToggle && tool->toggleState){
    tool->toggleState = FALSE;
    OnLeftClick(tool->index, FALSE);
    wxMemoryDC *dc2 = new wxMemoryDC(GetDC());
    DrawTool(*dc2, tool);
    delete dc2;
  }
}

void wxToolBar::DrawTool(wxMemoryDC& memDC, wxToolBarTool *tool)
{
  wxCanvasDC *dc = GetDC();
  wxBitmap *bitmap = tool->toggleState ? tool->bitmap2 : tool->bitmap1;

  if (bitmap)
  {
    if (bitmap->GetColourMap())
      memDC.SetColourMap(bitmap->GetColourMap());
      
    memDC.SelectObject(bitmap);
    dc->Blit(tool->x, tool->y,
            (float)bitmap->GetWidth(), (float)bitmap->GetHeight(),
            &memDC, 0, 0);
    // Select bitmap out of the DC
    memDC.SelectObject(NULL);
    memDC.SetColourMap(NULL);
  }
  // No second bitmap, so draw a thick line around bitmap, or invert if mono
  else if (tool->toggleState)
  {
    Bool drawBorder = FALSE;
#ifdef wx_x // X doesn't invert properly on colour
    drawBorder = wxColourDisplay();
#else       // Inversion works fine under Windows
    drawBorder = FALSE;
#endif

    if (!drawBorder)
    {
      memDC.SelectObject(tool->bitmap1);
      dc->Blit(tool->x, tool->y,
               (float)tool->bitmap1->GetWidth(),
               (float)tool->bitmap1->GetHeight(),
               &memDC, 0, 0, wxSRC_INVERT);
      memDC.SelectObject(NULL);
    }
    else
    {
      if (!thickBlackPen)
        thickBlackPen = new wxPen("BLACK", 3, wxSOLID);
      float x = tool->x;
      float y = tool->y;
      float w = (float)tool->bitmap1->GetWidth();
      float h = (float)tool->bitmap1->GetHeight();

      memDC.SelectObject(tool->bitmap1);
      dc->SetClippingRegion(tool->x, tool->y, w, h);
      dc->Blit(tool->x, tool->y, w, h,
               &memDC, 0, 0);
      dc->SetPen(thickBlackPen);
      dc->SetBrush(wxTRANSPARENT_BRUSH);
      dc->DrawRectangle(x, y, w-1, h-1);
      dc->DestroyClippingRegion();
      memDC.SelectObject(NULL);
    }
  }
}

// Only allow toggle if returns TRUE
Bool wxToolBar::OnLeftClick(int WXUNUSED(toolIndex), Bool WXUNUSED(toggleDown))
{
  return TRUE;
}

// Called when the mouse cursor enters a tool bitmap (no button pressed).
// Argument is -1 if mouse is exiting the toolbar.
void wxToolBar::OnMouseEnter(int WXUNUSED(toolIndex))
{
}
  
// If pushedBitmap is NULL, a reversed version of bitmap is
// created and used as the pushed/toggled image.
// If toggle is TRUE, the button toggles between the two states.
wxToolBarTool *wxToolBar::AddTool(int index, wxBitmap *bitmap, wxBitmap *pushedBitmap,
             Bool toggle, float xPos, float yPos, wxObject *clientData)
{
  wxToolBarTool *tool = new wxToolBarTool(index, bitmap, pushedBitmap, toggle, this, xPos, yPos);
  tool->clientData = clientData;

  if (xPos > -1)
    tool->x = xPos;
  else
    tool->x = xMargin;

  if (yPos > -1)
    tool->y = yPos;
  else
    tool->y = yMargin;

  // Calculate reasonable max size in case Layout() not called
  if ((tool->x + bitmap->GetWidth() + xMargin) > maxWidth)
    maxWidth = (tool->x + bitmap->GetWidth() + xMargin);

  if ((tool->y + bitmap->GetHeight() + yMargin) > maxHeight)
    maxHeight = (tool->y + bitmap->GetHeight() + yMargin);


  tools.Append((long)index, tool);
  return tool;
}

void wxToolBar::ClearTools(void)
{
  currentTool = -1;
  wxNode *node = tools.First();
  while (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    wxNode *nextNode = node->Next();
    delete tool;
    delete node;
    node = nextNode;
  }
}

void wxToolBar::EnableTool(int index, Bool enable)
{
  wxNode *node = tools.Find((long)index);
  if (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tool)
    {
      tool->enabled = enable;
    }  
  }
}

void wxToolBar::ToggleTool(int index, Bool toggle)
{
  wxNode *node = tools.Find((long)index);
  if (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tool && tool->isToggle)
    {
      Bool oldState = tool->toggleState;
      tool->toggleState = toggle;

      if (oldState != toggle)
      {
        wxMemoryDC memDC(GetDC());
        DrawTool(memDC, tool);
      }
    }
  }
}

void wxToolBar::SetToggle(int index, Bool value)
{
  wxNode *node=tools.Find((long)index);
  if (node){
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    tool->isToggle = value;
  }
}

Bool wxToolBar::GetToolState(int index)
{
  wxNode *node = tools.Find((long)index);
  if (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tool)
    {
      return tool->toggleState;
    }
    else return FALSE;
  }
  else return FALSE;
}

Bool wxToolBar::GetToolEnabled(int index)
{
  wxNode *node = tools.Find((long)index);
  if (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tool)
    {
      return tool->enabled;
    }
    else return FALSE;
  }
  else return FALSE;
}

wxObject *wxToolBar::GetToolClientData(int index)
{
  wxNode *node = tools.Find((long)index);
  if (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tool)
    {
      return tool->clientData;
    }
    else return NULL;
  }
  else return NULL;
}

wxToolBarTool *wxToolBar::FindToolForPosition(float x, float y)
{
  wxNode *node = tools.First();
  while (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if ((x >= tool->x) && (y >= tool->y) &&
        (x <= (tool->x + tool->GetWidth())) &&
        (y <= (tool->y + tool->GetHeight())))
      return tool;

    node = node->Next();
  }
  return NULL;
}

void wxToolBar::Layout(void)
{
  currentRowsOrColumns = 0;
  lastX = xMargin;
  lastY = yMargin;
  int maxToolWidth = 0;
  int maxToolHeight = 0;
  maxWidth = 0.0;
  maxHeight = 0.0;

  // Find the maximum tool width and height
  wxNode *node = tools.First();
  while (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tool->GetWidth() > maxToolWidth)
      maxToolWidth = (int)tool->GetWidth();
    if (tool->GetHeight() > maxToolHeight)
      maxToolHeight = (int)tool->GetHeight();
    node = node->Next();
  }

  node = tools.First();
  while (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tilingDirection == wxHORIZONTAL)
    {
      if (currentRowsOrColumns >= rowsOrColumns)
      {
        currentRowsOrColumns = 0;
        lastX = xMargin;
        lastY += maxToolHeight + yMargin;
      }
      tool->x = (float)(lastX + (maxToolWidth - tool->GetWidth())/2.0);
      tool->y = (float)(lastY + (maxToolHeight - tool->GetHeight())/2.0);

      lastX += maxToolWidth + xMargin;
    }
    else
    {
      if (currentRowsOrColumns >= rowsOrColumns)
      {
        currentRowsOrColumns = 0;
        lastX += maxToolWidth + xMargin;
        lastY = yMargin;
      }
      tool->x = (float)(lastX + (maxToolWidth - tool->GetWidth())/2.0);
      tool->y = (float)(lastY + (maxToolHeight - tool->GetHeight())/2.0);

      lastY += maxToolHeight + yMargin;
    }
    if (lastX > maxWidth)
      maxWidth = lastX;
    if (lastY > maxHeight)
      maxHeight = lastY;

    currentRowsOrColumns ++;
    node = node->Next();
  }
  if (tilingDirection == wxVERTICAL)
    maxWidth += (float)maxToolWidth;
  else
    maxHeight += (float)maxToolHeight;

  maxWidth += xMargin;
  maxHeight += yMargin;
}

void wxToolBar::GetMaxSize(float *w, float *h)
{
  *w = maxWidth;
  *h = maxHeight;
}

void wxToolBar::SetMargins(float x, float y)
{
  xMargin = x;
  yMargin = y;
}

// Okay, so we've left the tool we're in ... we must check if
// the tool we're leaving was a 'sprung push button' and if so,
// spring it back to the up state.
//
void
wxToolBar::SpringUpButton(int index)
{
  wxNode *node=tools.Find((long)index);
  if (node){
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    if (tool && !tool->isToggle && tool->toggleState){
      tool->toggleState = FALSE;
      wxMemoryDC memDC(GetDC());
      DrawTool(memDC, tool);
    }
  }
}

#endif
