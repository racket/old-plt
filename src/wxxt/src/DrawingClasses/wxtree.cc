/*
 * File:		wxtree.cc
 * Purpose:	Tree library
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation "wxtree.h"
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
    /* static const char sccsid[] = "%W% %G%"; */
    // For compilers that support precompilation, includes "wx.h".
#   include "wx_prec.h"
#   ifdef __BORLANDC__
#	pragma hdrstop
#   endif
#   ifndef WX_PRECOMP
#	include "wx.h"
#   endif
#   include "wxtree.h"
#else // wx_xt
    // The Xt port uses another include mechanism
#   define  Uses_wxTreeLayout
#   include "wx.h"
#endif // #ifndef wx_xt

//-- use a small margin around the text
#define TEXT_HMARGIN 2
#define TEXT_VMARGIN 1

/*
 * Abstract tree
 *
 */

IMPLEMENT_ABSTRACT_CLASS(wxTreeLayout, wxObject)

wxTreeLayout::wxTreeLayout(wxDC *dc)
{
  xSpacing = 16;
  ySpacing = 20;
  topMargin = 5;
  leftMargin = 5;
  orientation = FALSE;

  treeDC = dc;
}

void wxTreeLayout::DoLayout(long topId)
{
  if (topId != -1)
    SetTopNode(topId);

  long actualTopId = GetTopNode();
  long id = actualTopId;
  while (id != -1)
  {
    SetNodeX(id, 0.0);
    SetNodeY(id, 0.0);
    ActivateNode(id, FALSE);
    id = GetNextNode(id);
  }
  lastY = topMargin;
  lastX = leftMargin;
  CalcLayout(actualTopId, 0);
}

void wxTreeLayout::Draw()
{
#ifndef wx_xt
  if (treeDC)
    treeDC->Clear();
#endif
  DrawBranches();
  DrawNodes();
}

void wxTreeLayout::DrawNodes(void)
{  
  long id = GetTopNode();
  while (id != -1)
  {
    if (NodeActive(id))
      DrawNode(id);
    id = GetNextNode(id);
  }
}

void wxTreeLayout::DrawBranches(void)
{  
  long id = GetTopNode();
  while (id != -1)
  {
    if (GetNodeParent(id) > -1)
    {
      long parent = GetNodeParent(id);
      if (NodeActive(parent))
        DrawBranch(parent, id);
    }
    id = GetNextNode(id);
  }
}

void wxTreeLayout::DrawNode(long id)
{
//+++ markus: drawing depends on orientation of the tree +++
  if (treeDC)
  {
    static char *unnamed = "<unnamed>";
    char *name = GetNodeName(id);
    float x, y;
    treeDC->GetTextExtent(name ? name : unnamed, &x, &y);
    // leave small margin around text
    x += TEXT_HMARGIN*2; y += TEXT_VMARGIN*2;
    if (orientation == FALSE)
      // Left to right
      treeDC->DrawText(name ? name : unnamed,
		       GetNodeX(id)+TEXT_HMARGIN, GetNodeY(id)-y/2.0);
    else
      // Top to bottom
	treeDC->DrawText(name ? name : unnamed,
			 GetNodeX(id)-x/2.0, GetNodeY(id)+TEXT_VMARGIN);
  }
//--- markus (deleted old stuff)---
}

void wxTreeLayout::DrawBranch(long from, long to)
{
//+++ markus: drawing depends on orientation of the tree +++
  if (treeDC)
  {
    float w, h;
    GetNodeSize(from, &w, &h);
    if (orientation == FALSE)
      // Left to right
      treeDC->DrawLine(GetNodeX(from)+w,GetNodeY(from), GetNodeX(to),GetNodeY(to));
    else
      // Top to bottom
      treeDC->DrawLine(GetNodeX(from),GetNodeY(from)+h, GetNodeX(to),GetNodeY(to));
  }
//--- markus (deleted old stuff)---
}

void wxTreeLayout::Initialize(void)
{
}

void wxTreeLayout::GetNodeSize(long id, float *x, float *y)
{
  char *name = GetNodeName(id);
  if (name && treeDC)
  {
    treeDC->GetTextExtent(name, x, y);
    *x += 2*TEXT_HMARGIN; *y += 2*TEXT_VMARGIN; // add a small margin around text
  }
  else
  {
    *x = 70; *y = 20;
  }
}

void wxTreeLayout::SetTopNode(long id)
{
  parentNode = id;
}

long wxTreeLayout::GetTopNode(void)
{
  return parentNode;
}


void wxTreeLayout::CalcLayout(long nodeId, int level)
{
  wxList children;
  GetChildren(nodeId, children);
  int n = children.Number();

  if (orientation == FALSE)
  {
    // Left to right
    // X Calculations
    if (level == 0)
      SetNodeX(nodeId, leftMargin);
    else
    {
      float x = 0.0;
      float y = 0.0;
      long parentId = GetNodeParent(nodeId);
      if (parentId != -1)
        GetNodeSize(parentId, &x, &y);
      SetNodeX(nodeId, (float)(GetNodeX(parentId) + xSpacing + x));
    }    

    wxNode *node = children.First();
    while (node)
    {
      CalcLayout((long)node->Data(), level+1);
      node = node->Next();
    }

    // Y Calculations
    float averageY;
    ActivateNode(nodeId, TRUE);
  
    if (n > 0)
    {
      averageY = 0.0;
      node = children.First();
      while (node)
      {
        averageY += GetNodeY((long)node->Data());
        node = node->Next();
      }
      averageY = averageY / n;
      SetNodeY(nodeId, averageY);
    }
    else
    {
//+++ markus: adjust TOP-MOST node of tree +++
      if (lastY == topMargin)
      {
	float w, h;
	GetNodeSize(nodeId, &w, &h);
	lastY += h/2;
      }
//--- markus ---
      SetNodeY(nodeId, lastY);
      float x, y;
      GetNodeSize(nodeId, &x, &y);
 
      lastY = lastY + y + ySpacing;
    }
  }
  else
  {
    // Top to bottom
    
    // Y Calculations
    if (level == 0)
      SetNodeY(nodeId, topMargin);
    else
    {
      float x = 0.0;
      float y = 0.0;
      long parentId = GetNodeParent(nodeId);
      if (parentId != -1)
        GetNodeSize(parentId, &x, &y);
      SetNodeY(nodeId, (float)(GetNodeY(parentId) + ySpacing + y));
    }    

    wxNode *node = children.First();
    while (node)
    {
      CalcLayout((long)node->Data(), level+1);
      node = node->Next();
    }

    // X Calculations
    float averageX;
    ActivateNode(nodeId, TRUE);
  
    if (n > 0)
    {
      averageX = 0.0;
      node = children.First();
      while (node)
      {
        averageX += GetNodeX((long)node->Data());
        node = node->Next();
      }
      averageX = averageX / n;
      SetNodeX(nodeId, averageX);
    }
    else
    {
//+++ markus: adjust LEFT-MOST node of tree +++
      if (lastX == leftMargin)
      {
	float w, h;
	GetNodeSize(nodeId, &w, &h);
	lastX += w/2;
      }
//--- markus ---
      SetNodeX(nodeId, lastX);
      float x, y;
      GetNodeSize(nodeId, &x, &y);
 
      lastX = lastX + x + xSpacing;
    }
  }
}

/*
 * Tree with storage
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxStoredTree, wxTreeLayout)

wxStoredTree::wxStoredTree(wxDC *dc, int n):wxTreeLayout(dc)
{
  nodes = NULL;
  maxNodes = 0;
  Initialize(n);
}

wxStoredTree::~wxStoredTree(void)
{
  if (nodes)
    delete[] nodes;
}

void wxStoredTree::Initialize(int n)
{
  maxNodes = n;
  wxTreeLayout::Initialize();
  if (nodes) delete[] nodes;
  nodes = new _storedNode[maxNodes];
  int i;
  for (i = 0; i < n; i++)
  {
    nodes[i].name[0] = '\0';
    nodes[i].active = FALSE;
    nodes[i].parentId = -1;
    nodes[i].x = 0;
    nodes[i].y = 0;
  }
  num = 0;
}

void wxStoredTree::AddChild(char *name, char *parent)
{
  if (num < (maxNodes -1 ))
  {
    long i = -1;
    if (parent)
      i = NameToId(parent);
    else parentNode = num;

    nodes[num].parentId = i;
    sprintf(nodes[num].name, "%s", name);
    nodes[num].x = nodes[num].y = 0;
    num ++;
  }
}

long wxStoredTree::NameToId(char *name)
{
  long i;
  for (i = 0; i < num; i++)
    if (strcmp((char *)name, (char *)nodes[i].name) == 0)
      return i;
  return -1;
}

void wxStoredTree::GetChildren(long id, wxList& list)
{
  long currentId = GetTopNode();
  while (currentId != -1)
  {
    if (id == GetNodeParent(currentId))
      list.Append((wxObject *)currentId);
    currentId = GetNextNode(currentId);
  }
}

float wxStoredTree::GetNodeX(long id)
{
  return (float)nodes[id].x;
}

float wxStoredTree::GetNodeY(long id)
{
  return (float)nodes[id].y;
}

void wxStoredTree::SetNodeX(long id, float x)
{
  nodes[id].x = (int)x;
}

void wxStoredTree::SetNodeY(long id, float y)
{
  nodes[id].y = (int)y;
}

void wxStoredTree::SetNodeName(long id, char *name)
{
  sprintf(nodes[id].name, "%s", name);
}

char *wxStoredTree::GetNodeName(long id)
{
  return nodes[id].name;
}

long wxStoredTree::GetNodeParent(long id)
{
  if (id != -1)
    return nodes[id].parentId;
  else
    return -1;
}

long wxStoredTree::GetNextNode(long id)
{
  if ((id != -1) && (id < (num - 1)))
    return id + 1;
  else
    return -1;
}

void wxStoredTree::ActivateNode(long id, Bool active)
{
  nodes[id].active = active;
}

Bool wxStoredTree::NodeActive(long id)
{
  return nodes[id].active;
}
