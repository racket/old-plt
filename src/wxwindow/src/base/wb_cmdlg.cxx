/*
 * File:	wb_cmdlg.cc
 * Purpose:	Common dialogs: generic code
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation "wb_cmdlg.h"
#endif

#include "common.h"
#include "wx_gdi.h"
#include "wx_utils.h"
#include "wx_mgstr.h"
#include "wx_types.h"
#include "wx_frame.h"
#include "wx_dialg.h"
#include "wx_slidr.h"
#include "wx_check.h"
#include "wx_choic.h"
#include "wx_txt.h"
#include "wx_lbox.h"
#include "wx_rbox.h"
#include "wx_buttn.h"
#include "wx_messg.h"

#include "wb_cmdlg.h"

#endif

#include <stdlib.h>

/*
 * wxColourData
 */

IMPLEMENT_DYNAMIC_CLASS(wxColourData, wxObject)

wxColourData::wxColourData(void)
{
  for (int i = 0; i < 16; i++)
    custColours[i].Set(255, 255, 255);

  chooseFull = FALSE;
  dataColour.Set(0,0,0);
} 

wxColourData::~wxColourData(void)
{
}

void wxColourData::SetCustomColour(int i, wxColour& colour)
{
  if (i > 15 || i < 0)
    return;

  custColours[i] = colour;
}

wxColour wxColourData::GetCustomColour(int i)
{
  if (i > 15 || i < 0)
    return wxColour(0,0,0);

  return custColours[i];
}

void wxColourData::operator=(const wxColourData& data)
{
  for (int i = 0; i < 16; i++)
    custColours[i] = data.custColours[i];

  dataColour = (wxColour&)data.dataColour;
  chooseFull = data.chooseFull;
}

/*
 * Generic wxColourDialog
 */

#define NUM_COLS 48
static char *wxColourDialogNames[NUM_COLS]={"ORANGE",
				    "GOLDENROD",
				    "WHEAT",
				    "SPRING GREEN",
				    "SKY BLUE",
				    "SLATE BLUE",
				    "MEDIUM VIOLET RED",
				    "PURPLE",

				    "RED",
				    "YELLOW",
				    "MEDIUM SPRING GREEN",
				    "PALE GREEN",
				    "CYAN",
				    "LIGHT STEEL BLUE",
				    "ORCHID",
				    "LIGHT MAGENTA",
				    
				    "BROWN",
				    "YELLOW",
				    "GREEN",
				    "CADET BLUE",
				    "MEDIUM BLUE",
				    "MAGENTA",
				    "MAROON",
				    "ORANGE RED",

				    "FIREBRICK",
				    "CORAL",
				    "FOREST GREEN",
				    "AQUARAMINE",
				    "BLUE",
				    "NAVY",
				    "THISTLE",
				    "MEDIUM VIOLET RED",
				    
				    "INDIAN RED",
				    "GOLD",
				    "MEDIUM SEA GREEN",
				    "MEDIUM BLUE",
				    "MIDNIGHT BLUE",
				    "GREY",
				    "PURPLE",
				    "KHAKI",
				    
				    "BLACK",
				    "MEDIUM FOREST FREEN",
				    "KHAKI",
				    "DARK GREY",
				    "SEA GREEN",
				    "LIGHT GREY",
				    "MEDIUM SLATE BLUE",
				    "WHITE"
				    };

Bool wxGenericColourDialog::colourDialogCancelled = FALSE;
 
IMPLEMENT_DYNAMIC_CLASS(wxGenericColourDialog, wxDialogBox)

wxGenericColourDialog::wxGenericColourDialog(void)
{
  dialogParent = NULL;
  colourDialogCancelled = FALSE;
  whichKind = 1;
  colourSelection = 0;
}

wxGenericColourDialog::wxGenericColourDialog(wxWindow *parent, wxColourData *data):
  wxDialogBox(parent, "Colour", TRUE, 0, 0, 900, 900)
{
  colourDialogCancelled = FALSE;
  whichKind = 1;
  colourSelection = 0;
  Create(parent, data);
}

wxGenericColourDialog::~wxGenericColourDialog(void)
{
}

Bool wxGenericColourDialog::OnClose(void)
{
  colourDialogCancelled = TRUE;
  Show(FALSE);
  return FALSE;
}
 
Bool wxGenericColourDialog::OnCharHook(wxKeyEvent& event)
{
  if (event.KeyCode() == WXK_ESCAPE)
  {
    colourDialogCancelled = TRUE;
    Show(FALSE);
    return TRUE;
  }
  else return FALSE;
}

Bool wxGenericColourDialog::Create(wxWindow *parent, wxColourData *data)
{
  dialogParent = parent;
  
  if (data)
    colourData = *data;

  InitializeColours();
  CalculateMeasurements();
  CreateWidgets();
  
  return TRUE;
}

Bool wxGenericColourDialog::Show(Bool flag)
{
  wxDialogBox::Show(flag);
  if (flag)
  {
    if (colourDialogCancelled)
      return FALSE;
    else
      return TRUE;
  }
  return TRUE;
}


// Internal functions
void wxGenericColourDialog::OnEvent(wxMouseEvent& event)
{
  if (event.ButtonDown(1))
  {
    int x = (int)event.x;
    int y = (int)event.y;

    if ((x >= standardColoursRect.x && x <= (standardColoursRect.x + standardColoursRect.width)) &&
        (y >= standardColoursRect.y && y <= (standardColoursRect.y + standardColoursRect.height)))
    {
      int selX = (int)(x - standardColoursRect.x)/(smallRectangleSize.x + gridSpacing);
      int selY = (int)(y - standardColoursRect.y)/(smallRectangleSize.y + gridSpacing);
      int ptr = (int)(selX + selY*8);
      OnBasicColourClick(ptr);
    }
    else if ((x >= customColoursRect.x && x <= (customColoursRect.x + customColoursRect.width)) &&
        (y >= customColoursRect.y && y <= (customColoursRect.y + customColoursRect.height)))
    {
      int selX = (int)(x - customColoursRect.x)/(smallRectangleSize.x + gridSpacing);
      int selY = (int)(y - customColoursRect.y)/(smallRectangleSize.y + gridSpacing);
      int ptr = (int)(selX + selY*8);
      OnCustomColourClick(ptr);
    }
  }
}

void wxGenericColourDialog::OnPaint(void)
{
  wxDialogBox::OnPaint();
  PaintBasicColours();
  PaintCustomColours();
  PaintCustomColour();
  PaintHighlight(TRUE);
}

void wxGenericColourDialog::CalculateMeasurements(void)
{
  smallRectangleSize.x = 18;
  smallRectangleSize.y = 14;
  customRectangleSize.x = 40;
  customRectangleSize.y = 40;

  gridSpacing = 6;
  sectionSpacing = 15;

  standardColoursRect.x = 10;
  standardColoursRect.y = 15;
  standardColoursRect.width = (8*smallRectangleSize.x) + (7*gridSpacing);
  standardColoursRect.height = (6*smallRectangleSize.y) + (5*gridSpacing);

  customColoursRect.x = standardColoursRect.x;
  customColoursRect.y = standardColoursRect.y + standardColoursRect.height  + 20;
  customColoursRect.width = (8*smallRectangleSize.x) + (7*gridSpacing);
  customColoursRect.height = (2*smallRectangleSize.y) + (1*gridSpacing);

  singleCustomColourRect.x = customColoursRect.width + customColoursRect.x + sectionSpacing;
  singleCustomColourRect.y = 80;
  singleCustomColourRect.width = customRectangleSize.x;
  singleCustomColourRect.height = customRectangleSize.y;

  okButtonX = 10;
  customButtonX = singleCustomColourRect.x ;
  buttonY = customColoursRect.y + customColoursRect.height + 10;
}

static void wxGenericColourOk(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericColourDialog *dialog = (wxGenericColourDialog *)but.GetParent();
  dialog->OnOk();
}

static void wxGenericColourCancel(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericColourDialog *dialog = (wxGenericColourDialog *)but.GetParent();
  dialog->OnCancel();
}

static void wxGenericColourAddCustom(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericColourDialog *dialog = (wxGenericColourDialog *)but.GetParent();
  dialog->OnAddCustom();
}

static void wxGenericColourRed(wxSlider& but, wxCommandEvent& event)
{
  wxGenericColourDialog *dialog = (wxGenericColourDialog *)but.GetParent();
  dialog->OnRedSlider(event);
}

static void wxGenericColourGreen(wxSlider& but, wxCommandEvent& event)
{
  wxGenericColourDialog *dialog = (wxGenericColourDialog *)but.GetParent();
  dialog->OnGreenSlider(event);
}

static void wxGenericColourBlue(wxSlider& but, wxCommandEvent& event)
{
  wxGenericColourDialog *dialog = (wxGenericColourDialog *)but.GetParent();
  dialog->OnBlueSlider(event);
}

void wxGenericColourDialog::CreateWidgets(void)
{
  wxButton *okButton = new wxButton(this, (wxFunction)wxGenericColourOk, "OK", okButtonX, buttonY);
  wxButton *cancelButton = new wxButton(this, (wxFunction)wxGenericColourCancel, "Cancel");
  wxButton *customButton = new wxButton(this, (wxFunction)wxGenericColourAddCustom, "Add to custom colours",
   customButtonX, buttonY);

  int sliderX = singleCustomColourRect.x + singleCustomColourRect.width + sectionSpacing;
#ifdef wx_x
  int sliderSpacing = 75;
  int sliderHeight = 160;
#else
  int sliderSpacing = 45;
  int sliderHeight = 200;
#endif
  
  redSlider = new wxSlider(this, (wxFunction)wxGenericColourRed, "Red", 0, 0, 255, sliderHeight, sliderX, 10, wxVERTICAL);
  greenSlider = new wxSlider(this, (wxFunction)wxGenericColourGreen, "Green", 0, 0, 255, sliderHeight, sliderX + sliderSpacing, 10, wxVERTICAL);
  blueSlider = new wxSlider(this, (wxFunction)wxGenericColourBlue, "Blue", 0, 0, 255, sliderHeight, sliderX + 2*sliderSpacing, 10, wxVERTICAL);

  SetClientSize(sliderX + 3*sliderSpacing, buttonY + 30);
  Centre(wxBOTH);
}

void wxGenericColourDialog::InitializeColours(void)
{
  // Need some decent way of doing this
  for (int i = 0; i < 48; i++)
  {
    wxColour *col = wxTheColourDatabase->FindColour(wxColourDialogNames[i]);
    if (col)
      standardColours[i].Set(col->Red(), col->Green(), col->Blue());
    else
      standardColours[i].Set(0, 0, 0);
  }

  for (i = 0; i < 16; i++)
    customColours[i] = (wxColour&)colourData.GetCustomColour(i);

  singleCustomColour.Set(0, 0, 0);
}

void wxGenericColourDialog::PaintBasicColours(void)
{
  wxDC *dc = GetDC();

  dc->BeginDrawing();
  
  for (int i = 0; i < 6; i++)
  {
    for (int j = 0; j < 8; j++)
    {
      int ptr = i*8 + j;
      
      int x = (j*(smallRectangleSize.x+gridSpacing) + standardColoursRect.x);
      int y = (i*(smallRectangleSize.y+gridSpacing) + standardColoursRect.y);

      dc->SetPen(wxBLACK_PEN);
      wxBrush *brush = wxTheBrushList->FindOrCreateBrush(&(standardColours[ptr]), wxSOLID);
      dc->SetBrush(brush);

      dc->DrawRectangle(x, y, smallRectangleSize.x, smallRectangleSize.y);
    }
  }
  dc->EndDrawing();
}

void wxGenericColourDialog::PaintCustomColours(void)
{
  wxDC *dc = GetDC();
  dc->BeginDrawing();
  
  for (int i = 0; i < 2; i++)
  {
    for (int j = 0; j < 8; j++)
    {
      int ptr = i*8 + j;
      
      int x = (j*(smallRectangleSize.x+gridSpacing)) + customColoursRect.x;
      int y = (i*(smallRectangleSize.y+gridSpacing)) + customColoursRect.y;

      dc->SetPen(wxBLACK_PEN);
      wxBrush *brush = wxTheBrushList->FindOrCreateBrush(&(customColours[ptr]), wxSOLID);
      dc->SetBrush(brush);

      dc->DrawRectangle(x, y, smallRectangleSize.x, smallRectangleSize.y);
    }
  }
  dc->EndDrawing();
}

void wxGenericColourDialog::PaintHighlight(Bool draw)
{
  wxDC *dc = GetDC();
  dc->BeginDrawing();

  // Number of pixels bigger than the standard rectangle size
  // for drawing a highlight
  int deltaX = 2;
  int deltaY = 2;

  if (whichKind == 1)
  {
    // Standard colours
    int y = (int)(colourSelection / 8);
    int x = (int)(colourSelection - (y*8));

    x = (x*(smallRectangleSize.x + gridSpacing) + standardColoursRect.x) - deltaX;
    y = (y*(smallRectangleSize.y + gridSpacing) + standardColoursRect.y) - deltaY;

    if (draw)
      dc->SetPen(wxBLACK_PEN);
    else
      dc->SetPen(wxLIGHT_GREY_PEN);

    dc->SetBrush(wxTRANSPARENT_BRUSH);
//    dc->SetBrush(wxRED_BRUSH);
    dc->DrawRectangle(x, y, smallRectangleSize.x + (2*deltaX), smallRectangleSize.y + (2*deltaY));
  }
  else
  {
    // User-defined colours
    int y = (int)(colourSelection / 8);
    int x = (int)(colourSelection - (y*8));

    x = (x*(smallRectangleSize.x + gridSpacing) + customColoursRect.x) - deltaX;
    y = (y*(smallRectangleSize.y + gridSpacing) + customColoursRect.y) - deltaY;

    if (draw)
      dc->SetPen(wxBLACK_PEN);
    else
      dc->SetPen(wxLIGHT_GREY_PEN);
      
    dc->SetBrush(wxTRANSPARENT_BRUSH);
    dc->DrawRectangle(x, y, smallRectangleSize.x + (2*deltaX), smallRectangleSize.y + (2*deltaY));
  }
  
  dc->EndDrawing();
}

void wxGenericColourDialog::PaintCustomColour(void)
{
  wxDC *dc = GetDC();
  dc->BeginDrawing();
  
  dc->SetPen(wxBLACK_PEN);

  wxBrush *brush = new wxBrush(singleCustomColour, wxSOLID);
  dc->SetBrush(brush);

  dc->DrawRectangle(singleCustomColourRect.x, singleCustomColourRect.y, customRectangleSize.x, customRectangleSize.y);

  dc->SetBrush(NULL);
  delete brush;

  dc->EndDrawing();
}

void wxGenericColourDialog::OnBasicColourClick(int which)
{
  PaintHighlight(FALSE);
  whichKind = 1;
  colourSelection = which;
  colourData.SetColour(standardColours[colourSelection]);

  PaintHighlight(TRUE);
}

void wxGenericColourDialog::OnCustomColourClick(int which)
{
  PaintHighlight(FALSE);
  whichKind = 2;
  colourSelection = which;
  colourData.SetColour(customColours[colourSelection]);

  PaintHighlight(TRUE);
}

void wxGenericColourDialog::OnOk(void)
{
  Show(FALSE);
}

void wxGenericColourDialog::OnCancel(void)
{
  colourDialogCancelled = TRUE;
  Show(FALSE);
}

void wxGenericColourDialog::OnAddCustom(void)
{
  if (whichKind != 2)
  {
    PaintHighlight(FALSE);
    whichKind = 2;
    colourSelection = 0;
    PaintHighlight(TRUE);
  }

  customColours[colourSelection].Set(singleCustomColour.Red(), singleCustomColour.Green(), singleCustomColour.Blue());
  colourData.SetColour(customColours[colourSelection]);
  colourData.SetCustomColour(colourSelection, customColours[colourSelection]);
  
  PaintCustomColours();
}

void wxGenericColourDialog::OnRedSlider(wxCommandEvent& WXUNUSED(event))
{
  if (!redSlider)
    return;
    
  singleCustomColour.Set(redSlider->GetValue(), singleCustomColour.Green(), singleCustomColour.Blue());
  PaintCustomColour();
}

void wxGenericColourDialog::OnGreenSlider(wxCommandEvent& WXUNUSED(event))
{
  if (!greenSlider)
    return;

  singleCustomColour.Set(singleCustomColour.Red(), greenSlider->GetValue(), singleCustomColour.Blue());
  PaintCustomColour();
}

void wxGenericColourDialog::OnBlueSlider(wxCommandEvent& WXUNUSED(event))
{
  if (!blueSlider)
    return;

  singleCustomColour.Set(singleCustomColour.Red(), singleCustomColour.Green(), blueSlider->GetValue());
  PaintCustomColour();
}

/*
 * FONT DIALOG
 */

IMPLEMENT_DYNAMIC_CLASS(wxFontData, wxObject)

wxFontData::wxFontData(void)
{
  // Intialize colour to black.
  fontColour.Set(0, 0, 0);

  showHelp = FALSE;
  allowSymbols = TRUE;
  enableEffects = TRUE;
  initialFont = NULL;
  chosenFont = NULL;
  minSize = 0;
  maxSize = 0;
}

wxFontData::~wxFontData(void)
{
}

void wxFontData::operator=(const wxFontData& data)
{
   fontColour = (wxColour&)data.fontColour;
   showHelp = data.showHelp;
   allowSymbols = data.allowSymbols;
   enableEffects = data.enableEffects;
   initialFont = data.initialFont;
   chosenFont = data.chosenFont;
   minSize = data.minSize;
   maxSize = data.maxSize;
}

/*
 * Generic wxFontDialog
 */

Bool wxGenericFontDialog::fontDialogCancelled = FALSE;

IMPLEMENT_DYNAMIC_CLASS(wxGenericFontDialog, wxDialogBox)

wxGenericFontDialog::wxGenericFontDialog(void)
{
  dialogParent = NULL;
  dialogFont = NULL;
  fontDialogCancelled = FALSE;
}

wxGenericFontDialog::wxGenericFontDialog(wxWindow *parent, wxFontData *data):
  wxDialogBox(parent, "Font", TRUE, 0, 0, 600, 600)
{
  dialogFont = NULL;
  fontDialogCancelled = FALSE;
  Create(parent, data);
}

wxGenericFontDialog::~wxGenericFontDialog(void)
{
}

Bool wxGenericFontDialog::OnClose(void)
{
  fontDialogCancelled = TRUE;
  Show(FALSE);
  return FALSE;
}
 
Bool wxGenericFontDialog::OnCharHook(wxKeyEvent& event)
{
  if (event.KeyCode() == WXK_ESCAPE)
  {
    fontDialogCancelled = TRUE;
    Show(FALSE);
    return TRUE;
  }
  else return FALSE;
}

Bool wxGenericFontDialog::Create(wxWindow *parent, wxFontData *data)
{
  dialogParent = parent;
  
  if (data)
    fontData = *data;

  InitializeFont();
  CreateWidgets();
  
  return TRUE;
}

Bool wxGenericFontDialog::Show(Bool flag)
{
  wxDialogBox::Show(flag);
  if (flag)
  {
    GetDC()->SetFont(NULL);
    if (fontDialogCancelled)
    {
      fontData.chosenFont = NULL;
      if (dialogFont)
        delete dialogFont;
      return FALSE;
    }
    else
    {
      fontData.chosenFont = dialogFont;
      return TRUE;
    }
  }
  return TRUE;
}


// Internal functions
void wxGenericFontDialog::OnEvent(wxMouseEvent& event)
{
  wxDialogBox::OnEvent(event);
}

void wxGenericFontDialog::OnPaint(void)
{
  wxDialogBox::OnPaint();
  PaintFontBackground();
  PaintFont();
}

static void wxGenericFontOk(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericFontDialog *dialog = (wxGenericFontDialog *)but.GetParent();
  dialog->OnOk();
}

static void wxGenericFontCancel(wxButton& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericFontDialog *dialog = (wxGenericFontDialog *)but.GetParent();
  dialog->OnCancel();
}

static void wxGenericChangeFont(wxItem& but, wxCommandEvent& WXUNUSED(event))
{
  wxGenericFontDialog *dialog = (wxGenericFontDialog *)but.GetParent();
  dialog->OnChangeFont();
}

static void wxGenericChangeFontText(wxText& text, wxCommandEvent& event)
{
  if (event.GetEventType() == wxEVENT_TYPE_TEXT_ENTER_COMMAND)
  {
    wxGenericFontDialog *dialog = (wxGenericFontDialog *)text.GetParent();
    dialog->OnChangeFont();
  }
}

void wxGenericFontDialog::CreateWidgets(void)
{
  fontRect.x = 5;
#ifdef wx_x
  fontRect.y = 125;
#else
  fontRect.y = 115;
#endif
  fontRect.width = 350;
  fontRect.height = 100;

  SetLabelPosition(wxVERTICAL);

  char *families[] = { "Roman", "Decorative", "Modern", "Script", "Swiss" };
  char *styles[] = { "Normal", "Italic", "Slant" };
  char *weights[] = { "Normal", "Light", "Bold" };
/*
  char *families[5];
  families[0] = "Roman";
  families[1] = "Decorative";
  families[2] = "Modern";
  families[3] = "Script";
  families[4] = "Swiss";
  char *styles[3];
  styles[0] = "Normal";
  styles[1] = "Italic";
  styles[2] = "Slant";
  char *weights[3];
  weights[0] = "Normal";
  weights[1] = "Light";
  weights[2] = "Bold";
*/
  familyChoice = new wxChoice(this, (wxFunction)wxGenericChangeFont, "Family", -1, -1, 120, -1, 5, (char **)families); /* MATTHEW: BC */
  styleChoice = new wxChoice(this, (wxFunction)wxGenericChangeFont, "Style", -1, -1, 120, -1, 3, (char **)styles); /* MATTHEW: BC */
  weightChoice = new wxChoice(this, (wxFunction)wxGenericChangeFont, "Weight", -1, -1, 120, -1, 3, (char **)weights); /* MATTHEW: BC */

  NewLine();

  colourChoice = new wxChoice(this, (wxFunction)wxGenericChangeFont, "Colour", -1, -1, 200, -1, NUM_COLS, wxColourDialogNames);
//  SetLabelPosition(wxHORIZONTAL);
  pointSizeText = new wxText(this, (wxFunction)wxGenericChangeFontText, "Point size", "", -1, -1, 100, -1, wxPROCESS_ENTER);
  
  wxButton *okButton = new wxButton(this, (wxFunction)wxGenericFontOk, "OK", 5, (fontRect.y + fontRect.height + 5));
  wxButton *cancelButton = new wxButton(this, (wxFunction)wxGenericFontCancel, "Cancel");
  underLineCheckBox = new wxCheckBox(this, (wxFunction)wxGenericChangeFont, "Underline");

//  SetClientSize(sliderX + 3*sliderSpacing, buttonY + 30);

  familyChoice->SetStringSelection(wxFontFamilyIntToString(dialogFont->GetFamily()));
  styleChoice->SetStringSelection(wxFontStyleIntToString(dialogFont->GetStyle()));
  weightChoice->SetStringSelection(wxFontWeightIntToString(dialogFont->GetWeight()));
  char *name = wxTheColourDatabase->FindName(fontData.fontColour);
  if (name)
    colourChoice->SetStringSelection(name);
    
  underLineCheckBox->SetValue(dialogFont->GetUnderlined());

  char buf[40];
  sprintf(buf, "%d", dialogFont->GetPointSize());
  pointSizeText->SetValue(buf);

  Fit();
  Centre(wxBOTH);
}

void wxGenericFontDialog::InitializeFont(void)
{
  int fontFamily = wxSWISS;
  int fontWeight = wxNORMAL;
  int fontStyle = wxNORMAL;
  int fontSize = 12;
  int fontUnderline = FALSE;
  if (fontData.initialFont)
  {
    fontFamily = fontData.initialFont->GetFamily();
    fontWeight = fontData.initialFont->GetWeight();
    fontStyle = fontData.initialFont->GetStyle();
    fontSize = fontData.initialFont->GetPointSize();
    fontUnderline = fontData.initialFont->GetUnderlined();
  }
  dialogFont = new wxFont(fontSize, fontFamily, fontStyle, fontWeight, fontUnderline);

}

void wxGenericFontDialog::PaintFontBackground(void)
{
  wxDC *dc = GetDC();

  dc->BeginDrawing();

  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxWHITE_BRUSH);
  dc->DrawRectangle(fontRect.x, fontRect.y, fontRect.width, fontRect.height);
  dc->EndDrawing();
}

void wxGenericFontDialog::PaintFont(void)
{
  wxDC *dc = GetDC();
  dc->BeginDrawing();
  if (dialogFont)
  {
    dc->SetFont(dialogFont);
    // Calculate vertical centre
    float w, h;
    dc->GetTextExtent("X", &w, &h);
    float cx = fontRect.x + 10;
    float cy = (float)(fontRect.y + (fontRect.height/2.0) - (h/2.0));
    dc->SetTextForeground(&fontData.fontColour);
    dc->SetClippingRegion(fontRect.x, fontRect.y, fontRect.width-2.0, fontRect.height-2.0);
    dc->DrawText("ABCDEFGabcdefg12345", cx, cy);
    dc->DestroyClippingRegion();
  }
  dc->EndDrawing();
}

void wxGenericFontDialog::OnOk(void)
{
  Show(FALSE);
}

void wxGenericFontDialog::OnCancel(void)
{
  fontDialogCancelled = TRUE;
  Show(FALSE);
}

void wxGenericFontDialog::OnChangeFont(void)
{
  int fontFamily = wxFontFamilyStringToInt(familyChoice->GetStringSelection());
  int fontWeight = wxFontWeightStringToInt(weightChoice->GetStringSelection());
  int fontStyle = wxFontStyleStringToInt(styleChoice->GetStringSelection());
  int fontSize = atoi(pointSizeText->GetValue());
  int fontUnderline = underLineCheckBox->GetValue();

  dialogFont = new wxFont(fontSize, fontFamily, fontStyle, fontWeight, fontUnderline);
  if (colourChoice->GetStringSelection())
  {
    wxColour *col = wxTheColourDatabase->FindColour(colourChoice->GetStringSelection());
    if (col)
    {
      fontData.fontColour = *col;
    }
  }
  PaintFontBackground();
  PaintFont();
}

char *wxFontWeightIntToString(int weight)
{
  switch (weight)
  {
    case wxLIGHT:
      return "Light";
    case wxBOLD:
      return "Bold";
    case wxNORMAL:
    default:
      return "Normal";
  }
  return "Normal";
}

char *wxFontStyleIntToString(int style)
{
  switch (style)
  {
    case wxITALIC:
      return "Italic";
    case wxSLANT:
      return "Slant";
    case wxNORMAL:
    default:
      return "Normal";
  }
  return "Normal";
}

char *wxFontFamilyIntToString(int family)
{
  switch (family)
  {
    case wxROMAN:
      return "Roman";
    case wxDECORATIVE:
      return "Decorative";
    case wxMODERN:
      return "Modern";
    case wxSCRIPT:
      return "Script";
    case wxSWISS:
    default:
      return "Swiss";
  }
  return "Swiss";
}

int wxFontFamilyStringToInt(char *family)
{
  if (!family)
    return wxSWISS;
    
  if (strcmp(family, "Roman") == 0)
    return wxROMAN;
  else if (strcmp(family, "Decorative") == 0)
    return wxDECORATIVE;
  else if (strcmp(family, "Modern") == 0)
    return wxMODERN;
  else if (strcmp(family, "Script") == 0)
    return wxSCRIPT;
  else return wxSWISS;
}

int wxFontStyleStringToInt(char *style)
{
  if (!style)
    return wxNORMAL;
  if (strcmp(style, "Italic") == 0)
    return wxITALIC;
  else if (strcmp(style, "Slant") == 0)
    return wxSLANT;
  else
    return wxNORMAL;
}

int wxFontWeightStringToInt(char *weight)
{
  if (!weight)
    return wxNORMAL;
  if (strcmp(weight, "Bold") == 0)
    return wxBOLD;
  else if (strcmp(weight, "Light") == 0)
    return wxLIGHT;
  else
    return wxNORMAL;
}

/*
 * Miscellaneous common dialog code (was formerly in wb_dialg.cc)
 *
 */

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

// Generic file load/save dialog
// static inline char * // HP compiler complains
static char *
wxDefaultFileSelector(Bool load, const char *what, char *extension, char *default_name)
{
  char prompt[50];
  sprintf(prompt, load ? wxSTR_LOAD_FILE : wxSTR_SAVE_FILE, what);

  if (*extension == '.') extension++;
  char wild[60];
  sprintf(wild, "*.%s", extension);

  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild);
}


// Generic file load dialog
char *
wxLoadFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(TRUE, what, extension, default_name);
}


// Generic file save dialog
char *
wxSaveFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(FALSE, what, extension, default_name);
}

/*
 * Message centring code
 *
 */

void wxSplitMessage(char *message, wxList *messageList, wxPanel *panel)
{
  char *copyMessage = copystring(message);
  size_t i = 0;
  size_t len = strlen(copyMessage);
  char *currentMessage = copyMessage;

  while (i < len) {
    while ((i < len) && (copyMessage[i] != '\n')) i++;
    if (i < len) copyMessage[i] = 0;
    wxMessage *mess = new wxMessage(panel, currentMessage);
    messageList->Append(mess);
    panel->NewLine();

    currentMessage = copyMessage + i + 1;
  }
  delete[] copyMessage;
}

void wxCentreMessage(wxList *messageList)
{
  // Do the message centering
  for(wxNode *node = messageList->First(); node; node = node->Next()) {
    wxMessage *mess = (wxMessage *)node->Data();
    mess->Centre();
  }
}


/*
 * A general purpose dialog box with an OnClose that returns TRUE.
 *
 */

#ifdef __GNUG__
#pragma implementation
#pragma interface
#endif

class wxMessageBoxDialog: public wxDialogBox
{
 public:
  wxText *textItem;
  wxListBox *listBoxItem;
  char *textAnswer;
  char *listSelection;
  char *listClientSelection;
  int listPosition;
  int buttonPressed;
  int  * listSelections;
  int     nlistSelections;

  wxMessageBoxDialog(wxWindow *parent, char *caption, Bool isModal, int x, int y,
    int w, int h, long type):
   wxDialogBox(parent, caption, isModal, x, y, w, h, type)
 {
   textItem = NULL;
   listBoxItem = NULL;
   buttonPressed = wxCANCEL;

   textAnswer = NULL;
   listSelection = NULL;
   listClientSelection = NULL;
   listPosition = 0;
   buttonPressed = wxCANCEL;
   listSelections = NULL;
   nlistSelections = -1;
 }
 Bool OnClose(void)
 {
   return TRUE;
 }
};

void wxDialogOkButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  if (dialog->textItem)
  {
    if (dialog->textAnswer)
      delete[] dialog->textAnswer;
    dialog->textAnswer = copystring(dialog->textItem->GetValue());
  }

  if (dialog->listBoxItem)
    {
      if ( dialog->listBoxItem->multiple==wxSINGLE)
        {
          if (dialog->listSelection)
            delete[] dialog->listSelection;
          dialog->listSelection = 
            (dialog->listBoxItem->GetStringSelection() ?
             copystring(dialog->listBoxItem->GetStringSelection()) : NULL);
          dialog->listPosition = dialog->listBoxItem->GetSelection();
          dialog->listClientSelection = 
            dialog->listBoxItem->wxListBox::GetClientData(dialog->listPosition);
        }
      else 
        if (dialog->listBoxItem->multiple==wxMULTIPLE)
          {
            if (dialog->listSelections)
              delete[] dialog->listSelections;
            dialog->listSelections = 0;
            
            int * sels;
            dialog-> nlistSelections =
              dialog->listBoxItem->GetSelections (&sels);
            if ( dialog-> nlistSelections)
              {
                dialog->listSelections = new int [ dialog-> nlistSelections ];
                int i;
                for (i=0; i<  dialog-> nlistSelections; i++)
                  dialog->listSelections[i] = sels[i];
              }
          }
    }

  dialog->buttonPressed = wxOK;
  dialog->Show(FALSE);
  // delete dialog;
}

void wxDialogCancelButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxDialogBox *dialog = (wxDialogBox *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(dialog->__type,wxTYPE_DIALOG_BOX))
    dialog = (wxDialogBox*) ((wxPanel*)dialog)->GetParent() ;

  dialog->Show(FALSE);
  // delete dialog;
}

void wxDialogYesButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  dialog->buttonPressed = wxYES;
  dialog->Show(FALSE);
  // delete dialog;
}

void wxDialogNoButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  dialog->buttonPressed = wxNO;
  dialog->Show(FALSE);
  // delete dialog;
}

/* MATTHEW: So hitting return hit OK button */
void wxDialogReturn(wxButton& but, wxEvent& event)
/* "but" isn't really a button, but it's parent is what counts */
{
  if (event.eventClass == wxEVENT_TYPE_TEXT_ENTER_COMMAND)
    wxDialogOkButton(but, event);
}


/*
 * BUGBUG Julian Smart 12/93
 * define USE_PANEL_IN_PANEL
 * if you dare use panel in panel for dialogs; I can't
 * get it to work without absolute positioning, because
 * wxItem::SetSize DOES NOT WORK (try the hello.cc About box.
 *
 */

// Return NULL if cancel pressed
char *wxGetTextFromUser(char *message, char *caption, char *default_value,
                        wxWindow *parent, int x, int y, Bool centre)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, wxDEFAULT_DIALOG_STYLE);

  wxList *messageList = new wxList;
  wxSplitMessage(message, messageList, dialog);

  dialog->NewLine();

  /* MATTHEW: Make Return hit OK button: */
  dialog->textItem = new wxText(dialog, (wxFunction)wxDialogReturn, NULL, default_value, -1, -1, 320);
  dialog->NewLine();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);

  ok->SetDefault();
  dialog->textItem->SetFocus();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#ifndef wx_xview
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (dialog->buttonPressed == wxOK)
    return dialog->textAnswer;

  return NULL;
}


char *wxGetSingleChoice(char *message, char *caption, int n, char *choices[],
                        wxWindow *parent, int x, int y, Bool centre, int width, int height)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 600, 600, wxDEFAULT_DIALOG_STYLE);

  wxList *messageList = new wxList();
  wxSplitMessage(message, messageList, dialog);

  dialog->NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxSINGLE,
                     -1, -1, width, height, n, choices);

  dialog->listBoxItem->SetSelection(0);

  dialog->NewLine();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  // must call SetDefault AFTER creating all buttons of the subPanel
  // (see comment in x/wx_item.cc)
  ok->SetDefault();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (dialog->buttonPressed == wxOK) {
    if (dialog->listPosition >= 0)
      return dialog->listSelection;
  }

  return NULL;
}

int wxGetSingleChoiceIndex(char *message, char *caption, int n, char *choices[],
                           wxWindow *parent, int x, int y, Bool centre,
                           int width, int height)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 600, 600, wxDEFAULT_DIALOG_STYLE);

  wxList *messageList = new wxList();
  wxSplitMessage(message, messageList, dialog);

  dialog->NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxSINGLE,
                     -1, -1, width, height, n, choices);
  dialog->listBoxItem->SetSelection(0);
  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  // SetDefault must be done AFTER creating all buttons
  ok->SetDefault();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (dialog->buttonPressed == wxOK) {
    return dialog->listPosition;
  }

  return -1;
}

char *wxGetSingleChoiceData(char *message, char *caption, int n,
                            char *choices[], char *client_data[],
                            wxWindow *parent, int x, int y, Bool centre,
                            int width, int height)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 600, 600, wxDEFAULT_DIALOG_STYLE);

  wxList *messageList = new wxList();
  wxSplitMessage(message, messageList, dialog);

  dialog->NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxSINGLE, -1, -1,
                     width, height);
  dialog->listBoxItem->SetSelection(0);
  int i;
  for (i = 0; i < n; i++)
    dialog->listBoxItem->Append(choices[i], client_data[i]);
  dialog->listBoxItem->SetSelection(0);

  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  // SetDefault must be done AFTER creating all buttons
  ok->SetDefault();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (dialog->buttonPressed == wxOK) {
    if (dialog->listPosition >= 0)
      return dialog->listClientSelection;
  }

  return NULL;
}

/* Multiple choice dialog contributed by Robert Cowell
 *

The new data passed are in the "int nsel" and "int * selection"

The idea is to make a multiple selection from list of strings.
The returned value is the total number selected. initialily there
are nsel selected, with indices stored in
selection[0],...,selection[nsel-1] which appear highlighted to
begin with. On exit with value i
selection[0..i-1] contains the indices of the selected items.
(Some prior selectecions might be deselected.)
Thus selection must be as big as choices, in case all items are
selected.

*/

int wxGetMultipleChoice(char *message, char *caption,
			int n, char *choices[], 
			int nsel, int * selection,
			wxWindow *parent , int x , int y, Bool centre,
			int width, int height)
{
  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, wxDEFAULT_DIALOG_STYLE);

  wxList *messageList = new wxList();
  wxSplitMessage(message, messageList, dialog);

  dialog -> NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxMULTIPLE,
		    -1, -1, width, height, n, choices);

  int i;
  for (i=0; i < nsel; i++)
      dialog->listBoxItem ->SetSelection(selection[i],TRUE);
 
  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && defined(USE_PANEL_IN_PANEL))
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, "OK");
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, "Cancel");
  // SetDefault must be done AFTER creating all buttons
  ok->SetDefault();

  dialog->buttonPressed = 0;

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);
  
  dialog->Show(TRUE);
  if (dialog->buttonPressed == wxOK)
    {
      for( i=0; i<dialog->nlistSelections; i++)
         selection[i] = dialog->listSelections[i] ;
      return dialog->nlistSelections;
    }
   else 
      return -1;
}

// Pop up a message box: generic version used by X.
int wxbMessageBox(char *message, char *caption, long type,
                 wxWindow *parent, int x, int y)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, wxDEFAULT_DIALOG_STYLE);

  Bool centre = ((type & wxCENTRE) == wxCENTRE);

  wxList *messageList = new wxList();
  wxSplitMessage(message, messageList, dialog);

  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = NULL;
  wxButton *cancel = NULL;
  wxButton *yes = NULL;
  wxButton *no = NULL;

  if (type & wxYES_NO) {
    yes = new wxButton(but_panel, (wxFunction)&wxDialogYesButton, wxSTR_YES);
    no = new wxButton(but_panel, (wxFunction)&wxDialogNoButton, wxSTR_NO);
  }

  if (type & wxOK) {
    ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  }

  if (type & wxCANCEL) {
    cancel = new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  }

  if (ok)
  {
    ok->SetDefault();
    ok->SetFocus();
  }
  else if (yes)
  {
    yes->SetDefault();
    yes->SetFocus();
  }

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  // Do the message centering
  if (centre)
    wxCentreMessage(messageList);

#if (defined(wx_xview) || !USE_PANEL_IN_PANEL)
  // Since subpanels don't work on XView, we must center ok button
  if (ok && !cancel && !yes && !no)
    ok->Centre();
#endif

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  return dialog->buttonPressed;
}

