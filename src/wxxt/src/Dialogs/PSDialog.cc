/*								-*- C++ -*-
 * $Id: PSDialog.cc,v 1.2 1996/01/11 10:26:58 markus Exp $
 *
 * Purpose: postscript dialog
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#if 0 /* Replaced by common wb_ps.cxx/PSDC.cc */

#define  Uses_wxDialogBase
#define  Uses_wxPostScriptDC
#define  Uses_wxPrintSetup
#define  Uses_wxRadioBox
#define  Uses_wxText
#include "wx.h"

Bool wxPostScriptDC::XPrinterDialog(wxWindow *parent)
{
    Bool answer;

    wxDialogBase *box
	= DEBUG_NEW wxDialogBase(NULL, "Printer Settings",
				 wxCENTRE | wxOK | wxCANCEL, parent, -1, -1);
    box->SetLabelPosition(wxVERTICAL);

    float      xs, ys, xt, yt;
    wxText     *cmd, *flags, *x_scale, *y_scale, *x_trans, *y_trans;
    wxRadioBox *orient, *target;
    char       *str_orient[] = { "Portrait", "Landscape" };
    char       *str_target[] = { "Send to Printer", "Send to File", "Preview Only" };
    wxThePrintSetupData->GetPrinterTranslation(&xt, &yt);
    wxThePrintSetupData->GetPrinterScaling(&xs, &ys);

    cmd     = DEBUG_NEW wxText(box, NULL, "Printer Command:",
			       wxThePrintSetupData->GetPrinterCommand());
    flags   = DEBUG_NEW wxText(box, NULL, "Printer Options:",
			       wxThePrintSetupData->GetPrinterOptions());
    box->NewLine();
    orient  = DEBUG_NEW wxRadioBox(box, NULL, "Orientation:", -1, -1, 150, -1,
				   2, str_orient, 0, wxVERTICAL);
              orient->SetSelection(wxThePrintSetupData->GetPrinterOrientation());
    target  = DEBUG_NEW wxRadioBox(box, NULL, "PostScript:", -1, -1, 150, -1,
				   3, str_target, 0, wxVERTICAL);
              target->SetSelection(wxThePrintSetupData->GetPrinterMode());
    box->NewLine();
    x_scale = DEBUG_NEW wxText(box, NULL, "X Scaling: ", wxFloatToString(xs));
    y_scale = DEBUG_NEW wxText(box, NULL, "Y Scaling: ", wxFloatToString(ys));
    box->NewLine();
    x_trans = DEBUG_NEW wxText(box, NULL, "X Translation: ", wxFloatToString(xt));
    y_trans = DEBUG_NEW wxText(box, NULL, "Y Translation: ", wxFloatToString(yt));

    // wait for input
    if ((answer = (box->GetInput()==wxOK))) {
	wxThePrintSetupData->SetPrinterCommand(cmd->GetValue());
	wxThePrintSetupData->SetPrinterOptions(flags->GetValue());
	wxThePrintSetupData->SetPrinterOrientation(orient->GetSelection());
	wxThePrintSetupData->SetPrinterMode(target->GetSelection());
	StringToFloat(x_scale->GetValue(), &wxThePrintSetupData->printer_scale_x);
	StringToFloat(y_scale->GetValue(), &wxThePrintSetupData->printer_scale_y);
	StringToFloat(x_trans->GetValue(), &wxThePrintSetupData->printer_translate_x);
	StringToFloat(y_trans->GetValue(), &wxThePrintSetupData->printer_translate_y);
    }
    delete box;
    return answer;
}

#endif
