
@INCLUDE prefix.xci

#include "wx_gauge.h"

@INCLUDE wxs.xci

@HEADER

class wxsGauge : public wxGauge
{
 public:
   int range, pos;

   wxsGauge(wxPanel *panel, char *label, int rng,
	    int x = -1, int y = -1, int width = -1, int height = -1,
	    long style = wxHORIZONTAL, char *name = "gauge")
    : wxGauge(panel, label, rng, x, y, width, height,
	      style, name)
  {
    range = rng; pos = 0;
  }
  void SetRange(int r) {
    wxGauge *sElF = this;
    SETUP_VAR_STACK(1);
    VAR_STACK_PUSH(0, sElF);

    if (r > 0) {
      range = r;
      WITH_VAR_STACK(sElF->wxGauge::SetRange(r));
      if (pos > r) {
       pos = r;
       WITH_VAR_STACK(sElF->wxGauge::SetValue(r));
      }
    }
  }
  void SetValue(int v) {
    if (v >= 0 && v <= range) {
     pos = v;
     wxGauge::SetValue(v);
    }
  }
  int GetValue(void) { return pos; }
  int GetRange(void) { return range; }
};

@BEGINSYMBOLS gaugeStyle > > PRED BUNDLE
@SYM "vertical" : wxVERTICAL
@SYM "horizontal" : wxHORIZONTAL
@ENDSYMBOLS

@CLASSBASE wxsGauge "gauge" : "item"

@CREATOR (wxPanel!,nstring,int,int=-1,int=-1,int=-1,int=-1,SYM[gaugeStyle]=wxHORIZONTAL,string="gauge"); : : /NOZERO[5]|NOZERO[6]//

@INCLUDE wxs_item.xci

@ "set-range" : void SetRange(int);
@ "get-range" : int GetRange();
@ "set-value" : void SetValue(int);
@ "get-value" : int GetValue();

@END
