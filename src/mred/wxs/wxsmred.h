
extern int mred_eventspace_param;
extern int mred_event_dispatch_param;
extern int mred_ps_setup_param;

extern Scheme_Type mred_eventspace_type;

extern Scheme_Object *MrEdGetFrameList(void);
extern int MrEdCheckForBreak(void);
extern Scheme_Object *MrEdEventspaceConfig(Scheme_Object *);

extern Scheme_Object *MrEdMakeEventspace(Scheme_Config *c);

extern Scheme_Object *wxsBundlePSSetup(wxPrintSetupData *d);
extern wxPrintSetupData *wxsUnbundlePSSetup(Scheme_Object *s);

