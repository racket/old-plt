/* Call to start: */
extern void Drop_GetArgs(int *, char ***);

/* You supply: */
void Drop_Runtime(char **, int);
void Drop_Quit(void);

/* Utility: */
extern void ParseLine(char *, int *, char ***);

extern int scheme_mac_ready;
extern int scheme_mac_argc;
extern char **scheme_mac_argv;
