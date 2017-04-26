/*
 * CLMODES.H -- Return a boolean result for the state of the various cl modes.
 * Done by referring to the pointers declared in modes.c.
 * The pointers are set up initially from the entry of the corresponding
 *   parameter in the cl's pfile. see setclmodes() in modes.c.
 *   abbreviations is hairy enough that is a real function in modes.c.
 * A NULL pointer results in false, as does an undefined or indefinite value.
 */

extern struct param *clecho;
#define	echocmds()	(clecho != NULL && \
			    !(clecho->p_type & (OT_UNDEF|OT_INDEF)) && \
			    clecho->p_val.v_i)

extern struct param *clnotify;
#define	notify()	(clnotify != NULL && \
			    !(clnotify->p_type & (OT_UNDEF|OT_INDEF)) && \
			    clnotify->p_val.v_i)

extern struct param *clmenus;
#define	menus()		(clmenus != NULL && \
			    !(clmenus->p_type & (OT_UNDEF|OT_INDEF)) && \
			    clmenus->p_val.v_i)

extern struct param *clshowtype;
#define	showtype()	(clshowtype != NULL && \
			    !(clshowtype->p_type & (OT_UNDEF|OT_INDEF)) && \
			    clshowtype->p_val.v_i)

extern struct param *clkeeplog;
#define	keeplog()	(clkeeplog != NULL && \
			    !(clkeeplog->p_type & (OT_UNDEF|OT_INDEF)) && \
			    clkeeplog->p_val.v_i)

extern struct param *cllexmodes;
#define	lexmodes()	(cllexmodes != NULL && \
			    !(cllexmodes->p_type & (OT_UNDEF|OT_INDEF)) && \
			    cllexmodes->p_val.v_i)

/* Return a pointer to the name of the logfile, or NULL if not defined.
 */
extern struct param *cllogfile;
#define	logfile() \
	((cllogfile == NULL || (cllogfile->p_type & (OT_UNDEF|OT_INDEF))) ? \
	NULL : cllogfile->p_val.v_s)

/* Flags and macros for logging control.
 */
extern int	cllogmode;		/* NOT a *(struct param), see modes.c */

#define log_commands()		(cllogmode & LOG_COMMANDS)
#define log_background() 	(cllogmode & LOG_BACKGROUND)
#define log_errors() 		(cllogmode & LOG_ERRORS)
#define log_trace() 		(cllogmode & LOG_TRACE)

#define LOG_COMMANDS   0001
#define LOG_BACKGROUND 0002
#define LOG_ERRORS     0004
#define LOG_TRACE      0010

/* CL parameters for Eparam and Ehistory options.
 */
extern int 	ep_standout, 
		ep_showall;
extern int	eh_standout,
		eh_verify, 
		eh_bol;
