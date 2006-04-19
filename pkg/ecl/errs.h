/*
 * ERRS.H -- Type codes for first arg to error(). see errs.c.
 * Just use bits for easy testing. if the type is or'd with E_P,
 *   then the systems own error info will also be printed by error().
 * Also declare the external diagnostic strings.
 *
 * E_UERR is a normal user diagnostic.
 * E_IERR is an internal consistency check failure or system error.
 * E_FERR is a fatal internal error. it causes error() to call shutdown().
 * E_P or-ed in causes call to perror() to print system error message.
 */

#define	E_UERR		001
#define	E_IERR		002
#define	E_FERR		004
#define	E_P		01000


#ifndef ERRCOM_DEF
typedef struct {
    int  errflag;               /* set when error is posted             */
    int  errcode;               /* error code                           */
    int  nhandlers;             /* handler nesting level                */
    int  err_restart;           /* YES during estart, NO otherwise      */

    char errmsg[SZ_LINE+1];     /* error message string                 */
    char task[SZ_FNAME+1];      /* task posting the error               */
    char script[SZ_FNAME+1];    /* script calling task			*/
    int  linenum;           	/* lineno where error occurred		*/
} ErrCom, *ErrComPtr;

#define	ERRSIZ		btoi (sizeof (ErrCom))
#endif
#define ERRCOM_DEF


/* The diagnostic strings. defined in errs.c.
 */
extern char *e_appopen;
extern char *e_badstrop;
extern char *e_badsw;
extern char *e_edom;
extern char *e_erange;
extern char *e_fpe;
extern char *e_geonearg;
extern char *e_indexunf;
extern char *e_nominmax;
extern char *e_nopfile;
extern char *e_badpfile;
extern char *e_nostrcnv;
extern char *e_notbool;
extern char *e_onearg;
extern char *e_pambig;
extern char *e_pckambig;
extern char *e_pcknonexist;
extern char *e_posargs;
extern char *e_pnonexist;
extern char *e_ropen;
extern char *e_simplep;
extern char *e_strplusreal;
extern char *e_soverflow;
extern char *e_sunderflow;
extern char *e_tambig;
extern char *e_tnonexist;
extern char *e_twoargs;
extern char *e_unlink;
extern char *e_uopcode;
extern char *e_wopen;
extern char *e_fdivzero;
extern char *e_idivzero;
extern char *e_fdzvalue;
extern char *e_idzvalue;
