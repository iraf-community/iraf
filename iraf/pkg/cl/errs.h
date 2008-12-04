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
#ifndef _ERRS_H
#define _ERRS_H

#define	E_UERR		001
#define	E_IERR		002
#define	E_FERR		004
#define	E_P		01000


/* The diagnostic strings. defined in errs.c.
 */
extern const char *e_appopen;
extern const char *e_badstrop;
extern const char *e_badsw;
extern const char *e_edom;
extern const char *e_erange;
extern const char *e_fpe;
extern const char *e_geonearg;
extern const char *e_indexunf;
extern const char *e_nominmax;
extern const char *e_nopfile;
extern const char *e_badpfile;
extern const char *e_nostrcnv;
extern const char *e_notbool;
extern const char *e_onearg;
extern const char *e_pambig;
extern const char *e_pckambig;
extern const char *e_pcknonexist;
extern const char *e_posargs;
extern const char *e_pnonexist;
extern const char *e_ropen;
extern const char *e_simplep;
extern const char *e_strplusreal;
extern const char *e_soverflow;
extern const char *e_sunderflow;
extern const char *e_tambig;
extern const char *e_tnonexist;
extern const char *e_twoargs;
extern const char *e_unlink;
extern const char *e_uopcode;
extern const char *e_wopen;
extern const char *e_fdivzero;
extern const char *e_idivzero;
extern const char *e_invaldef;
extern const char *e_lookparm;

extern int errlog;

extern void cl_error ( int, const char *, ... );

#endif	/* _ERRS_H */
