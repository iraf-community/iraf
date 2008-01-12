#ifndef _ZOS_H
#define _ZOS_H

#include <time.h>

#define import_spp
#include <iraf.h>

#define	SZ_PROCNAME	256

#define safe_strcpy(dest,size,src) (0 < (size) ? strcpy((dest)+(size)-1,"") : NULL, strncpy(dest,src,0 < (size) ? (size)-1 : 0))
#define safe_strcat(dest,size,src) strncat(dest,src,(strlen(dest) < (size)) ? (size)-1-strlen(dest) : 0)

/* gmttolst.c */
extern time_t gmt_to_lst ( time_t );
/* zpanic.c */
extern int kernel_panic ( const char * );
/* zfiobf.c */
extern int vm_delete ( const char *, int );
extern int _u_fmode ( int );
/* zglobl.c */
extern char oscwd[];
extern char os_process_name[];		/* process name, set in zmain	*/
extern int save_prtype;			/* process type saved by zmain	*/
extern PKCHAR osfn_bkgfile[];		/* bkgfile fname if detached	*/
/* prwait.c */
extern int pr_enter ( int, int, int );
extern int pr_wait ( int );
extern int pr_getipc ( int, int *, int * );
extern int pr_release ( int );
/* zfiopr.c */
extern int ipc_isatty;
extern int ipc_in;
extern int ipc_out;
/* zshlib.c */
extern int sh_debug;
/* zzpstr.c */
extern int spp_debug( void );
/* zxwhen.c */
extern int debug_sig;
#ifdef MACOSX
#ifdef OLD_MACOSX
extern void ex_handler ( int, int, struct sigcontext * );
#else
extern void ex_handler ( int, siginfo_t *, void * );
#endif	/* OLD_MACOSX */
#endif	/* MACOSX */

#endif	/* _ZOS_H */
