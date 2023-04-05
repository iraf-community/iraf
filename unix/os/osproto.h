#ifndef _OSPROTO_H
#define _OSPROTO_H

#include <sys/types.h>
#include <time.h>               /* for time_t                   */
#include <signal.h>             /* for siginfo_t                */

/*
 * Functions exposed to IRAF sys
 */
#define import_kproto
#define import_spp
#include <iraf.h>

/*
 * Functions internal to host$os
 */
/* dio.c */
int directio (int fd, int advice);
/* getproc.c */
int uid_executing (int uid);
/* gmttolst.c */
time_t gmt_to_lst (time_t gmt);
/* irafpath.c */
char *irafpath (char *fname);
/* prwait.c */
void pr_enter (int pid, int inchan, int outchan);
int pr_wait (int pid);
int pr_getipc (int pid, int *inchan, int *outchan);
struct proctable *pr_findpid (int pid);
void pr_release (int pid);
/* zalloc.c */
int loggedin (int uid);
/* zfiobf.c */
int _u_fmode (int mode);
int vm_access (char *fname, int mode);
int vm_delete (char *fname, int force);
int vm_reservespace (long nbytes);
int vm_largefile (long nbytes);
int vm_directio (int fd, int flag);
/* zfioks.c */
void pr_mask (char *str);
/* zoscmd.c */
int pr_onint (int usig, int *hwcode, int *scp);
/* zpanic.c */
int kernel_panic (char *errmsg);
/* zxwhen.c */
void ex_handler (int unix_signal, siginfo_t *info, void *ucp);
/* zzpstr.c */
int spp_debug (void);
void spp_printstr (XCHAR *s);
void spp_printmemc (long memc_ptr);
/* zzsetk.c */
int ZZSETK (char *ospn, char *osbfn, int prtype, int isatty, int in, int out);

#endif /* _OSPROTO_H */
