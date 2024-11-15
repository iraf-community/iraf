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
void pr_release (int pid);
/* zfiobf.c */
int _u_fmode (int mode);
/* zfioks.c */
void pr_mask (char *str);
/* zpanic.c */
int kernel_panic (char *errmsg);
/* zzsetk.c */
int ZZSETK (char *ospn, char *osbfn, int prtype, int isatty, int in, int out);

#endif /* _OSPROTO_H */
