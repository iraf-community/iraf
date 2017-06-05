/*
 *  KPROTO.H -- IRAF Kernel prototype definitions.
 */

#include <stdio.h>
#include <time.h>               /* for time_t                   */
#include <signal.h>             /* for siginfo_t                */

#if (__SIZEOF_INT__ == 4 && __SIZEOF_POINTER__ == 4) /* ILP32 */

/* alloc.c */
extern int main(int argc, char *argv[]);
extern int alloc(char *argv[], int statonly);
extern int dealloc(char *argv[]);
extern int findsfs(char *argv[]);
/* dio.c */
extern int directio(int fd, int advice);
/* getproc.c */
extern int uid_executing(int uid);
/* gmttolst.c */
extern time_t gmt_to_lst(time_t gmt);
/* irafpath.c */
extern char *irafpath(char *fname);
/* prwait.c */
extern void pr_enter(int pid, int inchan, int outchan);
extern int pr_wait(int pid);
extern int pr_getipc(int pid, int *inchan, int *outchan);
extern struct proctable *pr_findpid(int pid);
extern void pr_release(int pid);
/* tape.c */
extern int main(int argc, char *argv[]);
extern void mtop(int op, int count);
extern char *nextcmd(FILE *in);
extern char *gettok(void);
extern char *prompt(void);
extern void pstatus(void);
extern void output(char *text);
extern void phelp(void);
/* zalloc.c */
extern int zdvall_(shortint *aliases, int *allflg, int *status);
extern int zdvown_(shortint *device, shortint *owner, int *maxch, int *status);
extern int loggedin(int uid);
/* zawset.c */
extern int zawset_(int *best_size, int *new_size, int *old_size, int *max_size);
/* zcall.c */
extern int zcall0_(int *proc);
extern int zcall1_(int *proc, void *arg1);
extern int zcall2_(int *proc, void *arg1, void *arg2);
extern int zcall3_(int *proc, void *arg1, void *arg2, void *arg3);
extern int zcall4_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4);
extern int zcall5_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5);
extern int zcall6_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6);
extern int zcall7_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7);
extern int zcall8_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8);
extern int zcall9_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9);
extern int zcalla_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, void *arg10);
/* zdojmp.c */
extern void zdojmp_(int *jmpbuf, int *status);
/* zfacss.c */
extern int zfacss_(shortint *fname, int *mode, int *type, int *status);
/* zfaloc.c */
extern int zfaloc_(shortint *fname, int *nbytes, int *status);
/* zfchdr.c */
extern int zfchdr_(shortint *newdir, int *status);
/* zfdele.c */
extern int zfdele_(shortint *fname, int *status);
/* zfgcwd.c */
extern int zfgcwd_(shortint *outstr, int *maxch, int *status);
/* zfinfo.c */
extern int zfinfo_(shortint *fname, int *finfo_struct, int *status);
/* zfiobf.c */
extern int zopnbf_(shortint *osfn, int *mode, int *chan);
extern int zclsbf_(int *fd, int *status);
extern int zardbf_(int *chan, shortint *buf, int *maxbytes, int *offset);
extern int zawrbf_(int *chan, shortint *buf, int *nbytes, int *offset);
extern int zawtbf_(int *fd, int *status);
extern int zsttbf_(int *fd, int *param, int *lvalue);
extern int _u_fmode(int mode);
extern int vm_access(char *fname, int mode);
extern int vm_delete(char *fname, int force);
extern int vm_reservespace(long nbytes);
extern int vm_largefile(long nbytes);
extern int vm_directio(int fd, int flag);
/* zfioks.c */
extern int zopnks_(shortint *x_server, int *mode, int *chan);
extern int zclsks_(int *chan, int *status);
extern int zardks_(int *chan, shortint *buf, int *totbytes, int *loffset);
extern int zawrks_(int *chan, shortint *buf, int *totbytes, int *loffset);
extern int zawtks_(int *chan, int *status);
extern int zsttks_(int *chan, int *param, int *lvalue);
extern void pr_mask(char *str);
/* zfiolp.c */
extern int zopnlp_(shortint *printer, int *mode, int *chan);
extern int zclslp_(int *chan, int *status);
extern int zardlp_(int *chan, shortint *buf, int *maxbytes, int *offset);
extern int zawrlp_(int *chan, shortint *buf, int *nbytes, int *offset);
extern int zawtlp_(int *chan, int *status);
extern int zsttlp_(int *chan, int *param, int *lvalue);
/* zfiomt.c */
extern int zzopmt_(shortint *device, int *acmode, shortint *devcap, int *devpos, int *newfile, int *chan);
extern int zzclmt_(int *chan, int *devpos, int *o_status);
extern int zzrdmt_(int *chan, shortint *buf, int *maxbytes, int *offset);
extern int zzwrmt_(int *chan, shortint *buf, int *nbytes, int *offset);
extern int zzwtmt_(int *chan, int *devpos, int *o_status);
extern int zzstmt_(int *chan, int *param, int *lvalue);
extern int zzrwmt_(shortint *device, shortint *devcap, int *o_status);
/* zfiond.c */
extern int zopnnd_(shortint *pk_osfn, int *mode, int *chan);
extern int zclsnd_(int *fd, int *status);
extern int zardnd_(int *chan, shortint *buf, int *maxbytes, int *offset);
extern int zawrnd_(int *chan, shortint *buf, int *nbytes, int *offset);
extern int zawtnd_(int *fd, int *status);
extern int zsttnd_(int *fd, int *param, int *lvalue);
/* zfiopl.c */
extern int zopnpl_(shortint *plotter, int *mode, int *chan);
extern int zclspl_(int *chan, int *status);
extern int zardpl_(int *chan, shortint *buf, int *maxbytes, int *offset);
extern int zawrpl_(int *chan, shortint *buf, int *nbytes, int *offset);
extern int zawtpl_(int *chan, int *status);
extern int zsttpl_(int *chan, int *param, int *lvalue);
/* zfiopr.c */
extern int zopcpr_(shortint *osfn, int *inchan, int *outchan, int *pid);
extern int zclcpr_(int *pid, int *exit_status);
extern int zardpr_(int *chan, shortint *buf, int *maxbytes, int *loffset);
extern int zawrpr_(int *chan, shortint *buf, int *nbytes, int *loffset);
extern int zawtpr_(int *chan, int *status);
extern int zsttpr_(int *chan, int *param, int *lvalue);
/* zfiosf.c */
extern int zopnsf_(shortint *osfn, int *mode, int *chan);
extern int zclssf_(int *fd, int *status);
extern int zardsf_(int *chan, shortint *buf, int *maxbytes, int *offset);
extern int zawrsf_(int *chan, shortint *buf, int *nbytes, int *offset);
extern int zawtsf_(int *fd, int *status);
extern int zsttsf_(int *fd, int *param, int *lvalue);
/* zfiotx.c */
extern int zopntx_(shortint *osfn, int *mode, int *chan);
extern int zclstx_(int *fd, int *status);
extern int zflstx_(int *fd, int *status);
extern int zgettx_(int *fd, shortint *buf, int *maxchars, int *status);
extern int znottx_(int *fd, int *offset);
extern int zputtx_(int *fd, shortint *buf, int *nchars, int *status);
extern int zsektx_(int *fd, int *znottx_offset, int *status);
extern int zstttx_(int *fd, int *param, int *value);
/* zfioty.c */
extern int zopnty_(shortint *osfn, int *mode, int *chan);
extern int zclsty_(int *fd, int *status);
extern int zflsty_(int *fd, int *status);
extern int zgetty_(int *fd, shortint *buf, int *maxchars, int *status);
extern int znotty_(int *fd, int *offset);
extern int zputty_(int *fd, shortint *buf, int *nchars, int *status);
extern int zsekty_(int *fd, int *znotty_offset, int *status);
extern int zsttty_(int *fd, int *param, int *value);
/* zfmkcp.c */
extern int zfmkcp_(shortint *osfn, shortint *new_osfn, int *status);
/* zfmkdr.c */
extern int zfmkdr_(shortint *newdir, int *status);
/* zfnbrk.c */
extern int zfnbrk_(shortint *vfn, int *uroot_offset, int *uextn_offset);
/* zfpath.c */
extern int zfpath_(shortint *osfn, shortint *pathname, int *maxch, int *nchars);
/* zfpoll.c */
extern int zfpoll_(int *pfds, int *nfds, int *timeout, int *npoll, int *status);
/* zfprot.c */
extern int zfprot_(shortint *fname, int *action, int *status);
/* zfrnam.c */
extern int zfrnam_(shortint *oldname, shortint *newname, int *status);
/* zfrmdr.c */
extern int zfrmdr_(shortint *dir, int *status);
/* zfsubd.c */
extern int zfsubd_(shortint *osdir, int *maxch, shortint *subdir, int *nchars);
/* zfunc.c */
extern int zfunc0_(int *proc);
extern int zfunc1_(int *proc, void *arg1);
extern int zfunc2_(int *proc, void *arg1, void *arg2);
extern int zfunc3_(int *proc, void *arg1, void *arg2, void *arg3);
extern int zfunc4_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4);
extern int zfunc5_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5);
extern int zfunc6_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6);
extern int zfunc7_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7);
extern int zfunc8_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8);
extern int zfunc9_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9);
extern int zfunca_(int *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, void *arg10);
/* zfutim.c */
extern int zfutim_(shortint *fname, int *atime, int *mtime, int *status);
/* zfxdir.c */
extern int zfxdir_(shortint *osfn, shortint *osdir, int *maxch, int *nchars);
/* zgcmdl.c */
extern int zgcmdl_(shortint *cmd, int *maxch, int *status);
/* zghost.c */
extern int zghost_(shortint *outstr, int *maxch);
/* zglobl.c */
/* zgmtco.c */
extern int zgmtco_(int *gmtcor);
/* zgtenv.c */
extern int zgtenv_(shortint *envvar, shortint *outstr, int *maxch, int *status);
/* zgtime.c */
extern int zgtime_(int *clock_time, int *cpu_time);
/* zgtpid.c */
extern int zgtpid_(int *pid);
/* zintpr.c */
extern int zintpr_(int *pid, int *exception, int *status);
/* zlocpr.c 
extern int zlocpr_(PFI proc, int *o_epa);
*/
/* zlocva.c */
extern int zlocva_(shortint *variable, int *location);
/* zmain.c */
extern int main(int argc, char *argv[]);
/* zmaloc.c */
extern int zmaloc_(int *buf, int *nbytes, int *status);
/* zmfree.c */
extern int zmfree_(int *buf, int *status);
/* zopdir.c */
extern int zopdir_(shortint *fname, int *chan);
extern int zcldir_(int *chan, int *status);
extern int zgfdir_(int *chan, shortint *outstr, int *maxch, int *status);
/* zopdpr.c */
extern int zopdpr_(shortint *osfn, shortint *bkgfile, shortint *queue, int *jobcode);
extern int zcldpr_(int *jobcode, int *killflag, int *exit_status);
/* zoscmd.c */
extern int zoscmd_(shortint *oscmd, shortint *stdin_file, shortint *stdout_file, shortint *stderr_file, int *status);
extern int pr_onint(int usig, int *hwcode, int *scp);
/* zpanic.c */
extern int zpanic_(int *errcode, shortint *errmsg);
extern int kernel_panic(char *errmsg);
/* zraloc.c */
extern int zraloc_(int *buf, int *nbytes, int *status);
/* zshlib.c */
extern void vlibinit_(void);
/* zwmsec.c */
extern int zwmsec_(int *msec);
/* zxwhen.c */
extern int zxwhen_(int *sig_code, int *epa, int *old_epa);
extern void ex_handler(int unix_signal, siginfo_t *info, void *ucp);
extern int zxgmes_(int *os_exception, shortint *errmsg, int *maxch);
/* zzepro.c */
extern int zzepro_(void);
/* zzexit.c */
extern int exit_(int *code);
/* zzpstr.c */
extern int spp_debug(void);
extern int zzpstr_(shortint *s1, shortint *s2);
extern int zzlstr_(shortint *s1, shortint *s2);
extern void spp_printstr(shortint *s);
extern void spp_printmemc(int memc_ptr);
/* zzsetk.c */
extern int zzsetk_(char *ospn, char *osbfn, int prtype, int isatty, int in, int out);
/* zzstrt.c */
extern int zzstrt_(void);
extern int zzstop_(void);
extern void ready_(void);
extern void mdump_(int *buf, int *nbytes);



#elif (__SIZEOF_LONG__ == 8 && __SIZEOF_POINTER__ == 8) /* LP64 */



/* dio.c */
extern int directio(int fd, int advice);
/* getproc.c */
extern int uid_executing(int uid);
/* gmttolst.c */
extern time_t gmt_to_lst(time_t gmt);
/* irafpath.c */
extern char *irafpath(char *fname);
/* prwait.c */
extern void pr_enter(int pid, int inchan, int outchan);
extern int pr_wait(int pid);
extern int pr_getipc(int pid, int *inchan, int *outchan);
extern struct proctable *pr_findpid(int pid);
extern void pr_release(int pid);
/* zalloc.c */
extern int zdvall_(shortint *aliases, long *allflg, long *status);
extern int zdvown_(shortint *device, shortint *owner, long *maxch, long *status);
extern int loggedin(int uid);
/* zawset.c */
extern int zawset_(long *best_size, long *new_size, long *old_size, long *max_size);
/* zcall.c */
/*
extern int zcall0_(long *proc);
extern int zcall1_(long *proc, void *arg1);
extern int zcall2_(long *proc, void *arg1, void *arg2);
extern int zcall3_(long *proc, void *arg1, void *arg2, void *arg3);
extern int zcall4_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4);
extern int zcall5_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5);
extern int zcall6_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6);
extern int zcall7_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7);
extern int zcall8_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8);
extern int zcall9_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9);
extern int zcalla_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, void *arg10);
*/
/* zdojmp.c */
extern void zdojmp_(long *jmpbuf, long *status);
/* zfacss.c */
extern int zfacss_(shortint *fname, long *mode, long *type, long *status);
/* zfaloc.c */
extern int zfaloc_(shortint *fname, long *nbytes, long *status);
/* zfchdr.c */
extern int zfchdr_(shortint *newdir, long *status);
/* zfdele.c */
extern int zfdele_(shortint *fname, long *status);
/* zfgcwd.c */
extern int zfgcwd_(shortint *outstr, long *maxch, long *status);
/* zfinfo.c */
extern int zfinfo_(shortint *fname, long *finfo_struct, long *status);
/* zfiobf.c */
extern int zopnbf_(shortint *osfn, long *mode, long *chan);
extern int zclsbf_(long *fd, long *status);
extern int zardbf_(long *chan, shortint *buf, long *maxbytes, long *offset);
extern int zawrbf_(long *chan, shortint *buf, long *nbytes, long *offset);
extern int zawtbf_(long *fd, long *status);
extern int zsttbf_(long *fd, long *param, long *lvalue);
extern int _u_fmode(int mode);
extern int vm_access(char *fname, int mode);
extern int vm_delete(char *fname, int force);
extern int vm_reservespace(long nbytes);
extern int vm_largefile(long nbytes);
extern int vm_directio(int fd, int flag);
/* zfioks.c */
extern int zopnks_(shortint *x_server, long *mode, long *chan);
extern int zclsks_(long *chan, long *status);
extern int zardks_(long *chan, shortint *buf, long *totbytes, long *loffset);
extern int zawrks_(long *chan, shortint *buf, long *totbytes, long *loffset);
extern int zawtks_(long *chan, long *status);
extern int zsttks_(long *chan, long *param, long *lvalue);
extern void pr_mask(char *str);
/* zfiolp.c */
extern int zopnlp_(shortint *printer, long *mode, long *chan);
extern int zclslp_(long *chan, long *status);
extern int zardlp_(long *chan, shortint *buf, long *maxbytes, long *offset);
extern int zawrlp_(long *chan, shortint *buf, long *nbytes, long *offset);
extern int zawtlp_(long *chan, long *status);
extern int zsttlp_(long *chan, long *param, long *lvalue);
/* zfiomt.c */
extern int zzopmt_(shortint *device, long *acmode, shortint *devcap, long *devpos, long *newfile, long *chan);
extern int zzclmt_(long *chan, long *devpos, long *o_status);
extern int zzrdmt_(long *chan, shortint *buf, long *maxbytes, long *offset);
extern int zzwrmt_(long *chan, shortint *buf, long *nbytes, long *offset);
extern int zzwtmt_(long *chan, long *devpos, long *o_status);
extern int zzstmt_(long *chan, long *param, long *lvalue);
extern int zzrwmt_(shortint *device, shortint *devcap, long *o_status);
/* zfiond.c */
extern int zopnnd_(shortint *pk_osfn, long *mode, long *chan);
extern int zclsnd_(long *fd, long *status);
extern int zardnd_(long *chan, shortint *buf, long *maxbytes, long *offset);
extern int zawrnd_(long *chan, shortint *buf, long *nbytes, long *offset);
extern int zawtnd_(long *fd, long *status);
extern int zsttnd_(long *fd, long *param, long *lvalue);
/* zfiopl.c */
extern int zopnpl_(shortint *plotter, long *mode, long *chan);
extern int zclspl_(long *chan, long *status);
extern int zardpl_(long *chan, shortint *buf, long *maxbytes, long *offset);
extern int zawrpl_(long *chan, shortint *buf, long *nbytes, long *offset);
extern int zawtpl_(long *chan, long *status);
extern int zsttpl_(long *chan, long *param, long *lvalue);
/* zfiopr.c */
extern int zopcpr_(shortint *osfn, long *inchan, long *outchan, long *pid);
extern int zclcpr_(long *pid, long *exit_status);
extern int zardpr_(long *chan, shortint *buf, long *maxbytes, long *loffset);
extern int zawrpr_(long *chan, shortint *buf, long *nbytes, long *loffset);
extern int zawtpr_(long *chan, long *status);
extern int zsttpr_(long *chan, long *param, long *lvalue);
/* zfiosf.c */
extern int zopnsf_(shortint *osfn, long *mode, long *chan);
extern int zclssf_(long *fd, long *status);
extern int zardsf_(long *chan, shortint *buf, long *maxbytes, long *offset);
extern int zawrsf_(long *chan, shortint *buf, long *nbytes, long *offset);
extern int zawtsf_(long *fd, long *status);
extern int zsttsf_(long *fd, long *param, long *lvalue);
/* zfiotx.c */
extern int zopntx_(shortint *osfn, long *mode, long *chan);
extern int zclstx_(long *fd, long *status);
extern int zflstx_(long *fd, long *status);
extern int zgettx_(long *fd, shortint *buf, long *maxchars, long *status);
extern int znottx_(long *fd, long *offset);
extern int zputtx_(long *fd, shortint *buf, long *nchars, long *status);
extern int zsektx_(long *fd, long *znottx_offset, long *status);
extern int zstttx_(long *fd, long *param, long *value);
/* zfioty.c */
extern int zopnty_(shortint *osfn, long *mode, long *chan);
extern int zclsty_(long *fd, long *status);
extern int zflsty_(long *fd, long *status);
extern int zgetty_(long *fd, shortint *buf, long *maxchars, long *status);
extern int znotty_(long *fd, long *offset);
extern int zputty_(long *fd, shortint *buf, long *nchars, long *status);
extern int zsekty_(long *fd, long *znotty_offset, long *status);
extern int zsttty_(long *fd, long *param, long *value);
/* zfmkcp.c */
extern int zfmkcp_(shortint *osfn, shortint *new_osfn, long *status);
/* zfmkdr.c */
extern int zfmkdr_(shortint *newdir, long *status);
/* zfnbrk.c */
extern int zfnbrk_(shortint *vfn, long *uroot_offset, long *uextn_offset);
/* zfpath.c */
extern int zfpath_(shortint *osfn, shortint *pathname, long *maxch, long *nchars);
/* zfpoll.c */
extern int zfpoll_(long *pfds, long *nfds, long *timeout, long *npoll, long *status);
/* zfprot.c */
extern int zfprot_(shortint *fname, long *action, long *status);
/* zfrnam.c */
extern int zfrnam_(shortint *oldname, shortint *newname, long *status);
/* zfsubd.c */
extern int zfsubd_(shortint *osdir, long *maxch, shortint *subdir, long *nchars);
/* zfunc.c */
extern long zfunc0_(long *proc);
extern long zfunc1_(long *proc, void *arg1);
extern long zfunc2_(long *proc, void *arg1, void *arg2);
extern long zfunc3_(long *proc, void *arg1, void *arg2, void *arg3);
extern long zfunc4_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4);
extern long zfunc5_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5);
extern long zfunc6_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6);
extern long zfunc7_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7);
extern long zfunc8_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8);
extern long zfunc9_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9);
extern long zfunca_(long *proc, void *arg1, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, void *arg10);
/* zfutim.c */
extern int zfutim_(shortint *fname, long *atime, long *mtime, long *status);
/* zfxdir.c */
extern int zfxdir_(shortint *osfn, shortint *osdir, long *maxch, long *nchars);
/* zgcmdl.c */
extern int zgcmdl_(shortint *cmd, long *maxch, long *status);
/* zghost.c */
extern int zghost_(shortint *outstr, long *maxch);
/* zglobl.c */
/* zgmtco.c */
extern int zgmtco_(long *gmtcor);
/* zgtenv.c */
extern int zgtenv_(shortint *envvar, shortint *outstr, long *maxch, long *status);
/* zgtime.c */
extern int zgtime_(long *clock_time, long *cpu_time);
/* zgtpid.c */
extern int zgtpid_(long *pid);
/* zintpr.c */
extern int zintpr_(long *pid, long *exception, long *status);
/* zlocpr.c */
/*
extern int zlocpr_(PFI proc, long *o_epa);
*/
/* zlocva.c */
extern int zlocva_(shortint *variable, long *location);
/* zmain.c */
extern int main(int argc, char *argv[]);
/* zmaloc.c */
extern int zmaloc_(long *buf, long *nbytes, long *status);
/* zmfree.c */
extern int zmfree_(long *buf, long *status);
/* zopdir.c */
extern int zopdir_(shortint *fname, long *chan);
extern int zcldir_(long *chan, long *status);
extern int zgfdir_(long *chan, shortint *outstr, long *maxch, long *status);
/* zopdpr.c */
extern int zopdpr_(shortint *osfn, shortint *bkgfile, shortint *queue, long *jobcode);
extern int zcldpr_(long *jobcode, long *killflag, long *exit_status);
/* zoscmd.c */
extern int zoscmd_(shortint *oscmd, shortint *stdin_file, shortint *stdout_file, shortint *stderr_file, long *status);
extern int pr_onint(int usig, int *hwcode, int *scp);
/* zpanic.c */
extern int zpanic_(long *errcode, shortint *errmsg);
extern int kernel_panic(char *errmsg);
/* zraloc.c */
extern int zraloc_(long *buf, long *nbytes, long *status);
/* zshlib.c */
extern void vlibinit_(void);
/* zwmsec.c */
extern int zwmsec_(long *msec);
/* zxwhen.c */
extern int zxwhen_(long *sig_code, long *epa, long *old_epa);
extern void ex_handler(int unix_signal, siginfo_t *info, void *ucp);
extern int zxgmes_(long *os_exception, shortint *errmsg, long *maxch);
extern int gfpucw_(long *xcw);
extern int sfpucw_(long *xcw);
/* zzepro.c */
/*
extern int zzepro_(void);
*/
/* zzexit.c */
extern int exit_(long *code);
/* zzpstr.c */
extern int spp_debug(void);
extern int zzpstr_(shortint *s1, shortint *s2);
extern int zzlstr_(shortint *s1, shortint *s2);
extern void spp_printstr(shortint *s);
extern void spp_printmemc(int memc_ptr);
/* zzsetk.c */
extern int zzsetk_(char *ospn, char *osbfn, int prtype, int isatty, int in, int out);
/* zzstrt.c */
extern int zzstrt_(void);
extern int zzstop_(void);
extern void ready_(void);
extern void mdump_(long *buf, long *nbytes);

#else

#error "No data model: need either LP64 or ILP32"

#endif
