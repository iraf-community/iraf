/*
 *  KPROTO.H -- IRAF Kernel prototype definitions.
 */

#ifndef D_spp
#include "iraf/spp.h"
#endif
#ifndef D_knames
#include "iraf/knames.h"
#endif

typedef	int   (*PFI)();

/* zalloc.c */
int ZDVALL (PKCHAR *aliases, XINT *allflg, XINT *status);
int ZDVOWN (PKCHAR *device, PKCHAR *owner, XINT *maxch, XINT *status);
/* zawset.c */
int ZAWSET (XINT *best_size, XINT *new_size, XINT *old_size, XINT *max_size);
/* zdojmp.c */
void ZDOJMP (XINT *jmpbuf, XINT *status);
/* zfacss.c */
int ZFACSS (PKCHAR *fname, XINT *mode, XINT *type, XINT *status);
/* zfaloc.c */
int ZFALOC (PKCHAR *fname, XLONG *nbytes, XINT *status);
/* zfchdr.c */
int ZFCHDR (PKCHAR *newdir, XINT *status);
/* zfdele.c */
int ZFDELE (PKCHAR *fname, XINT *status);
/* zfgcwd.c */
int ZFGCWD (PKCHAR *outstr, XINT *maxch, XINT *status);
/* zfinfo.c */
int ZFINFO(PKCHAR *fname, XLONG *finfo_struct, XINT *status);
/* zfiobf.c */
int ZOPNBF (PKCHAR *osfn, XINT *mode, XINT *chan);
int ZCLSBF (XINT *fd, XINT *status);
int ZARDBF (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
int ZAWRBF (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
int ZAWTBF (XINT *fd, XINT *status);
int ZSTTBF (XINT *fd, XINT *param, XLONG *lvalue);
/* zfioks.c */
int ZOPNKS (PKCHAR *x_server, XINT *mode, XINT *chan);
int ZCLSKS (XINT *chan, XINT *status);
int ZARDKS (XINT *chan, XCHAR *buf, XINT *totbytes, XLONG *loffset);
int ZAWRKS (XINT *chan, XCHAR *buf, XINT *totbytes, XLONG *loffset);
int ZAWTKS (XINT *chan, XINT *status);
int ZSTTKS (XINT *chan, XINT *param, XLONG *lvalue);
/* zfiolp.c */
int ZOPNLP (PKCHAR *printer, XINT *mode, XINT *chan);
int ZCLSLP (XINT *chan, XINT *status);
int ZARDLP (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
int ZAWRLP (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
int ZAWTLP (XINT *chan, XINT *status);
int ZSTTLP (XINT *chan, XINT *param, XLONG *lvalue);
/* zfiomt.c */
int ZZOPMT (PKCHAR *device, XINT *acmode, PKCHAR *devcap, XINT *devpos, XINT *newfile, XINT *chan);
int ZZCLMT (XINT *chan, XINT *devpos, XINT *o_status);
int ZZRDMT (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
int ZZWRMT (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
int ZZWTMT (XINT *chan, XINT *devpos, XINT *o_status);
int ZZSTMT (XINT *chan, XINT *param, XLONG *lvalue);
int ZZRWMT (PKCHAR *device, PKCHAR *devcap, XINT *o_status);
/* zfiond.c */
int ZOPNND (PKCHAR *pk_osfn, XINT *mode, XINT *chan);
int ZCLSND (XINT *fd, XINT *status);
int ZARDND (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
int ZAWRND (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
int ZAWTND (XINT *fd, XINT *status);
int ZSTTND (XINT *fd, XINT *param, XLONG *lvalue);
/* zfiopl.c */
int ZOPNPL (PKCHAR *plotter, XINT *mode, XINT *chan);
int ZCLSPL (XINT *chan, XINT *status);
int ZARDPL (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
int ZAWRPL (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
int ZAWTPL (XINT *chan, XINT *status);
int ZSTTPL (XINT *chan, XINT *param, XLONG *lvalue);
/* zfiopr.c */
int ZOPCPR (PKCHAR *osfn, XINT *inchan, XINT *outchan, XINT *pid);
int ZCLCPR (XINT *pid, XINT *exit_status);
int ZARDPR (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *loffset);
int ZAWRPR (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *loffset);
int ZAWTPR (XINT *chan, XINT *status);
int ZSTTPR (XINT *chan, XINT *param, XLONG *lvalue);
/* zfiosf.c */
int ZOPNSF (PKCHAR *osfn, XINT *mode, XINT *chan);
int ZCLSSF (XINT *fd, XINT *status);
int ZARDSF (XINT *chan, XCHAR *buf, XINT *maxbytes, XLONG *offset);
int ZAWRSF (XINT *chan, XCHAR *buf, XINT *nbytes, XLONG *offset);
int ZAWTSF (XINT *fd, XINT *status);
int ZSTTSF (XINT *fd, XINT *param, XLONG *lvalue);
/* zfiotx.c */
int ZOPNTX (PKCHAR *osfn, XINT *mode, XINT *chan);
int ZCLSTX (XINT *fd, XINT *status);
int ZFLSTX (XINT *fd, XINT *status);
int ZGETTX (XINT *fd, XCHAR *buf, XINT *maxchars, XINT *status);
int ZNOTTX (XINT *fd, XLONG *offset);
int ZPUTTX (XINT *fd, XCHAR *buf, XINT *nchars, XINT *status);
int ZSEKTX (XINT *fd, XLONG *znottx_offset, XINT *status);
int ZSTTTX (XINT *fd, XINT *param, XLONG *value);
/* zfioty.c */
int ZOPNTY (PKCHAR *osfn, XINT *mode, XINT *chan);
int ZCLSTY (XINT *fd, XINT *status);
int ZFLSTY (XINT *fd, XINT *status);
int ZGETTY (XINT *fd, XCHAR *buf, XINT *maxchars, XINT *status);
int ZNOTTY (XINT *fd, XLONG *offset);
int ZPUTTY (XINT *fd, XCHAR *buf, XINT *nchars, XINT *status);
int ZSEKTY (XINT *fd, XLONG *znotty_offset, XINT *status);
int ZSTTTY (XINT *fd, XINT *param, XLONG *value);
/* zfmkcp.c */
int ZFMKCP (PKCHAR *osfn, PKCHAR *new_osfn, XINT *status);
/* zfmkdr.c */
int ZFMKDR (PKCHAR *newdir, XINT *status);
/* zfnbrk.c */
int ZFNBRK (XCHAR *vfn, XINT *uroot_offset, XINT *uextn_offset);
/* zfpath.c */
int ZFPATH (XCHAR *osfn, XCHAR *pathname, XINT *maxch, XINT *nchars);
/* zfpoll.c */
int ZFPOLL (XINT *pfds, XINT *nfds, XINT *timeout, XINT *npoll, XINT *status);
/* zfprot.c */
int ZFPROT (PKCHAR *fname, XINT *action, XINT *status);
/* zfrnam.c */
int ZFRNAM (PKCHAR *oldname, PKCHAR *newname, XINT *status);
/* zfrmdr.c */
int ZFRMDR (PKCHAR *dir, XINT *status);
/* zfsubd.c */
int ZFSUBD (XCHAR *osdir, XINT *maxch, XCHAR *subdir, XINT *nchars);
/* zfutim.c */
int ZFUTIM (PKCHAR *fname, XLONG *atime, XLONG *mtime, XINT *status);
/* zfxdir.c */
int ZFXDIR (XCHAR *osfn, XCHAR *osdir, XINT *maxch, XINT *nchars);
/* zgcmdl.c */
int ZGCMDL (PKCHAR *cmd, XINT *maxch, XINT *status);
/* zghost.c */
int ZGHOST (PKCHAR *outstr, XINT *maxch);
/* zgmtco.c */
int ZGMTCO (XINT *gmtcor);
/* zgtenv.c */
int ZGTENV (PKCHAR *envvar, PKCHAR *outstr, XINT *maxch, XINT *status);
/* zgtime.c */
int ZGTIME (XLONG *clock_time, XLONG *cpu_time);
/* zgtpid.c */
int ZGTPID (XINT *pid);
/* zintpr.c */
int ZINTPR (XINT *pid, XINT *exception, XINT *status);
/* zlocpr.c */
int ZLOCPR (PFI	proc, XINT *o_epa);
/* zlocva.c */
int ZLOCVA (XCHAR *variable, XINT *location);
/* zmaloc.c */
int ZMALOC (XINT *buf, XINT *nbytes, XINT *status);
/* zmfree.c */
int ZMFREE (XINT *buf, XINT *status);
int ZFREE (void *buf);
/* zopdir.c */
int ZOPDIR (PKCHAR *fname, XINT *chan);
int ZCLDIR (XINT *chan, XINT *status);
int ZGFDIR (XINT *chan, PKCHAR *outstr, XINT *maxch, XINT *status);
/* zopdpr.c */
int ZOPDPR (PKCHAR *osfn, PKCHAR *bkgfile, PKCHAR *queue, XINT *jobcode);
int ZFODPR (void);
int ZCLDPR (XINT *jobcode, XINT *killflag, XINT *exit_status);
/* zoscmd.c */
int ZOSCMD (PKCHAR *oscmd, PKCHAR *stdin_file, PKCHAR *stdout_file, PKCHAR *stderr_file, XINT *status);
/* zpanic.c */
int ZPANIC (XINT *errcode, PKCHAR *errmsg);
/* zraloc.c */
int ZRALOC (XINT *buf, XINT *nbytes, XINT *status);
/* zsvjmp.S */
void ZSVJMP (XINT *jmpbuf, XINT *status);
/* zwmsec.c */
int ZWMSEC (XINT *msec);
/* zxwhen.c */
int ZXWHEN (XINT *sig_code, XINT *epa, XINT *old_epa);
int ZXGMES (XINT *os_exception, PKCHAR *errmsg, XINT *maxch);
/* zzepro.c */
int ZZEPRO (void);
/* zzexit.c */
int EXIT (XINT *code);
/* zzpstr.c */
int ZZPSTR (XCHAR *s1, XCHAR *s2);
int ZZLSTR (XCHAR *s1, XCHAR *s2);
/* zzstrt.c */
int ZZSTRT (void);
int ZZSTOP (void);
