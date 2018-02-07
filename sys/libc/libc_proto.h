/* atof.c */
extern double u_atof(char *str);
/* atoi.c */
extern int u_atoi(char *str);
/* atol.c */
extern long u_atol(char *str);
/* caccess.c */
extern int c_access(char *fname, int mode, int type);
/* callocate.c */
extern int c_allocate(char *device);
extern int c_deallocate(char *device, int u_rewind);
extern void c_devstatus(char *device, int out);
extern int c_devowner(char *device, char *owner, int maxch);
/* calloc.c */
extern char *u_calloc(unsigned int nelems, unsigned int elsize);
/* cclktime.c */
extern long c_clktime(long reftime);
extern long c_cputime(long reftime);
/* cclose.c */
extern int c_close(int fd);
/* ccnvdate.c */
extern char *c_cnvdate(long clktime, char *outstr, int maxch);
/* ccnvtime.c */
extern char *c_cnvtime(long clktime, char *outstr, int maxch);
/* cdelete.c */
extern int c_delete(char *fname);
/* cenvget.c */
extern char *u_envget(char *var);
extern int c_envgs(char *var, char *outstr, int maxch);
extern int c_envfind(char *var, char *outstr, int maxch);
extern int c_envgb(char *var);
extern int c_envgi(char *var);
extern void c_envputs(char *var, char *value);
extern void c_envreset(char *var, char *value);
/* cenvlist.c */
extern void c_envlist(int fd, char *prefix, int show_redefs);
/* cenvmark.c */
extern void c_envmark(int *envp);
extern int c_envfree(int envp, int userfcn);
extern int c_prenvfree(int pid, int envp);
/* cenvscan.c */
extern int c_envscan(char *input_source);
/* cerract.c */
extern void c_erract(int action);
/* cerrcode.c */
extern int c_errcode(void);
/* cerrget.c */
extern int c_errget(char *outstr, int maxch);
/* cerror.c */
extern void c_error(int errcode, char *errmsg);
/* cfchdir.c */
extern int c_fchdir(char *newdir);
/* cfilbuf.c */
extern int c_filbuf(struct _iobuf *fp);
/* cfinfo.c */
extern int c_finfo(char *fname, struct _finfo *fi);
/* cflsbuf.c */
extern int c_flsbuf(unsigned int ch, struct _iobuf *fp);
/* cflush.c */
extern void c_flush(int fd);
/* cfmapfn.c */
extern int c_fmapfn(char *vfn, char *osfn, int maxch);
/* cfmkdir.c */
extern int c_fmkdir(char *newdir);
/* cfnextn.c */
extern int c_fnextn(char *vfn, char *extn, int maxch);
/* cfnldir.c */
extern int c_fnldir(char *vfn, char *ldir, int maxch);
/* cfnroot.c */
extern int c_fnroot(char *vfn, char *root, int maxch);
/* cfpath.c */
extern int c_fpathname(char *vfn, char *osfn, int maxch);
/* cfredir.c */
extern int c_fredir(int fd, char *fname, int mode, int type);
/* cfseti.c */
extern void c_fseti(int fd, int param, int value);
/* cfstati.c */
extern int c_fstati(int fd, int param);
/* cgetpid.c */
extern int c_getpid(void);
/* cgetuid.c */
extern char *c_getuid(char *outstr, int maxch);
/* cgflush.c */
extern void c_gflush(int stream);
/* cimaccess.c */
extern int c_imaccess(char *imname, int mode);
/* cimdrcur.c */
extern int c_imdrcur(char *device, float *x, float *y, int *wcs, int *key, char *strval, int maxch, int d_wcs, int pause);
/* ckimapc.c */
extern int c_kimapchan(int chan, char *nodename, int maxch);
/* clexnum.c */
extern int c_lexnum(char *str, int *toklen);
/* cmktemp.c */
extern int c_mktemp(char *root, char *temp_filename, int maxch);
/* cndopen.c */
extern int c_ndopen(char *fname, int mode);
/* cnote.c */
extern long c_note(int fd);
/* copen.c */
extern int c_open(char *fname, int mode, int type);
/* coscmd.c */
extern int c_oscmd(char *cmd, char *infile, char *outfile, char *errfile);
/* cpoll.c */
extern int c_poll_open(void);
extern int c_poll(int fds, int nfds, int timeout);
extern void c_poll_close(int fds);
extern void c_poll_zero(int fds);
extern void c_poll_set(int fds, int fd, int type);
extern void c_poll_clear(int fds, int fd, int type);
extern int c_poll_test(int fds, int fd, int type);
extern int c_poll_get_nfds(int fds);
extern void c_poll_print(int fds);
/* cprcon.c */
extern unsigned int c_propen(char *process, int *in, int *out);
extern int c_prclose(unsigned int pid);
extern int c_prstati(int pid, int param);
extern int c_prsignal(unsigned pid, int signal);
extern int c_prredir(unsigned pid, int stream, int new_fd);
extern int c_prchdir(int pid, char *newdir);
extern int c_prenvset(int pid, char *envvar, char *value);
/* cprdet.c */
extern unsigned int c_propdpr(char *process, char *bkgfile, char *bkgmsg);
extern unsigned int c_prfodpr(void);
extern int c_prcldpr(unsigned job);
extern int c_prdone(unsigned job);
extern int c_prkill(unsigned job);
/* cprintf.c */
extern int c_printf(char *format);
extern int c_fprintf(int fd, char *format);
extern void c_pargb(int ival);
extern void c_pargc(int ival);
extern void c_pargs(short sval);
extern void c_pargi(int ival);
extern void c_pargl(long lval);
extern void c_pargr(float rval);
extern void c_pargd(double dval);
extern void c_pargstr(char *strval);
/* crcursor.c */
extern int c_rcursor(int fd, char *outstr, int maxch);
/* crdukey.c */
extern int c_rdukey(char *obuf, int maxch);
/* cread.c */
extern int c_read(int fd, char *buf, int maxbytes);
/* crename.c */
extern int c_rename(char *old_fname, char *new_fname);
/* creopen.c */
extern int c_reopen(int fd, int mode);
/* csalloc.c */
extern char *c_salloc(unsigned nbytes);
extern void c_smark(int *sp);
extern void c_sfree(int sp);
/* cseek.c */
extern int c_seek(int fd, long offset);
/* csppstr.c */
extern short *c_sppstr(char *str);
/* cstropen.c */
extern int c_stropen(short *obuf, int maxch, int mode);
/* cstrpak.c */
extern char *c_strpak(short *sppstr, char *cstr, int maxch);
/* cstrupk.c */
extern short *c_strupk(char *str, short *outstr, int maxch);
/* ctsleep.c */
extern void c_tsleep(int nseconds);
/* cttset.c */
extern void c_sttyco(char *args, int ttin, int ttout, int outfd);
extern void c_ttseti(int fd, int param, int value);
extern int c_ttstati(int fd, int param);
extern void c_ttsets(int fd, int param, char *value);
extern int c_ttstats(int fd, int param, char *outstr, int maxch);
/* cttycdes.c */
extern void c_ttycdes(XINT tty);
/* cttyclear.c */
extern void c_ttycr(int fd, XINT tty);
/* cttyclln.c */
extern void c_ttycn(int fd, XINT tty);
/* cttyctrl.c */
extern int c_ttyctrl(int fd, XINT tty, char *cap, int afflncnt);
/* cttygetb.c */
extern int c_ttygb(XINT tty, char *cap);
/* cttygeti.c */
extern XINT c_ttygi(XINT tty, char *cap);
/* cttygetr.c */
extern float c_ttygr(XINT tty, char *cap);
/* cttygets.c */
extern int c_ttygs(XINT tty, char *cap, char *outstr, int maxch);
/* cttygoto.c */
extern void c_ttygoto(int fd, XINT tty, int col, int line);
/* cttyinit.c */
extern void c_ttyinit(int fd, XINT tty);
/* cttyodes.c */
extern XINT c_ttyodes(char *ttyname);
/* cttyputl.c */
extern void c_ttype(int fd, XINT tty, char *line, int map_cc);
/* cttyputs.c */
extern int c_ttyps(int fd, XINT tty, char *cap, int afflncnt);
/* cttyseti.c */
extern void c_ttyseti(XINT tty, int param, int value);
/* cttyso.c */
extern void c_ttyso(int fd, XINT tty, int onoff);
/* cttystati.c */
extern XINT c_ttystati(XINT tty, int param);
/* ctype.c */
/* cungetc.c */
extern int c_ungec(int fd, int ch);
/* cungetl.c */
extern int c_ungetline(int fd, char *str);
/* cvfnbrk.c */
extern void c_vfnbrk(char *vfn, int *root, int *extn);
/* cwmsec.c */
extern void c_wmsec(int msec);
/* cwrite.c */
extern int c_write(int fd, char *buf, int nbytes);
/* cxgmes.c */
extern void c_xgmes(int *oscode, char *oserrmsg, int maxch);
/* cxonerr.c */
extern void c_xonerr(int errcode);
/* cxttysize.c */
extern void c_xttysize(int *ncols, int *nlines);
/* cxwhen.c
extern void c_xwhen(int exception, PFI new_handler, PFI *old_handler);
 */
/* eprintf.c */
extern void u_eprintf(char *format, ...);
/* fclose.c */
extern int u_fclose(struct _iobuf *fp);
/* fdopen.c */
extern struct _iobuf *u_fdopen(int fd, char *mode);
/* fflush.c */
extern int u_fflush(struct _iobuf *fp);
/* fgetc.c */
extern int u_fgetc(struct _iobuf *fp);
/* fgets.c */
extern char *u_fgets(char *buf, int maxch, struct _iobuf *fp);
/* fopen.c */
extern struct _iobuf *u_fopen(char *fname, char *modestr);
/* fputc.c */
extern int u_fputc(char ch, struct _iobuf *fp);
/* fputs.c */
extern void u_fputs(char *str, struct _iobuf *fp);
/* fread.c */
extern int u_fread(char *bp, int szelem, int nelem, struct _iobuf *fp);
/* free.c */
extern void u_free(char *buf);
/* freopen.c */
extern struct _iobuf *u_freopen(char *fname, char *modestr, struct _iobuf *fp);
/* fseek.c */
extern int u_fseek(struct _iobuf *fp, long offset, int mode);
/* ftell.c */
extern long u_ftell(struct _iobuf *fp);
/* fwrite.c */
extern int u_fwrite(char *bp, int szelem, int nelem, struct _iobuf *fp);
/* gets.c */
extern char *u_gets(char *buf);
/* getw.c */
extern int u_getw(struct _iobuf *fp);
/* index.c */
extern char *u_index(char *str, int ch);
/* isatty.c */
extern int u_isatty(int fd);
/* malloc.c */
extern char *u_malloc(unsigned nbytes);
/* mktemp.c */
extern char *u_mktemp(char *template);
/* perror.c */
extern void u_perror(char *prefix);
/* printf.c */
extern void u_printf(char *format, ...);
extern void u_fprintf(struct _iobuf *fp, char *format, ...);
/*
extern void u_doprnt(char *format, va_list *argp, struct _iobuf *fp);
extern void u_doarg(struct _iobuf *fp, short *formspec, va_list **argp, int prec[], int varprec, int dtype);
*/
/* puts.c */
extern int u_puts(char *str);
/* putw.c */
extern int u_putw(int word, struct _iobuf *fp);
/* qsort.c */
extern void u_qsort(char *base, int n, int size, int (*compar)(void));
/* realloc.c */
extern char *u_realloc(char *buf, unsigned newsize);
/* rewind.c */
extern long u_rewind(struct _iobuf *fp);
/* rindex.c */
extern char *u_rindex(char *str, int ch);
/* scanf.c */
extern int u_scanf(char *format, ...);
extern int u_fscanf(struct _iobuf *fp, char *format, ...);
extern int u_sscanf(char *str, char *format, ...);
/* setbuf.c */
extern void u_setbuf(struct _iobuf *fp, char *buf);
extern void u_setfbf(struct _iobuf *fp, char *buf, int size);
extern void u_setlinebuf(struct _iobuf *fp);
/* spf.c */
extern int spf_open(char *buf, int maxch);
extern void spf_close(int fd);
/* sprintf.c */
extern char *u_sprintf(char *str, char *format, ...);
/* stgio.c */
extern int c_stggetline(int fd, char *buf, int maxch);
extern int c_stgputline(int fd, char *buf);
/* strcat.c */
extern char *u_strcat(char *s1, char *s2);
/* strcmp.c */
extern int u_strcmp(char *s1, char *s2);
/* strdup.c */
extern char *u_strdup(char *str);
/* strcpy.c */
extern char *u_strcpy(char *s1, char *s2);
/* strlen.c */
extern int u_strlen(char *s);
/* strncat.c */
extern char *u_strnt(char *s1, char *s2, int n);
/* strncmp.c */
extern int u_strnp(char *s1, char *s2, int n);
/* strncpy.c */
extern char *u_strny(char *s1, char *s2, int n);
/* system.c */
extern int u_system(char *cmd);
/* ungetc.c */
extern int u_ungetc(int ch, struct _iobuf *fp);
/* zztest.c */
extern int thello_(void);
extern int tprint_(void);
extern int tcopy_(void);
extern int tscan_(void);
extern int onint(int *code, int *old_handler);
extern int tgettk_(void);
