/*
 *  VOSPROTO.H -- C callable prototypes of the SPP library procedures
 */
#ifndef D_iraf_vosproto_h
#define D_iraf_vosproto_h

#include "iraf_spp.h"
#include "iraf_xnames.h"

/*
 * MEMIO -- Memory allocation and management facilities.
 */
int REALLOC (XPOINTER *ubufp, XINT *nelems, XINT *dtype);
int CALLOC (XPOINTER *ubufp, XINT *buflen, XINT *dtype);
XPOINTER COERCE (XPOINTER *ptr, XINT *type1, XINT *type2);
int MFREE (XPOINTER *ptr, XINT *dtype);
XINT IRAF_MAIN (XCHAR *a_cmd, XINT *a_inchan, XINT *a_outchan, XINT *a_errchan, XINT *a_driver, XINT *a_devtype, XINT *prtype, XCHAR *bkgfile, XINT *jobcode, int (*sys_runtask)(XCHAR *task, XCHAR *cmd, XINT *ruk_argoff, XINT *ruk_interact), XINT (*onentry)(XINT *prtype, XCHAR *bkgfile, XCHAR *cmd));
int MALLOC (XPOINTER *ubufp, XINT *nelems, XINT *dtype);
int SALLOC (XPOINTER *output_pointer, XINT *nelem, XINT *datatype);
int SMARK (XPOINTER *old_sp);
int SFREE (XPOINTER *old_sp);
XINT SIZEOF (XINT *dtype);
int VMALLOC (XPOINTER *ubufp, XINT *nelems, XINT *dtype);
XINT BEGMEM (XINT *best_size, XINT *old_size, XINT *max_size);
int FIXMEM (XINT *old_size);
/*
 * OSB -- Bit and byte primitives.
 */
XREAL URAND (XLONG *lseed);
/*
 * KI -- Kernel Interface Package
 */
XINT KI_MAPCHAN (XINT *chan, XCHAR *nodename, XINT *maxch);
XINT KI_EXTNODE (XCHAR *resource, XCHAR *nodename, XINT *maxch, XINT *nchars);
XINT ONENTRY (XINT *prtype, XCHAR *bkgfile, XCHAR *cmd);
/*
 * TTY -- Terminal Control Interface
 */
XREAL TTYGETR (XPOINTER *tty, XCHAR *cap);
int TTYGOTO (XINT *fd, XPOINTER *tty, XINT *col, XINT *line);
int TTYPUTS (XINT *fd, XPOINTER *tty, XCHAR *ctrlstr, XINT *afflncnt);
int TTYSETI (XPOINTER *tty, XINT *parameter, XINT *value);
int TTYCDES (XPOINTER *tty);
int TTYSO (XINT *fd, XPOINTER *tty, XINT *onflag);
int TTYCLEARLN (XINT *fd, XPOINTER *tty);
XPOINTER TTYODES (XCHAR *ttyname);
XINT TTYSTATI (XPOINTER *tty, XINT *parameter);
XPOINTER TTYOPEN (XCHAR *termcap_file, XCHAR *device, XINT *ttyload);
XINT TTYGETI (XPOINTER *tty, XCHAR *cap);
XINT TTYGETS (XPOINTER *tty, XCHAR *cap, XCHAR *outstr, XINT *maxch);
int TTYINIT (XINT *fd, XPOINTER *tty);
int TTYPUTLINE (XINT *fd, XPOINTER *tty, XCHAR *text, XINT *map_cc);
int TTYCLEAR (XINT *fd, XPOINTER *tty);
XBOOL TTYGETB (XPOINTER *tty, XCHAR *cap);
int TTYCLOSE (XPOINTER *tty);
XINT TTYCTRL (XINT *fd, XPOINTER *tty, XCHAR *cap, XINT *afflncnt);
XPOINTER TTYGDES (XCHAR *ttyname);
/*
 * CLIO -- Command Language I/O.
 */
XINT RDUKEY (XCHAR *keystr, XINT *maxch);
/*
 * IMIO -- Image I/O Routines
 */
XINT IMACCESS (XCHAR *image, XINT *acmode);
/*
 * FMTIO -- Formatted I/O
 */
XINT XTOC (XCOMPLEX *xval, XCHAR *outstr, XINT *maxch, XINT *decpl, XINT *fmt, XINT *width);
XINT GLTOC (XLONG *lval, XCHAR *outstr, XINT *maxch, XINT *base);
int PARGX (XCOMPLEX *xval);
XINT GPATMATCH (XCHAR *str, XCHAR *pat, XINT *first_char, XINT *last_char);
XINT GPATMAKE (XCHAR *patstr, XINT *from, XINT *delim, XCHAR *patbuf, XINT *sz_pat);
int PARGSTR (XCHAR *str);
XINT CTOD (XCHAR *str, XINT *ip, XDOUBLE *dval);
XINT STRMATCH (XCHAR *str, XCHAR *pat);
XINT GSTRMATCH (XCHAR *str, XCHAR *pat, XINT *first_char, XINT *last_char);
int PRINTF (XCHAR *format_string);
int PARGD (XDOUBLE *dval);
int PARGC (XCHAR *cval);
int PARGS (XSHORT *sval);
int PARGI (XINT *ival);
int PARGL (XLONG *lval);
int PARGR (XREAL *rval);
XINT GCTOL (XCHAR *str, XINT *ip, XLONG *lval, XINT *radix);
int SPRINTF (XCHAR *outstr, XINT *maxch, XCHAR *format_string);
XINT LEXNUM (XCHAR *str, XINT *ip_start, XINT *nchars);
XINT DTOC (XDOUBLE *dval, XCHAR *outstr, XINT *maxch, XINT *decpl, XINT *a_fmt, XINT *width);
XINT GCTOD (XCHAR *str, XINT *ip, XDOUBLE *odval);
int FPRINTF (XINT *fd, XCHAR *format_string);
XINT GCTOX (XCHAR *str, XINT *ip, XCOMPLEX *oxval);
int PARGB (XBOOL *bval);
int STRTBL (XINT *fd, XCHAR *buf, XINT *strp, XINT *nstr, XINT *first_col, XINT *last_col, XINT *maxch, XINT *ncol);
XINT CTOX (XCHAR *str, XINT *ip, XCOMPLEX *xval);
/*
 * ETC -- Miscellaneous System Stuff
 */
int ONEXIT (int (*user_proc)(XINT *exit_code));
int PRKILL (XINT *job);
int CNVDATE (XLONG *ltime, XCHAR *outstr, XINT *maxch);
XINT PRCLDPR (XINT *job);
XINT ENVGETS (XCHAR *key, XCHAR *value, XINT *maxch);
int GETUID (XCHAR *user_name, XINT *maxch);
XLONG CLKTIME (XLONG *old_time);
XINT ERRCODE (void);
XINT LPOPEN (XCHAR *device, XINT *mode, XINT *type);
XBOOL ENVGETB (XCHAR *varname);
int BRKTIME (XLONG *ltime, XINT *tm);
int PRCHDIR (XINT *pid, XCHAR *newdir);
XLONG CPUTIME (XLONG *old_cputime);
XINT ENVGETI (XCHAR *varname);
int XERPSH (void);
XBOOL XERPOP (void);
XINT XERPOPI (void);
int XWHEN (XINT *signal, XINT *handler, XINT *old_handler);
XINT XISATTY (XINT *fd);
XINT PRFILBUF (XINT *fd);
int PRENVSET (XINT *pid, XCHAR *envvar, XCHAR *valuestr);
XBOOL ITOB (XINT *integer_value);
int ONERROR (int (*user_proc)(XINT *status));
int XONERR (XINT *status);
XINT GETPID (void);
int CNVTIME (XLONG *ltime, XCHAR *outstr, XINT *maxch);
XINT PRCLOSE (XINT *pid);
int ERROR (XINT *error_code, XCHAR *message);
int FATAL (XINT *error_code, XCHAR *message);
XINT PRCLCPR (XINT *pid);
int XMJBUF (XPOINTER *bp);
XINT PRFODPR (void);
int PRUPDATE (XINT *pid, XCHAR *message, XINT *flushout);
XINT ONENTRY (XINT *prtype, XCHAR *bkgfile, XCHAR *cmd);
XINT PRENVFREE (XINT *pid, XINT *marker);
int ENVLIST (XINT *fd, XCHAR *prefix, XINT *print_redefined_variables);
int ENVRESET (XCHAR *key, XCHAR *value);
XINT PRSTATI (XINT *pid, XINT *param);
int PRSIGNAL (XINT *pid, XINT *signal);
XINT IRAF_MAIN (XCHAR *a_cmd, XINT *a_inchan, XINT *a_outchan, XINT *a_errchan, XINT *a_driver, XINT *a_devtype, XINT *prtype, XCHAR *bkgfile, XINT *jobcode, int (*sys_runtask)(XCHAR *task, XCHAR *cmd, XINT *ruk_argoff, XINT *ruk_interact), XINT (*onentry)(XINT *prtype, XCHAR *bkgfile, XCHAR *cmd));
XINT BTOI (XBOOL *boolean_value);
int STTYCO (XCHAR *args, XINT *ttin, XINT *ttout, XINT *outfd);
XINT OSCMD (XCHAR *cmd, XCHAR *infile, XCHAR *outfile, XCHAR *errfile);
int TSLEEP (XINT *seconds);
XINT XALLOCATE (XCHAR *device);
XINT XDEALLOCATE (XCHAR *device, XINT *rewind);
int XDEVSTATUS (XCHAR *device, XINT *out);
XINT XDEVOWNER (XCHAR *device, XCHAR *owner, XINT *maxch);
XINT ENVSCAN (XCHAR *cmd);
XINT PRDONE (XINT *job);
XINT ERRGET (XCHAR *outstr, XINT *maxch);
int XER_RESET (void);
int ENVINIT(void);
XINT ENVFIND (XCHAR *key, XCHAR *value, XINT *maxch);
XINT ENVPUTS (XCHAR *key, XCHAR *value);
int ENVMARK (XINT *old_top);
XINT ENVFREE (XINT *old_top, XINT *userfcn);
int PRREDIR (XINT *pid, XINT *stream, XINT *new_fd);
int ERRACT (XINT *severity);
XINT PROPCPR (XCHAR *process, XINT *in, XINT *out);
int QSORT (XINT *x, XINT *nelem, XINT *compare);
XINT PROPDPR (XCHAR *process, XCHAR *bkgfile, XCHAR *bkgmsg);
int XTTYSIZE (XINT *width, XINT *height);
XINT PROPEN (XCHAR *process, XINT *in, XINT *out);
int TTSETI (XINT *fd, XINT *param, XINT *value);
XINT TTSTATI (XINT *fd, XINT *param);
int TTSETS (XINT *fd, XINT *param, XCHAR *svalue);
XINT TTSTATS (XINT *fd, XINT *param, XCHAR *outstr, XINT *maxch);
/*
 * GIO -- Graphics I/O
 */
XBOOL FP_EQUALD (XDOUBLE *x, XDOUBLE *y);
int GTR_GFLUSH (XINT *stream);
XINT RCURSOR (XINT *stream, XCHAR *outstr, XINT *maxch);
int PRPSINIT (void);
XINT STG_GETLINE (XINT *fd, XCHAR *obuf);
int STG_PUTLINE (XINT *fd, XCHAR *text);
XINT IMDRCUR(XCHAR *device, XREAL *x, XREAL *y, XINT *wcs, XINT *key, XCHAR *strval, XINT *maxch, XINT *in_wcs, XINT *pause);
/*
 * FIO -- File I/O
 */
XINT AWAITB (XINT *fd);
int PUTCC (XINT *fd, XCHAR *ch);
XPOINTER POLL_OPEN (void);
XINT POLL (XPOINTER *fds, XINT *nfds, XINT *timeout);
int POLL_CLOSE (XPOINTER *fds);
int POLL_ZERO (XPOINTER *fds);
int POLL_SET (XPOINTER *fds, XINT *fd, XINT *type);
XINT POLL_GET_NFDS (XPOINTER *fds);
int POLL_CLEAR (XPOINTER *fds, XINT *fd, XINT *type);
XINT POLL_TEST (XPOINTER *fds, XINT *fd, XINT *type);
int POLL_PRINT (XPOINTER *fds);
XPOINTER VFNOPEN (XCHAR *vfn, XINT *mode);
XINT VFNMAP (XPOINTER *vfd, XCHAR *osfn, XINT *maxch);
XINT VFNADD (XPOINTER *vfd, XCHAR *osfn, XINT *maxch);
XINT VFNDEL (XPOINTER *vfd, XCHAR *osfn, XINT *maxch);
XINT VFNUNMAP (XPOINTER *vfd, XCHAR *osfn, XCHAR *vfn, XINT *maxch);
int VFNCLOSE (XPOINTER *vfd, XINT *update_enable);
XINT FDEVBLK (XCHAR *path);
int FALLOC (XCHAR *fname, XLONG *file_size);
XINT FNTGFN (XPOINTER *pp, XCHAR *outstr, XINT *maxch);
XPOINTER FNTOPN (XCHAR *template);
int FNTCLS (XPOINTER *pp);
int MKTEMP (XCHAR *seed, XCHAR *temp_file, XINT *maxchars);
int PUTC (XINT *fd, XCHAR *ch);
int PUTCHAR (XCHAR *ch);
int FOWNER (XCHAR *fname, XCHAR *owner, XINT *maxch);
int FLSBUF (XINT *fd, XINT *nreserve);
int FCHDIR (XCHAR *newdir);
XLONG NOTE (XINT *fd);
int FSTATS (XINT *fd, XINT *what, XCHAR *outstr, XINT *maxch);
XINT FNROOT (XCHAR *vfn, XCHAR *outstr, XINT *maxch);
XINT FNEXTN (XCHAR *vfn, XCHAR *outstr, XINT *maxch);
XCHAR GETC (XINT *fd, XCHAR *ch);
XINT FSTATI (XINT *fd, XINT *what);
int FLUSH (XINT *fd);
XINT AWAIT (XINT *fd);
XINT FINFO (XCHAR *fname, XLONG *ostruct);
XINT OPEN (XCHAR *fname, XINT *mode, XINT *type);
int FSETI (XINT *fd, XINT *param, XINT *value);
int FPATHNAME (XCHAR *vfn, XCHAR *output_pathname, XINT *maxchars);
int UNGETLINE (XINT *fd, XCHAR *str);
XINT PROTECT (XCHAR *fname, XINT *action);
XINT ACCESS (XCHAR *fname, XINT *mode, XINT *type);
int DELETE (XCHAR *fname);
int AWRITE (XINT *fd, XCHAR *buffer, XINT *nchars, XLONG *char_offset);
XINT STROPEN (XCHAR *str, XINT *maxch, XINT *mode);
int UNREAD (XINT *fd, XCHAR *buf, XINT *nchars);
int SEEK (XINT *fd, XLONG *offset);
int FREDIR (XINT *fd, XCHAR *fname, XINT *mode, XINT *type);
int FREDIRO (XINT *fd, XINT *newfd);
int AREADB (XINT *fd, XCHAR *buffer, XINT *maxbytes, XLONG *byte_offset);
int CLOSE (XINT *fd_arg);
int AREAD (XINT *fd, XCHAR *buffer, XINT *maxchars, XLONG *char_offset);
XINT READ (XINT *fd, XCHAR *buffer, XINT *maxchars);
XINT NDOPEN (XCHAR *fname, XINT *mode);
int WRITE (XINT *fd, XCHAR *buffer, XINT *maxchars);
int FMKDIR (XCHAR *newdir);
XINT FNLDIR (XCHAR *vfn, XCHAR *outstr, XINT *maxch);
int UNGETC (XINT *fd, XCHAR *ch);
int FDEBUG (XINT *out, XINT *fd1_arg, XINT *fd2_arg);
int AWRITEB (XINT *fd, XCHAR *buffer, XINT *nbytes, XLONG *byte_offset);
XINT REOPEN (XINT *fd, XINT *mode);
XLONG FSTATL (XINT *fd, XINT *what);
int PUTLINE (XINT *fd, XCHAR *linebuf);
int FMAPFN (XCHAR *vfn, XCHAR *osfn, XINT *maxch);
XINT DIROPEN (XCHAR *fname, XINT *mode);
XINT FILBUF (XINT *fd);
int RENAME (XCHAR *oldname, XCHAR *newname);
int FDIRNAME (XCHAR *vfn, XCHAR *path, XINT *maxch);
XCHAR GETCHAR (XCHAR *ch);
XINT GETLINE (XINT *fd, XCHAR *linebuf);
int FCOPY (XCHAR *oldfile, XCHAR *newfile);
int FCOPYO (XINT *in, XINT *out);
/*
 * LIBC mathf.f -- C callable FORTRAN math functions.
 */
XINT XNINT(XDOUBLE *x);
XDOUBLE XEXP(XDOUBLE *x);
XDOUBLE XLOG(XDOUBLE *x);
XDOUBLE XLOG10(XDOUBLE *x);
XDOUBLE XPOW(XDOUBLE *x, XDOUBLE *y);
XDOUBLE XSQRT(XDOUBLE *x);
XDOUBLE XSIN(XDOUBLE *x);
XDOUBLE XCOS(XDOUBLE *x);
XDOUBLE XTAN(XDOUBLE *x);
XDOUBLE XASIN(XDOUBLE *x);
XDOUBLE XACOS(XDOUBLE *x);
XDOUBLE XATAN(XDOUBLE *x);
XDOUBLE XATAN2(XDOUBLE *x, XDOUBLE *y);
/*
 * lib/sysruk.x -- Run one of the tasks in a process.
 */
int SYSRUK(XCHAR *task, XCHAR *cmd, XINT *ruk_argoff, XINT *ruk_interact);

#endif /* D_iraf_vosproto_h */
