/*
 * LIBC.H -- Definitions which should be included by all C source files which
 * use the IRAF runtime C library.
 */

#ifndef D_libc
#ifndef D_spp
#ifndef import_spp
#include "spp.h"
#endif
#endif

#define	XCHAR		short
#if (__SIZEOF_LONG__ == 8 && __SIZEOF_POINTER__ == 8) /* LP64 */
#define	XINT		long
#define	XLONG		long
#elif (__SIZEOF_INT__ == 4 && __SIZEOF_POINTER__ == 4) /* ILP32 */
#define	XINT		int
#define	XLONG		int
#else
#error "No data model: need either LP64 or ILP32"
#endif

#define	SZ_DEFIOBUF	1024
#define	FIO_MAXFD	4096

#define	FIOCOM		fiocom_		/* [MACHDEP] */
#define	MEMCOM		mem_
#define	XERPSH		xerpsh_
#define	XERPOP		xerpop_
#define	XERPOPI		xerpoi_
#define	c_main		cmain_

/* Error handling.
 */
#define	iferr(stmt)	{XERPSH();stmt;}if(XERPOPI())

/* SPP/C pointer conversions.
 */
extern	char		MEMCOM[];
#define	Memc		(((XCHAR *)MEMCOM)-1)
#define	Memi		(((XINT *)MEMCOM)-1)
#define	Memcptr(addr)	((XCHAR *)(addr) - Memc)
#define	Memiptr(addr)	((XINT *)(addr) - Memi)

/* External names.
 */
#ifndef NOLIBCNAMES

#define	getenv		envget
#define	sys_nerr	u_sysnerr
#define	sys_errlist	u_syserrlist

#define	atof		u_atof
#define	atoi		u_atoi
#define	atol		u_atol
#define	calloc		u_calloc
#define	envget		u_envget
#define	eprintf		u_eprintf
#define	fclose		u_fclose
#define	fdopen		u_fdopen
#define	fflush		u_fflush
#define	fgetc		u_fgetc
#define	fgets		u_fgets
#define	fopen		u_fopen
#define	fprintf		u_fprintf
#define	fputc		u_fputc
#define	fputs		u_fputs
#define	fread		u_fread
#define	freadline	u_readline
#define	free		u_free
#define	freopen		u_freopen
#define	fscanf		u_fscanf
#define	fseek		u_fseek
#define	ftell		u_ftell
#define	fwrite		u_fwrite
#define	gets		u_gets
#define	getw		u_getw
#define	index		u_index
#define	isatty		u_isatty
#define	malloc		u_malloc
#define	mktemp		u_mktemp
#define	perror		u_perror
#define	printf		u_printf
#define	puts		u_puts
#define	putw		u_putw
#define	qsort		u_qsort
#define	realloc		u_realloc
#define	rewind		u_rewind
#define	rindex		u_rindex
#define	scanf		u_scanf
#define	setbuf		u_setbuf
#define	setbuffer	u_setfbf		/* collision	*/
#define	setlinebuf	u_setlinebuf
#define	sprintf		u_sprintf
#define	sscanf		u_sscanf
#define	strcat		u_strcat
#define	strchr		u_index
#define	strcmp		u_strcmp
#define	strdup		u_strdup
#define	strcpy		u_strcpy
#define	strlen		u_strlen
#define	strncat		u_strnt			/* collision	*/
#define	strncmp		u_strnp			/* collision	*/
#define	strncpy		u_strny			/* collision	*/
#define	strrchr		u_rindex
#define	system		u_system
#define	ungetc		u_ungetc

/* C_SPP names not unique in the first seven characters.
 */
#define	c_envgetb	c_envgb
#define	c_envgeti	c_envgi
#define	c_envgets	c_envgs
#define	c_ttyclear	c_ttycr
#define	c_ttyclearln	c_ttycn
#define	c_ttygetb	c_ttygb
#define	c_ttygeti	c_ttygi
#define	c_ttygetr	c_ttygr
#define	c_ttygets	c_ttygs
#define	c_ttyputline	c_ttype
#define	c_ttyputs	c_ttyps
#define c_ungetc	c_ungec
#define c_ungetstr	c_unges


/*
 * Prototype definitions for the IRAF runtime C library.
 */

/*extern long      XERPSH(), XERPOPI();*/	/* standard for iferr use */

extern struct _iobuf  *fdopen (XINT fd, char *mode);
extern struct _iobuf  *fopen (char *fname, char *modestr);
extern struct _iobuf  *freopen (char *fname, char *modestr, struct _iobuf *fp);
extern char    *c_cnvdate (long clktime, char *outstr, int maxch);
extern char    *c_cnvtime (long clktime, char *outstr, int maxch);
extern char    *c_getuid (char *outstr, int maxch);
extern char    *c_salloc (unsigned nbytes);
extern char    *c_strpak (short *sppstr, char *cstr, int maxch);
extern char    *calloc (unsigned int nelems, unsigned int elsize);
extern char    *envget (char *var);
extern char    *fgets (char *buf, int maxch, struct _iobuf *fp);
extern char    *gets (char *buf);
extern char    *index (char *str, int ch);
extern char    *malloc (unsigned nbytes);
extern char    *mktemp (char *template);
extern char    *freadline (char *prompt);
extern char    *realloc (char *buf, unsigned newsize);
extern char    *rindex (char *str, int ch);
extern char    *sprintf (char *str, char *format, ...);
extern char    *strcat (char *s1, char *s2);
extern char    *strdup (char *str);
extern char    *strcpy (char *s1, char *s2);
extern char    *strncat (char *s1, char *s2, int n);
extern char    *strncpy (char *s1, char *s2, int n);
extern int	strncmp (char *s1, char *s2, int n);

extern double   atof (char *str);
extern float    c_ttygr (XINT tty, char *cap);
extern int	atoi (char *str);
extern int	c_access (char *fname, int mode, int type);
extern int	c_allocate (char *device);
extern int	c_close (XINT fd);
extern int	c_deallocate (char *device, int rewind);
extern int	c_delete (char *fname);
extern int	c_devowner (char *device, char *owner, int maxch);
extern int	c_envfind (char *var, char *outstr, int maxch);
extern int	c_envfree (int envp, int userfcn);
extern int	c_envgb (char *var);
extern int	c_envgi (char *var);
extern int	c_envgs (char *var, char *outstr, int maxch);
extern int	c_envscan (char *input_source);
extern int	c_errcode (void);
extern int	c_errget (char *outstr, int maxch);
extern int	c_fchdir (char *newdir);
extern int	c_filbuf (struct _iobuf *fp);
extern int	c_flsbuf (unsigned int ch, struct _iobuf *fp);
extern int	c_fmapfn (char *vfn, char *osfn, int maxch);
extern int	c_fmkdir (char *newdir);
extern int	c_fnextn (char *vfn, char *extn, int maxch);
extern int	c_fnldir (char *vfn, char *ldir, int maxch);
extern int	c_fnroot (char *vfn, char *root, int maxch);
extern int	c_fpathname (char *vfn, char *osfn, int maxch);
extern int	c_fprintf (XINT fd, char *format);
extern int	c_fredir (XINT fd, char *fname, int mode, int type);
extern int	c_frediro (XINT fd, XINT newfd);
extern int	c_fstati (XINT fd, int param);
extern int	c_getpid (void);
extern int	c_imaccess (char *imname, int mode);
extern int	c_imdrcur (char *device, float *x, float *y, int *wcs, 
		     int *key, char *strval, int maxch, int d_wcs, int pause);
extern int	c_kimapchan (int chan, char *nodename, int maxch);
extern int	c_lexnum (char *str, int *toklen);
extern int	c_mktemp (char *root, char *temp_filename, int maxch);
extern int	c_ndopen (char *fname, int mode);
extern int	c_open (char *fname, int mode, int type);
extern int	c_oscmd (char *cmd, char *infile, char *outfile, char *errfile);
extern int	c_poll (XINT fds, int nfds, int timeout);
extern int	c_poll_get_nfds (XINT fds);
extern XINT	c_poll_open (void);
extern int	c_poll_test (XINT fds, XINT fd, int type);
extern int	c_prchdir (int pid, char *newdir);
extern int	c_prcldpr (unsigned job);
extern int	c_prclose (unsigned int pid);
extern int	c_prdone (unsigned job);
extern int	c_prenvfree (int pid, int envp);
extern int	c_prenvset (int pid, char *envvar, char *value);
extern int	c_printf (char *format);
extern int	c_prkill (unsigned job);
extern int	c_prredir (unsigned pid, int stream, int new_fd);
extern int	c_prsignal (unsigned pid, int signal);
extern int	c_prstati (int pid, int param);
extern int	c_rcursor (int fd, char *outstr, int maxch);
extern int	c_rdukey (char *obuf, int maxch);
extern int	c_read (XINT fd, char *buf, int maxbytes);
extern int	c_rename (char *old_fname, char *new_fname);
extern int	c_reopen (XINT fd, int mode);
extern int	c_seek (XINT fd, long offset);
extern int	c_stggetline (XINT fd, char *buf, int maxch);
extern int	c_stgputline (XINT fd, char *buf);
extern int	c_stropen (short *obuf, int maxch, int mode);
extern int	c_ttstati (XINT fd, int param);
extern int	c_ttstats (XINT fd, int param, char *outstr, int maxch);
extern int	c_ttyctrl (XINT fd, XINT tty, char *cap, int afflncnt);
extern int	c_ttygb (XINT tty, char *cap);
extern XINT	c_ttygi (XINT tty, char *cap);
extern int	c_ttygs (XINT tty, char *cap, char *outstr, int maxch);
extern XINT	c_ttyodes (char *ttyname);
extern int	c_ttyps (XINT fd, XINT tty, char *cap, int afflncnt);
extern XINT	c_ttystati (XINT tty, int param);
extern int	c_ungec (XINT fd, int ch);
extern int	c_ungetline (XINT fd, char *str);
extern int	c_write (XINT fd, char *buf, int nbytes);
extern int	fclose (struct _iobuf *fp);
extern int	fflush (struct _iobuf *fp);
extern int	fgetc (struct _iobuf *fp);
extern int	fputc (char ch, struct _iobuf *fp);
extern int	fread (char *bp, int szelem, int nelem, struct _iobuf *fp);
extern int	fscanf (struct _iobuf *fp, char *format, ...);
extern int	fseek (struct _iobuf *fp, long offset, int mode);
extern int	fwrite (char *bp, int szelem, int nelem, struct _iobuf *fp);
extern int	getw (struct _iobuf *fp);
extern int	isatty (XINT fd);
extern int	puts (char *str);
extern int	putw (int word, struct _iobuf *fp);
extern int	scanf (char *format, ...);
extern int	spf_open (char *buf, int maxch);
extern int	sscanf (char *str, char *format, ...);
extern int	strcmp (char *s1, char *s2);
extern int	strlen (char *s);
extern int	system (char *cmd);
extern int	ungetc (int ch, struct _iobuf *fp);
extern long	atol (char *str);
extern long	c_clktime (long reftime);
extern long	c_cputime (long reftime);
extern long	c_note (XINT fd);
extern long	ftell (struct _iobuf *fp);
extern long	rewind (struct _iobuf *fp);
extern short   *c_sppstr (char *str);
extern short   *c_strupk (char *str, short *outstr, int maxch);
extern unsigned int c_propdpr (char *process, char *bkgfile, char *bkgmsg);
extern unsigned int c_propen (char *process, int *in, int *out);
extern void	c_devstatus (char *device, int out);
extern void	c_envlist (XINT fd, char *prefix, int show_redefs);
extern void	c_envmark (XINT *envp);
extern void	c_envputs (char *var, char *value);
extern void	c_envreset (char *var, char *value);
extern void	c_erract (int action);
extern void	c_error (int errcode, char *errmsg);
extern void	c_flush (XINT fd);
extern void	c_fseti (XINT fd, int param, int value);
extern void	c_gflush (int stream);
extern void	c_pargb (int ival);
extern void	c_pargc (int ival);
extern void	c_pargd (double dval);
extern void	c_pargi (int ival);
extern void	c_pargl (long lval);
extern void	c_pargr (float rval);
extern void	c_pargs (short sval);
extern void	c_pargstr (char *strval);
extern void	c_poll_clear (XINT fds, XINT fd, int type);
extern void	c_poll_close (XINT fds);
extern void	c_poll_print (XINT fds);
extern void	c_poll_set (XINT fds, XINT fd, int type);
extern void	c_poll_zero (XINT fds);
extern void	c_sfree (int sp);
extern void	c_smark (int *sp);
extern void	c_sttyco (char *args, XINT ttin, XINT ttout, XINT outfd);
extern void	c_tsleep (int nseconds);
extern void	c_ttseti (XINT fd, int param, int value);
extern void	c_ttsets (XINT fd, int param, char *value);
extern void	c_ttycdes (XINT tty);
extern void	c_ttycn (XINT fd, XINT tty);
extern void	c_ttycr (XINT fd, XINT tty);
extern void	c_ttygoto (XINT fd, XINT tty, int col, int line);
extern void	c_ttyinit (XINT fd, XINT tty);
extern void	c_ttype (XINT fd, XINT tty, char *line, int map_cc);
extern void	c_ttyseti (XINT tty, int param, int value);
extern void	c_ttyso (XINT fd, XINT tty, int onoff);
extern void	c_vfnbrk (char *vfn, int *root, int *extn);
extern void	c_wmsec (int msec);
extern void	c_xgmes (int *oscode, char *oserrmsg, int maxch);
extern void	c_xonerr (int errcode);
extern void	c_xttysize (int *ncols, int *nlines);
extern void	eprintf (char *format, ...);
extern void	fprintf (struct _iobuf *fp, char *format, ...);
extern void	fputs (char *str, struct _iobuf *fp);
extern void	free (char *buf);
extern void	perror (char *prefix);
extern void	printf (char *format, ...);
extern void	qsort (char *base, int n, int size, int  (*compar) (void));
extern void	setbuf (struct _iobuf *fp, char *buf);
extern void	setfbf (struct _iobuf *fp, char *buf, int size);
extern void	setlinebuf (struct _iobuf *fp);
extern void	spf_close (XINT fd);

/*  The following have conflicts because of the order in which the
**  include files are done in iraf.h.  Commented out for now.
extern int	c_finfo (char *fname, struct _finfo *fi);
extern void	c_xwhen (int exception, PFI new_handler, PFI *old_handler);
*/

#endif

/*
*/
#include "../../f2c/libf2c/f2c.h"
#include "vosproto.h"

#define	D_libc
#define	D_libc_proto
#endif
