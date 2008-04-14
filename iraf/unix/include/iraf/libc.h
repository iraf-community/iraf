#ifndef	_IRAF_LIBC_H
#define	_IRAF_LIBC_H

/*
 * LIBC.H -- Definitions which should be included by all C source files which
 * use the IRAF runtime C library.
 */

#include <iraf/spp.h>

typedef long iraf_ssize_t;
typedef unsigned long iraf_mode_t;
typedef unsigned long iraf_pid_t;
typedef long iraf_uid_t;
typedef unsigned long job_t;

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

/* ../../../sys/libc/???.c */
/* cstropen.c */
extern int c_stropen ( XCHAR *, XINT, iraf_mode_t );
/* caccess.c */
extern int c_access ( const char *, int, int );
/* callocate.c */
extern int c_allocate ( const char * );
extern int c_deallocate ( const char *, int );
extern void c_devstatus ( const char *, int );
extern int c_devowner ( const char *, char *, iraf_size_t );
/* cclktime.c */
extern long c_clktime ( long );
extern long c_cputime ( long );
/* cclose.c */
extern int c_close ( int );
/* ccnvdate.c */
extern char *c_cnvdate ( long, char *, iraf_size_t );
/* ccnvtime.c */
extern char *c_cnvtime ( long, char *, iraf_size_t );
/* cdelete.c */
extern int c_delete ( const char * );
/* cenvget.c */
extern int c_envgets ( const char *, char *, iraf_size_t );
extern int c_envfind ( const char *, char *, iraf_size_t );
extern int c_envgetb ( const char * );
extern int c_envgeti ( const char * );
extern void c_envputs ( const char *, char * );
extern void c_envreset ( const char *, char * );
/* cenvlist.c */
extern void c_envlist ( int, const char *, int );
/* cenvmark.c */
extern void c_envmark ( int * );
extern int c_envfree ( int, PFU );
extern int c_prenvfree ( int, int );
/* cenvscan.c */
extern int c_envscan ( const char * );
/* cfchdir.c */
extern int c_fchdir ( const char * );
/* cflush.c */
extern void c_flush ( int );
/* cfmapfn.c */
extern int c_fmapfn ( const char *, char *, iraf_size_t );
/* cfmkdir.c */
extern int c_fmkdir ( const char * );
/* cfnextn.c */
extern int c_fnextn ( const char *, char *, iraf_size_t );
/* cfnldir.c */
extern int c_fnldir ( const char *, char *, iraf_size_t );
/* cfnroot.c */
extern int c_fnroot ( const char *, char *, iraf_size_t );
/* cfpath.c */
extern int c_fpathname ( const char *, char *, iraf_size_t );
/* cfredir.c */
extern int c_fredir ( int, const char *, int, int );
/* cfseti.c */
extern void c_fseti ( int, int, int );
extern void c_fsetl ( int, int, long );
extern void c_fsetp ( int, int, void * );
/* cfstati.c */
extern int c_fstati ( int, int );
extern long c_fstatl ( int, int );
extern void *c_fstatp ( int, int );
/* cgetpid.c */
extern iraf_pid_t c_getpid( void );
/* cgetuid.c */
extern char *c_getuid ( char *, iraf_size_t );
/* cgflush.c */
extern void c_gflush ( int );
/* cimaccess.c */
extern int c_imaccess ( const char *, int );
/* cimdrcur.c */
extern int c_imdrcur ( const char *, float *, float *, int *, int *, 
		       char *, iraf_size_t, int, int );
/* ckimapc.c */
extern int c_kimapchan ( int, char *, iraf_size_t );
/* clexnum.c */
extern int c_lexnum ( const char *, int * );
/* cmktemp.c */
extern int c_mktemp ( const char *, char *, iraf_size_t );
/* cndopen.c */
extern int c_ndopen ( const char *, int );
/* cnote.c */
extern long c_note ( int );
/* copen.c */
extern int c_open ( const char *, iraf_mode_t, int );
/* coscmd.c */
extern int c_oscmd ( const char *, const char *, const char *, const char * );
/* cpoll.c */
extern void *c_poll_open ( void  );
extern int c_poll ( void *, int, int );
extern void c_poll_close ( void * );
extern void c_poll_zero ( void * );
extern void c_poll_set ( void *, int, int );
extern void c_poll_clear ( void *, int, int );
extern int c_poll_test ( void *, int, int );
extern int c_poll_get_nfds ( void * );
extern void c_poll_print ( void * );
/* cprcon.c */
extern iraf_pid_t c_propen ( const char *, int *, int * );
extern int c_prclose ( iraf_pid_t );
extern int c_prstati ( iraf_pid_t, int );
extern int c_prsignal ( iraf_pid_t, int );
extern int c_prredir ( iraf_pid_t, int, int );
extern int c_prchdir ( iraf_pid_t, const char * );
extern int c_prenvset ( iraf_pid_t, const char *, const char * );
/* cprdet.c */
extern job_t c_propdpr ( const char *, const char *, const char * );
extern int c_prcldpr ( job_t );
extern int c_prdone ( job_t );
extern int c_prkill ( job_t );
/* cprintf.c */
extern int c_printf ( const char * );
extern int c_fprintf ( int, const char * );
extern void c_pargb ( int );
extern void c_pargc ( int );
extern void c_pargs ( short );
extern void c_pargi ( int );
extern void c_pargl ( long );
extern void c_pargr ( float );
extern void c_pargd ( double );
extern void c_pargstr ( const char * );
/* crcursor.c */
extern int c_rcursor ( int, char *, iraf_size_t );
/* crdukey.c */
extern int c_rdukey ( char *, iraf_size_t );
/* cread.c */
extern iraf_ssize_t c_read ( int, char *, iraf_size_t );
/* crename.c */
extern int c_rename ( const char *, const char * );
/* creopen.c */
extern int c_reopen ( int, iraf_mode_t );
/* csalloc.c */
extern char *c_salloc ( iraf_size_t );
extern void c_smark ( void ** );
extern void c_sfree ( void * );
/* cseek.c */
extern int c_seek ( int, long );
/* ctsleep.c */
extern void c_tsleep ( int );
/* cttset.c */
extern void c_sttyco ( const char *, int, int, int );
extern void c_ttseti ( int, int, int );
extern void c_ttsetl ( int, int, long );
extern void c_ttsetp ( int, int, void * );
extern int c_ttstati ( int, int );
extern long c_ttstatl ( int, int );
extern void *c_ttstatp ( int, int );
extern void c_ttsets ( int, int, const char * );
extern iraf_ssize_t c_ttstats ( int, int, char *, iraf_size_t );
/* cttycdes.c */
extern void c_ttycdes ( void * );
/* cttyclear.c */
extern void c_ttyclear ( int, void * );
/* cttyclln.c */
extern void c_ttyclearln ( int, void * );
/* cttyctrl.c */
extern int c_ttyctrl ( int, void *, const char *, int );
/* cttygetb.c */
extern int c_ttygetb ( void *, const char * );
/* cttygeti.c */
extern int c_ttygeti ( void *, const char * );
/* cttygetr.c */
extern float c_ttygetr ( void *, const char * );
/* cttygets.c */
extern iraf_ssize_t c_ttygets ( void *, const char *, char *, iraf_size_t );
/* cttygoto.c */
extern void c_ttygoto ( int, void *, int, int );
/* cttyinit.c */
extern void c_ttyinit ( int, void * );
/* cttyodes.c */
extern void *c_ttyodes ( const char * );
/* cttyputl.c */
extern void c_ttyputline ( int, void *, const char *, int );
/* cttyputs.c */
extern int c_ttyputs ( int, void *, const char *, int );
/* cttyseti.c */
extern void c_ttyseti ( void *, int, int );
/* cttyso.c */
extern void c_ttyso ( int, void *, int );
/* cttystati.c */
extern int c_ttystati ( void *, int );
/* cungetc.c */
extern int c_ungetc ( int, int );
/* cungetl.c */
extern int c_ungetline ( int, const char * );
/* cvfnbrk.c */
extern void c_vfnbrk ( const char *, int *, int * );
/* cwmsec.c */
extern void c_wmsec ( unsigned long );
/* cwrite.c */
extern iraf_ssize_t c_write ( int, const char *, iraf_size_t );
/* cxgmes.c */
extern void c_xgmes ( int *, char *, iraf_size_t );
/* cxonerr.c */
extern void c_xonerr ( int );
/* cxttysize.c */
extern void c_xttysize ( int *, int * );
/* spf.c */
extern int spf_open ( char *, iraf_size_t );
extern void spf_close ( int );
/* stgio.c */
extern int c_stggetline ( int, char *, iraf_size_t );
extern int c_stgputline ( int, const char * );


/* Symbols defined in the standard libc. 
 */
#ifndef NOLIBCNAMES
#define	_IRAF_LIBC_LIBCNAMES

# ifndef NULL
#define	NULL		((void*)0)
# endif

#define size_t iraf_size_t
#define ssize_t iraf_ssize_t
#define mode_t iraf_mode_t
#define pid_t iraf_pid_t
#define uid_t iraf_uid_t

#define	getenv		envget
#define	sys_nerr	u_sysnerr
#define	sys_errlist	u_syserrlist

#define	atof		u_atof
#define	atoi		u_atoi
#define	atol		u_atol
#define	calloc		u_calloc
#define	envget		u_envget
#define	free		u_free
#define	index		u_index
#define	isatty		u_isatty
#define	malloc		u_malloc
#define	mktemp		u_mktemp
#define	qsort		u_qsort
#define	realloc		u_realloc
#define	rindex		u_rindex
#define	strcat		u_strcat
#define	strchr		u_index
#define	strcmp		u_strcmp
#define	strcpy		u_strcpy
#define	strlen		u_strlen
#define	strncat		u_strnt			/* collision	*/
#define	strncmp		u_strnp			/* collision	*/
#define	strncpy		u_strny			/* collision	*/
#define	strrchr		u_rindex
#define	system		u_system

extern double atof ( const char * );
extern int atoi ( const char * );
extern long atol ( const char * );
extern void *calloc ( size_t, size_t );
extern char *envget ( const char * );
extern void free ( void * );
extern char *index ( const char *, int );
extern int isatty ( int );
extern void *malloc ( size_t );
extern char *mktemp ( char * );
extern void qsort ( void *, size_t, size_t,
		    int (*compar)(const void *, const void *) );
extern void *realloc ( void *, size_t );
extern char *rindex ( const char *, int );
extern char *strcat ( char *, const char * );
extern int strcmp ( const char *, const char * );
extern char *strcpy ( char *, const char * );
extern size_t strlen ( const char * );
extern char *strncat ( char *, const char *, size_t );
extern int strncmp ( const char *, const char *, size_t );
extern char *strncpy ( char *, const char *, size_t );
extern int system ( const char * );


#endif	/* ! NOLIBCNAMES */

#endif	/* ! _IRAF_LIBC_H */
