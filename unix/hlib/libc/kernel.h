/*
 * KERNEL.H -- Machine dependent definitions for the 4.1BSD UNIX IRAF Kernel.
 * The UNIX include file <stdio.h> must also be loaded by any program which
 * references "kernel.h".  The companion include file "language.h" defines
 * the (generally) machine independent kernel definitions.
 */

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>

#ifndef NOKNET
#define	NOKNET			/* no networking desired in kernel	*/
#endif

/* Tunable kernel parameters.  All buffer sizes are in units of bytes.
 * Buffer lengths are in units of whatever the buffer contains.
 */
#define SZ_DISKBLOCK	512	/* used in zsttbf if dev block invar.	*/
#define	FILE_MODEBITS	0666	/* protection bits for new files	*/
#define	MAXOFILES	256	/* maximum open files (see <stdio.h>)	*/
#define	MAXPROCS	20	/* maximum subprocesses per process	*/
#define	SZ_DEFWORKSET	67108864  /* default working set size, bytes	*/
#define	SZ_MAXWORKSET	268435456 /* maximum working set (max physmem)	*/
#define	CLKFREQ		60	/* clock frequency (see zgtime.c)	*/

#define TX_OPTBUFSIZE	SZ_LINE	/* optimum buffer size for text file	*/
#define TX_MAXBUFSIZE	0	/* maximum buffer size for text file	*/
#define BF_OPTBUFSIZE	65536	/* optimum buffer size for binary file	*/
#define BF_MAXBUFSIZE	0	/* maximum buffer size for binary file	*/
#define SF_OPTBUFSIZE	65536	/* optimum buffer size for static file	*/
#define SF_MAXBUFSIZE	0	/* maximum buffer size for static file	*/
#define	KS_OPTBUFSIZE	65536	/* optimal buffer size for KS i/o	*/
#define	KS_MAXBUFSIZE	0	/* maximum buffer size for KS i/o	*/
#define	PR_OPTBUFSIZE	65536	/* optimal buffer size for IPC i/o	*/
#define	PR_MAXBUFSIZE	4096	/* maximum buffer size for IPC i/o	*/
#define	ND_OPTBUFSIZE	65536	/* optimal buffer size for ND i/o	*/
#define	ND_MAXBUFSIZE	0	/* maximum buffer size for ND i/o	*/
#define PL_OPTBUFSIZE	1024	/* optimum buffer size for plotter	*/
#define PL_MAXBUFSIZE	0	/* maximum buffer size for plotter	*/
#define LP_OPTBUFSIZE	1024	/* optimum buffer size for line printer	*/
#define LP_MAXBUFSIZE	0	/* maximum buffer size for line printer	*/

/* ZLOCVA style pointer to address conversions.  These macros are used to
 * convert host pointer addresses (in bytes) to/from iraf pointer values
 * in units of XCHAR.
 *
 * The address LOC needs to be not negative, so we explicitly cast it
 * into an unsigned before rightshifting. Hopefully (!) this will
 * shift in a zero...
 */
#define	ADDR_TO_LOC(addr) 	(((unsigned XINT)((XCHAR *)(addr)))>>(sizeof(XCHAR)-1))
#define	LOC_TO_ADDR(loc,type)   ((type *)((XCHAR *)((loc)<<(sizeof(XCHAR)-1))))


/* Kernel file descriptor for accessing UNIX files.  A static array ZFD of
 * descriptor structures is used, indexed by UNIX file descriptor numbers
 * numbered beginning at 0, the standard input.
 */
struct fiodes {
	FILE	*fp;			/* file pointer if text file	*/
	long	fpos;			/* file offset, bytes		*/
	long	filesize;		/* file size at open time	*/
	int	nbytes;			/* last nbytes r|w		*/
	int	io_flags;		/* fcntl flags			*/
	short	flags;			/* access mode flags		*/
	char	*port;			/* tty port if tty		*/
};
extern	struct fiodes zfd[];		/* array of descriptors		*/

#define	KF_CHARMODE	01		/* char input mode, text files	*/
#define	KF_NOSEEK	02		/* seeks are illegal on device	*/
#define	KF_NOSTTY	04		/* stty,gtty calls illegal	*/
#define	KF_NDELAY	010		/* nonblocking reads		*/
#define	KF_DIRECTIO	020		/* use direct (unbuffered) i/o	*/
#define	TTYNAME		"/dev/tty"	/* user terminal (for ZFIOTY)	*/
#define U_STDIN		"unix-stdin"	/* special filename for stdin	*/
#define U_STDOUT	"unix-stdout"	/* special filename for stdout	*/
#define U_STDERR	"unix-stderr"	/* special filename for stderr	*/
#define	LEN_RAWCMD	5		/* nchars in rawcmd string	*/
#define RAWOFF		"\033-rAw"	/* turn raw mode off		*/
#define RAWON		"\033+rAw"	/* turn raw mode on		*/
#define	LEN_SETREDRAW	6		/* nchars in setredraw string	*/
#define SETREDRAW	"\033=rDw"	/* set/enable screenredraw code	*/

typedef	void  (*SIGFUNC)();

typedef	void  (*PFV)();
#ifdef __LP64__
typedef	long  (*PFI)();
#else
typedef	int   (*PFI)();
#endif


extern	char *irafpath();

#define	D_kernel
