#include  <stdio.h>

extern  FILE	*_fdtofile[];	/* map file descriptor (small integer) to
				   FILE pointer.  Ratfor uses file descriptors,
				   we must use FILE pointers for stdio lib */

/*
 * The following definitions must be the same as those used by the
 * Ratfor system.
 */
#define		REOF	(-1)	/* Ratfor EOF */
#define		REOS	(-2)	/* Ratfor end-of-string */
#define		RERR	(-3)	/* Ratfor error return */
#define		NO	0
#define		YES	1
#define		NOERR	0
#define		OK	(-2)
#define		MAXLINE 128
#define		FILENAMESIZE 40	/* max num chars per filename */

#define		READ	1	/* modes for file open */
#define		WRITE	2
#define		READWRITE 3
#define		APPEND	4

/*
 * The following typedefs refer to the data types passed by the
 * Fortran compiler (Ratfor) calling us.
 */
#ifdef ILP32
typedef int RCHAR;		/* Ratfor character string */
typedef int FINT;		/* Fortran plain vanilla integer */
				/* integer*2 with new f77 on Unix */
#else
typedef long int RCHAR;		/* Ratfor character string */
typedef long int FINT;		/* Fortran plain vanilla integer */
				/* integer*2 with new f77 on Unix */
#endif


/* All names of C functions called from ratfor are defined here to make them
 * easy to change to reflect the characteristics of the host machine.  Some
 * versions of UNIX append an underscore to Fortran external names, some
 * prepend an underscore, and some do both.  VMS renders C and Fortran external
 * names the same, making it easier to mix the two languages but causing
 * name conflicts.
 */
#define	AMOVE	amove_
#define	CANT	cant_
#define	CLOSE	rfclos_
#define	CREATE	create_
#define	ENDST	endst_
#define	EXIT	rexit_
#define	FLUSH	rfflus_
#define	GETARG	getarg_
#define	GETCH	getch_
#define	GETLIN	getlin_
#define	GETNOW	getnow_
#define	INITST	initst_
#define	ISATTY	isatty_
#define	MKUNIQ	mkuniq_
#define	NOTE	rfnote_
#define	OPEN	rfopen_
#define	PUTCH	putch_
#define	PUTHOL	puthol_
#define	PUTLIN	putlin_
#define	RATFOR	ratfor_
#define	READF	readf_
#define	REMARK	remark_
#define	REMOVE	rfrmov_
#define	RWIND	rwind_
#define	SEEK	rfseek_
#define	WRITEF	writef_
