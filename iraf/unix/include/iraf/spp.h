#ifndef	_IRAF_SPP_H
#define	_IRAF_SPP_H

/*
 * SPP.H -- Language definitions for interfacing SPP to C and C to UNIX.
 * Note that many of the definitions must agree with those in the SPP
 * compiler and with <iraf.h> and <mach.h>.
 */

#include <iraf/spptypes.h>

/* Assorted machine constants. [MACHDEP]
 * Use osb$zzeps.f to compute the machine epsilon.
 */
#define	OSOK		0		/* normal successful completion	*/
#define	LEN_JUMPBUF	1024		/* C "jmp_buf" len + 1 (or larger) */
#define	EPSILON		(1.192e-7)	/* smallest real E s.t. (1.0+E > 1.0) */
#define EPSILOND	(2.220d-16)	/* double precision epsilon */
#if defined(SPP_LP64) || defined(SPP_ILP64)
#define	MAX_LONG	9223372036854775807L
#else
#define	MAX_LONG	2147483647
#endif
#define	FNNODE_CHAR	'!'		/* node name delimiter character */

/* Indefinite valued numbers. (potentially MACHDEP)
 */
#if defined(SPP_LP64) || defined(SPP_ILP64)
#ifdef SPP_LP64
#define	INDEFI		(0x80000001)
#define	INDEFL		(0x8000000000000001L)
#else	/* ILP64 */
#define	INDEFI		(0x8000000000000001L)
#define	INDEFL		(0x8000000000000001L)
#endif
#else	/* ILP32 */
#define	INDEFI		(0x80000001)
#define	INDEFL		(0x80000001)
#endif

#define	INDEFS		(-32767)
#define	INDEFR		1.6e38
#define	INDEFD		1.6e308
#define	INDEFX		(INDEF,INDEF)
#define	INDEF		INDEFR


/* Oft used constants.
 */
#define	SZ_LINE		1023
#define	SZ_FNAME	255
#define	SZ_PATHNAME	511
#define	SZ_COMMAND	2047
#define	EOS		'\0'
#define	ERR		(-1)
#define	OK		0
#define	YES		1
#define	NO		0
#define	MAX_DIGITS	25

#ifndef min
#define	min(a,b)	(((a)<(b))?(a):(b))
#endif
#ifndef max
#define	max(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef NULL
#define	NULL		((void*)0)
#endif

#ifndef EOF
#define	EOF		(-1)
#endif

/* SPP constants.
 */
#define	XEOS		0
#define	XERR		(-1)
#define	XEOF		(-2)
#define	XBOF		(-3)
#define	XOK		0
#define	XNO		0
#define	XYES		1

#define	BOFL		(-3L)
#define	EOFL		(-2L)

/* SPP datatypes.
 */
#define	TY_BOOL		1		/* SPP datatype codes		*/
#define	TY_CHAR		2
#define	TY_SHORT	3
#define	TY_INT		4
#define	TY_LONG		5
#define	TY_REAL		6
#define	TY_DOUBLE	7
#define	TY_COMPLEX	8
#define	TY_STRUCT	9
#define	TY_POINTER	10
#define	TY_USHORT	11
#define	TY_UBYTE	12

/* Definitions for the MII Machine Independent Integer package.
 */
#define	MII_BYTE	8
#define	MII_SHORT	16
#define	MII_LONG	32
#define	MII_REAL	-32
#define	MII_DOUBLE	-64
#define	MII_INT		MII_LONG

/* File I/O constants.
 */
#define	READ_ONLY	1		/* file access modes		*/
#define	READ_WRITE	2
#define	WRITE_ONLY	3
#define	APPEND		4
#define	NEW_FILE	5
		
#define	TEXT_FILE	11		/* file types			*/
#define	BINARY_FILE	12
#define	DIRECTORY_FILE	13
#define SPOOL_FILE	(-2)

#define	CLIN		1
#define	CLOUT		2
#define	STDIN		3
#define	STDOUT		4
#define	STDERR		5
#define	STDGRAPH	6
#define	STDIMAGE	7
#define	STDPLOT		8
#define	PSIOCTRL	9

/* Filename Mapping definitions.
 */

#define	VFN_READ	1		/* VFN access modes for VFNOPEN	*/
#define	VFN_WRITE	2
#define	VFN_UNMAP	3

#define	VFN_NOUPDATE	0		/* update flag for VFNCLOSE */
#define	VFN_UPDATE	1

#define	MEMCOM		mem_

/* SPP/C pointer conversions.
 */
extern	char		MEMCOM[];
#define	Memc		(((XCHAR *)MEMCOM)-1)
#define	Mems		(((XSHORT *)MEMCOM)-1)
#define	Memi		(((XINT *)MEMCOM)-1)
#define	Memb		(((XBOOL *)MEMCOM)-1)
#define	Meml		(((XLONG *)MEMCOM)-1)
#define	Memp		(((XPOINTER *)MEMCOM)-1)
#define	Memr		(((XREAL *)MEMCOM)-1)
#define	Memd		(((XDOUBLE *)MEMCOM)-1)
#define	Memx		(((XCOMPLEX *)MEMCOM)-1)
#define	Memcptr(addr)	((XCHAR *)(addr) - Memc)
#define	Memsptr(addr)	((XSHORT *)(addr) - Mems)
#define	Memiptr(addr)	((XINT *)(addr) - Memi)
#define	Membptr(addr)	((XBOOL *)(addr) - Memb)
#define	Memlptr(addr)	((XLONG *)(addr) - Meml)
#define	Mempptr(addr)	((XPOINTER *)(addr) - Memp)
#define	Memrptr(addr)	((XREAL *)(addr) - Memr)
#define	Memdptr(addr)	((XDOUBLE *)(addr) - Memd)
#define	Memxptr(addr)	((XCOMPLEX *)(addr) - Memx)

typedef unsigned long iraf_size_t;

/* Oft referenced functions.
 */
extern XCHAR *c_sppstr( const char * );
extern XCHAR *c_strupk ( const char *, XCHAR *, iraf_size_t );
extern char *c_strpak ( const XCHAR *, char *, iraf_size_t );

#endif	/* ! _IRAF_SPP_H */
