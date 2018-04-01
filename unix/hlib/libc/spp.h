/*
 * SPP.H -- Language definitions for interfacing SPP to C and C to UNIX.
 * Note that many of the definitions must agree with those in the SPP
 * compiler and with <iraf.h> and <mach.h>.
 */

#ifndef D_spp

/* Assorted machine constants. [MACHDEP]
 * Use osb$zzeps.f to compute the machine epsilon.
 */
#define	OSOK		0		/* normal successful completion	*/
#define	LEN_JUMPBUF	1024		/* C "jmp_buf" len + 1 (or larger) */
#define	EPSILON		(1.192e-7)	/* smallest real E s.t. (1.0+E > 1.0) */
#define EPSILOND	(2.220d-16)	/* double precision epsilon */
#define	MAX_LONG	2147483647
#define	FNNODE_CHAR	'!'		/* node name delimiter character */


/* Indefinite valued numbers. (potentially MACHDEP)
 */
#define	INDEFS		(-32767)
#define	INDEFL		(0x80000001)
#define	INDEFI		INDEFL
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
#define	NULL		0
#endif

#ifndef EOF
#define	EOF		(-1)
#endif

/* SPP constants.
 */
#define	XEOS		0
#define	XERR		(-1)
#define	XEOF		(-2)
#define XBOF		(-3)
#define	XOK		0
#define	XNO		0
#define	XYES		1

#define	BOFL		(-3L)
#define EOFL		(-2L)


/*  SPP datatypes. (potentially MACHDEP)
 *  Must match sizes in hlib$iraf.h
 */
#ifndef XCHAR
#define	XCHAR		short
#endif

#if (__SIZEOF_LONG__ == 8 && __SIZEOF_POINTER__ == 8) /* LP64 */
#define	XINT		long
#define	XLONG		long
#define	XSTRUCT		long
#define	XPOINTER	long
#define	XBOOL		long	
#elif (__SIZEOF_INT__ == 4 && __SIZEOF_POINTER__ == 4) /* ILP32 */
#define	XINT		int
#define	XLONG		int
#define	XSTRUCT		int
#define	XPOINTER	int
#define	XBOOL		int
#else
#error "No data model: need either LP64 or ILP32"
#endif

#define	PKCHAR		XCHAR
#define XUBYTE		unsigned char
#define	XSHORT		short
#define	XUSHORT		unsigned short
#define	XREAL		float
#define	XDOUBLE		double
#define XCOMPLEX	struct cplx

struct cplx {
	float	r;
	float	i;
};

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
#define	STATIC_FILE	14
#define	SYMLINK_FILE	15
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

/* Oft referenced functions.
 */
XCHAR	*c_sppstr(char *str);
XCHAR	*c_strupk(char *str, short *outstr, int maxch);
char	*c_strpak(short *sppstr, char *cstr, int maxch);

#define	D_spp
#endif
