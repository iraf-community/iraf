/*  XPP error codes.
 */
#define	XPP_OK  	OSOK		/* no problems */
#define	XPP_COMPERR	101		/* compiler error */
#define	XPP_BADXFILE	102		/* cannot open .x file */
#define	XPP_SYNTAX	104		/* language error */

/* Contexts.
 */
#define	GLOBAL		01
#define	DECL		02
#define	BODY		04
#define	DEFSTMT		010
#define	DATASTMT	020
#define	PROCSTMT	040

/* String type codes.
 */
#define	STR_INLINE	0
#define	STR_DEFINE	1
#define	STR_DECL	2

/* SPP keywords.  The datatype keywords bool through pointer must be assigned
 * the lowest numbers.
 */ 
#define	XTY_BOOL	1
#define	XTY_CHAR	2
#define	XTY_SHORT	3
#define	XTY_INT		4
#define	XTY_LONG	5
#define	XTY_REAL	6
#define	XTY_DOUBLE	7
#define	XTY_COMPLEX	8
#define	XTY_POINTER	9
#define	XTY_PROC	10
#define	XTY_TRUE	11
#define	XTY_FALSE	12
#define	XTY_IFERR	13
#define	XTY_IFNOERR	14
#define XTY_EXTERN	15
#define XTY_ERROR	16
#define	MAX_KEY		16

/* RPP type keywords (must match type codes above).
 */
#define	RPP_TYPES {\
	"",\
	"x$bool",\
	"x$short", 	/* MACHDEP */\
	"x$short",\
	"x$int",\
	"x$long",\
	"x$real",\
	"x$dble",\
	"x$cplx",\
	"x$pntr",\
	"x$fcn",\
	".true.",\
	".false.",\
	"iferr",\
	"ifnoerr",\
	"x$extn",\
	"error"\
}
