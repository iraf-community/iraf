#ifndef	_IRAF_SPPTYPES_H
#define	_IRAF_SPPTYPES_H

/* SPP datatypes. (potentially MACHDEP)
 */

#ifndef F2C_INCLUDE	/* for native C code */


#if defined(SPP_LP64) || defined(SPP_ILP64)

#ifdef SPP_LP64
#define	XINT		int
#define	XBOOL		int
#define	XLONG		long int
#define	XPOINTER	long int
#else	/* ILP64 */
#define	XINT		long int
#define	XBOOL		long int
#define	XLONG		long int
#define	XPOINTER	long int
#endif

#else	/* ILP32 */

#define	XINT		int
#define	XBOOL		int
#define	XLONG		int
#define	XPOINTER	int

#endif

#define	XCHAR		short int
#define	PKCHAR		XCHAR
#define XUBYTE		unsigned char
#define	XSHORT		short int
#define	XUSHORT		unsigned short
#define	XREAL		float
#define	XDOUBLE		double
#define XCOMPLEX	struct cplx

#define	XSTRUCT		XPOINTER

#define	XSIZE_T		XLONG

struct cplx {
	float	r;
	float	i;
};

typedef	void  (*PFV)();
typedef	int   (*PFU)();
typedef	XINT  (*PFI)();

/* Signal handler in IRAF SPP */
typedef	void (*XSIGFUNC)(XINT *,void (**)());


#else	/* For C code written by f2c */


#if defined(SPP_LP64) || defined(SPP_ILP64)

#ifdef SPP_LP64
#define	XINT		integer
#define	XBOOL		logical
#define	XLONG		longint
#define	XSTRUCT		longint
#define	XPOINTER	longint
#else	/* ILP64 */
#define	XINT		integer
#define	XBOOL		logical
#define	XLONG		integer
#define	XSTRUCT		integer
#define	XPOINTER	integer
#endif

#else	/* ILP32 */

#define	XINT		integer
#define	XBOOL		logical
#define	XLONG		integer
#define	XSTRUCT		integer
#define	XPOINTER	integer

#endif

#define	XCHAR		shortint
#define	PKCHAR		XCHAR
#define XUBYTE		integer1
#define	XSHORT		shortint
#define	XUSHORT		shortint
#define	XREAL		real
#define	XDOUBLE		doublereal
#define XCOMPLEX	complex

#define	XSIZE_T		XLONG

#define PFU U_fp
#define PFI I_fp


#endif	/* ! F2C_INCLUDE */


#endif	/* ! _IRAF_SPPTYPES_H */
