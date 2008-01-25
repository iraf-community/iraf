/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc. */

#define import_spp
#define import_knames
#define import_endian
#include <iraf.h>

/*
.help IEEE
.nf ------------------------------------------------------------------------
Low level primitives for IEEE to native floating point datatype conversions.
See also the MII package, which provides a higher level interface, and the
IEEE related definitions in <mach.h>.

	         ieepak[rd] (datum)			# scalar conversions
	         ieeupk[rd] (datum)
		ieevpak[rd] (native, ieee, nelem)	# vector conversions
		ieevupk[rd] (ieee, native, nelem)
	     iee[sg]nan[rd] (NaN)			# NaN handling
	     iee[sg]map[rd] (mapin, mapout)
		ieestat[rd] (nin, nout)
	       ieezstat[rd] ()

The first two routines handle scalar conversions, the second two routines
vector conversions.  The input and output vectors may be the same.
Unfortunately, for portability reasons, functions cannot be used, so the
scalar operators do an in-place conversion instead, and are a no-op on an
unswapped IEEE system.  The routines iee[sg]nan[rd] set/get the native
floating value used to replace NaNs or overflows occuring when converting
IEEE to the native floating format (any floating value will do, e.g., zero or
INDEF).  If NaN mapping is enabled, the ieestat[rd] routines may be used to
determine the number of input or output NaN conversions occuring since the
last call to ieezstat[rd].

The NaN mapping enable switch and statistics counters are UNDEFINED at
process startup; programs which use the IEEE conversion package should call
ieesmap[rd] to enable or disable NaN mapping, and ieezstat[rd] to initialize
the statistics counters.

The routines in this file are the "portable" versions.  The "portable"
solution it to merely copy the array, swapping the bytes if necessary - this
works on any host that uses the IEEE floating format.  NaN mapping is
implemented in the portable code, but will work properly only for input
conversions; for output, the IEEE NaN value is undefined in the portable
version of the code (it is trivial to supply this value in an as$ieee.gx
version of the code).
If the local host does
not use IEEE floating, or if a significant efficiency gain can be realized
by programming in assembler or C, a host specific version of this file should
be written, placed in AS, and referenced in the MKPKG special file list.
.endhelp -------------------------------------------------------------------
*/

#ifdef DATATYPE_REAL
# define	BSWAP	BSWAP4
# define	NSWAP	4
# define	IOFF	0
# define	PIXEL	XREAL
# define	AMOVE	AMOVR
#else
# define	BSWAP	BSWAP8
# define	NSWAP	8
/* MACHDEP (normally 0, 1 on e.g. Intel) */
# if defined(I386) || defined(X86_64)
#  define	IOFF	1
# else
#  define	IOFF	0
# endif
# define	PIXEL	XDOUBLE
# define	AMOVE	AMOVD
#endif

/* common /ieenan$t/ */
static PIXEL native_NaN = 0, ieee_NaN = 0;
static XINT mapin = 0, mapout = 0, nin = 0, nout = 0, NaNmask = 0;

/*
  IEEVPAK -- Convert an array in the native floating point format into an
  array in IEEE floating format.  The input and output arrays can be the same.

  native[] : #I input native floating format array
  ieee[]   : #O output IEEE floating format array
  nelem    : #I number of floating point numbers
*/

#ifdef DATATYPE_REAL
int IEEVPAKR ( PIXEL *native, void *ieee, XINT *nelem )
#else
int IEEVPAKD ( PIXEL *native, void *ieee, XINT *nelem )
#endif
{
    XINT i;

    if (mapout == XNO) {
	if (IEEE_SWAP == YES) {
	    XINT c_1 = 1;
	    XINT x_n = *nelem * NSWAP;
	    BSWAP (native, &c_1, ieee, &c_1, &x_n);
	}
	else {
	    AMOVE (native, (PIXEL *)ieee, nelem);
	}
    } else {
	IEEE_SIGMASK();
	for ( i=0 ; i < *nelem ; i++ ) {
	    if (native[i] == native_NaN) {
		((PIXEL *)ieee)[i] = ieee_NaN;
		nout = nout + 1;
	    } else {
		((PIXEL *)ieee)[i] = native[i];
	    }
	}
	/* Byteswap if necessary. */
	if (IEEE_SWAP == YES) {
	    XINT c_1 = 1;
	    XINT x_n = *nelem * NSWAP;
	    BSWAP (ieee, &c_1, ieee, &c_1, &x_n);
	}
	IEEE_SIGRESTORE();
    }
    ZZEPRO();
    return 0;
}

/*
  IEEVUPK -- Convert an array in IEEE floating format into the native
  floating point format.  The input and output arrays can be the same.

  ieee[]   : #I input IEEE floating format array
  native[] : #O output native floating format array
  nelem    : #I number of floating point numbers
*/

#ifdef DATATYPE_REAL
int IEEVUPKR ( void *ieee, PIXEL *native, XINT *nelem )
#else
int IEEVUPKD ( void *ieee, PIXEL *native, XINT *nelem )
#endif
{
    static PIXEL fval[1];
    XINT *ival = (XINT *)fval;
    XINT expon, i;

    if (IEEE_SWAP == YES) {
	XINT c_1 = 1;
	XINT x_n = *nelem * NSWAP;
	BSWAP (ieee, &c_1, native, &c_1, &x_n);
	if (mapin != XNO) {
	    /* Check for IEEE exceptional values and map NaN to the native  */
	    /* NaN value, and denormalized numbers (zero exponent) to zero. */
	    IEEE_SIGMASK();
	    for ( i=0 ; i < *nelem ; i++ ) {
		fval[0] = native[i];
		expon = (ival[IOFF] & NaNmask);
		if (expon == 0) {
		    native[i] = 0;
		} else if (expon == NaNmask) {
		    native[i] = native_NaN;
		    nin = nin + 1;
		}
	    }
	    IEEE_SIGRESTORE();
	}
    } else {
	if (mapin == XNO) {
	    AMOVE ((PIXEL *)ieee, native, nelem);
	}
	else {
	    /* Check for IEEE exceptional values and map NaN to the native  */
	    /* NaN value, and denormalized numbers (zero exponent) to zero. */
	    IEEE_SIGMASK();
	    for ( i=0 ; i < *nelem ; i++ ) {
		fval[0] = ((PIXEL *)ieee)[i];
		expon = (ival[IOFF] & NaNmask);
		if (expon == 0) {
		    native[i] = 0;
		} else if (expon == NaNmask) {
		    native[i] = native_NaN;
		    nin = nin + 1;
		} else
		    native[i] = ((PIXEL *)ieee)[i];
	    }
	    IEEE_SIGRESTORE();
	}
    }
    ZZEPRO();
    return 0;
}

/*
  IEEPAK -- Convert a native floating point number into IEEE format.

  x : #U datum to be converted
*/

#ifdef DATATYPE_REAL
int IEEPAKR ( PIXEL *x )
#else
int IEEPAKD ( PIXEL *x )
#endif
{
    if (mapout != XNO) {
	IEEE_SIGMASK();
	if ( *x == native_NaN ) {
	    *x = ieee_NaN;
	    nout = nout + 1;
	}
	IEEE_SIGRESTORE();
    }
    if (IEEE_SWAP == YES) {
	XINT c_1 = 1;
	XINT x_n = NSWAP;
	BSWAP (x, &c_1, x, &c_1, &x_n);
    }
    ZZEPRO();
    return 0;
}

/*
  IEEUPK -- Convert an IEEE format number into native floating point.

  x : #U datum to be converted
*/

#ifdef DATATYPE_REAL
int IEEUPKR ( PIXEL *x )
#else
int IEEUPKD ( PIXEL *x )
#endif
{
    static PIXEL fval[1];
    XINT *ival = (XINT *)fval;
    XINT expon;

    if (IEEE_SWAP == YES) {
	XINT c_1 = 1;
	XINT x_n = NSWAP;
	BSWAP (x, &c_1, x, &c_1, &x_n);
    }

    /* Check for IEEE exceptional values and map NaN to the native NaN */
    /* value, and denormalized numbers (zero exponent) to zero.        */

    if (mapin != XNO) {
	IEEE_SIGMASK();
	fval[0] = *x;
	expon = (ival[IOFF] & NaNmask);
	if (expon == 0)
	    *x = 0;
	else if (expon == NaNmask) {
	    *x = native_NaN;
	    nin = nin + 1;
	}
	IEEE_SIGRESTORE();
    }
    ZZEPRO();
    return 0;
}

/*
  IEESNAN -- Set the native floating point value used to replace NaNs and
  overflows when converting IEEE to native.  This must be a legal (finite)
  native floating point value.

  x : #I native value which will replace NaN
*/

#ifdef DATATYPE_REAL
int IEESNANR ( PIXEL *x )
#else
int IEESNAND ( PIXEL *x )
#endif
{
    native_NaN = *x;
    nin = 0;
    nout = 0;
    ZZEPRO();
    return 0;
}

/*
  IEEGNAN -- Get the NaN value.

  x : #O native value which will replace NaN
*/
#ifdef DATATYPE_REAL
int IEEGNANR ( PIXEL *x )
#else
int IEEGNAND ( PIXEL *x )
#endif
{
    *x = native_NaN;
    ZZEPRO();
    return 0;
}

/*
  IEESTAT -- Return statistics on the number of NaNs encountered in input
  conversions (unpack) and output conversions (pack).

  o_nin  : #O number of NaN seen on input
  o_nout : #O number of NaN values output
*/
#ifdef DATATYPE_REAL
int IEESTATR ( XINT *o_nin, XINT *o_nout )
#else
int IEESTATD ( XINT *o_nin, XINT *o_nout )
#endif
{
    *o_nin = nin;
    *o_nout = nout;
    ZZEPRO();
    return 0;
}

/*
  IEEZSTAT -- Zero the statistics counters.
*/
#ifdef DATATYPE_REAL
int IEEZSTATR ( void )
#else
int IEEZSTATD ( void )
#endif
{
    nin = 0;
    nout = 0;
    ZZEPRO();
    return 0;
}

/*
  IEEMAP -- Same as IEESMAP.  Retained for backwards compatibility.
*/
#ifdef DATATYPE_REAL
int IEEMAPR ( XINT *inval, XINT *outval )
{
    IEESMAPR (inval, outval);
    ZZEPRO();
    return 0;
}
#else
int IEEMAPD ( XINT *inval, XINT *outval )
{
    IEESMAPD (inval, outval);
    ZZEPRO();
    return 0;
}
#endif

/*
  IEEGMAP -- Query the current values of the input and output mapping
  enables.

  inval  : #O get input mapping enable flag
  outval : #O get output mapping enable flag
*/
#ifdef DATATYPE_REAL
int IEEGMAPR ( XINT *inval, XINT *outval )
#else
int IEEGMAPD ( XINT *inval, XINT *outval )
#endif
{
    *inval = mapin;
    *outval = mapout;
    ZZEPRO();
    return 0;
}

/*
  MACHINE DEPENDENT PART.
  ---------------------------

  IEESMAP -- Enable or disable NaN mapping.

  sEEE EEEE Emmm mmmm mmmm mmmm mmmm mmmm
   3           2            1           0
  1098 7654 3210 9876 5432 1098 7654 3210
     7    f    8    0    0    0    0    0

  inval  : #I enable NaN mapping for input?
  outval : #I enable NaN mapping for output?
*/

#ifdef DATATYPE_REAL
int IEESMAPR ( XINT *inval, XINT *outval )
#else
int IEESMAPD ( XINT *inval, XINT *outval )
#endif
{
#ifdef DATATYPE_REAL
    static struct {
	XINT e_1;
    } equiv_0 = { 0x7ff7ffff };
#else
    static struct {
	XINT e_1[2];
	XDOUBLE e_2;
    } equiv_0 = { {0x7ff7ffff, -1}, 0.0 };
#endif
    PIXEL *fval = (PIXEL *)&equiv_0;
    XINT  *ival = (XINT *)&equiv_0;
    
    mapin = *inval;
    mapout = *outval;

    /* MACHDEP. */
    if (mapout == XYES)
	ieee_NaN = fval[0];

    if (mapin == XYES) {
#ifdef DATATYPE_REAL
	NaNmask = 0x7F800000;
#else
	NaNmask = 0x7FF00000;
#endif
    }
    ZZEPRO();
    return 0;
}

#ifdef DATATYPE_REAL
/*
  IEEE_SIGMASK, IEEE_SIGRESTORE -- Routines for masking IEEE exceptions.

	ieee_sigmask()
	ieee_sigrestore()

  These routines are meant to be used only internally by the routines in
  this file.  iee_sigmask saves the current IEEE FPU exception mask, and
  sets a new mask which masks the invalid operand exception.  This is
  necessary to permit the routines in this file to handle NaN values without
  raising the IEEE invalid operand exception.  iee_sigrestore restores
  the original exception mask.  These routines are meant to be called as
  pairs to temporarily block the invalid operand exception within an IEEE
  conversion routine.
 */

#ifdef D_FLG
#undef D_FLG
#endif

#if defined(LINUX) || defined(CYGWIN)
#define D_FLG
/* Linux */
static XINT fpucw = 0;
int IEEE_SIGMASK ( void )
{
    XINT x_i;
    GFPUCW (&fpucw);
#ifdef POWERPC
    x_i = fpucw | 0x0080;
#else
    x_i = fpucw | 1;
#endif
    SFPUCW (&x_i);
    ZZEPRO();
    return 0;
}

int IEEE_SIGRESTORE ( void )
{
    SFPUCW (&fpucw);
    ZZEPRO();
    return 0;
}
#endif	/* LINUX || CYGWIN */

#if defined(MACOSX) && defined(POWERPC)
#define D_FLG
/* MACOSX */
int IEEE_SIGMASK ( void )
{
    MXMASK();
    ZZEPRO();
    return 0;
}

int IEEE_SIGRESTORE ( void )
{
    MXUMSK();
    ZZEPRO();
    return 0;
}
#endif	/* MACOSX && POWERPC */

#ifndef D_FLG
/* Other OSs (dummy) */
int IEEE_SIGMASK ( void )
{
    ZZEPRO();
    return 0;
}

int IEEE_SIGRESTORE ( void )
{
    ZZEPRO();
    return 0;
}
#endif

#endif	/* DATATYPE_REAL */
