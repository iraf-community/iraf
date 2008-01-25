/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#define import_endian
#include <iraf.h>

/*
  MIIUPK32 -- Unpack a 32 bit signed MII array into an SPP array of the
  indicated datatype.

  mii[]        : #I input MII format array
  spp[]        : #O output SPP format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

/* This code supports ILP32, LP64 and ILP64 models */

#if defined(SPP_ILP64)
# define SZ_XINT_IS_8 1
#else
# define SZ_XINT_IS_8 0
#endif

#define MII2SMALL(x_type) { \
	    for ( i = 0 ; i < *nelems ; i++ ) { \
		buf_ptr[i0] = mii_ptr[i][0]; \
		buf_ptr[i1] = mii_ptr[i][1]; \
		buf_ptr[i2] = mii_ptr[i][2]; \
		buf_ptr[i3] = mii_ptr[i][3]; \
		if ( SZ_XINT_IS_8 ) { \
		    if ( (buf[0] & 0x080000000L) != 0 ) \
			buf[0] |= 0xffffffff00000000L; \
		    else \
			buf[0] &= 0x00000000ffffffffL; \
		} \
		((x_type *)spp)[i] = (x_type)(buf[0]); \
	    } \
	}

#define MII2LARGE(x_type) { \
	    for ( i = *nelems ; 0 < i ; ) { \
		i--; \
		buf_ptr[i0] = mii_ptr[i][0]; \
		buf_ptr[i1] = mii_ptr[i][1]; \
		buf_ptr[i2] = mii_ptr[i][2]; \
		buf_ptr[i3] = mii_ptr[i][3]; \
		if ( SZ_XINT_IS_8 ) { \
		    if ( (buf[0] & 0x080000000L) != 0 ) \
			buf[0] |= 0xffffffff00000000L; \
		    else \
			buf[0] &= 0x00000000ffffffffL; \
		} \
		((x_type *)spp)[i] = (x_type)(buf[0]); \
	    } \
	}

#define MII2COMPLEX(x_type) { \
	    for ( i = *nelems ; 0 < i ; ) { \
		i--; \
		buf_ptr[i0] = mii_ptr[i][0]; \
		buf_ptr[i1] = mii_ptr[i][1]; \
		buf_ptr[i2] = mii_ptr[i][2]; \
		buf_ptr[i3] = mii_ptr[i][3]; \
		if ( SZ_XINT_IS_8 ) { \
		    if ( (buf[0] & 0x080000000L) != 0 ) \
			buf[0] |= 0xffffffff00000000L; \
		    else \
			buf[0] &= 0x00000000ffffffffL; \
		} \
		((x_type *)spp)[i].r = (float)(buf[0]); \
		((x_type *)spp)[i].i = 0; \
	    } \
	}

int MIIUPK32 ( void *mii, void *spp, XINT *nelems, XINT *spp_datatype )
{
	XINT buf[1] = { 0 };
	unsigned char *buf_ptr = (unsigned char *)buf;
	unsigned char (*mii_ptr)[4] = (unsigned char (*)[4])mii;
	XINT i0, i1, i2, i3;
	XINT i;

	if (BYTE_SWAP == YES) {
	    i0=3; i1=2; i2=1; i3=0;
	}
	else {
	    if ( SZ_XINT_IS_8 ) {	/* ILP64 */
		i0=4; i1=5; i2=6; i3=7;
	    }
	    else {
		i0=0; i1=1; i2=2; i3=3;
	    }
	}

	switch ( *spp_datatype ) {
	case TY_UBYTE:
	    MII2SMALL(XUBYTE);
	    break;
	case TY_USHORT:
	    MII2SMALL(XUSHORT);
	    break;
	case TY_CHAR:
	    MII2SMALL(XCHAR);
	    break;
	case TY_SHORT:
	    MII2SMALL(XSHORT);
	    break;
	case TY_INT:
	    MII2LARGE(XINT);
	    break;
	case TY_POINTER:
	    MII2LARGE(XPOINTER);
	    break;
	case TY_STRUCT:
	    MII2LARGE(XPOINTER);
	    break;
	case TY_LONG:
	    MII2LARGE(XLONG);
	    break;
	case TY_REAL:
	    MII2LARGE(XREAL);
	    break;
	case TY_DOUBLE:
	    MII2LARGE(XDOUBLE);
	    break;
	case TY_COMPLEX:
	    MII2COMPLEX(XCOMPLEX);
	    break;
	default:
	    break;
	}

	return 0;
}
