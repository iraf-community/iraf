/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#define import_endian
#include <iraf.h>

/*
  MIIPAK32 -- Pack an SPP array of the indicated datatype into an 32 bit
  signed MII array.

  spp[]        : #I input array of SPP integers
  mii[]        : #O output MII format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

/* This code supports ILP32, LP64 and ILP64 models */

#if defined(SPP_ILP64)
# define SZ_XINT_IS_8 1
#else
# define SZ_XINT_IS_8 0
#endif

#define SMALL2MII(x_type) { \
	    for ( i = *nelems ; 0 < i ; ) { \
		i--; \
		buf[0] = (XINT)(((x_type *)spp)[i]); \
		mii_ptr[i][0] = buf_ptr[i0]; \
		mii_ptr[i][1] = buf_ptr[i1]; \
		mii_ptr[i][2] = buf_ptr[i2]; \
		mii_ptr[i][3] = buf_ptr[i3]; \
	    } \
	}

#define LARGE2MII(x_type) { \
	    for ( i = 0 ; i < *nelems ; i++ ) { \
		buf[0] = (XINT)(((x_type *)spp)[i]); \
		mii_ptr[i][0] = buf_ptr[i0]; \
		mii_ptr[i][1] = buf_ptr[i1]; \
		mii_ptr[i][2] = buf_ptr[i2]; \
		mii_ptr[i][3] = buf_ptr[i3]; \
	    } \
	}

#define COMPLEX2MII(x_type) { \
	    for ( i = 0 ; i < *nelems ; i++ ) { \
		buf[0] = (XINT)(((x_type *)spp)[i].r); \
		mii_ptr[i][0] = buf_ptr[i0]; \
		mii_ptr[i][1] = buf_ptr[i1]; \
		mii_ptr[i][2] = buf_ptr[i2]; \
		mii_ptr[i][3] = buf_ptr[i3]; \
	    } \
	}

int MIIPAK32 ( void *spp, void *mii, XSIZE_T *nelems, XINT *spp_datatype )
{
	XINT buf[1] = { 0 };
	unsigned char *buf_ptr = (unsigned char *)buf;
	unsigned char (*mii_ptr)[4] = (unsigned char (*)[4])mii;
	XINT i0, i1, i2, i3;
	XSIZE_T i;

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
	    SMALL2MII(XUBYTE);
	    break;
	case TY_USHORT:
	    SMALL2MII(XUSHORT);
	    break;
	case TY_CHAR:
	    SMALL2MII(XCHAR);
	    break;
	case TY_SHORT:
	    SMALL2MII(XSHORT);
	    break;
	case TY_INT:
	    LARGE2MII(XINT);
	    break;
	case TY_POINTER:
	    LARGE2MII(XPOINTER);
	    break;
	case TY_STRUCT:
	    LARGE2MII(XPOINTER);
	    break;
	case TY_LONG:
	    LARGE2MII(XLONG);
	    break;
	case TY_REAL:
	    LARGE2MII(XREAL);
	    break;
	case TY_DOUBLE:
	    LARGE2MII(XDOUBLE);
	    break;
	case TY_COMPLEX:
	    COMPLEX2MII(XCOMPLEX);
	    break;
	default:
	    break;
	}

	return 0;
}
