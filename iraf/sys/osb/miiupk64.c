/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#define import_endian
#include <iraf.h>

/*
  MIIUPK64 -- Unpack a 64 bit signed MII array into an SPP array of the
  indicated datatype.

  mii[]        : #I input MII format array
  spp[]        : #O output SPP format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

/* This code supports ILP32, LP64 and ILP64 models */

#define MII2SMALL(x_type) { \
	    for ( i = 0 ; i < *nelems ; i++ ) { \
		buf_ptr[i0] = mii_ptr[i][0]; \
		buf_ptr[i1] = mii_ptr[i][1]; \
		buf_ptr[i2] = mii_ptr[i][2]; \
		buf_ptr[i3] = mii_ptr[i][3]; \
		buf_ptr[i4] = mii_ptr[i][4]; \
		buf_ptr[i5] = mii_ptr[i][5]; \
		buf_ptr[i6] = mii_ptr[i][6]; \
		buf_ptr[i7] = mii_ptr[i][7]; \
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
		buf_ptr[i4] = mii_ptr[i][4]; \
		buf_ptr[i5] = mii_ptr[i][5]; \
		buf_ptr[i6] = mii_ptr[i][6]; \
		buf_ptr[i7] = mii_ptr[i][7]; \
		((x_type *)spp)[i].r = (float)(buf[0]); \
		((x_type *)spp)[i].i = 0; \
	    } \
	}

int MIIUPK64 ( void *mii, void *spp, XSIZE_T *nelems, XINT *spp_datatype )
{
	long long buf[1] = { 0 };
	unsigned char *buf_ptr = (unsigned char *)buf;
	unsigned char (*mii_ptr)[8] = (unsigned char (*)[8])mii;
	XINT i0, i1, i2, i3;
	XINT i4, i5, i6, i7;
	XSIZE_T i;

	if (BYTE_SWAP == YES) {
	    i0=7; i1=6; i2=5; i3=4;
	    i4=3; i5=2; i6=1; i7=0;
	}
	else {
	    i0=0; i1=1; i2=2; i3=3;
	    i4=4; i5=5; i6=6; i7=7;
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
	    MII2SMALL(XINT);
	    break;
	case TY_POINTER:
	    MII2SMALL(XPOINTER);
	    break;
	case TY_STRUCT:
	    MII2SMALL(XPOINTER);
	    break;
	case TY_LONG:
	    MII2SMALL(XLONG);
	    break;
	case TY_REAL:
	    MII2SMALL(XREAL);
	    break;
	case TY_DOUBLE:
	    MII2SMALL(XDOUBLE);
	    break;
	case TY_COMPLEX:
	    MII2COMPLEX(XCOMPLEX);
	    break;
	default:
	    break;
	}

	return 0;
}
