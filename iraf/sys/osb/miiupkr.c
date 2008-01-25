/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  MIIUPKR -- Unpack a 32 bit IEEE floating array into an SPP array of the
  indicated datatype.

  mii[]        : #I input MII format array
  spp[]        : #O output SPP format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

#define TMP_BUFSIZE 65536

#define MII2SMALL(x_type) { \
	    XREAL buf[TMP_BUFSIZE]; \
	    XREAL *mii_ptr = (XREAL *)mii; \
	    XINT x_n = TMP_BUFSIZE; \
	    XINT i,j; \
	    for ( i=0 ; i < *nelems ; i += TMP_BUFSIZE ) { \
		if (*nelems-i < TMP_BUFSIZE) x_n = *nelems-i; \
		IEEVUPKR (mii_ptr + i, buf, &x_n); \
		for ( j=0 ; j < x_n ; j++ ) { \
		    ((x_type *)spp)[i+j] = (x_type)(buf[j]); \
		} \
	    } \
	}

int MIIUPKR ( void *mii, void *spp, XINT *nelems, XINT *spp_datatype )
{
	switch ( *spp_datatype ) {
	/* large to small */
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
	/* small to large */
	default:
	    IEEVUPKR (mii, (XREAL *)spp, nelems);
	    if ( *spp_datatype != TY_REAL ) {
		ACHTR ((XREAL *)spp, spp, nelems, spp_datatype);
	    }
	    break;
	}

	return 0;
}
