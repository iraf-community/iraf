/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  MIIUPKD -- Unpack a 64 bit IEEE floating array into an SPP array of the
  indicated datatype.

  mii[]        : #I input MII format array
  spp[]        : #O output SPP format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

#define TMP_BUFSIZE 65536

#define MII2SMALL(x_type) { \
	    XDOUBLE buf[TMP_BUFSIZE]; \
	    XDOUBLE *mii_ptr = (XDOUBLE *)mii; \
	    XINT x_n = TMP_BUFSIZE; \
	    XINT i,j; \
	    for ( i=0 ; i < *nelems ; i += TMP_BUFSIZE ) { \
		if (*nelems-i < TMP_BUFSIZE) x_n = *nelems-i; \
		IEEVUPKD (mii_ptr + i, buf, &x_n); \
		for ( j=0 ; j < x_n ; j++ ) { \
		    ((x_type *)spp)[i+j] = (x_type)(buf[j]); \
		} \
	    } \
	}

int MIIUPKD ( void *mii, void *spp, XINT *nelems, XINT *spp_datatype )
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
	/* small to large */
	default:
	    IEEVUPKD (mii, (XDOUBLE *)spp, nelems);
	    if ( *spp_datatype != TY_DOUBLE ) {
		ACHTD ((XDOUBLE *)spp, spp, nelems, spp_datatype);
	    }
	    break;
	}

	return 0;
}

