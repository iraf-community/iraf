/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  MIIPAKR -- Pack an SPP array of the indicated datatype into an 32 bit
  IEEE floating format.

  spp[]        : #I input array of SPP integers
  mii[]        : #O output MII format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

int MIIPAKR ( void *spp, void *mii, XINT *nelems, XINT *spp_datatype )
{
	if ( *spp_datatype == TY_REAL ) {
	    IEEVPAKR ((XREAL *)spp, mii, nelems);
	}
	else {
	    switch ( *spp_datatype ) {
	    case TY_UBYTE:
		ACHTBR ((XUBYTE *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_USHORT:
		ACHTUR ((XUSHORT *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_CHAR:
		ACHTCR ((XCHAR *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_SHORT:
		ACHTSR ((XSHORT *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_INT:
		ACHTIR ((XINT *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_POINTER:
		ACHTPR ((XPOINTER *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_STRUCT:
		ACHTPR ((XPOINTER *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_LONG:
		ACHTLR ((XLONG *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_DOUBLE:
		ACHTDR ((XDOUBLE *)spp, (XREAL *)mii, nelems);
		break;
	    case TY_COMPLEX:
		ACHTXR ((XCOMPLEX *)spp, (XREAL *)mii, nelems);
		break;
	    default:
		AMOVR ((XREAL *)spp, (XREAL *)mii, nelems);
	        break;
	    }

	    IEEVPAKR ((XREAL *)mii, mii, nelems);
	}

	return 0;
}

