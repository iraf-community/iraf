/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  MIIPAKD -- Pack an SPP array of the indicated datatype into an 64 bit
  IEEE floating format.

  spp[]        : #I input array of SPP integers
  mii[]        : #O output MII format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

int MIIPAKD ( void *spp, void *mii, XINT *nelems, XINT *spp_datatype )
{
	if ( *spp_datatype == TY_DOUBLE ) {
	    IEEVPAKD ((XDOUBLE *)spp, mii, nelems);
	}
	else {
	    switch ( *spp_datatype ) {
	    case TY_UBYTE:
		ACHTBD ((XUBYTE *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_USHORT:
		ACHTUD ((XUSHORT *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_CHAR:
		ACHTCD ((XCHAR *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_SHORT:
		ACHTSD ((XSHORT *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_INT:
		ACHTID ((XINT *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_POINTER:
		ACHTPD ((XPOINTER *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_STRUCT:
		ACHTPD ((XPOINTER *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_LONG:
		ACHTLD ((XLONG *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_REAL:
		ACHTRD ((XREAL *)spp, (XDOUBLE *)mii, nelems);
		break;
	    case TY_COMPLEX:
		ACHTXD ((XCOMPLEX *)spp, (XDOUBLE *)mii, nelems);
		break;
	    default:
		AMOVD ((XDOUBLE *)spp, (XDOUBLE *)mii, nelems);
		break;
	    }

	    IEEVPAKD ((XDOUBLE *)mii, mii, nelems);
	}

	return 0;
}
