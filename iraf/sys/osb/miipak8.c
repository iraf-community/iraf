/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  MIIPAK8 -- Pack an SPP array of the indicated datatype into an 8 bit
  unsigned MII array.

  spp[]        : #I input array of SPP integers
  mii[]        : #O output MII format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/
int MIIPAK8 ( void *spp, void *mii, XSIZE_T *nelems, XINT *spp_datatype )
{
	switch ( *spp_datatype ) {
	case TY_UBYTE:
	    ACHTBB ((XUBYTE *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_USHORT:
	    ACHTUB ((XUSHORT *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_CHAR:
	    ACHTCB ((XCHAR *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_SHORT:
	    ACHTSB ((XSHORT *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_INT:
	    ACHTIB ((XINT *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_POINTER:
	    ACHTPB ((XPOINTER *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_STRUCT:
	    ACHTPB ((XPOINTER *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_LONG:
	    ACHTLB ((XLONG *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_REAL:
	    ACHTRB ((XREAL *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_DOUBLE:
	    ACHTDB ((XDOUBLE *)spp, (XUBYTE *)mii, nelems);
	    break;
	case TY_COMPLEX:
	    ACHTXB ((XCOMPLEX *)spp, (XUBYTE *)mii, nelems);
	    break;
	default:
	    break;
	}

	return 0;
}
