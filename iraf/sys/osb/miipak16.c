/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#define import_endian
#include <iraf.h>

/*
  MIIPAK16 -- Pack an SPP array of the indicated datatype into an 16 bit
  signed MII array.

  spp[]        : #I input array of SPP integers
  mii[]        : #O output MII format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/
int MIIPAK16 ( void *spp, void *mii, XSIZE_T *nelems, XINT *spp_datatype )
{
	switch ( *spp_datatype ) {
	case TY_UBYTE:
	    ACHTBS ((XUBYTE *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_USHORT:
	    ACHTUS ((XUSHORT *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_CHAR:
	    ACHTCS ((XCHAR *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_SHORT:
	    ACHTSS ((XSHORT *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_INT:
	    ACHTIS ((XINT *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_POINTER:
	    ACHTPS ((XPOINTER *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_STRUCT:
	    ACHTPS ((XPOINTER *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_LONG:
	    ACHTLS ((XLONG *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_REAL:
	    ACHTRS ((XREAL *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_DOUBLE:
	    ACHTDS ((XDOUBLE *)spp, (XSHORT *)mii, nelems);
	    break;
	case TY_COMPLEX:
	    ACHTXS ((XCOMPLEX *)spp, (XSHORT *)mii, nelems);
	    break;
	default:
	    break;
	}

	if (BYTE_SWAP == YES) {
	    XSIZE_T c_1 = 1;
	    XSIZE_T x_n = *nelems * 2;
	    BSWAP2 (mii, &c_1, mii, &c_1, &x_n);
	}

	return 0;
}
