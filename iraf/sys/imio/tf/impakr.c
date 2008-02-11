/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#define import_xnames
#include <iraf.h>

/*
  IMPAK? -- Convert an array of pixels of a specific datatype to the 
  datatype given as the final argument.
*/

static XINT Error_code = 1;
static XCHAR Error_msg[] = {'U','n','k','n','o','w','n',' ',
			    'd','a','t','a','t','y','p','e',' ','i','n',' ',
			    'i','m','a','g','e','f','i','l','e',XEOS };

int IMPAKR ( XREAL *a, void *b, XSIZE_T *npix, XINT *dtype )
{
	switch ( *dtype ) {
	case TY_USHORT:
	    ACHTRU (a, (XUSHORT *)b, npix);
	    break;
	case TY_SHORT:
	    ACHTRS (a, (XSHORT *)b, npix);
	    break;
	case TY_INT:
	    ACHTRI (a, (XINT *)b, npix);
	    break;
	case TY_LONG:
	    ACHTRL (a, (XLONG *)b, npix);
	    break;
	case TY_REAL:
	    ACHTRR (a, (XREAL *)b, npix);
	    break;
	case TY_DOUBLE:
	    ACHTRD (a, (XDOUBLE *)b, npix);
	    break;
	case TY_COMPLEX:
	    ACHTRX (a, (XCOMPLEX *)b, npix);
	    break;
	default:
	    ERROR (&Error_code, Error_msg);
	    break;
	}

	return 0;
}
