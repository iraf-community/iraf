/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  MIIUPK -- Unpack a MII array of type mii_type into a SPP array of type
  spp_type.  The mii_types are defined in mii.h.

  mii[]        : #I input MII format array
  spp[]        : #O output SPP format array
  nelems       : #I number of integers to be converted
  mii_datatype : #I MII datatype code
  spp_datatype : #I SPP datatype code
*/

int MIIUPK ( void *mii, void *spp, XSIZE_T *nelems, XINT *mii_datatype, XINT *spp_datatype )
{
	switch ( *mii_datatype ) {
	case MII_BYTE:
	    MIIUPK8 (mii, spp, nelems, spp_datatype);
	    break;
	case MII_SHORT:
	    MIIUPK16 (mii, spp, nelems, spp_datatype);
	    break;
	case MII_LONG:
	    MIIUPK32 (mii, spp, nelems, spp_datatype);
	    break;
	case MII_LONGLONG:
	    MIIUPK64 (mii, spp, nelems, spp_datatype);
	    break;
	case MII_REAL:
	    MIIUPKR (mii, spp, nelems, spp_datatype);
	    break;
	case MII_DOUBLE:
	    MIIUPKD (mii, spp, nelems, spp_datatype);
	    break;
	default:
	    break;
	}
	return 0;
}
