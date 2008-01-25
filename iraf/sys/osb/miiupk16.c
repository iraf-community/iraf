/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#define import_endian
#include <iraf.h>

/*
  MIIUPK16 -- Unpack a 16 bit signed MII array into an SPP array of the
  indicated datatype.

  mii[]        : #I input MII format array
  spp[]        : #O output SPP format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

int MIIUPK16 ( void *mii, void *spp, XINT *nelems, XINT *spp_datatype )
{
	if (BYTE_SWAP == YES) {
	    XSHORT buf[1] = { 0 };
	    unsigned char *buf_ptr = (unsigned char *)buf;
	    unsigned char (*mii_ptr)[2] = (unsigned char (*)[2])mii;
	    XINT c_1 = 1;
	    XINT x_n, i;
	    switch ( *spp_datatype ) {
	    case TY_UBYTE:	/* large to small */
		for ( i=0 ; i < *nelems ; i++ ) {
		    buf_ptr[0] = mii_ptr[i][1];
		    buf_ptr[1] = mii_ptr[i][0];
		    ((XUBYTE *)spp)[i] = (XUBYTE)(buf[0]);
		}
		break;
	    default:		/* small to large */
		x_n = *nelems * 2;
		BSWAP2 (mii, &c_1, spp, &c_1, &x_n);
		ACHTS ((XSHORT *)spp, spp, nelems, spp_datatype);
		break;
	    }
	}
	else {
	    ACHTS ((XSHORT *)mii, spp, nelems, spp_datatype);
	}

	return 0;
}
