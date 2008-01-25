/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
  MIIUPK8 -- Unpack an 8 bit unsigned MII array into an SPP array of the
  indicated datatype.

  mii[]        : #I input MII format array
  spp[]        : #O output SPP format array
  nelems       : #I number of integers to be converted
  spp_datatype : #I SPP datatype code
*/

int MIIUPK8 ( void *mii, void *spp, XINT *nelems, XINT *spp_datatype )
{
	ACHTB ((XUBYTE *)mii, spp, nelems, spp_datatype);
	return 0;
}
