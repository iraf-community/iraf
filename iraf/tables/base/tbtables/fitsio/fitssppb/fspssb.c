#include "c_include.h"

/*
  Write a subsection of byte values to the primary array.
  A subsection is defined to be any contiguous rectangular
  array of pixels within the n-dimensional FITS data file.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
*/

int FSPSSB_U(XINT *iunit, XINT *group, XINT *naxis, XINT *naxes, XINT *fpixel,
	     XINT *lpixel, void *array, XINT *status)
{
    FTPSSB_U(iunit, group, naxis, naxes, fpixel, lpixel, array, status,
	     MAX_INT);
    ZZEPRO();
    return 0;
}
