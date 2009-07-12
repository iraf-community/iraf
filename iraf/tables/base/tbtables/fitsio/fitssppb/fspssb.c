#include "c_include.h"

/*
  Write a subsection of byte values to the primary array.
  A subsection is defined to be any contiguous rectangular
  array of pixels within the n-dimensional FITS data file.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
*/

int fspssb_(XINT *iunit, XINT *group, XINT *naxis, XINT *naxes, XINT *fpixel,
	    XINT *lpixel, XINT *array, XINT *status)
{
    ftpssb_(iunit, group, naxis, naxes, fpixel, lpixel, array, status,
	    MAX_INT);
    zzepro_();
    return 0;
}
