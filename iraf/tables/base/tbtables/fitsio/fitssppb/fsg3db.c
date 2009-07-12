#include "c_include.h"

/*
  Read a 3-d cube of byte values from the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
*/

int fsg3db_(XINT *ounit, XINT *group, XINT *nulval, XINT *dim1, XINT *dim2,
	    XINT *nx, XINT *ny, XINT *nz, XINT *array, XBOOL *anyflg,
	    XINT *status)
{
    ftg3db_(ounit, group, nulval, dim1, dim2, nx, ny, nz, array, anyflg,
	    status, MAX_INT, MAX_INT);
    zzepro_();
    return 0;
}
