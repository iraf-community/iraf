#include "c_include.h"

/*
  Read a 2-d image of byte values from the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
*/

int fsg2db_(XINT *ounit, XINT *group, XINT *nulval, XINT *dim1, XINT *nx,
	    XINT *ny, XINT *array, XBOOL *anyflg, XINT *status)
{
    ftg2db_(ounit, group, nulval, dim1, nx, ny, array, anyflg, status,
	    MAX_INT, MAX_INT);
    zzepro_();
    return 0;
}
