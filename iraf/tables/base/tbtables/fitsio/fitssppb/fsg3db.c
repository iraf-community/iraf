#include "c_include.h"

/*
  Read a 3-d cube of byte values from the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
*/

int FSG3DB_U(XINT *ounit, XINT *group, XINT *nulval, XINT *dim1, XINT *dim2,
	     XINT *nx, XINT *ny, XINT *nz, XINT *array, XBOOL *anyflg,
	     XINT *status)
{
    FTG3DB_U(ounit, group, nulval, dim1, dim2, nx, ny, nz, array, anyflg,
	     status, MAX_INT, MAX_INT);
    ZZEPRO();
    return 0;
}
