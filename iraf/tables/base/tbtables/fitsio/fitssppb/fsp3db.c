#include "c_include.h"

/*
  Write a 3-d cube of byte values into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int FSP3DB_U(XINT *ounit, XINT *group, XINT *dim1, XINT *dim2, XINT *nx,
	     XINT *ny, XINT *nz, XINT *array, XINT *status)
{
    FTP3DB_U(ounit, group, dim1, dim2, nx, ny, nz, array, status, MAX_INT);
    ZZEPRO();
    return 0;
}
