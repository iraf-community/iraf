#include "c_include.h"

/*
  Write a 2-d image of byte values into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int FSP2DB_U(XINT *ounit, XINT *group, XINT *dim1, XINT *nx, XINT *ny,
	     XINT *array, XINT *status)
{
    FTP2DB_U(ounit, group, dim1, nx, ny, array, status, MAX_INT);
    ZZEPRO();
    return 0;
}
