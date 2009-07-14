#include "c_include.h"

/*
  Read a 2-d image of byte values from the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
*/

int FSG2DB_U(XINT *ounit, XINT *group, XINT *nulval, XINT *dim1, XINT *nx,
	     XINT *ny, XINT *array, XBOOL *anyflg, XINT *status)
{
    FTG2DB_U(ounit, group, nulval, dim1, nx, ny, array, anyflg, status,
	     MAX_INT, MAX_INT);
    ZZEPRO();
    return 0;
}
