#include "c_include.h"

/*
  Write an array of byte values into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int FSPPNB_U(XINT *ounit, XINT *group, XINT *felem, XINT *nelem, void *array,
	     XCHAR *nulval, XINT *status)
{
    char c_nulval = *nulval;
    FTPPNB_U(ounit, group, felem, nelem, array, &c_nulval, status,
	     MAX_INT, MAX_INT);
    *nulval = c_nulval;
    ZZEPRO();
    return 0;
}
