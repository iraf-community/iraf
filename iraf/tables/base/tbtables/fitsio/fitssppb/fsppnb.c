#include "c_include.h"

/*
  Write an array of byte values into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int fsppnb_(XINT *ounit, XINT *group, XINT *felem, XINT *nelem, XINT *array,
	    XINT *nulval, XINT *status)
{
    ftppnb_(ounit, group, felem, nelem, array, nulval, status,
	    MAX_INT, MAX_INT);
    zzepro_();
    return 0;
}
