#include "c_include.h"

/*
  Write an array of byte values into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int fspprb_(XINT *ounit, XINT *group, XINT *felem, XINT *nelem, XINT *array,
	    XINT *status)
{
    ftpprb_(ounit, group, felem, nelem, array, status, MAX_INT);
    zzepro_();
    return 0;
}
