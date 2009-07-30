#include "c_include.h"

/*
  Write an array of byte values into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int FSPPRB_U(XINT *ounit, XINT *group, XINT *felem, XINT *nelem, void *array,
	     XINT *status)
{
    FTPPRB_U(ounit, group, felem, nelem, array, status, MAX_INT);
    ZZEPRO();
    return 0;
}
