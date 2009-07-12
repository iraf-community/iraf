#include "c_include.h"

/*
  Write an array of group parmeters into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int fspgpb_(XINT *ounit, XINT *group, XINT *fparm, XINT *nparm, XINT *array,
	    XINT *status)
{
    ftpgpb_(ounit, group, fparm, nparm, array, status, MAX_INT);
    zzepro_();
    return 0;
}
