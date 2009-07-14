#include "c_include.h"

/*
  Write an array of group parmeters into the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being written).
*/

int FSPGPB_U(XINT *ounit, XINT *group, XINT *fparm, XINT *nparm, XINT *array,
	     XINT *status)
{
    FTPGPB_U(ounit, group, fparm, nparm, array, status, MAX_INT);
    ZZEPRO();
    return 0;
}
