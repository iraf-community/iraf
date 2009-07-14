#include "c_include.h"

/*
  Read an array of group parameter values from the primary array.
  Data conversion and scaling will be performed if necessary
  (e.g, if the datatype of the FITS array is not the same
  as the array being read).
*/

int FSGGPB_U(XINT *iunit, XINT *group, XINT *fparm, XINT *nparm, XINT *array,
	     XINT *status)
{
    FTGGPB_U(iunit, group, fparm, nparm, array, status, MAX_INT);
    ZZEPRO();
    return 0;
}
