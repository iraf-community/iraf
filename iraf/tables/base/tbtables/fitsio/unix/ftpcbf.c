#include <f2c/sys/sys.h>
#include "../f2c_proto.h"

int ftpcbf_(XINT *iunit, XINT *convrt, XINT *nbytes, char *array,
	    XINT *status, ftnlen array_len)
{
    ftpbyt_(iunit, nbytes, array, status);
    zzepro_();
    return 0;
}
