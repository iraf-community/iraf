#include <f2c/sys/sys.h>
#include "c_proto.h"
#include "../f2c_proto.h"

int FTPCBF_U(XINT *iunit, XINT *convrt, XINT *nbytes, char *array,
	     XINT *status, ftnlen array_len)
{
    FTPBYT_U(iunit, nbytes, (XCHAR *)array, status);
    ZZEPRO();
    return 0;
}
