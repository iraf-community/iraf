/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#define import_error
#include <iraf.h>

/* C_ERROR -- Post an error.
 */
/* errcode : error code    */
/* errmsg  : error message */
void c_error ( int errcode, const char *errmsg )
{
	XINT x_errcode = errcode;
	ERROR (&x_errcode, c_sppstr(errmsg));
}
