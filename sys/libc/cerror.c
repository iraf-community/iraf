/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_ERROR -- Post an error.
 */
c_error (errcode, errmsg)
int	errcode;		/* error code			*/
char	*errmsg;		/* error message		*/
{
	ERROR (&errcode, c_sppstr(errmsg));
}
