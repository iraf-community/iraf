/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_ERROR -- Post an error.
*/
void
c_error (
  int	errcode,		/* error code			*/
  char	*errmsg			/* error message		*/
)
{
	XINT  x_errcode = errcode;
	ERROR (&x_errcode, c_sppstr(errmsg));
}
