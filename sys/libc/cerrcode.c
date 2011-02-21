/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_ERRCODE -- Get the error code of the most recent error.
*/
int
c_errcode ( void )
{
	return (ERRCODE());
}
