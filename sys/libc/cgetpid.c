/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_libc
#define import_xnames
#include <iraf.h>


/* C_GETPID -- Get the process id.
*/
int
c_getpid ( void )
{
	return ((int) GETPID());
}
