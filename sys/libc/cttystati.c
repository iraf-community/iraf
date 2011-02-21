/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYSTATI -- Return the value of a TTY integer interface parameter.
*/
XINT
c_ttystati (
  XINT	tty,			/* tty descriptor		*/
  int	param			/* code of param to be set	*/
)
{
	XINT  x_tty = tty, x_param = param;

	return (TTYSTATI (&x_tty, &x_param));
}
