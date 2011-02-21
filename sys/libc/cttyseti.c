/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYSETI -- Set a TTY interface parameter of type integer.
*/
void
c_ttyseti (
  XINT	tty,			/* tty descriptor		*/
  int	param,			/* code of param to be set	*/
  int	value			/* value to be set		*/
)
{
	XINT  x_tty = tty, x_param = param, x_value = value;

	TTYSETI (&x_tty, &x_param, &x_value);
}
