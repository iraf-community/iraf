/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYSETI -- Set a TTY interface parameter of type integer.
 */
c_ttyseti (tty, param, value)
int	tty;			/* tty descriptor		*/
int	param;			/* code of param to be set	*/
int	value;			/* value to be set		*/
{
	TTYSETI (&tty, &param, &value);
}
