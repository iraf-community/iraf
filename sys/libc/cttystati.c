/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYSTATI -- Return the value of a TTY integer interface parameter.
 */
c_ttystati (tty, param)
int	tty;			/* tty descriptor		*/
int	param;			/* code of param to be set	*/
{
	return (TTYSTATI (&tty, &param));
}
