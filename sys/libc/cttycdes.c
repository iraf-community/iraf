/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_TTYCDES -- Close the TTY descriptor.
*/
void
c_ttycdes (
  XINT	tty			/* SPP pointer to descriptor	*/
)
{
	XINT  x_tty = tty;

	TTYCDES (&x_tty);
}
