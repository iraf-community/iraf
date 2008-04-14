/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_TTYCDES -- Close the TTY descriptor.
 */
/* tty : SPP pointer to descriptor */
void c_ttycdes ( void *tty )
{
	XPOINTER x_tty = (XPOINTER)tty;
	TTYCDES (&x_tty);
}
