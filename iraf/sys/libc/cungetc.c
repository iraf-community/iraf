/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_UNGETC -- Push a character back into the input stream.  Pushback is last
 * in first out, i.e., the last character pushed back is the first one
 * read by GETC.  Characters (and strings) may be pushed back until the
 * FIO pushback buffer overflows.
 */
/* fd : file              */
/* ch : char to be pushed */
int c_ungetc ( int fd, int ch )
{
	XINT	x_fd = fd;
	XCHAR	xch = ch;

	iferr (UNGETC (&x_fd, &xch))
	    return (ERR);
	else
	    return (OK);
}
