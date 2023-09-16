/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

int
PUTCH (
    RCHAR *c,
    FINT  *fd
)
{
	register FILE *file;

	file = _fdtofile[*fd];
	putc(*c, file);
	return 0;
}
