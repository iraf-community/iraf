/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

PUTLIN(line, fd)
register RCHAR *line;
FINT	*fd;
{
	register FILE *fp;
	register c;

	fp = _fdtofile[*fd];
	while((c = *line++) != REOS)
		putc(c, fp);
}
