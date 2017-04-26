/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

void PUTLIN(line, fd)
RCHAR *line;
FINT	*fd;
{
	register FILE *fp;
	register int c;

	fp = _fdtofile[*fd];
	while((c = *line++) != REOS)
		putc(c, fp);
}
