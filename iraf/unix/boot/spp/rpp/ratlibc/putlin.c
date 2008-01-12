/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

void PUTLIN( RCHAR *line, FINT *fd )
{
	FILE *fp;
	int c;

	fp = _fdtofile[*fd];
	while((c = *line++) != REOS)
		putc(c, fp);
}
