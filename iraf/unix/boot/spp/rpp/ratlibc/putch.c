/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

void PUTCH( RCHAR *c, FINT  *fd )
{
	FILE *file;

	file = _fdtofile[*fd];
	putc(*c, file);
	return;
}
