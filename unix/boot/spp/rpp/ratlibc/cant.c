/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

CANT(rname)
register RCHAR *rname;
{
	while (*rname != REOS)
		putc(*rname++, stderr);
	fprintf(stderr, ": cant open\n");
	ENDST();
}
