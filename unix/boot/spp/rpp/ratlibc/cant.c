#include "ratdef.h"

CANT(rname)
register RCHAR *rname;
{
	while (*rname != REOS)
		putc(*rname++, stderr);
	fprintf(stderr, ": cant open\n");
	ENDST();
}
