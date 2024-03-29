/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

extern void r4tocstr (RCHAR *rstr, char *cstr);

FINT
OPEN(RCHAR *rname, FINT *mode)
{
	register FILE  *fp;
	char	       cname[FILENAMESIZE];

	r4tocstr(rname, cname);

	if (*mode == APPEND)
		fp = fopen(cname, "a");
	else if (*mode == READWRITE || *mode == WRITE)
		fp = fopen(cname, "w");
	else
		fp = fopen(cname, "r");

	if (fp == NULL)
		return(RERR);	/* unable to open file */

	_fdtofile[fileno(fp)] = fp;
	return(fileno(fp));
}
