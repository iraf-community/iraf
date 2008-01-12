/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

/* r4tocstr.c */
extern void r4tocstr( RCHAR *, char *, size_t );

FINT OPEN( RCHAR *rname, FINT *mode )
{
	FILE *fp;
	char cname[FILENAMESIZE];

	r4tocstr(rname, cname, FILENAMESIZE);

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
