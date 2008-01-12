/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

/* PUTS -- Put a null terminated string to the standard output, followed by a
 * newline.
 */
/* str : input string */
int puts ( const char *str )
{
	FILE *fp = stdout;
	const char *ip;

	for (ip=str;  *ip != EOS;  ip++)
	    putc (*ip, fp);
	return (putc ('\n', fp));
}
