/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>

/* FPUTS -- Put a null terminated string to the output file.
 */
fputs (str, fp)
char	*str;			/* input string			*/
register FILE	*fp;		/* output file			*/
{
	register char *ip;

	for (ip=str;  *ip != EOS;  ip++)
	    putc (*ip, fp);
}
