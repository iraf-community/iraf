/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* PUTS -- Put a null terminated string to the standard output, followed by a
** newline.
*/
int
puts (
  char	*str			/* input string			*/
)
{
	register FILE	*fp = stdout;
	register char	*ip;


	for (ip=str;  *ip != EOS;  ip++)
	    putc (*ip, fp);
	return (putc ('\n', fp));
}
