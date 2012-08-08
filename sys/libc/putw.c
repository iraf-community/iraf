/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>

/* PUTW -- Put a word (integer) to the output stream.  When used in conjunction
** with GETW this permits storage and retrieval of binary words to any file,
** albeit somewhat inefficiently.
*/
int
putw (
  int	word,			/* data word to be output	*/
  FILE	*fp			/* output file			*/
)
{
	register char	*ip;
	register int	n = sizeof (int);


	for (ip=(char *)&word;  --n >= 0;  ip++)
	    putc (*ip, fp);

	return (ferror(fp) ? EOF : word);
}
