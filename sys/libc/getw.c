/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* GETW -- Get a word (integer) from the input stream.  When used in conjunction
** with PUTW this permits storage and retrieval of binary words in any file,
** albeit somewhat inefficiently.
*/
int
getw (
  FILE	*fp				/* input file			*/
)
{
	int	word;
	register char	*op = (char *)&word;
	register int	n = sizeof (int);


	while (--n >= 0)
	    *op++ = getc (fp);

	return ((fp->_fflags & (_FEOF|_FERR)) ? EOF : word);
}
