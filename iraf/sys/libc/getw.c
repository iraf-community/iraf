/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

/* GETW -- Get a word (integer) from the input stream.  When used in conjunction
 * with PUTW this permits storage and retrieval of binary words in any file,
 * albeit somewhat inefficiently.
 */
/* fp : input file */
int getw ( FILE *fp )
{
	int word, n;
	char *op = (char *)&word;

	for ( n=0 ; n < sizeof (int) ; n++ )
	    op[n] = getc (fp);

	return ((fp->_fflags & (_FEOF|_FERR)) ? EOF : word);
}
