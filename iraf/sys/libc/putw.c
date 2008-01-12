/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

/* PUTW -- Put a word (integer) to the output stream.  When used in conjunction
 * with GETW this permits storage and retrieval of binary words to any file,
 * albeit somewhat inefficiently.
 */
/* word : data word to be output */
/* fp   : output file            */
int putw ( int word, FILE *fp )
{
	char *ip = (char *)&word;
	size_t n;

	for ( n=0 ; n < sizeof (int) ; n++ )
	    putc (ip[n], fp);

	return (ferror(fp) ? EOF : word);
}
