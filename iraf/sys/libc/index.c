/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_libc
#define import_spp
#include <iraf.h>

/* INDEX -- Search string STR for char CH, returning a pointer to the first
 * occurrence of CH in STR or NULL.
 */
/* str : string to be searched          */
/* ch  : character we are searching for */
char *index ( const char *str, int ch )
{
	const char *ip = str;

	do {
	    if (*ip == ch)
		return ((char *)ip);
	} while (*ip++);

	return (NULL);
}
