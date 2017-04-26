/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#define	import_spp
#include <iraf.h>


/* INDEX -- Search string STR for char CH, returning a pointer to the first
** occurrence of CH in STR or NULL.
*/
char *
index (
  char	*str,			/* string to be searched		*/
  int	ch			/* character we are searching for	*/
)
{
	register char	*ip = str;

	do {
	    if (*ip == ch)
		return (ip);
	} while (*ip++);

	return ((char *) NULL);
}
