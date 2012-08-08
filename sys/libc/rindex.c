/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_libc
#define	import_spp
#include <iraf.h>


/* RINDEX -- Search string STR for char CH, returning a pointer to the last
** occurrence of CH in STR or NULL.
*/
char *
rindex (
  char	*str,			/* string to be searched		*/
  int	ch			/* character we are searching for	*/
)
{
	register char	*ip = str;
	register char	*last = NULL;

	do {
	    if (*ip == ch)
		last = ip;
	} while (*ip++);

	return (last);
}
