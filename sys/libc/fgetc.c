/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* FGETC -- Get a character from the input file.  Offered as a functionally
** equivalent alternative to the macro GETC.
*/
int
fgetc (
  FILE	*fp
)
{
	return (getc (fp));
}
