/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* REWIND -- Position the named stream to beginning of file, i.e., arrange
** for the next read or write to read or write the first byte of the file.
*/
long
rewind (
  FILE	*fp			/* operand file				*/
)
{
	return ((long) fseek (fp, 0L, 0));
}
