/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#include <iraf.h>


/* FTELL -- Get the FSEEK offset of the current position in a file, i.e.,
** the file offset at which the next read or write will occur.  For a text
** file this is a magic number, for a binary file it is the zero-indexed
** offset in bytes from the beginning of the file.
*/
long
ftell (
  FILE	*fp			/* operand file				*/
)
{
	return (c_note (fileno(fp)));
}
