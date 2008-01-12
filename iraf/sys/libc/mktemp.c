/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#include <iraf.h>

/* MKTEMP -- Make a unique temporary file name.  This procedure is syntactically
 * equivalent to the UNIX procedure of the same name, but the XXXXXX are not
 * required in the input filename.
 */
/* template : root filename, e.g., "tmp$xx" */
char *mktemp ( char *template )
{
	static	char unique[SZ_FNAME];

	if (c_mktemp (template, unique, SZ_FNAME) > 0) {
	    strcpy (template, unique);
	    return (unique);
	} else
	    return (NULL);
}
