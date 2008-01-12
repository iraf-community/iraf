/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_MKTEMP -- FIO make temporary (unique) filename.
 */
/* root          : root filename                */
/* temp_filename : generated filename           */
/* bufsize       : buffer size of temp_filename */
int c_mktemp ( const char *root, char *temp_filename, size_t bufsize )
{
	XCHAR	temp[SZ_FNAME+1];
	XINT	sz_temp = SZ_FNAME;

	iferr (MKTEMP (c_sppstr(root), temp, &sz_temp))
	    return (0);
	else
	    return (strlen (c_strpak (temp, temp_filename, bufsize)));
}
