/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_MKTEMP -- FIO make temporary (unique) filename.
*/
int
c_mktemp (
  char	*root,			/* root filename		*/
  char	*temp_filename,		/* generated filename		*/
  int	maxch			/* max chars in output filename	*/
)
{
	XCHAR	temp[SZ_FNAME+1];
	XINT	sz_temp = SZ_FNAME;


	iferr (MKTEMP (c_sppstr(root), temp, &sz_temp))
	    return (0);
	else
	    return (strlen (c_strpak (temp, temp_filename, maxch)));
}
