/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* C_IMDRCUR -- Read the logical image cursor (temporary routine, will be
 * phased out in a later release).
 */
c_imdrcur (device, x, y, wcs, key, strval, maxch, d_wcs, pause)
char	*device;		/* logical device name or "stdimage" */
float	*x, *y;			/* cursor coordinates (out) */
int	*wcs;			/* wcs of coords (out, = frame*100+d_wcs) */
int	*key;			/* keystroke which triggered read (out) */
char	*strval;		/* string value, if key=':' */
int	maxch;			/* max chars out */
int	d_wcs;			/* 0 for frame coords, 1 for image coords */
int	pause;			/* true to pause for key to terminate read */
{
	PKCHAR	x_strval[SZ_LINE+1];

	if (IMDRCUR (c_sppstr(device), x, y, wcs, key, x_strval, &maxch,
	    &d_wcs, &pause) >= 0)
	    c_strpak (x_strval, strval, maxch);

	return (*key = (*key == XEOF) ? EOF : *key);
}
