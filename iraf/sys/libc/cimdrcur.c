/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_IMDRCUR -- Read the logical image cursor (temporary routine, will be
 * phased out in a later release).
 */
/* device  : logical device name or "stdimage"       */
/* x, y    : cursor coordinates (out)                */
/* wcs     : wcs of coords (out, = frame*100+d_wcs)  */
/* key     : keystroke which triggered read (out)    */
/* strval  : string value, if key=':'                */
/* bufsize : buffer size of strval                   */
/* d_wcs   : 0 for frame coords, 1 for image coords  */
/* pause   : true to pause for key to terminate read */
int c_imdrcur ( const char *device, float *x, float *y, int *wcs, int *key, 
		char *strval, size_t bufsize, int d_wcs, int pause )
{
	PKCHAR x_strval[SZ_LINE+1];
	XREAL x_x = *x;
	XREAL x_y = *y;
	XINT x_wcs = *wcs;
	XINT x_key = *key;
	XINT x_maxch = SZ_LINE;
	XINT x_d_wcs = d_wcs;
	XINT x_pause = pause;

	if (IMDRCUR (c_sppstr(device), &x_x, &x_y, &x_wcs, &x_key, x_strval, &x_maxch,
	    &x_d_wcs, &x_pause) >= 0)
	    c_strpak (x_strval, strval, bufsize);

	*x = x_x;
	*y = x_y;
	*wcs = x_wcs;
	*key = x_key;

	return (x_key = (x_key == XEOF) ? EOF : x_key);
}
