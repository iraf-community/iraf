/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* C_KIMAPCHAN -- Map a KI channel number into the corresponding OS channel
 * number or pid and node name.
 */
/* chan     : KI channel number         */
/* nodename : receives server node name */
/* bufsize  : buffer size of nodename   */
int c_kimapchan ( int chan, char *nodename, size_t bufsize )
{
	XCHAR	x_nodename[SZ_FNAME+1];
	XINT	ki_chan = chan, x_maxch = SZ_FNAME;
	int	os_chan;

	os_chan = KI_MAPCHAN (&ki_chan, x_nodename, &x_maxch);
	c_strpak (x_nodename, nodename, bufsize);

	return (os_chan);
}
