/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_KIMAPCHAN -- Map a KI channel number into the corresponding OS channel
** number or pid and node name.
*/
int
c_kimapchan (
  int	chan,			/* KI channel number		*/
  char	*nodename,		/* receives server node name	*/
  int	maxch			/* maxch chars out		*/
)
{
	XCHAR	x_nodename[SZ_FNAME+1];
	XINT	ki_chan = chan, x_maxch = SZ_FNAME;
	int	os_chan;

	os_chan = (int) KI_MAPCHAN (&ki_chan, x_nodename, &x_maxch);
	c_strpak (x_nodename, nodename, SZ_FNAME);

	return (os_chan);
}
