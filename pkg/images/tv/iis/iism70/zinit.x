# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"../lib/ids.h"
include "iis.h"

# ZINIT -- initialize for IIS operation
# in general case, would use nfr and ngr to determine maximum file size
# which would encompass all the images and graphics planes and all the
# devices too.  Then, file mapped i/o could move most of the device indep.
# code to the reading and writing routines.
# not done for IIS

procedure zinit (nfr, ngr, filesize)

short	nfr			# maximum number of image frames
short	ngr			# maximum number of graphics bit planes
long	filesize		# returned value

short	pl[IDS_MAXIMPL+2]
short	zm[4]

include "../lib/ids.com"
include	"iis.com"

begin
	i_snap = false
	# we have no place to store all the zoom and scroll information.
	# so we initialize to zoom = 1 and scroll = center for all planes
	pl[1] = IDS_EOD
	call ids_expand(pl, i_maxframes, true)
	zm[1] = 1
	zm[2] = IIS_XCEN * MCXSCALE
	zm[3] = IIS_YCEN * MCYSCALE
	zm[4] = IDS_EOD
	call iiszoom(short(IDS_WRITE), pl, short(4), zm)
	call iisscroll(short(IDS_WRITE), pl, short(3), zm[2])

	# We also need to set the i_frame_on variable (iis.com), which
	# we do with a "trick":  We call zdisplay_i with quad == EOD;
	# this is a "nop" for the display code, but will set the variable.

	call zdisplay_i (short(IDS_ON), short(IDS_EOD), short(IDS_EOD),
			short(IDS_EOD))
end
