# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	<fset.h>
include	"../lib/ids.h"

# IDS_POINT -- Plot a point in the current plane at given (GKI) coordinates.

procedure ids_point (ax,ay,flag)

short	ax,ay			# point coordinates, GKI
bool	flag			# true if should plot point, false if just a
				# pen move
int	xp, yp
int	bufsize
int	fstati()

include	"../lib/ids.com"

begin
	# convert to device coords, plot max value, then record in i_pt
	xp = real(ax) * i_xres /(GKI_MAXNDC+1)
	yp = real(ay) * i_yres /(GKI_MAXNDC+1)

	# if flag is true, we plot the point.  If false, we just want
	# to record the points (a pen move), so skip the plot commands

	if (flag) {
	    # set buffer to size one
	    bufsize = fstati (i_out, F_BUFSIZE)
	    call fseti (i_out, F_BUFSIZE, 1)

	    # plot it
	    call zseek (i_out, xp, yp)
	    call write(i_out, short(IDS_ZRES(i_kt)-1), 1)

	    # restore buffer
	    call fseti (i_out, F_BUFSIZE, bufsize)
	}
	i_pt_x = xp
	i_pt_y = yp
end


# IDS_RPOINT - Plot a point in the current plane at given (device coord) offsets
# from current point.

procedure ids_rpoint (dx,dy)

short	dx,dy			# DEVICE coordinate increments from cur. pos.

int	xp, yp

include	"../lib/ids.com"

begin
	xp = i_pt_x + dx
	yp = i_pt_y + dy

	call zseek (i_out, xp, yp)
	call write(i_out, short(IDS_ZRES(i_kt)-1), 1)

	i_pt_x = xp
	i_pt_y = yp
end
