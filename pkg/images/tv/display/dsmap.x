# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imset.h>
include <fset.h>

# DSMAP -- Map the display, i.e., open the display device as an imagefile.

pointer procedure dsmap (frame, mode, color, chan)

int	frame
int	mode
int	color
int	chan[ARB]

pointer	ds
char	device[SZ_FNAME]

int	imstati(), fstati(), envgets(), imdopen()
extern	imdopen()
pointer	imdmap()
errchk	imdmap

begin
	if (envgets ("stdimage", device, SZ_FNAME) == 0)
	    call error (1, "variable `stdimage' not defined in environment")

	ds = imdmap (device, mode, imdopen)
	chan[1] = fstati (imstati (ds, IM_PIXFD), F_CHANNEL)
	chan[2] = color

	return (ds)
end
