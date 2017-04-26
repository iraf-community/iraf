# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gki.h>
include	"gtr.h"

# GTRSET -- Set the workstation transformation.  The workstation transformation
# is automatically zeroed whenever the screen is cleared or when a workstation
# is opened.

procedure gtrset (fd, x1, x2, y1, y2)

int	fd			# graphics stream to be set
real	x1, x2			# range of workstation viewport in X
real	y1, y2			# range of workstation viewport in Y
include	"gtr.com"

begin
	mx1 = x1 * GKI_MAXNDC
	mx2 = x2 * GKI_MAXNDC
	my1 = y1 * GKI_MAXNDC
	my2 = y2 * GKI_MAXNDC

	xscale = GKI_MAXNDC / (mx2 - mx1)
	yscale = GKI_MAXNDC / (my2 - my1)

	wstranset = YES
end
