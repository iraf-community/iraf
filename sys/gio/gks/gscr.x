# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GSCR -- Set color representation.  Currently implemented as a no-op.

procedure gscr (wkstation, color_index, rgb)

int	wkstation		# Workstation id
int	color_index
real	rgb[3]
include	"gks.com"

begin
	;
end
