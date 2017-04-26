# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# GSPMCI -- Set polymarker colour index.  This function is currently
# implemented as setting the polymarker width, not color.

procedure gspmci (coli)

int	coli		# Polymarker colour index.

begin
	call gsawr (G_PMCOLOR, real (coli))
end
