# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# GSPLC -- Set polyline colour index.  This function is currently 
# implemented as setting the polyline width, not color.

procedure gsplci (coli)

int	coli		# Polyline colour index

begin
	call gsawr (G_PLWIDTH, real (coli))
end
