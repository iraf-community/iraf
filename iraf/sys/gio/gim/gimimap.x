# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_INITMAPPINGS -- Initialize the Gterm widget raster mappings.

procedure gim_initmappings (gp)

pointer	gp			#I graphics descriptor

short	c_0

begin
	c_0 = 0
	call gescape (gp, GIM_INITMAPPINGS, c_0, GIM_INITMAPPINGS_LEN)
end
