# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_RASTERINIT -- Initialize the Gterm widget imaging subsystem.  Destroys
# any existing rasters, mappings, and dynamic colors.

procedure gim_rasterinit (gp)

pointer	gp			#I graphics descriptor

begin
	call gescape (gp, GIM_RASTERINIT, 0, GIM_RASTERINIT_LEN)
end
