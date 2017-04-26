# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_DESTROYRASTER -- Destroy a raster.

procedure gim_destroyraster (gp, raster)

pointer	gp			#I graphics descriptor
int	raster			#I raster number (0 is display window)

short	gim[GIM_DESTROYRASTER_LEN]

begin
	gim[GIM_DESTROYRASTER_RN] = raster
	call gescape (gp, GIM_DESTROYRASTER, gim, GIM_DESTROYRASTER_LEN)
end
