# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_CREATERASTER -- Create, recreate, or resize a raster.

procedure gim_createraster (gp, raster, type, width, height, depth)

pointer	gp			#I graphics descriptor
int	raster			#I raster number (0 is display window)
int	type			#I raster type (normal,ximage,pixmap)
int	width			#I raster width in pixels
int	height			#I raster height in pixels
int	depth			#I raster depth, bits per pixel

short	gim[GIM_CREATERASTER_LEN]

begin
	gim[GIM_CREATERASTER_RN] = raster
	gim[GIM_CREATERASTER_RT] = type
	gim[GIM_CREATERASTER_NX] = width
	gim[GIM_CREATERASTER_NY] = height
	gim[GIM_CREATERASTER_BP] = depth

	call gescape (gp, GIM_CREATERASTER, gim, GIM_CREATERASTER_LEN)
end
