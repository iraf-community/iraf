# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>

# GIM_SETRASTER -- Set the raster to be used as the coordinate system for
# graphics drawing operations.  A setraster for raster N causes subsequent
# drawing operations to be drawn using any raster-to-screen mappings defined
# for raster N.  A setraster to raster=0 restores the normal semantics of
# drawing directly to the screen with no additional transformations.
# Applications which use gim_setraster to draw graphics or text overlays on
# a raster should always restore the raster to zero when done so that
# subsequent drawing operations (e.g., in cursor mode) behave normally.
# The setraster is not reset automatically except in a screen clear.
#
# NOTE - Most applications should use gseti(gp,G_RASTER,n) instead of the
# lower level gim_setraster.

procedure gim_setraster (gp, raster)

pointer	gp			#I graphics descriptor
int	raster			#I raster number

short	gim[GIM_SETRASTER_LEN]

begin
	gim[GIM_SETRASTER_RN] = raster
	call gescape (gp, GIM_SETRASTER, gim, GIM_SETRASTER_LEN)
end
