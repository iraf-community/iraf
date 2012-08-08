# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>
include	<gim.h>
include	<gki.h>

# GIM_REFRESHPIX -- Update any mappings defined upon the given region of
# the given source raster, as if the pixel values had been set with a write
# pixels call.

procedure gim_refreshpix (gp, raster, ct, x1, y1, width, height)

pointer	gp			#I graphics descriptor
int	raster			#I raster number (0 is display window)
int	ct			#I coordinate type
real	x1, y1			#I region to be refreshed
real	width, height		#I region to be refreshed

short	gim[GIM_REFRESHPIXELS_LEN]

begin
	gim[GIM_REFRESHPIXELS_RN] = raster
	gim[GIM_REFRESHPIXELS_CT] = ct

        if (ct == CT_PIXEL) {
            gim[GIM_REFRESHPIXELS_X1] = x1
            gim[GIM_REFRESHPIXELS_Y1] = y1
            gim[GIM_REFRESHPIXELS_NX] = width
            gim[GIM_REFRESHPIXELS_NY] = height
        } else {
            gim[GIM_REFRESHPIXELS_X1] = x1 * GKI_MAXNDC
            gim[GIM_REFRESHPIXELS_Y1] = y1 * GKI_MAXNDC
            gim[GIM_REFRESHPIXELS_NX] = nint (width * GKI_MAXNDC)
            gim[GIM_REFRESHPIXELS_NY] = nint (height * GKI_MAXNDC)
        }

	call gescape (gp, GIM_REFRESHPIXELS, gim, GIM_REFRESHPIXELS_LEN)
end
