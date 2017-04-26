# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>
include	<gim.h>
include	<gki.h>

# GIM_SETPIX -- Set the pixels in a region of a raster to a solid color.
# If width=height=0 the entire raster will be written.

procedure gim_setpix (gp, raster, ct, x1, y1, width, height, color, rop)

pointer	gp			#I graphics descriptor
int	raster			#I raster number (0 is display window)
int	ct			#I coordinate type
real	x1, y1			#I region to be refreshed
real	width, height		#I region to be refreshed
int	color			#I pixel value
int	rop			#I rasterop

short	gim[GIM_SETPIXELS_LEN]

begin
	gim[GIM_SETPIXELS_RN] = raster
	gim[GIM_SETPIXELS_CT] = ct
	gim[GIM_SETPIXELS_CO] = color
	gim[GIM_SETPIXELS_OP] = rop

        if (ct == CT_PIXEL) {
            gim[GIM_SETPIXELS_X1] = x1
            gim[GIM_SETPIXELS_Y1] = y1
            gim[GIM_SETPIXELS_NX] = width
            gim[GIM_SETPIXELS_NY] = height
        } else {
            gim[GIM_SETPIXELS_X1] = x1 * GKI_MAXNDC
            gim[GIM_SETPIXELS_Y1] = y1 * GKI_MAXNDC
            gim[GIM_SETPIXELS_NX] = nint (width * GKI_MAXNDC)
            gim[GIM_SETPIXELS_NY] = nint (height * GKI_MAXNDC)
        }

	call gescape (gp, GIM_SETPIXELS, gim, GIM_SETPIXELS_LEN)
end
