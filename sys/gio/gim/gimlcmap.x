# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>
include	<mach.h>
include	<gim.h>
include	<gki.h>

# GIM_LOADCOLORMAP -- Load a colormap into the display (hardware) colormap.
# Any number of colormaps may be defined, but only one may be loaded at a
# time.  A linear transformation may optionally be applied to the (normalized)
# colormap when it is loaded.  Set offset=0.5, slope=1.0 to load the colormap
# without scaling.  A negative slope inverts the image.
#
# The offset refers to the center of the mapped region of the transfer
# function, which is why the center value is at 0.5.  For example, if the
# range of raster pixel intensities is normalized to the range 0.0 to 1.0,
# then a transfer function of [offset=0.3,slope=3.0] will display the region
# of intenstities centered around the normalized intenstity of 0.3, with a
# contrast of 3.0 (the screen intensity changes 3 units for a unit change in
# raster pixel intensity).  The transfer function [offset=0.3,slope=-3.0]
# will display the same range of pixel intensitites, but with a negative
# contrast.  The transfer function [offset=0.5,slope=1.0] has intercepts
# of [0,0] and [1,1] hence it displays the full range of raster pixel
# intensities - the input colormap is used as is, without resampling.

procedure gim_loadcolormap (gp, colormap, offset, slope)

pointer	gp			#I graphics descriptor
int	colormap		#I colormap number (0 is display colormap)
real	offset, slope		#I linear transformation on colormap

real	veclen, scale
short	gim[GIM_LOADCMAP_LEN]

begin
	scale = GIM_LOADCMAP_SCALE
	gim[GIM_LOADCMAP_MP] = colormap
	gim[GIM_LOADCMAP_OF] = ((GKI_MAXNDC + 1) / scale) *
	    max(-scale, min(scale, offset))

	if (abs(slope) < EPSILONR)
	    veclen = GKI_MAXNDC
	else {
	    veclen = GKI_MAXNDC + 1
	    veclen = min (veclen / 2, veclen / abs(slope) / 2)
	}
	gim[GIM_LOADCMAP_DX] = veclen
	gim[GIM_LOADCMAP_DY] = veclen * slope

	call gescape (gp, GIM_LOADCMAP, gim, GIM_LOADCMAP_LEN)
end
