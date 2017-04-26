# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	<gescape.h>

# GIM_WRITEPIXELS -- Write to a rectangular region of a raster.

procedure gim_writepixels (gp, raster, data, nbits, x1, y1, nx, ny)

pointer	gp			#I graphics descriptor
int	raster			#I raster number (0 is display window)
short	data[ARB]		#I output pixel data
int	nbits			#I nbits per raster pixel (1,8,16,32)
int	x1, y1			#I first pixel to be written
int	nx, ny			#I size of region to be written

int	npix, nchars, nwords
short	gim[GIM_WRITEPIXELS_LEN]
errchk	gpl_flush, gpl_cache
include "../gpl.com"

begin
        # Flush any buffered polyline output.  Make sure the wcs transformation
        # in the cache is up to date.

        if (op > 1)
            call gpl_flush()
        else if (gp != gp_out || GP_WCS(gp) != wcs)
            call gpl_cache (gp)

	# Output the writepixels escape.
	npix = nx * ny
	nchars = (npix * nbits / NBITS_BYTE + SZB_CHAR-1) / SZB_CHAR
	nwords = (nchars + SZ_SHORT-1) / SZ_SHORT

	gim[GIM_WRITEPIXELS_RN] = raster
	gim[GIM_WRITEPIXELS_EC] = 0
	gim[GIM_WRITEPIXELS_X1] = x1
	gim[GIM_WRITEPIXELS_Y1] = y1
	gim[GIM_WRITEPIXELS_NX] = nx
	gim[GIM_WRITEPIXELS_NY] = ny
	gim[GIM_WRITEPIXELS_BP] = nbits

	call gki_wescape (GP_FD(gp), GIM_WRITEPIXELS,
	    gim, GIM_WRITEPIXELS_LEN, data, nwords)
end
