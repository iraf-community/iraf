# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	<gio.h>
include	<fset.h>
include	<gescape.h>

# GIM_READPIXELS -- Read from a rectangular region of a raster.

procedure gim_readpixels (gp, raster, data, nbits, x1, y1, nx, ny)

pointer	gp			#I graphics descriptor
int	raster			#I raster number (0 is display window)
short	data[ARB]		#O returned pixel data
int	nbits			#I nbits per raster pixel (1,8,16,32)
int	x1, y1			#I first pixel to be written
int	nx, ny			#I size of region to be written

int	npix, nchars, nwords
short	gim[GIM_READPIXELS_LEN]
short	retval[GIM_RET_RPIX_LEN]
errchk	gpl_flush, gflush, read, syserrs
string	s_readpixels "gim_readpixels"
int	read()

begin
	call gpl_flush()
	npix = nx * ny
	nchars = (npix * nbits / NBITS_BYTE + SZB_CHAR-1) / SZB_CHAR
	nwords = (nchars + SZ_SHORT-1) / SZ_SHORT

	gim[GIM_READPIXELS_RN] = raster
	gim[GIM_READPIXELS_EC] = 0
	gim[GIM_READPIXELS_X1] = x1
	gim[GIM_READPIXELS_Y1] = y1
	gim[GIM_READPIXELS_NX] = nx
	gim[GIM_READPIXELS_NY] = ny
	gim[GIM_READPIXELS_BP] = nbits

	call gki_escape (gp, GIM_READPIXELS, gim, GIM_READPIXELS_LEN)
	call flush (GP_FD(gp))

        # Get return value instruction header.
        nchars = GIM_RET_RPIX_LEN * SZ_SHORT
        if (read (GP_FD(gp), retval, nchars) != nchars) {
	    call fseti (GP_FD(gp), F_CANCEL, OK)
            call syserrs (SYS_FREAD, s_readpixels)
	}

	# Get the pixel data.
        npix = retval[GIM_RET_RPIX_NP]
	nchars = (npix * nbits / NBITS_BYTE + SZB_CHAR-1) / SZB_CHAR
        if (read (GP_FD(gp), data, nchars) != nchars) {
	    call fseti (GP_FD(gp), F_CANCEL, OK)
            call syserrs (SYS_FREAD, s_readpixels)
	}

	call fseti (GP_FD(gp), F_CANCEL, OK)
	if (npix != nx * ny)
            call syserrs (SYS_IMNOPIX, s_readpixels)
end
