# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gescape.h>
include	<fset.h>
include	<gio.h>

# GIM_QUERYRASTER -- Query a raster.  The function value (YES/NO) indicates
# whether or not the raster exists.  If the raster exists, the raster type
# and size are returned as output arguments.

int procedure gim_queryraster (gp, raster, type, width, height, depth)

pointer	gp			#I graphics descriptor
int	raster			#I raster number (0 is display window)
int	type			#O raster type (ximage,pixmap)
int	width			#O raster width in pixels
int	height			#O raster height in pixels
int	depth			#O raster depth, bits per pixel

int	nchars, nread
short	gim[GIM_QUERYRASTER_LEN]
short	retval[GIM_RET_QRAS_LEN]
errchk	gescape, flush, read, syserrs
int	read()

begin
	call gpl_flush()
	gim[GIM_QUERYRASTER_RN] = raster
	call gescape (gp, GIM_QUERYRASTER, gim, GIM_QUERYRASTER_LEN)
	call flush (GP_FD(gp))

	# This assumes a normal stream type GKI connection.
	nchars = GIM_RET_QRAS_LEN * SZ_SHORT
	nread = read (GP_FD(gp), retval, nchars)
	call fseti (GP_FD(gp), F_CANCEL, OK)
	if (nread != nchars)
	    call syserrs (SYS_FREAD, "gim_queryraster")

	type   = retval[GIM_RET_QRAS_RT]
	width  = retval[GIM_RET_QRAS_NX]
	height = retval[GIM_RET_QRAS_NY]
	depth  = retval[GIM_RET_QRAS_BP]

	return (retval[GIM_RET_QRAS_EX])
end
