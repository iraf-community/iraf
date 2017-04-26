# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gescape.h>
include	<fset.h>
include	<gio.h>
include	<gki.h>
include	<gim.h>

# GIM_GETMAPPING -- Get the parameters defining a mapping.  The function value
# is YES if the mapping is defined and enabled and NO if the mapping is
# defined but not enabled.  If the mapping is not defined ERR is returned.

int procedure gim_getmapping (gp, mapping, rop,
	src,st,sx,sy,sw,sh, dst,dt,dx,dy,dw,dh)

pointer	gp			#I graphics descriptor
int	mapping			#I mapping to be queried
int	rop			#O rasterop
int     src			#O source raster
int     st			#O coordinate type for source raster
int     sx,sy,sw,sh		#O source rect
int     dst			#O destination raster
int     dt			#O coordinate type for destination raster
int     dx,dy,dw,dh		#O destination rect

int	nchars, nread
short	gim[GIM_GETMAPPING_LEN]
short	retval[GIM_RET_GMAP_LEN]
errchk	gescape, flush, read, syserrs
int	read(), btoi()

begin
	call gpl_flush()
	gim[GIM_GETMAPPING_MP] = mapping
	call gescape (gp, GIM_GETMAPPING, gim, GIM_GETMAPPING_LEN)
	call flush (GP_FD(gp))

	# This assumes a normal stream type GKI connection.
	nchars = GIM_RET_GMAP_LEN * SZ_SHORT
	nread = read (GP_FD(gp), retval, nchars)
	call fseti (GP_FD(gp), F_CANCEL, OK)
	if (nread != nchars)
	    call syserrs (SYS_FREAD, "gim_getmapping")

	# EN=0 not defined, EN=1 defined not enabled, EN=2 defined enabled.
	if (retval[GIM_RET_GMAP_EN] == 0)
	    return (ERR)
	else {
	    rop = retval[GIM_RET_GMAP_OP]

	    src = retval[GIM_RET_GMAP_SR]
	    st  = retval[GIM_RET_GMAP_ST]

	    if (st == CT_PIXEL) {
		sx = retval[GIM_RET_GMAP_SX]
		sy = retval[GIM_RET_GMAP_SY]
		sw = retval[GIM_RET_GMAP_SW]
		sh = retval[GIM_RET_GMAP_SH]
	    } else {
		sx = real (retval[GIM_RET_GMAP_SX]) / GKI_MAXNDC
		sy = real (retval[GIM_RET_GMAP_SY]) / GKI_MAXNDC
		sw = real (retval[GIM_RET_GMAP_SW]) / GKI_MAXNDC
		sh = real (retval[GIM_RET_GMAP_SH]) / GKI_MAXNDC
	    }

	    dst = retval[GIM_RET_GMAP_SR]
	    dt  = retval[GIM_RET_GMAP_DT]

	    if (dt == CT_PIXEL) {
		dx = retval[GIM_RET_GMAP_DX]
		dy = retval[GIM_RET_GMAP_DY]
		dw = retval[GIM_RET_GMAP_DW]
		dh = retval[GIM_RET_GMAP_DH]
	    } else {
		dx = real (retval[GIM_RET_GMAP_DX]) / GKI_MAXNDC
		dy = real (retval[GIM_RET_GMAP_DY]) / GKI_MAXNDC
		dw = real (retval[GIM_RET_GMAP_DW]) / GKI_MAXNDC
		dh = real (retval[GIM_RET_GMAP_DH]) / GKI_MAXNDC
	    }

	}

	return (btoi (retval[GIM_RET_GMAP_EN] == 2))
end
