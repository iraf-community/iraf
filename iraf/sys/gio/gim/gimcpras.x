# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gescape.h>
include	<gki.h>
include	<gim.h>

# GIM_COPYRASTER -- Copy a portion of the source raster to a rectangular
# region of the destination raster.

procedure gim_copyraster (gp, rop, src,st,sx,sy,sw,sh, dst,dt,dx,dy,dw,dh)

pointer	gp			#I graphics descriptor
int	rop			#I rasterop
int	src			#I source raster
int	st			#I coordinate type for source raster
real	sx,sy,sw,sh		#I source rect
int	dst			#I destination raster
int	dt			#I coordinate type for destination raster
real	dx,dy,dw,dh		#I destination rect

short	gim[GIM_COPYRASTER_LEN]

begin
	gim[GIM_COPYRASTER_OP] = rop
	gim[GIM_COPYRASTER_SR] = src
	gim[GIM_COPYRASTER_ST] = st

	if (st == CT_PIXEL) {
	    gim[GIM_COPYRASTER_SX] = sx
	    gim[GIM_COPYRASTER_SY] = sy
	    gim[GIM_COPYRASTER_SW] = sw
	    gim[GIM_COPYRASTER_SH] = sh
	} else {
	    gim[GIM_COPYRASTER_SX] = sx * GKI_MAXNDC
	    gim[GIM_COPYRASTER_SY] = sy * GKI_MAXNDC
	    gim[GIM_COPYRASTER_SW] = nint (sw * GKI_MAXNDC)
	    gim[GIM_COPYRASTER_SH] = nint (sh * GKI_MAXNDC)
	}

	gim[GIM_COPYRASTER_DR] = dst
	gim[GIM_COPYRASTER_DT] = dt

	if (dt == CT_PIXEL) {
	    gim[GIM_COPYRASTER_DX] = dx
	    gim[GIM_COPYRASTER_DY] = dy
	    gim[GIM_COPYRASTER_DW] = dw
	    gim[GIM_COPYRASTER_DH] = dh
	} else {
	    gim[GIM_COPYRASTER_DX] = dx * GKI_MAXNDC
	    gim[GIM_COPYRASTER_DY] = dy * GKI_MAXNDC
	    gim[GIM_COPYRASTER_DW] = nint (dw * GKI_MAXNDC)
	    gim[GIM_COPYRASTER_DH] = nint (dh * GKI_MAXNDC)
	}

	call gescape (gp, GIM_COPYRASTER, gim, GIM_COPYRASTER_LEN)
end
