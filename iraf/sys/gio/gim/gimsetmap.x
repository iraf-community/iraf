# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<gescape.h>
include	<gki.h>
include	<gim.h>

# GIM_SETMAPPING -- Define a mapping between a source rect and a destination
# rect.  While the mapping is enabled, any changes to the source rect will
# be automatically propagated to the destination rect.  If the source and
# destination rects are not the same size the source rect will be scaled to
# fit the output rect.  A negative DW or DH causes the X or Y axis to be
# flipped during the mapping.  Setmapping automatically enables a new
# mapping, but no data is copied until the source rect is subsequently
# modified or the mapping is modified or refreshed.  Setmapping may be called
# on an already existing mapping to edit the mapping.  If the mapping is
# enabled the effect of the edit will be visible immediately.  Only the
# modified regions of the destination rect will be updated by a mapping.

procedure gim_setmapping (gp, mapping, rop,
	src,st,sx,sy,sw,sh, dst,dt,dx,dy,dw,dh)

pointer	gp			#I graphics descriptor
int	mapping			#I mapping to be defined or edited
int	rop			#I rasterop
int	src			#I source raster
int	st			#I coordinate type for source raster
real	sx,sy,sw,sh		#I source rect
int	dst			#I destination raster
int	dt			#I coordinate type for destination raster
real	dx,dy,dw,dh		#I destination rect

short	gim[GIM_SETMAPPING_LEN]
errchk	gpl_flush, gpl_cache
include "../gpl.com"

begin
        # Flush any buffered polyline output.  Make sure the wcs transformation
        # in the cache is up to date.

        if (op > 1)
            call gpl_flush()
        else if (gp != gp_out || GP_WCS(gp) != wcs)
            call gpl_cache (gp)

	# Output the setmapping escape.
	gim[GIM_SETMAPPING_MP] = mapping
	gim[GIM_SETMAPPING_OP] = rop
	gim[GIM_SETMAPPING_SR] = src
	gim[GIM_SETMAPPING_ST] = st

	if (st == CT_PIXEL) {
	    gim[GIM_SETMAPPING_SX] = sx
	    gim[GIM_SETMAPPING_SY] = sy
	    gim[GIM_SETMAPPING_SW] = sw
	    gim[GIM_SETMAPPING_SH] = sh
	} else {
	    gim[GIM_SETMAPPING_SX] = sx * GKI_MAXNDC
	    gim[GIM_SETMAPPING_SY] = sy * GKI_MAXNDC
	    gim[GIM_SETMAPPING_SW] = nint (sw * GKI_MAXNDC)
	    gim[GIM_SETMAPPING_SH] = nint (sh * GKI_MAXNDC)
	}

	gim[GIM_SETMAPPING_DR] = dst
	gim[GIM_SETMAPPING_DT] = dt

	if (dt == CT_PIXEL) {
	    gim[GIM_SETMAPPING_DX] = dx
	    gim[GIM_SETMAPPING_DY] = dy
	    gim[GIM_SETMAPPING_DW] = dw
	    gim[GIM_SETMAPPING_DH] = dh
	} else {
	    gim[GIM_SETMAPPING_DX] = dx * GKI_MAXNDC
	    gim[GIM_SETMAPPING_DY] = dy * GKI_MAXNDC
	    gim[GIM_SETMAPPING_DW] = nint (dw * GKI_MAXNDC)
	    gim[GIM_SETMAPPING_DH] = nint (dh * GKI_MAXNDC)
	}

	call gescape (gp, GIM_SETMAPPING, gim, GIM_SETMAPPING_LEN)
end
