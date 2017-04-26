# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <pmset.h>
include	<plio.h>

# PM_STENCIL -- Perform a rasterop operation from the source mask to the
# destination mask at the given offsets, but only within the regions set to
# one in the stencil mask.

procedure pm_stencil (pm_src, vs_src, pm_dst, vs_dst, pm_stn, vs_stn, vn, rop)

pointer	pm_src			#I source mask or NULL
long	vs_src[PM_MAXDIM]	#I start vector in source mask
pointer	pm_dst			#I destination mask (required)
long	vs_dst[PM_MAXDIM]	#I start vector in destination mask
pointer	pm_stn			#I stencil mask (required)
long	vs_stn[PM_MAXDIM]	#I start vector in stencil mask
long	vn[PM_MAXDIM]		#I vector giving subregion size
long	rop			#I rasterop

int	i
long	v5[PM_MAXDIM], v6[PM_MAXDIM]
include	"pmio.com"

begin
	# If an image section is in use on any of the input mask operands,
	# perform a coordination transformation into physical mask space
	# before performing the stencil operation.

	if (PM_MAPXY(pm_src) == YES || PM_MAPXY(pm_dst) == YES ||
	    PM_MAPXY(pm_stn) == YES) {

	    # Compute the geometry V1:V3 of the source mask.
	    call imaplv (PM_REFIM(pm_src), vs_src, v1, PM_MAXDIM)

	    call aaddl (vs_src, vn, v2, PM_MAXDIM)
	    call asubkl (v2, 1, v2, PM_MAXDIM)
	    call aminl (v2, IM_LEN(PM_REFIM(pm_src),1), v2, PM_MAXDIM)
	    call imaplv (PM_REFIM(pm_src), v2, v3, PM_MAXDIM)

	    # Swap V1 and V3 if necessary.
	    call aminl (v1, v3, v1, PM_MAXDIM)

	    # Compute the geometry V2:V4 of the destination mask.
	    call imaplv (PM_REFIM(pm_dst), vs_dst, v2, PM_MAXDIM)

	    call aaddl (vs_dst, vn, v3, PM_MAXDIM)
	    call asubkl (v3, 1, v3, PM_MAXDIM)
	    call aminl (v3, IM_LEN(PM_REFIM(pm_dst),1), v3, PM_MAXDIM)
	    call imaplv (PM_REFIM(pm_dst), v3, v4, PM_MAXDIM)

	    # Compute v3 = vn for rasterop.  Input: SRC=v1:v3, DST=v2:v4
	    # This also swaps v2 and v4 if necessary.

	    do i = 1, PM_MAXDIM
		if (v2[i] > v4[i]) {
		    v3[i] = v2[i] - v4[i] + 1
		    v2[i] = v4[i]
		} else
		    v3[i] = v4[i] - v2[i] + 1

	    # Compute the start vector V4 of the stencil mask.
	    call imaplv (PM_REFIM(pm_stn), vs_stn, v4, PM_MAXDIM)
	    call aaddl (vs_stn, vn, v5, PM_MAXDIM)
	    call asubkl (v5, 1, v5, PM_MAXDIM)
	    call aminl (v5, IM_LEN(PM_REFIM(pm_stn),1), v5, PM_MAXDIM)
	    call imaplv (PM_REFIM(pm_stn), v5, v6, PM_MAXDIM)
	    call aminl (v4, v6, v4, PM_MAXDIM)

	    # Perform the rasterop operation.
	    call pl_stencil (pm_src, v1, pm_dst, v2, pm_stn, v4, v3, rop)

	} else
	    call pl_stencil (pm_src, vs_src, pm_dst, vs_dst,
		pm_stn, vs_stn, vn, rop)
end
