# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/rg.h>

# RG_INDICES -- Return the indices in the ranges.

procedure rg_indices (rg, indices, npts, type)

pointer	rg				# Ranges
pointer	indices				# Indices
int	npts				# Number of indices
int	type				# Data type of points

int	i, j, k, step

begin
	# Error check the range pointer.

	if (rg == NULL)
	    call error (0, "Range descriptor undefined")

	# Determine the number of range points.

	indices = NULL
	npts = 0
	if (RG_NRGS (rg) == 0)
	    return

	do i = 1, RG_NRGS(rg) {
	    if (RG_X1(rg, i) > RG_X2(rg, i))
		npts = npts + RG_X1(rg, i) - RG_X2(rg, i) + 1
	    else
		npts = npts + RG_X2(rg, i) - RG_X1(rg, i) + 1
	}

	# Allocate the range points array.

	call malloc (indices, npts, type)

	# Set the range points.

	k = indices
	do i = 1, RG_NRGS(rg) {
	    if (RG_X1(rg, i) > RG_X2(rg, i))
		step = -1
	    else
		step = 1

	    switch (type) {
	    case TY_SHORT:
	    	do j = RG_X1(rg, i), RG_X2(rg, i), step {
		    Mems[k] = j
		    k = k + 1
		}
	    case TY_INT:
	    	do j = RG_X1(rg, i), RG_X2(rg, i), step {
		    Memi[k] = j
		    k = k + 1
		}
	    case TY_LONG:
	    	do j = RG_X1(rg, i), RG_X2(rg, i), step {
		    Meml[k] = j
		    k = k + 1
		}
	    case TY_REAL:
	    	do j = RG_X1(rg, i), RG_X2(rg, i), step {
		    Memr[k] = j
		    k = k + 1
		}
	    case TY_DOUBLE:
	    	do j = RG_X1(rg, i), RG_X2(rg, i), step {
		    Memd[k] = j
		    k = k + 1
		}
	    default:
		call error (0, "rg_indices:  Datatype not available")
	    }
	}

	return
end
