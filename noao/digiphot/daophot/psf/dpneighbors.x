include <mach.h>
include "../lib/daophotdef.h"
include "../lib/apsel.h"

# DP_NEIGHBORS -- Find the neighbors of the star at the given position.
# A neighbor is a star closer than the critical radius.

procedure dp_neighbors (dao, istar, xstar, ystar, radius, top_star)

pointer	dao			# pointer to the daophot structure
int	istar			# number of the input star
real	xstar, ystar		# position of the input star
real	radius			# critical radius
int	top_star		# pointer to 1st non-neighbor

int	i
pointer	apsel
real 	radsq, xi, yi, delta_rsq

begin
	# Define the apselect structure.
	apsel = DP_APSEL(dao)

	# First move the selected star to the top of the list.
	call dp_swap (1, istar, Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	    Memr[DP_APMSKY(apsel)])

	radsq = radius * radius

	# Search the list.
	top_star = 2
	do i = 2, DP_APNUM(apsel) {

	    # Check the separation.
	    xi = Memr[DP_APXCEN(apsel)+i-1]
	    yi = Memr[DP_APYCEN(apsel)+i-1]
	    if (! IS_INDEFR(xi) && ! IS_INDEFR(yi))
	        delta_rsq = (xi - xstar) * (xi - xstar) + (yi - ystar) *
		    (yi - ystar)
	    else
		delta_rsq = MAX_REAL

	    # Swap the positions of the two stars.
	    if (delta_rsq <= radsq) {
		call dp_swap (i, top_star, Memi[DP_APID(apsel)], 
		    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)], 
		    Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)])
		top_star = top_star + 1
	    }
	}
end
