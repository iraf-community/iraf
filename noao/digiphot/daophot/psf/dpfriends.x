include <mach.h>
include "../lib/daophotdef.h"
include "../lib/apsel.h"

# DP_FRIENDS -- Find the neighbors of a list of stars which are above
# a specified point in the list. # A neighbor is a star closer than the
# critical radius. 

procedure dp_friends (dao, radius, top_star)

pointer	dao			# pointer to the daophot structure
real	radius			# critical radius
int	top_star		# pointer to the first non neighbor

int	i, j, itop_star
pointer	apsel
real 	radsq, xi, yi, xstar, ystar, delta_rsq

begin
	# Define the daophot pointers.
	apsel = DP_APSEL(dao)

	# Start looking
	radsq = radius * radius
	for (j = 2; j < top_star; j = j + 1) {

	    xstar = Memr[DP_APXCEN(apsel)+j-1]
	    ystar = Memr[DP_APYCEN(apsel)+j-1]

	    itop_star = top_star
	    do i = itop_star, DP_APNUM(apsel) {

	    	xi = Memr[DP_APXCEN(apsel)+i-1]
	    	yi = Memr[DP_APYCEN(apsel)+i-1]
	        if (! IS_INDEFR(xi) && ! IS_INDEFR(yi))
	            delta_rsq = (xi - xstar) * (xi - xstar) + (yi - ystar) *
		        (yi - ystar)
	        else
		    delta_rsq = MAX_REAL

	        if (delta_rsq <= radsq) {
		    call dp_swap (i, top_star, Memi[DP_APID(apsel)], 
		        Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)], 
			Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)])
		    top_star = top_star + 1
	    	}
	    }
	}
end
