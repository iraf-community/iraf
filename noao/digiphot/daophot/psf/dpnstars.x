include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_NSTARS -- Procedure to compute the nearby neighbours.

int procedure dp_nstars (dao, psfgr, newfile)

pointer	dao			# pointer to the daophot structure
pointer	psfgr			# pointer to the psf group
bool	newfile			# new file

int	top_star
pointer	psf
real	crit_radius

begin
	# Define the pointer to the psf fitting structure.
	psf = DP_PSF(dao)

	# Initialize. Top star will point to the first star that is not
	# a neighbour.

	top_star = 1

	# The critical radius for detecting neighbours is the psf radius plus
	# twice the fitting radius plus 1.

	crit_radius = DP_PSFRAD(dao) + 2.0 * DP_FITRAD(dao) + 1.0

	# Find the neighbour stars.

	call dp_neighbors (dao, DP_CUR_PSF(psf), DP_CUR_PSFX(psf), 
	    DP_CUR_PSFY(psf), crit_radius, top_star)

	# Find stars within 2 fitting radii of the stars already found.
	crit_radius = 2.0 * DP_FITRAD (dao)

	# Find the friends of the psf star neighbours.
	call dp_friends (dao, crit_radius, top_star)

	# Write out the group information.
	call dp_pwrtgrp (dao, psfgr, top_star, newfile)

	return (top_star - 2)
end
