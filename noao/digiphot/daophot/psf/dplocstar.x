include <mach.h>
include <imhdr.h>
include "../lib/daophotdef.h"
include	"../lib/apsel.h"
include	"../lib/psfdef.h"

# DP_LOCSTAR -- Given an x,y position locate the star in the aperture
# photometry list which is within a critical radius of the input position.

int 	procedure dp_locstar (dao, im, x, y)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
real	x, y			# input position

int	i, box_size
pointer	psf, apsel
real	crit_rad, rad, ax, ay

begin
	# Set up some constants.
	psf = DP_PSF(dao)
	apsel = DP_APSEL(dao)
	crit_rad = DP_MATCHRAD(dao) * DP_MATCHRAD(dao)
	box_size = DP_SZLOOKUP(psf)

	# Search the list.
	do i = 1, DP_APNUM (apsel) {

	    # Compute distance of star from the cursur position.
	    ax = Memr[DP_APXCEN(apsel)+i-1]
	    ay = Memr[DP_APYCEN(apsel)+i-1]
	    if (! IS_INDEFR(ax) && ! IS_INDEFR(ay))
	        rad = (ax - x) * (ax - x) + (ay - y) * (ay - y)
	    else 
		rad = MAX_REAL

	    # Found the star.
	    if (rad <= crit_rad) {

		DP_CUR_PSF(psf) = i
		DP_CUR_PSFID(psf) = Memi[DP_APID(apsel)+i-1]
		DP_CUR_PSFX(psf) = ax
		DP_CUR_PSFY(psf) = ay
		DP_CUR_PSFSKY(psf) = Memr[DP_APMSKY(apsel)+i-1]
		DP_CUR_PSFMAG(psf) = Memr[DP_APMAG(apsel)+i-1]

		# Is the star too close to the edge ?
		if ((DP_CUR_PSFX(psf) - box_size / 2) < 1.0 || 
	    	    (DP_CUR_PSFY(psf) - box_size / 2) < 1.0 || 
	    	    (DP_CUR_PSFX(psf) + box_size / 2) > IM_LEN(im, 1) || 
	    	    (DP_CUR_PSFY(psf) + box_size / 2) > IM_LEN(im, 2) ) {
	    	    return (-1)
		} else
		    return (i)
	    }
	}

	return (0)
end
