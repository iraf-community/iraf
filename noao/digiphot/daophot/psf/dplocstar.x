include <mach.h>
include <imhdr.h>
include "../lib/daophotdef.h"
include	"../lib/apseldef.h"
include	"../lib/psfdef.h"

# DP_LOCSTAR -- Given an x,y position locate the star in the aperture
# photometry list which is within a critical radius of the input position.

int procedure dp_locstar (dao, im, x, y)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
real	x, y			# input position

int	i
pointer	psf, apsel
real	crit_rad, fitrad, rad, ax, ay

begin
	# Set up some constants.
	psf = DP_PSF(dao)
	apsel = DP_APSEL(dao)
	crit_rad = DP_MATCHRAD(dao) * DP_MATCHRAD(dao)
	fitrad = DP_FITRAD(dao)

	# Search the list.
	do i = 1, DP_APNUM(apsel) {

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
		if (int (ax - fitrad) < 0 || int (ax + fitrad) > IM_LEN(im,1) ||
		    int (ay - fitrad) < 0 || int (ay + fitrad) > IM_LEN(im,2))
		    return (-i)
		else
		    return (i)
	    }
	}

	return (0)
end


# DP_IDSTAR -- Given an id number locate the star in the aperture
# photometry list.

int procedure dp_idstar (dao, im, idnum)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
int	idnum			# id number from photometry list

int	i
pointer	psf, apsel
real	fitrad, x, y

begin
	# Set up some constants.
	psf = DP_PSF(dao)
	apsel = DP_APSEL(dao)
	fitrad = DP_FITRAD(dao)

	# Search the list.
	do i = 1, DP_APNUM (apsel) {

	    # Test the id number.
	    if (idnum != Memi[DP_APID(apsel)+i-1])
		next

	    # Found the star.
	    x = Memr[DP_APXCEN(apsel)+i-1]
	    y = Memr[DP_APYCEN(apsel)+i-1]
	    DP_CUR_PSF(psf) = i
	    DP_CUR_PSFID(psf) = Memi[DP_APID(apsel)+i-1]
	    DP_CUR_PSFX(psf) = Memr[DP_APXCEN(apsel)+i-1]
	    DP_CUR_PSFY(psf) = Memr[DP_APYCEN(apsel)+i-1]
	    DP_CUR_PSFSKY(psf) = Memr[DP_APMSKY(apsel)+i-1]
	    DP_CUR_PSFMAG(psf) = Memr[DP_APMAG(apsel)+i-1]

	    # Is the star too close to the edge ?
	    if (IS_INDEFR(x) || IS_INDEFR(y))
		return (0)
	    else if (int (x - fitrad) < 0 || int (x + fitrad) >
	        IM_LEN(im,1) || int (y - fitrad) < 0 || int (y + fitrad) >
		IM_LEN(im,2))
	    	return (-i)
	    else
		return (i)
	}

	return (0)
end
