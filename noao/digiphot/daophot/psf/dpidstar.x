include <mach.h>
include <imhdr.h>
include "../lib/daophotdef.h"
include	"../lib/apsel.h"
include	"../lib/psfdef.h"

# DP_IDSTAR -- Given an id number locate the star in the aperture
# photometry list.

int procedure dp_idstar (dao, im, idnum)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
int	idnum			# id number from photometry list

int	i, box_size
pointer	psf, apsel

begin
	# Set up some constants.
	psf = DP_PSF(dao)
	apsel = DP_APSEL(dao)
	box_size = DP_SZLOOKUP(psf)

	# Search the list.
	do i = 1, DP_APNUM (apsel) {

	    # Test the id number.
	    if (idnum != Memi[DP_APID(apsel)+i-1])
		next

	    # Found the star.
	    DP_CUR_PSF(psf) = i
	    DP_CUR_PSFID(psf) = Memi[DP_APID(apsel)+i-1]
	    DP_CUR_PSFX(psf) = Memr[DP_APXCEN(apsel)+i-1]
	    DP_CUR_PSFY(psf) = Memr[DP_APYCEN(apsel)+i-1]
	    DP_CUR_PSFSKY(psf) = Memr[DP_APMSKY(apsel)+i-1]
	    DP_CUR_PSFMAG(psf) = Memr[DP_APMAG(apsel)+i-1]

	    # Is the star too close to the edge ?
	    if (IS_INDEFR(DP_CUR_PSFX(psf)) || IS_INDEFR(DP_CUR_PSFY(psf))) {
		break
	    } else if ((DP_CUR_PSFX(psf) - box_size / 2) < 1.0 || 
	    	(DP_CUR_PSFY(psf) - box_size / 2) < 1.0 || 
	    	(DP_CUR_PSFX(psf) + box_size / 2) > IM_LEN(im, 1) || 
	    	(DP_CUR_PSFY(psf) + box_size / 2) > IM_LEN(im, 2) ) {
	    	return (-1)
	    } else {
		return (i)
	    }
	}

	return (0)
end
