include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_CURSKY -- Return the sky value of the current psf star.

real procedure dp_cursky (dao)

pointer	dao				# pointer to daophot structure

pointer	psf

begin
	psf = DP_PSF(dao)
	return (DP_CUR_PSFSKY(psf))
end


# DP_CURMAG -- Return the magnitude of the current psf star.

real procedure dp_curmag (dao)

pointer	dao				# pointer to the daophot structure

pointer	psf

begin
	psf = DP_PSF(dao)
	return (DP_CUR_PSFMAG(psf))
end
