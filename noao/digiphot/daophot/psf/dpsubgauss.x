include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_SUBGAUSS -- Subtract the best fitting Gaussian from the stellar
#  profile and return the result. Also update the PSF structure with
# the new PSF maximum and minimum.

procedure dp_subgauss (dao, subrast, result, xdimen, ydimen, xstar, ystar,
	x1, y1, scale, sky)

pointer	dao			# pointer to daophot structure
real	subrast[xdimen, ydimen]	# data subraster
real	result[xdimen, ydimen]	# result raster
int	xdimen, ydimen		# dimensions of input
real	xstar, ystar		# stars position
int	x1, y1			# position of subraster in image
real	scale			# brightness relative to PSF
real	sky			# sky value

int	i, j
pointer	psffit, psf
real	erfx, erfy, xzero, yzero

real erf()

begin
	# Get pointer to daophot structures.
	psf = DP_PSF (dao)
	psffit = DP_PSFFIT (dao)

	# Initialize.
	xzero = xstar - x1 + 1 + DP_PSFDXCEN (psffit)
	yzero = ystar - y1 + 1 + DP_PSFDYCEN (psffit)

	# Subtract the fitted Gaussian.
	do j = 1, ydimen {
	    erfy = erf (real (j), yzero, DP_PSFSIGY(psffit),
	        DP_PSFDHDYC(psffit), DP_PSFDHDSY(psffit))
	    do i = 1, xdimen {
	        erfx = erf (real (i), xzero, DP_PSFSIGX(psffit),
		    DP_PSFDHDXC(psffit), DP_PSFDHDSX(psffit))
		result[i,j] = subrast[i, j] - scale * DP_PSFHEIGHT(psffit) *
		    erfx * erfy - sky
	    }
	}

	# Compute the new min max.
	call alimr (result, xdimen * ydimen, DP_PSFMIN(psf), DP_PSFMAX(psf))
end
