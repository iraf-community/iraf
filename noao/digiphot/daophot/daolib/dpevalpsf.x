include "../lib/daophotdef.h"

# DP_EVALPSF -- Evaluate the point-spread function at a given point.
# The value returned is the sum of a two-dimensional integral under a 
# bivariate Gaussian function, and a value obtained interpolation in a
# lookup table.

real procedure dp_evalpsf (x, y, psffit, dx, dy, varpsf, dvdx, dvdy)

real	x, y			# coordinates for evaluation
pointer	psffit			# Pointer to PSF fit structure
real	dx, dy			# distance between star and PSF star
int	varpsf			# variable psf across image
real	dvdx, dvdy		# 1st derivatives of PSF wrt x,y

int	status
real	half_psf, xx, yy, dfdx, dfdy
real	erfx, erfy, value
real	dedxc, dedyc, dummy

real	erf(), psf_interp()

begin
	# Set up local variables.
	half_psf = real ((DP_PSFSIZE(psffit) + 1) / 2 )
	dvdx = 0.0
	dvdy = 0.0
	xx = 2.0 * x + half_psf
	yy = 2.0 * y + half_psf

	# xx and yy are the coordinates within the lookup table which
	# has a half pixel grid size. Xx and yy are relative to the
	# corner of the lookup table.

	if ((xx < 2.0) || (xx > (DP_PSFSIZE(psffit) - 1.0)))
	    return (0.0)
	if ((yy < 2.0) || (yy > (DP_PSFSIZE(psffit) - 1.0)))
	    return (0.0)

	# Evaluate the approximating Gaussian.
	erfx = erf (x, DP_PSFDXCEN(psffit), DP_PSFSIGX(psffit), dedxc, dummy)
	erfy = erf (y, DP_PSFDYCEN(psffit), DP_PSFSIGY(psffit), dedyc, dummy)

	# Add a value interpolated from the look up table to the approximating
	# Gaussian.

	value = DP_PSFHEIGHT(psffit) * erfx * erfy  + 
		psf_interp (Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit), 
		DP_PSFSIZE(psffit), dx, dy, varpsf, xx, yy, dfdx, dfdy, status)
	dvdx = 2.0 * dfdx - DP_PSFHEIGHT(psffit) * dedxc * erfy
	dvdy = 2.0 * dfdy - DP_PSFHEIGHT(psffit) * dedyc * erfx

	return (value)

end
