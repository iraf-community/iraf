include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

define	NTERM		5
define 	MAX_ITER	100
define	CONVERGE	0.0001

define	SIGMA_INIT	2.0

define	BOX_INIT	2
define	ONE		1.0
define	THREE		3.0

# DP_GAUSSFIT -- Fit an integrated Gaussian function to the central
# part of the stellar profile. Five parameters are solved for:
# the height, the X and Y center and the X and Y sigma of the Gaussian

int procedure dp_gaussfit (dao, subrast, xdimen, ydimen)

pointer	dao			# pointer to the daophot structure
real	subrast[xdimen,ydimen]	# subraster containing star
int	xdimen, ydimen		# dimensions of the subraster

int	i, j, k, l
int	ixcen, iycen, x_box, y_box, niter, invstat
pointer	psf, psffit
real	xcen, ycen, dxcen, dycen, height, sigma_x, sigma_y
real	resid [NTERM], normal [NTERM, NTERM], deriv [NTERM], result[NTERM]
real	erfx, erfy, d_height, dhdxc, dhdyc, dhdsx, dhdsy, xzero, yzero

real erf()

begin
	# Define some daophot pointers.
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Subtract the sky from the data.
	call asubkr (subrast, DP_CUR_PSFSKY(psf), subrast, xdimen * ydimen)

	# Set up for the fit
	xcen = DP_CUR_PSFX (psf)
	ycen = DP_CUR_PSFY (psf)
	ixcen = xcen
	iycen = ycen
	xcen = xcen - ixcen + xdimen/2
	ycen = ycen - iycen + ydimen/2
	ixcen = nint (xcen)
	iycen = nint (ycen)
	height = DP_PSFMAX (psf) - DP_CUR_PSFSKY (psf)
	sigma_x = SIGMA_INIT
	sigma_y = SIGMA_INIT
	dxcen = 0.0
	dycen = 0.0	
	x_box = BOX_INIT
	y_box = BOX_INIT	

	for (niter = 1; niter <= MAX_ITER; niter = niter + 1) {

	    # Zero out the accumulators.
	    do i = 1, NTERM {
		resid (i) = 0.0
		do j = 1, NTERM
		    normal (i,j) = 0.0
	    }

	    if (sigma_x <= ONE)
		x_box = 1
	    else if (sigma_x >= THREE)
		x_box = 3
	    if (sigma_y <= ONE)
		y_box = 1
	    else if (sigma_y >= THREE)
		y_box = 3

	    # Accumulate the matrix to be fit.
	    xzero = xcen + dxcen
	    yzero = ycen + dycen
	    do j = iycen - y_box, iycen + y_box {
		do i = ixcen - x_box, ixcen + x_box {
		    erfx = erf (float(i), xzero, sigma_x, dhdxc, dhdsx)
		    erfy = erf (float(j), yzero, sigma_y, dhdyc, dhdsy)
		    deriv[1] = erfx * erfy
		    deriv[2] = height * dhdxc * erfy
		    deriv[3] = height * dhdyc * erfx
		    deriv[4] = height * dhdsx * erfy
		    deriv[5] = height * dhdsy * erfx
		    d_height = subrast[i,j] - height * deriv[1]
		    do k = 1, NTERM {
			resid[k] = resid[k] + d_height * deriv[k]
			do l = 1, NTERM {
			    normal[k,l] = normal[k,l] + deriv[k] * deriv[l]
			}
		    }
		}
	    }	

	    # Invert the matrix.
	    call invers (normal, NTERM, NTERM, invstat)
	    if (invstat != 0)
		return (ERR)

	    # Compute the new fit.
	    call mvmul (normal, NTERM, NTERM, resid, result)

	    # Correct the parameters
	    height = height + result[1] / (1.0 + 4.0 * abs (result[1] / height))
	    dxcen = dxcen + result[2] / (1.0 + 3.0 * abs (result[2]))
	    dycen = dycen + result[3] / (1.0 + 3.0 * abs (result[3]))
	    sigma_x = sigma_x + result[4] / (1.0 + 4.0 * abs (result[4] / 
	        sigma_x))
	    sigma_y = sigma_y + result[5] / (1.0 + 4.0 * abs (result[5] /
	        sigma_y))

	    # Test for convergence
	    if (abs (result[1] / height) + abs (result[4] / sigma_x) +
		abs (result[5] / sigma_y) <= CONVERGE) {
		DP_PSFHEIGHT(psffit) = height
		DP_PSFDXCEN(psffit) = dxcen
		DP_PSFDYCEN(psffit) = dycen
		DP_PSFSIGX(psffit) = sigma_x
		DP_PSFSIGY(psffit) = sigma_y
		DP_PSFDHDXC(psffit) = dhdxc
		DP_PSFDHDYC(psffit) = dhdyc
		DP_PSFDHDSX(psffit) = dhdsx
		DP_PSFDHDSY(psffit) = dhdsy
		return (OK)
	    }
	}

	return (ERR)
end
