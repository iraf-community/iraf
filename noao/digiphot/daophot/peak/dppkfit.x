include <mach.h>
include	"../lib/daophot.h"
include	"../lib/daophotdef.h"

define	NTERM		     3	      # the number of terms to be fit
define	SIGN_CHECK	     -1.0e-37 # check for change of sign (make 0.0 ?)
define	MAX_DELTA_BRIGHTER   5.25     # max permitted brightness increase
define	MAX_DELTA_FAINTER    0.84     # max permitted brightness decrease
define	MAX_DELTA_PIX	     0.5      # max +/- change in x/y positions
define	MAX_NEW_ERRMAG	     0.05     # 1st convergenge check on magnitude
define	MAX_NEW_RELBRIGHT    0.001    # 2nd convergence check on magnitude
define	MAX_PIXERR	     0.01     # convergence check on x/y positions
define	MIN_NITER	     3        # min number of iterations
define	MAX_ERRMAG	     1.9995   # convergence check on magnitude error
define	MIN_SHARP	     -99.99   # min sharpness value
define	MAX_SHARP	     99.99    # max sharpness value

define	PEAK_ERR_NUMB	     0.027    # amplitude of the flat/bias error
define	PEAK_EPSILONR	     1.0e-6   # test for inclusion inside fitrad
define	NCORE_SIGMASQ	     36.0     # max gsigma-sq for sharpness value
define	INTERP_ERR	     0.0075   # amplitude of interpolation error
define	MAX_NSIGMA	     10.0     # max relative error in chi
define	CHI_NORM	     1.2533   # sqrt (PI / 2.0)
define	MIN_SUMWT	     3.0      # min value of the radial weight sum
define	MIN_NPIX	     3	      # min number of pixels for fit


# DP_IPKFIT -- Allocate the memory required by the PEAK fitting routines.

procedure dp_ipkfit ()

include "pfit.com"

begin
	# Allocate working space for the fit.
	call malloc (clamp, NTERM, TY_REAL)
	call malloc (normal, NTERM * NTERM, TY_REAL)
	call malloc (resid, NTERM, TY_REAL)
	call malloc (deriv, NTERM, TY_REAL)
	call malloc (result, NTERM, TY_REAL)
	call malloc (old_result, NTERM, TY_REAL)
end


# DP_PKFIT -- Do the actual fit.

procedure dp_pkfit (dao, subim, nx, ny, varpsf, x, y, dx, dy, rel_bright, sky,
	errmag, chi, sharp, iter)

pointer	dao		# pointer to the DAOPHOT Structure
real	subim[nx,ny]	# pointer to the image subraster
int	nx, ny		# size of the image subraster
int	varpsf		# use the variable psf fitting code ?
real	x, y		# initial estimate of the stellar position
real	dx, dy		# distance of star from the psf position
real	rel_bright	# initial estimate of stellar brightness
real	sky		# the sky value associated with this star
real	errmag		# error estimate for this star
real	chi		# estimated goodness of fit parameter
real	sharp		# broadness of the profile compared to the PSF
int	iter		# number of iterations needed for a fit.

bool	converge, redo
int	i, flag
pointer	psffit
real	numer, denom, chiold, sum_weight

include "pfit.com"

begin
	# Get the pointer to the PSF structure.
	psffit = DP_PSFFIT (dao)

	# Initialize the parameters which control the fit.

	chiold = 1.0
	iter = 1
	sharp = 0.0
	converge = false
	call amovkr (1.0, Memr[clamp], NTERM)
	call amovkr (0.0, Memr[old_result], NTERM)

	# Iterate until a solution is found.

	while ((iter <= DP_MAXITER(dao)) && ! converge) {

	    # Initialize the matrices and vectors required by the fit.

	    chi = 0.0
	    numer = 0.0
	    denom = 0.0
	    sum_weight = 0.0
	    call aclrr (Memr[resid], NTERM)
	    call aclrr (Memr[normal], NTERM * NTERM)

	    # Build up the vector of residuals and the normal matrix.

	    call dp_fitbuild (dao, subim, nx, ny, varpsf, x, y, dx, dy,
	        rel_bright, sky, chi, chiold, iter, Memr[clamp],
		Memr[normal], Memr[resid], Memr[deriv],
		numer, denom, sum_weight)

	    # Return if the iteration was unsuccessful.

	    if (iter <= 0) {
		iter = -1
		return
	    }

	    # Invert the matrix. Return if inversion was unsuccessful.

	    call invers (Memr[normal], NTERM, NTERM, flag)
	    if (flag != 0) {
		iter = -1
		return
	    }

	    # Solve the matrix.

	    call mvmul (Memr[normal], NTERM, NTERM, Memr[resid], Memr[result])

	    # In the beginning the brightness of the star will not be
	    # permitted to change by more than two magnitudes per
	    # iteration (that is to say if the estimate is getting
	    # brighter, it may not get brighter by more than 525%
	    # per iteration, and if it is getting fainter, it may not
	    # get fainter by more than 84% per iteration). The x and y
	    # coordinates of the centroid will be allowed to change by
	    # no more than one-half pixel per iteration. Any time
	    # that a parameter correction changes sign, the maximum
	    # permissible change in that parameter will be reduced
	    # by a factor of 2.

	    # Perform the sign check.

	    do i = 1, NTERM {
		if ((Memr[old_result+i-1] * Memr[result+i-1]) < SIGN_CHECK) 
		    Memr[clamp+i-1] = Memr[clamp+i-1] / 2.0
		Memr[old_result+i-1] = Memr[result+i-1]
	    }

	    # Compute the new x, y and magnitude.

	    rel_bright = rel_bright + Memr[result] / (1.0 + max (Memr[result] /
	        (MAX_DELTA_BRIGHTER * rel_bright), -Memr[result] /
		(MAX_DELTA_FAINTER * rel_bright)) / Memr[clamp])
	    x = x + Memr[result+1] / (1.0 + abs (Memr[result+1]) /
	        (MAX_DELTA_PIX * Memr[clamp+1]))
	    y = y + Memr[result+2] / (1.0 + abs (Memr[result+2]) /
	        (MAX_DELTA_PIX * Memr[clamp+2]))

	    sharp = 2.0 * DP_PSFSIGX (psffit) * DP_PSFSIGY (psffit) * numer /
		(DP_PSFHEIGHT (psffit) * rel_bright * denom)
	    errmag = chiold * sqrt (Memr[normal])

	    # Start on the assumption the fit has converged.

	    redo = false
	    converge = true

	    # Check for convergence. If the most recent computed correction 
	    # to the brightness is larger than 0.1% of the brightness or
	    # 0.05 * sigma of the brightness whichever is larger, convergence
	    # has not been achieved.

	    if (abs (Memr[result]) > max ((MAX_NEW_ERRMAG * errmag),
	        (MAX_NEW_RELBRIGHT * rel_bright)))
		redo = true

	    # If the absolute change in x and y is greater than MAX_PIXERR
	    # convergence has not been achieved.

	    if (max (abs (Memr[result+1]), abs (Memr[result+2])) > MAX_PIXERR)
	        redo = true

	    # The star must go at least three iterations to make sure
	    # that a reasonable attempt has been made to estimate
	    # errmag.

	    if (iter < MIN_NITER)
		converge = false

	    # Give up if the solution has gone to the maximum number of
	    # iterations or if the standard error of the brightness
	    # is greater than 200 %.

	    if (redo && (errmag <= MAX_ERRMAG) && (iter < DP_MAXITER (dao)))
		converge = false

	    iter = iter + 1

	}

	# Normal return.
	if (sharp <= MIN_SHARP || sharp >= MAX_SHARP)
	    sharp = INDEFR

	iter = iter - 1
end


# DP_FPKFIT -- Free the memory required by the fitting code.

procedure dp_fpkfit ()

include "pfit.com"

begin
	call mfree (clamp, TY_REAL)
	call mfree (normal, TY_REAL)
	call mfree (resid, TY_REAL)
	call mfree (deriv, TY_REAL)
	call mfree (result, TY_REAL)
	call mfree (old_result, TY_REAL)
end


# DP_FITBUILD -- Build the normal and vector of residuals for the fit.

procedure dp_fitbuild (dao, subim, nx, ny, varpsf, x, y, xfrom_psf,
	yfrom_psf, rel_bright, sky, chi, chiold, iter, clamp, normal,
	resid, deriv, numer, denom, sum_weight)

pointer	dao			# pointer to the DAOPHOT Structure
real	subim[nx,ny]		# subimage containing star
int	nx, ny			# size of the image subraster
int	varpsf			# use the variable psf fitting code
real	x, y			# initial estimate of the position
real	xfrom_psf, yfrom_psf	# distance from the psf star
real	rel_bright		# initial estimate of stellar brightness
real	sky			# the sky value associated with this star
real	chi			# estimated goodness of fit parameter
real	chiold			# previous estimate of gof
int	iter			# current iteration number
real	clamp[NTERM]		# clamp array
real	normal[NTERM,NTERM]	# normal matrix
real	resid[NTERM]		# residual matrix
real	deriv[NTERM]		# derivative matrix
real	numer, denom		# used in sharpness calculation
real	sum_weight		# sum of the weights

int	i, j, ix, iy, lowx, lowy, highx, highy, npix
pointer	psffit
real	fitradsq, peakerr, datamin, datamax, read_noise
real	dx, dy, dxsq, dysq, radsq
real	dvdx, dvdy, d_pixval
real	pred_pixval, sigma, sigmasq, relerr, weight
real 	rhosq, pixval, dfdsig

real	dp_evalpsf()

begin
	# Get the pointer to the PSF structure.
	psffit = DP_PSFFIT (dao)

	# Set up some constants.
	read_noise = (DP_READ_NOISE(dao) / DP_PHOT_ADC(dao)) ** 2
	fitradsq = DP_FITRAD (dao) * DP_FITRAD (dao)
	peakerr = PEAK_ERR_NUMB / (DP_PSFSIGX(psffit) *
	    DP_PSFSIGY(psffit)) ** 2
	if (IS_INDEFR (DP_MINGDATA(dao)))
	    datamin = -MAX_REAL
	else
	    datamin = DP_MINGDATA(dao)
	if (IS_INDEFR (DP_MAXGDATA(dao)))
	    datamax = MAX_REAL
	else
	    datamax = DP_MAXGDATA(dao)

	# Define the size of the subraster to be read.
	lowx = max (1, int (x - DP_FITRAD (dao)))
	lowy = max (1, int (y - DP_FITRAD (dao)))
	highx = min (nx, int (x + DP_FITRAD (dao)) + 1)	
	highy = min (ny, int (y + DP_FITRAD (dao)) + 1)	

	npix = 0
	do iy = lowy, highy {

	    dy = real (iy) - y
	    dysq = dy * dy

	    do ix = lowx, highx {

		pixval = subim[ix, iy]
		if (pixval < datamin || pixval > datamax)
		    next
		dx = real(ix) - x
		dxsq = dx * dx
		radsq = (dxsq + dysq) / fitradsq
		if ((1.0 - radsq)  <= PEAK_EPSILONR)
		    next

		# We have a good pixel within the fitting radius.
		deriv[1] = dp_evalpsf (dx, dy, psffit, xfrom_psf,
		    yfrom_psf, varpsf, dvdx, dvdy)
		deriv[2] = -rel_bright * dvdx
		deriv[3] = -rel_bright * dvdy

	        # Get the residual from the PSF fit and the pixel
		# intensity as predicted by the fit
		d_pixval = pixval - rel_bright * deriv[1] - sky
		pred_pixval = max (0.0, pixval - d_pixval)

		# Calculate the anticipated error in the intensity of
		# in this pixel including READOUT noise, photon stats
		# and the error of interpolating within the PSF.

		sigmasq = pred_pixval / DP_PHOT_ADC (dao) +  read_noise +
		    (INTERP_ERR * pred_pixval) ** 2 + (peakerr *
		    (pred_pixval - sky)) ** 2
		sigma = sqrt (sigmasq)
		relerr = d_pixval / sigma

		# Reject any pixel with a 10-sigma residual.
		if ((abs (relerr / chiold) <= MAX_NSIGMA) || (iter < 2)) {

		    weight = 5.0 / (5.0 + radsq / (1.0 - radsq))

		    # Include in the sharpness calculation only those pixels
		    # which are within NCORE_SIGMASQ core-sigmasq of the
		    # centroid.

		    rhosq = dxsq / DP_PSFSIGX(psffit) ** 2 + dysq /
		        DP_PSFSIGY(psffit) ** 2
		    if (rhosq <= NCORE_SIGMASQ) {
			rhosq = 0.5 * rhosq
			dfdsig = exp (-rhosq) * (rhosq - 1.0)
		        pred_pixval = max (0.0, pixval - sky) + sky
		    	sigma = pred_pixval / DP_PHOT_ADC (dao) + read_noise +
		            (INTERP_ERR * pred_pixval) ** 2 + (peakerr *
			    (pred_pixval - sky)) ** 2
			numer = numer + dfdsig * d_pixval / sigma
			denom = denom + (dfdsig ** 2) / sigma
		    }

		    # Derive the weight of this pixel. First of all
		    # the weight depends on the distance of the pixel
		    # from the centroiud of the star --- it is determined
		    # from a function which is very nearly unity for
		    # radii much smaller than the fitting radius, and
		    # which goes to zero for radii very near the fitting
		    # radius. Then rejectt any pixels with 10 sigma
		    # resduals after the first iteration. Finally scale
		    # the weight to the inverse square of the expected
		    # mean error.

		    chi = chi + weight * abs (relerr)
		    sum_weight = sum_weight + weight
		    weight = weight / sigmasq

		    # Reduce the weight of a bad pixel.
		    if (iter >= 2) 
			weight = weight / (1.0 + (0.4 * relerr / chiold) ** 8)

		    # Now add this pixel into the vector of residuals
		    # and the normal matrix.
		    do i = 1, NTERM {
			resid[i] = resid[i] + d_pixval * weight * deriv[i]
			do j = 1, NTERM {
			    normal[i,j] = normal[i,j] + deriv[i] * deriv[j] *
			        weight
			}
		    }

		    npix = npix + 1
		}
	    }
	}

	# Compute the goodness of fit index CHI. CHI is pulled toward its
	# expected value of unity before being stored in CHIOLD to keep the
	# statistics of a small number of pixels from dominating the error
	# analysis.

	if (sum_weight > MIN_SUMWT) {
	    chi = CHI_NORM * chi * sqrt (1.0 / (sum_weight * (sum_weight -
	        MIN_SUMWT)))
	    chiold = ((sum_weight - MIN_SUMWT) * chi + MIN_SUMWT) / sum_weight
	}

	# Check that at least MIN_NPIX pixels remain.
	if (npix < MIN_NPIX) 
	    iter = -1

	return

end
