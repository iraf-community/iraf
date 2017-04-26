include <mach.h>
include	"../lib/daophotdef.h"
include	"../lib/peakdef.h"

# DP_PKFIT -- Do the actual fit.

int procedure dp_pkfit (dao, subim, nx, ny, radius, x, y, dx, dy, rel_bright,
	sky, errmag, chi, sharp, iter)

pointer	dao		# pointer to the DAOPHOT Structure
real	subim[nx,ny]	# pointer to the image subraster
int	nx, ny		# size of the image subraster
real	radius		# the fitting radius
real	x, y		# initial estimate of the stellar position
real	dx, dy		# distance of star from the psf position
real	rel_bright	# initial estimate of stellar brightness
real	sky		# the sky value associated with this star
real	errmag		# error estimate for this star
real	chi		# estimated goodness of fit parameter
real	sharp		# broadness of the profile compared to the PSF
int	iter		# number of iterations needed for a fit.

bool	clip, redo
int	i, flag, npix
pointer	psffit, peak
real	ronoise, numer, denom, chiold, sum_weight, noise, wcrit
int	dp_fitbuild()

begin
	# Get the pointer to the PSF structure.
	psffit = DP_PSFFIT (dao)
	peak = DP_PEAK(dao)

	# Initialize the parameters which control the fit.

	chiold = 1.0
	sharp = INDEFR
	clip = false
	ronoise = (DP_READNOISE(dao) / DP_PHOTADU(dao)) ** 2

	call amovkr (1.0, Memr[DP_PKCLAMP(peak)], DP_PKNTERM(peak))
	call amovkr (0.0, Memr[DP_PKOLDRESULT(peak)], DP_PKNTERM(peak))

	# Iterate until a solution is found.

	for (iter = 1; iter <= DP_MAXITER(dao); iter = iter + 1) {

	    # Initialize the matrices and vectors required by the fit.

	    chi = 0.0
	    numer = 0.0
	    denom = 0.0
	    sum_weight = 0.0
	    call aclrr (Memr[DP_PKRESID(peak)], DP_PKNTERM(peak))
	    call aclrr (Memr[DP_PKNORMAL(peak)], DP_PKNTERM(peak) *
	        DP_PKNTERM(peak))

	    # Set up the critical error limit.
	    if (iter >= WCRIT_NMAX)
		wcrit = WCRIT_MAX
	    else if (iter >= WCRIT_NMED)
		wcrit = WCRIT_MED
	    else if (iter >= WCRIT_NMIN)
		wcrit = WCRIT_MIN
	    else
		wcrit = MAX_REAL


	    # Build up the vector of residuals and the normal matrix.

	    npix = dp_fitbuild (dao, subim, nx, ny, radius, x, y, dx, dy,
	        rel_bright, sky, chiold, chi, clip, iter,
		Memr[DP_PKCLAMP(peak)], Memr[DP_PKNORMAL(peak)],
		Memr[DP_PKRESID(peak)], Memr[DP_PKDERIV(peak)],
		DP_PKNTERM[(peak)], numer, denom, sum_weight)

	    # Return if the iteration was unsuccessful.

	    if (npix < MIN_NPIX)
		return (PKERR_NOPIX)

	    # Invert the matrix. Return if inversion was unsuccessful or
	    # if any of the diagonal elements are less or equal to 0.0.

	    call invers (Memr[DP_PKNORMAL(peak)], DP_PKNTERM(peak),
	        DP_PKNTERM(peak), flag)
	    if (flag != 0)
		return (PKERR_SINGULAR)
	    do i = 1, DP_PKNTERM(peak) {
		if (Memr[DP_PKNORMAL(peak)+(i-1)*DP_PKNTERM(peak)+i-1] <= 0.0)
		    return (PKERR_SINGULAR)
	    }
	    # Solve the matrix.

	    call mvmul (Memr[DP_PKNORMAL(peak)], DP_PKNTERM(peak),
	        DP_PKNTERM(peak), Memr[DP_PKRESID(peak)],
		Memr[DP_PKRESULT(peak)])

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

	    do i = 1, DP_PKNTERM(peak) {
		if ((Memr[DP_PKOLDRESULT(peak)+i-1] *
		    Memr[DP_PKRESULT(peak)+i-1]) < 0.0) 
		    Memr[DP_PKCLAMP(peak)+i-1] = 0.5 *
		        Memr[DP_PKCLAMP(peak)+i-1]
		else
		    Memr[DP_PKCLAMP(peak)+i-1] = min (1.0, 1.1 *
		        Memr[DP_PKCLAMP(peak)+i-1])
		Memr[DP_PKOLDRESULT(peak)+i-1] = Memr[DP_PKRESULT(peak)+i-1]
	    }

	    # Compute the new x, y, sky, and magnitude.
	    rel_bright = rel_bright + Memr[DP_PKRESULT(peak)] /
	        (1.0 + max (Memr[DP_PKRESULT(peak)] /
	        (MAX_DELTA_BRIGHTER * rel_bright), -Memr[DP_PKRESULT(peak)] /
		(MAX_DELTA_FAINTER * rel_bright)) / Memr[DP_PKCLAMP(peak)])

	    # Return if the star becomes too faint)
	    if (rel_bright < MIN_REL_BRIGHT)
		return (PKERR_FAINT)

	    if (DP_RECENTER(dao) == YES) {
	        x = x + Memr[DP_PKRESULT(peak)+1] / (1.0 +
	            abs (Memr[DP_PKRESULT(peak)+1]) / (MAX_DELTA_PIX *
		    Memr[DP_PKCLAMP(peak)+1]))
	        y = y + Memr[DP_PKRESULT(peak)+2] / (1.0 +
	            abs (Memr[DP_PKRESULT(peak)+2]) / (MAX_DELTA_PIX *
		    Memr[DP_PKCLAMP(peak)+2]))
	    }

	    if (DP_FITSKY(dao) == YES) {
		noise = sqrt ((abs (sky / DP_PHOTADU(dao)) + ronoise)) 
	        sky = sky + max (-3.0 * noise, min (Memr[DP_PKRESULT(peak)+
		    DP_PKNTERM(peak)-1], 3.0 * noise)) 
	    }

	    # Force at least one iteration before checking for convergence.
	    if (iter <= 1)
		next

	    # Start on the assumption the fit has converged.

	    redo = false

	    # Check for convergence. If the most recent computed correction 
	    # to the brightness is larger than 0.1% of the brightness or
	    # 0.05 * sigma of the brightness whichever is larger, convergence
	    # has not been achieved.

	    errmag = chiold * sqrt (Memr[DP_PKNORMAL(peak)])
	    if (clip) {
	        if (abs (Memr[DP_PKRESULT(peak)]) > max ((MAX_NEW_ERRMAG *
		    errmag), (MAX_NEW_RELBRIGHT1 * rel_bright))) {
		    redo = true
	        } else {
		    if (DP_RECENTER(dao) == YES) {
		        if (max (abs (Memr[DP_PKRESULT(peak)+1]),
	                    abs (Memr[DP_PKRESULT(peak)+2])) > MAX_PIXERR1)
		            redo = true
		    }
		    if (DP_FITSKY(dao) == YES) {
		        if (abs (Memr[DP_PKRESULT(peak)+DP_PKNTERM(peak)-1]) >
			    1.0e-4 * sky)
			    redo = true
		    }
		}
	    } else {
	        if (abs (Memr[DP_PKRESULT(peak)]) > max (errmag,
		    (MAX_NEW_RELBRIGHT2 * rel_bright))) {
		    redo = true
	        } else {
		    if (DP_RECENTER(dao) == YES) {
		        if (max (abs (Memr[DP_PKRESULT(peak)+1]),
	                    abs (Memr[DP_PKRESULT(peak)+2])) > MAX_PIXERR2)
		            redo = true
		    }
		    if (DP_FITSKY(dao) == YES) {
		        if (abs (Memr[DP_PKRESULT(peak)+DP_PKNTERM(peak)-1]) >
			    1.0e-4 * sky)
			    redo = true
		    }
		}
	    }
	    if (redo)
		next

	    if ((iter < DP_MAXITER(dao)) && (! clip)) {
		if (DP_CLIPEXP(dao) > 0)
		    clip = true
		call aclrr (Memr[DP_PKOLDRESULT(peak)], DP_PKNTERM(peak))
		call amaxkr (Memr[DP_PKCLAMP(peak)], MAX_CLAMPFACTOR,
		    Memr[DP_PKCLAMP(peak)], DP_PKNTERM(peak))
	    } else {
		sharp = 1.4427 * Memr[DP_PSFPARS(psffit)] *
	    	    Memr[DP_PSFPARS(psffit)+1] * numer / (DP_PSFHEIGHT(psffit) *
	    	    rel_bright * denom)
		if (sharp <= MIN_SHARP || sharp >= MAX_SHARP)
	    	    sharp = INDEFR
		break
	    }
	}

	if (iter > DP_MAXITER(dao))
	    iter = iter - 1
	if ((errmag / rel_bright) >= wcrit)
	    return (PKERR_FAINT)
	else
	    return (PKERR_OK)
end


# DP_FITBUILD -- Build the normal and vector of residuals for the fit.

int procedure dp_fitbuild (dao, subim, nx, ny, radius, x, y, xfrom_psf,
	yfrom_psf, rel_bright, sky, chiold, chi, clip, iter, clamp, normal,
	resid, deriv, nterm, numer, denom, sum_weight)

pointer	dao			# pointer to the DAOPHOT Structure
real	subim[nx,ny]		# subimage containing star
int	nx, ny			# size of the image subraster
real	radius			# the fitting radius
real	x, y			# initial estimate of the position
real	xfrom_psf, yfrom_psf	# distance from the psf star
real	rel_bright		# initial estimate of stellar brightness
real	sky			# the sky value associated with this star
real	chiold			# previous estimate of gof
real	chi			# estimated goodness of fit parameter
bool	clip			# clip the weights ?
int	iter			# current iteration number
real	clamp[ARB]		# clamp array
real	normal[nterm,ARB]	# normal matrix
real	resid[ARB]		# residual matrix
real	deriv[ARB]		# derivative matrix
int	nterm			# the number of terms to be fit
real	numer, denom		# used in sharpness calculation
real	sum_weight		# sum of the weights

int	i, j, ix, iy, lowx, lowy, highx, highy, npix
pointer	psffit
real	fitradsq, pererr, peakerr, datamin, datamax, read_noise
real	dx, dy, dxsq, dysq, radsq, dvdx, dvdy, d_pixval
real	pred_pixval, sigma, sigmasq, relerr, weight
real 	rhosq, pixval, dfdsig

real	dp_usepsf()

begin
	# Get the pointer to the PSF structure.
	psffit = DP_PSFFIT (dao)

	# Set up some constants.
	fitradsq = radius * radius
	read_noise = (DP_READNOISE(dao) / DP_PHOTADU(dao)) ** 2
	pererr = 0.01 * DP_FLATERR(dao)
	peakerr = 0.01 * DP_PROFERR(dao) /
	    (Memr[DP_PSFPARS(psffit)] * Memr[DP_PSFPARS(psffit)+1])
	if (IS_INDEFR (DP_MINGDATA(dao)))
	    datamin = -MAX_REAL
	else
	    datamin = DP_MINGDATA(dao)
	if (IS_INDEFR (DP_MAXGDATA(dao)))
	    datamax = MAX_REAL
	else
	    datamax = DP_MAXGDATA(dao)

	# Define the size of the subraster to be used in the fit.
	lowx = max (1, int (x - radius))
	lowy = max (1, int (y - radius))
	highx = min (nx, int (x + radius) + 1)	
	highy = min (ny, int (y + radius) + 1)	

	npix = 0
	do iy = lowy, highy {

	    dy = real (iy) - y
	    dysq = dy * dy

	    do ix = lowx, highx {

		# Is this pixel in the good data range.
		pixval = subim[ix,iy]
		if (pixval < datamin || pixval > datamax)
		    next

		# Is this pixel inside the fitting radius?
		dx = real(ix) - x
		dxsq = dx * dx
		radsq = (dxsq + dysq) / fitradsq
		if ((1.0 - radsq)  <= PEAK_EPSILONR)
		    next

		# We have a good pixel within the fitting radius.
		deriv[1] = dp_usepsf (DP_PSFUNCTION(psffit), dx, dy,
		    DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		    DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit), xfrom_psf,
		    yfrom_psf, dvdx, dvdy)
		if (((rel_bright * deriv[1] + sky) > datamax) && (iter >= 3))
		    next
		if (DP_RECENTER(dao) == YES) {
		    deriv[2] = rel_bright * dvdx
		    deriv[3] = rel_bright * dvdy
		}
		if (DP_FITSKY(dao) == YES)
		    deriv[nterm] = 1.0

	        # Get the residual from the PSF fit and the pixel
		# intensity as predicted by the fit
		d_pixval = (pixval - sky) - rel_bright * deriv[1]
		pred_pixval = max (0.0, pixval - d_pixval)

		# Calculate the anticipated error in the intensity of
		# in this pixel including READOUT noise, photon statistics
		# and the error of interpolating within the PSF.

		sigmasq = pred_pixval / DP_PHOTADU (dao) +  read_noise +
		    (pererr * pred_pixval) ** 2 + (peakerr *
		    (pred_pixval - sky)) ** 2
		if (sigmasq <= 0.0)
		    next
		sigma = sqrt (sigmasq)
		relerr = abs (d_pixval / sigma)

		# Compute the radial wweighting function.
		weight = 5.0 / (5.0 + radsq / (1.0 - radsq))

		# Now add this pixel into the quantities which go to make
		# up the sharpness index.
		rhosq = dxsq / Memr[DP_PSFPARS(psffit)] ** 2 + dysq /
		        Memr[DP_PSFPARS(psffit)+1] ** 2

		# Include in the sharpness calculation only those pixels
		# which are within NCORE_SIGMASQ core-sigmasq of the
		# centroid. This saves time and floating underflows
		# bu excluding pixels which contribute less than one
		# part in a million to the fit.

		if (rhosq <= NCORE_SIGMASQ) {
		    rhosq = 0.6931472 * rhosq
		    dfdsig = exp (-rhosq) * (rhosq - 1.0)
		    #pred_pixval = max (0.0, pixval - sky) + sky
		    numer = numer + dfdsig * d_pixval / sigmasq
		    denom = denom + (dfdsig ** 2) / sigmasq
		}


		# Derive the weight of this pixel. First of all the weight
		# depends on the distance of the pixel from the centroid of
		# the star --- it is determined from a function which is very
		# nearly unity for radii much smaller than the fitting radius,
		# and which goes to zero for radii very near the fitting
		# radius. Then reject any pixels with 10 sigma residuals
		# after the first iteration. 

		chi = chi + weight * relerr
		sum_weight = sum_weight + weight

		# Now the weight is scaled to the inverse square of the
		# expected mean error.

		weight = weight / sigmasq

		# Reduce the weight of a bad pixel. A pixel having a residual
		# of 2.5 sigma gets reduced to half weight; a pixel having
		# a residual of of 5.0 sigma gets weight 1/257.

		if (clip) 
		    weight = weight / (1.0 + (relerr / (DP_CLIPRANGE(dao) *
		        chiold)) ** DP_CLIPEXP(dao))

		# Now add this pixel into the vector of residuals
		# and the normal matrix.
		do i = 1, nterm {
		    resid[i] = resid[i] + d_pixval * deriv[i] * weight
		    do j = 1, nterm {
			normal[i,j] = normal[i,j] + deriv[i] * deriv[j] *
			    weight
		    }
		}

		npix = npix + 1
	    }
	}

	# Compute the goodness of fit index CHI. CHI is pulled toward its
	# expected value of unity before being stored in CHIOLD to keep the
	# statistics of a small number of pixels from dominating the error
	# analysis.

	if (sum_weight > nterm) {
	    chi = CHI_NORM * chi / sqrt (sum_weight * (sum_weight - nterm))
	    chiold = ((sum_weight - MIN_SUMWT) * chi + MIN_SUMWT) / sum_weight
	} else {
	    chi = 1.0
	    chiold = 1.0
	}

	return (npix)
end
