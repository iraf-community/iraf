include <mach.h>
include	<imhdr.h>
include "../lib/daophot.h"
include "../lib/daophotdef.h"
include "../lib/nstardef.h"
include "../lib/apsel.h"

define	CUT_FACTOR	   0.999998    # the pixel cutoff radius in fitrad ** 2
define	PEAK_ERR_NUMB	   0.027       # amplitude of flat/bias error
define	MINSEP		   2.773       # min separation in gsigma for merging
define	NITER_MAX	   12          # iteration limit for difficult stars
define	NITER_MED	   8           # iteration limit for moderate stars
define	NITER_MIN	   4	       # iteration limit for easy stars
define	WCRIT_INIT	   400.0       # initial max value of N/S squared
define	WCRIT_MAX	   0.25	       # max N/S squared for difficult stars
define	WCRIT_MED	   0.4444444   # max N/S squared for moderate stars 
define	WCRIT_MIN	   1.0         # max N/S squared for easy stars
define	INTERP_ERR	   0.0075      # amplitude of interp error
define	FRACTION_MINSEP    0.14	       # min sep in MINSEP*gsigma for merging
define	NCORE_SIGMASQ	   36.0	       # max gsigma-sq for sharpness
define	MAX_NSIGMA	   20.0        # max relative error in chi
define 	MIN_NPIX	   4	       # min pixels per star for fit
define	CHI_NORM	   1.2533      # sqrt (PI / 2.0)
define	MIN_SUMWT	   3.0	       # min value for radial weight sum
define	MAX_NEW_ERRMAG	   0.05        # 1st convergence check on magnitude
define	MAX_NEW_RELBRIGHT  0.001       # 2nd convergence check on magnitude
define	MAX_PIXERR	   0.01        # 1st convergence check on x/y positions
define	MAX_CENTROIDERR	   0.1         # 2nd convergence check on x/y positions
define	SIGN_CHECK	   -1.0e-37    # check for change of sign
define	MAX_DELTA_FAINTER  0.84        # max permitted brightness decrease
define	MAX_DELTA_BRIGHTER 5.25        # max permitted brightness increase
define	MAX_DELTA_PIX	   0.4         # max +/- change in x/y positions
define	MAX_PIX_INCREMENT  0.001       # test for nearness to edge of image
define	MIN_REL_BRIGHT	   1.0e-5      # min relative brightness
define	MIN_ITER	   4           # min number of iterations
define	MIN_FAINT	   0.25        # min N/S



# DP_NSTARFIT -- Do most of the actual work for the fiting proceduree of 
# NSTAR

int procedure dp_nstarfit (dao, im, nin_group, mean_sky, cdimen, chiold,
	clip, converge, iter)

pointer	dao			# pointer to the daophot structure
pointer im			# pointer to the input image
int	nin_group		# original group size
real	mean_sky		# the mean sky for the group
int	cdimen			# dimensions of the coefficient matrix
real	chiold			# old chi square
bool	clip			# clip the fit?
bool	converge		# did the fit converge
int	iter			# the current iteration

bool 	refit
int	i, j, k, xpix, ypix, group_size, nterm, tifaint, ifaint, flag, ncols
int	ixmin, ixmax, iymin, iymax, ifxmin, ifxmax, ifymin, ifymax, mindex
pointer	psffit, nstar, subim, ypixel, pixel, apsel

real	mingdata, maxgdata, peakerr, minsep, fitradsq, psfradsq, cutoff
real	xmin, xmax, ymin, ymax,	datum, ds, dswt, read_noise
real	weight, sumres, grpwt, pred_pixval, relerr, sigma, sigmasq, faint
real	tfaint, wcrit, xtemp, ytemp

bool	dp_nstmerge(), dp_ntomit(), dp_ntmin()
pointer imgs2r()
real	dp_ntsubtract()

begin
	# Define the daophot pointers.
	psffit = DP_PSFFIT (dao)
	apsel = DP_APSEL(dao)
	nstar = DP_NSTAR (dao)

	# Set up some daophot constants. At some point these will be computed
	# when the NSTAR task is started up instead of at the beginning of
	# each group fit. For the moment it is convenient and not too
	# costly to compute them here.

	fitradsq = DP_FITRAD (dao) ** 2
	psfradsq = DP_PSFRAD(dao) ** 2
	cutoff = CUT_FACTOR * fitradsq
	group_size = nin_group
	nterm = 3 * group_size
	peakerr = PEAK_ERR_NUMB / (DP_PSFSIGX(psffit) *
	    DP_PSFSIGY(psffit)) ** 2
	read_noise = (DP_READ_NOISE(dao) / DP_PHOT_ADC(dao)) ** 2
	minsep = MINSEP * (DP_PSFSIGX(psffit) ** 2 + DP_PSFSIGY(psffit) ** 2)
	if (IS_INDEFR(DP_MINGDATA(dao)))
	    mingdata = -MAX_REAL
	else
	    mingdata = DP_MINGDATA(dao)
	if (IS_INDEFR(DP_MAXGDATA(dao)))
	    maxgdata = MAX_REAL
	else
	    maxgdata = DP_MAXGDATA(dao)

	# Begin fitting the current group of stars.
	repeat {

	    # Initialize the convergence criteria.
	    converge = false

	    # Check to see if any two stars have merged. 
	    if (group_size < 1) {
	        return (group_size)

	    } else {

	        # Set up the critical magnitude S/N ratio for star rejection.
	        if (iter >= NITER_MAX)
	            wcrit = WCRIT_MAX
	        else if (iter >= NITER_MED)
	            wcrit = WCRIT_MED
	        else if (iter >= NITER_MIN)
	            wcrit = WCRIT_MIN
	        else
	            wcrit = WCRIT_INIT

	        # Initialize arrays.
	        call aclrr (Memr[DP_APCHI(apsel)], group_size)
	        call aclrr (Memr[DP_SUMWT(nstar)], group_size)
	        call aclrr (Memr[DP_NUMER(nstar)], group_size)
	        call aclrr (Memr[DP_DENOM(nstar)], group_size)
	        call alimr (Memr[DP_APXCEN(apsel)], group_size, xmin, xmax) 
	        call alimr (Memr[DP_APYCEN(apsel)], group_size, ymin, ymax) 

		# Check to see whether any two stars are within the critical
		# difference from each other.

	        if ((group_size > 1) && dp_nstmerge (Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		    Memr[DP_APERR(apsel)], group_size, minsep, wcrit,
		    i, j, k)) {

		    # Compute the new centroid and brightness.
		    call dp_nstcen (Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)], i, j, k)

		    # Print out verbose comments.
		    if (DP_VERBOSE (dao) == YES)  {
		        call printf (
			    "\tStar %-5d has merged with star %-5d\n")
		            call pargi (Memi[DP_APID(apsel)+k-1])
		            call pargi (Memi[DP_APID(apsel)+i-1])
		    }

		    # Now remove the k-th star from the group.
		    call dp_remove (k, group_size, Memi[DP_APID(apsel)],
		        Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		        Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)])

		    # After deleting a star, resize the matrix, release all of
		    # the clamps and back up the iteration counter.

		    nterm = 3 * group_size
		    clip = false
		    call aclrr (Memr[DP_XOLD(nstar)], nterm)
		    call amovkr (1.0, Memr[DP_XCLAMP(nstar)], nterm)
		    iter = max (1, iter - 1)
		    next
	        }

	    }	   		

	    # Now we can proceed with the iteration. If this is the first
	    # iteration read in the subraster. Determine the size of the
	    # subraster we need to extract from the image for this group.

	    if (iter == 1) {
	        ixmin = max (1, int (xmin - DP_PSFRAD(dao) -
		    DP_FITRAD(dao)) + 1)
	        iymin = max (1, int (ymin - DP_PSFRAD(dao) -
		    DP_FITRAD(dao)) + 1)
	        ixmax = min (IM_LEN(im,1), int (xmax + DP_PSFRAD(dao) +
		    DP_FITRAD(dao)))
	        iymax = min (IM_LEN(im,2), int (ymax + DP_PSFRAD(dao) +
		    DP_FITRAD(dao)))
	        subim = imgs2r (im, ixmin, ixmax, iymin, iymax)
	        ncols = ixmax - ixmin + 1
	    }

	    # Compute the area on the subraster that is off interest to
	    # the current iteration.

	    ifxmin = max (ixmin, int (xmin - DP_FITRAD(dao)) + 1)
	    ifymin = max (iymin, int (ymin - DP_FITRAD(dao)) + 1)
	    ifxmax = min (ixmax, int (xmax + DP_FITRAD(dao)))
	    ifymax = min (iymax, int (ymax + DP_FITRAD(dao)))

	    # Zero the normal matrix and the vector of residuals.

	    call aclrr (Memr[DP_V(nstar)], nterm)
	    call aclrr (Memr[DP_C(nstar)], cdimen * cdimen)
	    call aclri (Memi[DP_NPIX(nstar)], group_size)

	    sumres = 0.0
	    grpwt = 0.0

	    # Loop over the pixels in the subraster.

	    ypixel = subim + (ifymin - iymin) * ncols + ifxmin - ixmin - 1
	    do ypix = ifymin, ifymax {

		ytemp = ypix
	        pixel = ypixel

	        do xpix = ifxmin, ifxmax {

		    xtemp = xpix
		    pixel = pixel + 1
		    datum = Memr[pixel]

		    # Reject pixel if not in good data range.
		    if ((datum < mingdata) || (datum > maxgdata))
		        next

		    # If this pixel is within one fitting radius of at
		    # least one star then include it in the solution.
		    # While figuring this out, compute the squared distance
		    # of this pixel from the centroid of each star in the
		    # group, and sum its contribution to the number
		    # of pixels within one fitting radius of each star.

		    if (dp_ntomit (Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memr[DP_RPIXSQ(nstar)],
			Memi[DP_SKIP(nstar)], Memi[DP_NPIX(nstar)],
			group_size, xtemp, ytemp, cutoff))
		        next

		    # Subtract the mean sky from the pixel.
		    ds = datum - mean_sky

		    # Now loop over the stars and subtract from this pixel
		    # the light contribution from each star within one psf
		    # radius.
		    #
		    # The condition equation for pixel[xpix,ypix] is
		    #
		    # residual = data[xpix,ypix] - sky - sum (scale *
		    #            psf[xpix-xc,ypix-yc])
		    #
		    # The scale, xc, and yc are iterated until
		    #
		    #          sum (weight * residual ** 2)
		    #
		    # is minimized. Weight will be a function of 1) the
		    # distance of the pixel from the center of the nearest
		    # star, 2) the model-predicted brightness of the pixel
		    # (taking into consideration the readout noise, the
		    # photons/ADU, and the interpolating error of the PSF,
		    # and, 3) the size of the residual itself. One is
		    # necessary to prevent the non-linear least squares
		    # solution from oscillating: oftimes it will come
		    # to pass that if you include a pixel in the solution
		    # then the predicted shift of the centroid will cause
		    # that pixel to be excluded in the next iteration, and the 
		    # new predicted shift of the centroid will cause that
		    # pixel to be included again. This could go on ad
		    # infinitum. The cure is to have the weight of the pixel
		    # go asymptotically to zero as its distance from the
		    # stellar centroid approaches the fitting radius. In
		    # the case like that just described, the solution can
		    # then find a real minimum of the sum of the weighted
		    # squared residuals with that pixel at some low-weight
		    # position just inside the fitting radius. Two is
		    # just sensible weighting. Three is a crude attempt
		    # at making the solution more robust against bad pixels.

		    weight = dp_ntsubtract (psffit, Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memr[DP_RPIXSQ(nstar)],
		        Memr[DP_APMAG(apsel)], Memi[DP_SKIP(nstar)],
		        Memr[DP_X(nstar)], group_size, xtemp, ytemp, ds,
			DP_VARPSF(dao), psfradsq, fitradsq)

		    # At this point the vector X contains the first
		    # derivative of the condition equation, for the pixel
		    # under consideration, with respect to each of the
		    # fitting parameters for all of these stars. We now need
		    # to add these values into the normal matrix and the vector
		    # of residuals.

		    # The expected random error in the pixel is the quadratic
		    # sum of the Poisson statistics, plus the readout noise,
		    # plus an estimated error of 0.75% of the total brightness
		    # of the total brightness for the difficulty of flat-
		    # fielding and bias subtraction, plus an estimated error
		    # of the same fraction of the fourth derivative at the
		    # peak of the profile, to account for the difficulty 
		    # of accurately interpolating within the point-spread
		    # function. The fourth derivative of the PSF is
		    # proportional to H / sigma ** 4 (sigma is the Gaussian
		    # width parameter for the stellar core); using the geometric
		    # mean of sigma(x) and sigma(y), this becomes H / [sigma(x)
		    # * sigma(y)] ** 2. The ratio of the fitting error to this
		    # quantity is estimated to be approximately 0.027 from a
		    # good-seeing CTIO frame. (This is probably a function of
		    # seeing, sampling etc.)

		    # Get the residual from the PSF fit and the pixel
		    # intensity as predicted by the fit. Pred_pixval = raw data
		    # minus residual = model predicted value of the intensity at
		    # this point.

		    pred_pixval = max (0.0, datum - ds)
		    sigmasq = pred_pixval / DP_PHOT_ADC (dao) + read_noise + 
		        (INTERP_ERR * pred_pixval) ** 2 + (peakerr *
		        (pred_pixval - mean_sky)) ** 2
		    relerr = abs (ds) / sqrt (sigmasq)

		    # If this pixel has a MAX_NSIGMA error then reject it out of
		    # hand if clipping is enabled.

		    if (clip && (relerr > (MAX_NSIGMA * chiold)))
		        next

		    # Add this residual into the weighted sum of the
		    # absolute relative residuals.

		    sumres = sumres + relerr * weight
		    grpwt = grpwt + weight

		    # The following value is the model predicted value of the
		    # intensity at this point.

		    pred_pixval = max (0.0, datum - mean_sky) + mean_sky
		    sigma = pred_pixval / DP_PHOT_ADC (dao) + read_noise +
		        (INTERP_ERR * pred_pixval) ** 2 + (peakerr *
		        (pred_pixval - mean_sky)) ** 2

		    # Add into the accumulating sums of the weighted 
		    # absolute relative residuals and of the image sharpness
		    # parameter for each of the stars. Include in the
		    # sharpness index only those pixels within NCORE_SIGMA
		    # sigma of the centroid of the object. This saves time
		    # and floating underflows by excluding pixels
		    # which contribute less than about one part in a
		    # million to the index.

		    call dp_acsharp (Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memi[DP_SKIP(nstar)],
			Memr[DP_NUMER(nstar)], Memr[DP_DENOM(nstar)],
			Memr[DP_SUMWT(nstar)], Memr[DP_APCHI(apsel)],
			group_size, xtemp, ytemp, DP_PSFSIGX(psffit),
			DP_PSFSIGY(psffit), ds, sigma, relerr, weight)

		    # If clipping is in effect, reduce the weight of a bad
		    # pixel. A pixel having a residual of 2.5 sigma gets
		    # reduced to half weight and one with a rersidual of 5
		    # sigma gets weight of 1 / 257.

		    weight = weight / sigmasq
		    if (clip) 
		        weight = weight / (1.0 + (0.4 * relerr / chiold) ** 8)
		    dswt = ds * weight

		    # Work this pixel into the normal matrix.
		    call dp_mataccum (Memr[DP_X(nstar)], Memr[DP_C(nstar)],
		        cdimen, Memr[DP_V(nstar)],  Memi[DP_SKIP(nstar)],
		        group_size, weight, dswt)

	        }

	        ypixel = ypixel + ncols
	    }

	    # Make sure that every star in the group has at least MIN_FIT_PIXEL
	    # pixels within one fitting radius.

	    refit = dp_ntmin (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APMSKY(apsel)], Memi[DP_NPIX(nstar)], group_size,
	        nterm, DP_VERBOSE(dao))
	    if (group_size < 1)
	        return (group_size)
	    if (refit)
	        next

	    # Reflect the normal matrix across the diagonal.
	    call dp_mreflect (Memr[DP_C(nstar)], cdimen, nterm)

	    # Compute the robust estimate of the standard deviation of the
	    # residuals for the group as a whole, and for each star. This
	    # estimate is sqrt (PI/2) * weighted mean absolute relative
	    # residual. Do you like that "absolute relative" stuff? (PBS)
	    # NO! (LED)
	    #
	    #      CHI = CHI_NORM * SUM (weight * resid) / (# of pixels)
	    # 
	    # This gets corrected for bias by being multiplied by
	    #
	    #	SQRT (# of pixels) / (# of pixels - 3)

	    # Then the value is driven towards unity, depending on
	    # exactly how many pixels were involved: if CHIOLD is based
	    # on a total weight of 3, then it is extremely poorly determined
	    # and we just want to keep CHIOLD = 1. The larger GRPWT is, the
	    # better determined CHIOLD is, and the less we want to force it
	    # toward unity. So, just take the weighted average of CHIOLD and 
	    # unity, with weights GRPWT - 3 and 1, respectively.

	    if (grpwt > MIN_SUMWT) {
	        chiold = CHI_NORM * sumres * sqrt (1.0 / (grpwt * (grpwt -
	            MIN_SUMWT)))
	        chiold = ((grpwt - MIN_SUMWT) * chiold + MIN_SUMWT) / grpwt
	    }

	    # CHIOLD has been pulled toward its expected value of unity to
	    # keep the statistics of a small number of pixels from completely
	    # dominating the error analysis. Similarly, the photometric 
	    # errors for the individual stars will be pulled toward unity
	    # now. Later on, if the number of stars in the group is 
	    # greated than one, CHI will be nudged toward the group average.
	    # In order to work optimally, of course, this requires that	
	    # the # of photons / ADC, the READ NOISE and the other noise
	    # contributors are properly specified.

	    call dp_ntchi (Memr[DP_SUMWT(nstar)], Memr[DP_APCHI(apsel)],
	        group_size, chiold)

	    # Invert the matrix and solve.
	    call invers (Memr[DP_C(nstar)], cdimen, nterm, flag)
	    call mvmul (Memr[DP_C(nstar)], cdimen, nterm, Memr[DP_V(nstar)],
	        Memr[DP_X(nstar)])

	    if (iter <= 1)
	        refit = true
	    else
	        refit = false

	    # In the beginning, the brightness of each star will be permitted
	    # to change by no more than 2 magnitudes per iteration, and the x,y
	    # coordinates of each centroid will be permitted to change by
	    # no more than 0.4 pixels per iteration. Any time that the 
	    # parameter correction changes sign from one iteration to the
	    # next, the maximum permissible change will be reduced by a factor
	    # of two. These clamps are released any time a star disappears.

	    call dp_ntclamp (Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
	        Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
	        Memr[DP_APSHARP(apsel)], Memr[DP_APCHI(apsel)],
	        Memr[DP_NUMER(nstar)], Memr[DP_DENOM(nstar)], group_size,
	        DP_PSFSIGX(psffit), DP_PSFSIGY(psffit), DP_PSFHEIGHT(psffit),
		Memr[DP_C(nstar)], cdimen, Memr[DP_X(nstar)],
	        Memr[DP_XOLD(nstar)], Memr[DP_XCLAMP(nstar)], chiold, clip,
		refit)

	    # Check whether the estimated centroid of the any star has
	    # moved so far out of the limits of the picture that it has fewer
	    # than 4 or 5 pixels within one fitting radius.

	    call dp_ntcentroid (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APMSKY(apsel)], group_size, int (IM_LEN(im,1)),
	        int (IM_LEN(im,2)), fitradsq, refit, DP_VERBOSE(dao))

	    if (group_size < 1)
	        return (group_size)

	    # Update matrix dimensions
	    nterm = 3 * group_size

	    # Now check whether any of the stars is too faint (more than 12.5
	    # magnitudes fainter than the PSF star). If several stars are too
	    # faint, delete the faintest one, and set the brightness of the 
	    # other faint ones to 12.5 magnitudes below the PSF star. That way
	    # on the next iteration we will see whether these stars want to 
	    # grow or to disappear.

	    faint = 0.0
	    ifaint = 0
	    call dp_ntfmag (Memr[DP_APMAG(apsel)], group_size, tfaint, tifaint)

	    # If at least one star is more than 12.5 magnitudes fainter
	    # than the PSF then ifaint is the relative index of the faintest
	    # of them, and faint is the relative brightness of the
	    # faintest of them.

	    if (tifaint <= 0) {

	        # If the solution has not converged and if the number of
	        # iterations is less than MIN_ITER, perform another iteration
	        # with no questions asked.

	        if ((refit) && (iter < MIN_ITER))
		    return (group_size)
	    
	        # If the solution doesn't think it has converged, after the
	        # fourth iteration delete the least certain star if it is less
	        # less than a one-sigma detection; after the eighth iteration
	        # delete the least certain star if it is less than a 1.5 sigma
	        # detection; after the twelfth iteration OR if the solution
	        # thinks it has converged, delete the least certain star if it 
	        # is less than a two-sigma detection.

	        call dp_fsnoise (Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		    group_size, faint, ifaint)

	        if ((refit) && (iter < DP_MAXITER (dao)) && (faint < wcrit))
		    return (group_size)
	    }

	    if ((tifaint > 0) || (faint >= MIN_FAINT)) {

	        # Either (a) the solution thinks it has not converged
	        # and the faintest star is more uncertain than sqrt(wcrit)
	        # or (b) the solution thinks it has converged and the
	        # faintest star is more uncertain than two-sigma.

	        if (DP_VERBOSE (dao) == YES) {
		    mindex = max (tifaint, ifaint)
		    call printf (
		    "\tStar %-5d has been deleted because it is too faint\n")
		        call pargi (Memi[DP_APID(apsel)+mindex-1])
	        }

	        call dp_remove (max (tifaint, ifaint), group_size,
	            Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		    Memr[DP_APMSKY(apsel)])

	        if (group_size < 1)
		    return (group_size)

	        nterm = 3 * group_size
	        call aclrr (Memr[DP_XOLD(nstar)], nterm)
	        call amovkr (1.0, Memr[DP_XCLAMP(nstar)], nterm)
	        clip = false
	        iter = max (1, iter - 1)
	        next
	    }

	    # Solution has either converged or gone to the maximum number
	    # of iterations.

	    if ((iter < DP_MAXITER (dao)) && (! clip)) {

	        # The first convergence milestone has been reached. Turn on the
	        # clipper, loosen the clamps and keep on going.

	        clip = true
	        converge = false
	        call aclrr (Memr[DP_XOLD(nstar)], nterm)
	        call amaxkr (Memr[DP_XCLAMP(nstar)], MIN_FAINT,
	            Memr[DP_XCLAMP(nstar)], nterm)
	        return (group_size)
	    }

	    converge = true		    

	} until (converge)

	return (group_size)
end


# DP_NSTMERGE -- Decide whether two stars in a group should merge.

bool procedure dp_nstmerge (xcen, ycen, mag, magerr, group_size, minsep, wcrit,
	i, j, k)

real	xcen[ARB]		# array of x centers	
real	ycen[ARB]		# array of y centers	
real	mag[ARB]		# array of magnitudes
real	magerr[ARB]		# array of magnitude centers
int	group_size		# group size
real	minsep			# minimum separation
real	wcrit			# critical size
int	i, j, k			# output indices

real	separation, fminsep

begin
	fminsep = FRACTION_MINSEP * minsep
	do i = 1, group_size {
  	    do j = 1,  i - 1 {

		# Compute the separation.
	        separation = (xcen[j] - xcen[i]) ** 2 +
		    (ycen[j] - ycen[i]) ** 2
		if (separation > minsep)
		    next

		# Find the fainter of the two stars.
		k = j
		if (mag[i] < mag[j]) 
		    k = i
		if ((separation <  fminsep) || ((magerr[k] /
		    mag[k]) ** 2 > wcrit))
		        return (true)
	    }
	}

	return (false)
end


# DP_NSTCEN --  Recompute the centroid and brightness of the i-th star.

procedure dp_nstcen (xcen, ycen, mag, i, j, k)

real	xcen[ARB]		# the x centers
real	ycen[ARB]		# the y centers
real	mag[ARB]		# the magnitudes
int	i, j, k			# array indices

begin
	# Now eliminate the fainter of the two stars. The k-th
	# star is now the fainters of the two, the i-th the
	# brighter.

        if (mag[i] < mag[j])
            i = j

	# Now replace the centroid of the i-th star with the
	# weighted mean of the most recent estimates of the 
	# centroids of the i-th and the k-th stars, and the
	# brightness of i-th with the sum of the brightnesses.

	xcen[i] = xcen[i] * mag[i] + xcen[k] * mag[k]
	ycen[i] = ycen[i] * mag[i] + ycen[k] * mag[k]
	mag[i] = mag[i] + mag[k]
	xcen[i] = xcen[i] / mag[i]
	ycen[i] = ycen[i] / mag[i]
end


# DP_NTOMIT -- Check whether a pixel is within one fitting radius of another
# star. 

bool procedure dp_ntomit (xcen, ycen, rpixsq, skip, npix, group_size,
	fx, fy, cutoff)

real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	rpixsq[ARB]		# radii squared
int	skip[ARB]		# skip array
int	npix[ARB]		# number of pixels
int	group_size		# maximum group size
real	fx, fy			# pixel position in image
real	cutoff			# factor * fitting radius squared

bool	omit
int	i

begin
	omit = true
	do i = 1, group_size {
	    skip[i] = YES
	    rpixsq[i] = (fx - xcen[i]) ** 2 + (fy - ycen[i]) ** 2
	    if (rpixsq[i] > cutoff)
		next
	    skip[i] = NO
	    npix[i] = npix[i] + 1
	    omit = false
	}	 

	return (omit)
end


# DP_NTSUBTRACT -- Procedure to subtract the contribution of a particular
# pixel from a particular star.

real procedure dp_ntsubtract (psffit, xcen, ycen, rpixsq, mag, skip, x,
	group_size, fx, fy, ds, varpsf, psfradsq, fitradsq)

pointer	psffit			# pointer to psf structure
real	xcen[ARB]		# pointer to x centers array
real	ycen[ARB]		# pointer to y centers array
real	rpixsq[ARB]		# array of pixel differences
real	mag[ARB]		# magnitudes array
int	skip[ARB]		# skip pixel array
real	x[ARB]			# x accumulate array
int	group_size		# size of the group
real	fx, fy			# center of pixel in image
real	ds			# pixel value
int	varpsf			# variable psf
real	psfradsq		# psf radius squared
real	fitradsq		# fit radius squared

int	i, i3, k
real	weight, dx, dy, deltax, deltay, val, dvdx, dvdy, rsq
real	dp_evalpsf()

begin
	weight = 0.0
	do i = 1, group_size {
	    if (rpixsq[i] > psfradsq)
		next
	    dx = fx - xcen[i]
	    dy = fy - ycen[i]
	    deltax = xcen[i] - DP_XPSF(psffit)
	    deltay = ycen[i] - DP_YPSF(psffit)
	    val = dp_evalpsf (dx, dy, psffit, deltax, deltay, varpsf,
	        dvdx, dvdy)
	    ds = ds - mag[i] * val
	    rsq = rpixsq[i] / fitradsq
	    if (skip[i] == YES)
		next
	    weight = max (weight, 5.0 / (5.0 + rsq / (1.0 - rsq)))
	    skip[i] = NO
	    i3 = 3 * i
	    k = i3 - 2
	    x[k] = -val
	    k = i3 - 1
	    x[k] = mag[i] * dvdx
	    x[i3] = mag[i] * dvdy
	}

	return (weight)
end


# DP_ACSHARP -- Procedure to accumulate sums of the weighted absolute
# relative residuals and the image sharpness parameter for each of the
# stars.

procedure dp_acsharp (xcen, ycen, skip, numer, denom, sumwt, chi, group_size,
	fx, fy, sigmax, sigmay, ds, sigma, relerr, weight)

real	xcen[ARB]		# array of object x centers
real	ycen[ARB]		# array of object y centers
int	skip[ARB]		# skip pixel array
real	numer[ARB]		# numerator array
real	denom[ARB]		# denominator array
real	sumwt[ARB]		# array of summed weights
real	chi[ARB]		# array of chis
int	group_size		# group size paramter.
real	fx, fy			# position of data in image
real	sigmax, sigmay		# gaussian core widths in x and y
real	ds			# data residual
real	sigma			# sigma array
real	relerr			# relative error
real	weight			# weight

int	i
real	rhosq, dfdsig

begin
	do i = 1, group_size {

	    if (skip[i] == YES)
		next

	    # Include in the sharpness index only those pixels
	    # within NCORE_SIGMASQ of the centroid of the
	    # object. (This saves time and floating underflows
	    # by excluding pixels which contribute very little
	    # to the index.

	    chi[i] = chi[i] + relerr * weight
	    sumwt[i] = sumwt[i] + weight
	    rhosq = ((xcen[i] - fx) / sigmax) ** 2 + ((ycen[i] - fy) /
	        sigmay) ** 2
	    if (rhosq > NCORE_SIGMASQ)
		next

	    rhosq = 0.5 * rhosq
	    dfdsig = exp (-rhosq) * (rhosq - 1.0)
	    numer[i] = numer[i] + dfdsig * ds / sigma
	    denom[i] = denom[i] + (dfdsig ** 2) / sigma
	}			
end


# DP_MATACCUM -- Procedure to accumulate the data into the matrices.

procedure dp_mataccum (x, c, cdimen, v, skip, group_size, weight, dswt)

real	x[ARB]			# x array
real	c[cdimen,ARB]		# coefficient matrix
int	cdimen			# dimensions of the coefficient matrix
real	v[ARB]			# vector array
int	skip[ARB]		# skip vector
int	group_size		# size of the group
real	weight			# weight
real	dswt			# data weight

int	i, i3, i3m2, k, j, l

begin
	do i = 1, group_size {
	    if (skip[i] == YES)
		next
	    i3 = i * 3
	    i3m2 = i3 - 2
	    do k = i3m2, i3
		v[k] = v[k] + x[k] * dswt
	    do j = 1, i {
		if (skip[j] == YES)
		    next
		do k = i3m2, i3 {
		    do l = 3 * j - 2, min (k, 3 * j)
			c[k,l] = c[k,l] + x[k] * x[l] * weight
		}
	    }
	}
end


# DP_NTMIN -- Make sure that every star in the group has at least 
# MIN_NPIX pixels within one fitting radius.

bool procedure dp_ntmin (ids, xcen, ycen, mag, nsky, npix, group_size, nterm,
	verbose)

int	ids[ARB]		# integer ids
real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of magnitudes
real	nsky[ARB]		# array of skys
int	npix[ARB]		# number of pixels
int	group_size		# size of the group
int	nterm			# number of terms
int	verbose			# verbose flag

bool	redo
int	i

begin
	redo = false
	do i = 1, group_size {
	    if (npix[i] >= MIN_NPIX)
		next
	    redo = true
	    if (verbose == YES) {
		call printf (
		    "\tStar %-5d has been deleted: too few good pixels\n")
		    call pargi (ids[i])
	    }
	    call dp_remove (i, group_size, ids, xcen, ycen, mag, nsky)
	    if (group_size <= 0)
		return (redo)
	    nterm = 3 * group_size
	}

	return (redo)
end


# DP_MREFLECT -- Reflect the normal matrix around the diagonal.

procedure dp_mreflect (c, cdimen, nterm)

real	c[cdimen,ARB]		# coefficient matrix
int	cdimen			# dimension of the c matrix
int	nterm			# number of terms

int	l, k

begin
	# Reflect the normal matrix across the diagonal.
	do l = 2, nterm {
	    do k = 1, l - 1
		c[k,l] = c[l,k]
	}
end


# DP_NTCHI -- Compute the chi value for each star.

procedure dp_ntchi (sumwt, chi, group_size, chiold)

real	sumwt[ARB]		# sum of the weights
real	chi[ARB]		# the chis:wq
int	group_size		# size of the group
real	chiold			# value of old chi

int	i

begin
	do i = 1, group_size {
	    if (sumwt[i] > MIN_SUMWT) {
		chi[i] = CHI_NORM * chi[i] * sqrt (1.0 / ((sumwt[i] -
		    MIN_SUMWT) * sumwt[i]))
		chi[i] = ((sumwt[i] - MIN_SUMWT) * chi[i] + MIN_SUMWT) /
		    sumwt[i]
	    } else 
		chi[i] = chiold
	}
end


# DP_NTCLAMP -- Restrict the amount the solution can vary on each iteration.

procedure dp_ntclamp (xcen, ycen, mag, magerr, sharp, chi, numer, denom,
	group_size, sigmax, sigmay, height, c, cdimen, x, xold, clamp,
	chiold, clip, redo)

real	xcen[ARB]		# x centers array
real	ycen[ARB]		# y centers array
real	mag[ARB]		# magnitude array
real	magerr[ARB]		# magnitude errors array
real	sharp[ARB]		# array of sharpness parameters
real	chi[ARB]		# array of chi squared values
real	numer[ARB]		# array of numerators
real	denom[ARB]		# array of denominatores
int	group_size		# size of the group
real	sigmax, sigmay		# gaussian core widths in x and y
real	height			# gaussian height
real	c[cdimen, ARB]		# coefficient matrix
int	cdimen			# dimensions of c
real	x[ARB]			# x vector
real	xold[ARB]		# old x vector
real	clamp[ARB]		# clamp on solution matrix
real	chiold			# old value of chi
bool	clip			# clip the matrix
bool	redo			# redo the solution

int	i, l, j, k

begin
	do i = 1, group_size {

	    sharp[i] = 2.0 * sigmax * sigmay * numer[i] /
		       (mag[i] * height * denom[i])
	    l = 3 * i
	    k = l - 1
	    j = l - 2

	    # There are two milestones in the convergence process: the fits
	    # proceed normally until each star's magnitude changes by less
	    # than its standard error or MAX_NEW_ERRMAG magnitudes, whichever
	    # is greater, and its x and y centroids change by less than 0.1 
	    # pixel. At this point the least squares begins to reject any 
	    # pixel with a residual greater than 20 sigma and begins to 
	    # apply the down-weighting of other pixels with large residuals
	    # as described above. The fits then continue until each star's
	    # magnitude changes by less than MAX (MAX_NEW_ERRMAG * std. error,
	    # MAX_NEW_RELBRIGHT magnitude), ad its centroids change by
	    # less than MAX_PIXERR pixel.

	    if (! redo)  {
	        if (clip) {
		    if (abs (x[j]) > max (MAX_NEW_ERRMAG * chi[i] *
		        sqrt (c[j,j]), MAX_NEW_RELBRIGHT * mag[i]))
		        redo = true
		    else if (max (abs(x[k]), abs(x[l])) > MAX_PIXERR)
		        redo = true
	        } else {
		    if (abs (x[j]) > max (chi[i] * sqrt (c[j,j]),
		        MAX_NEW_ERRMAG * mag[i]))
		        redo = true
		    else if (max (abs (x[k]), abs (x[l])) > MAX_CENTROIDERR)
		        redo = true
	        }
	    }

	    # If any correction has changed sign since the last
	    # iteration, reduce the maximum permissible change by
	    # a factor of two.

	    if ((xold[j] * x[j] / mag[i] ** 2) < SIGN_CHECK) 
		clamp[j] = 0.5 * clamp[j]
	    if ((xold[k] * x[k]) < SIGN_CHECK)
		clamp[k] = 0.5 * clamp[k]
	    if ((xold[l] * x[l]) < SIGN_CHECK)
		clamp[l] = 0.5 * clamp[l]

	    # Note that the sign of the correction is such that it
	    # must be SUBTRACTED from the current value of the 
	    # parameter to get the improved parameter value. This being
	    # the case, if the correction to the brightness is
	    # negative (the least-squares thinks that the star should
	    # be brighter) a change of 1 magnitude is a change of a
	    # factor of 2.5; if the brightness correction is positive
	    # (the star should be fainter) a change of 1 magnitude
	    # is a change of 60%.

	    mag[i] = mag[i] - x[j] / (1.0 + max (x[j] / (MAX_DELTA_FAINTER *
	        mag[i]), -x[j] / (MAX_DELTA_BRIGHTER * mag[i])) / clamp[j])
	    xcen[i] = xcen[i] - x[k] / (1.0 + abs(x[k]) / (clamp[k] *
	        MAX_DELTA_PIX))
	    ycen[i] = ycen[i] - x[l] / (1.0 + abs(x[l]) / (clamp[l] *
	        MAX_DELTA_PIX))

	    xold[j] = x[j]
	    xold[k] = x[k]
	    xold[l] = x[l]

	    # Drive the magnitude error toward the expected value.
	    magerr[i] = c[j,j] * (group_size * chi[i] ** 2 + (group_size - 1) *
	        chiold ** 2) / (2.0 * group_size - 1.0)
	}
end


# DP_NTCENTROID -- Check the new centroids to see if they have moved too
# far off the edge of the image.

procedure dp_ntcentroid (ids, xcen, ycen, mag, nsky, group_size, ncols, nlines,
	fitradsq, redo, verbose)

int	ids[ARB]			# array of ids
real	xcen[ARB]			# array of x centers
real	ycen[ARB]			# array of y centers
real	mag[ARB]			# array of magnitudes
real	nsky[ARB]			# array of sky value
int	group_size			# size of the group
int	ncols, nlines			# size of the image
real	fitradsq			# fit radius squared
bool	redo				# redo fit
int	verbose				# verbose mode

int	i
real	dx, dy

begin
	# Check whether the centroid of any star has moved so far outside
	# the picture that it has fewer than four or five pixels within
	# one fitting radius.

	do i = 1, group_size {

	    # If the centroid of the star is outside the picture in x or
	    # y, then DX or DY is its distance from the center of the edge 
	    # pixel; otherwise DX and DY are zero.

	    dx = max (1.0 - xcen[i], xcen[i] - ncols, 0.0)
	    dy = max (1.0 - ycen[i], ycen[i] - nlines, 0.0)
	    if ((dx <= MAX_PIX_INCREMENT) && (dy <= MAX_PIX_INCREMENT))
		    next
	    if (((dx + 1.0) ** 2 + (dy + 1.0) ** 2) < fitradsq)
		next

	    # Print a warning message about the deleted star.

	    if (verbose == YES) {
		call printf (
		"\tStar %-5d has been deleted: new center too far off image\n")
		    call pargi (i)
	    }

	    call dp_remove (i, group_size, ids, xcen, ycen, mag, nsky)
	    if (group_size < 1)
		break
	    redo = true
	}
end


# DP_NTFMAG -- Check for faint stars.

procedure dp_ntfmag (mag, group_size, faint, ifaint)

real	mag[ARB]		# array of magnitudes
int	group_size		# size of the group
real	faint			# faintest magnitude
int	ifaint			# index of faintest magnitude

int	i

begin
	faint = 1.0
	ifaint = 0
	do i = 1, group_size {
	    if (mag[i] > MIN_REL_BRIGHT)
		next
	    if (mag[i] <= faint) {
		faint = mag[i]
		ifaint = i
	    }
	    mag[i] = MIN_REL_BRIGHT
	}
end


# DP_FSNOISE -- Compute the smallest signal to noise ratio.

procedure dp_fsnoise (mag, magerr, group_size, faint, ifaint)

real	mag[ARB]		# array of magnitudes
real	magerr[ARB]		# array of magnitude errors
int	group_size		# size of group
real	faint			# faint value
int	ifaint			# faint index

int	i
real	weight

begin
	faint = 0.0
	ifaint = 0
	do i = 1, group_size {
	    weight = magerr[i] / mag[i] ** 2
	    if (weight < faint)
		next
	    faint = weight
	    ifaint = i
	}
end


# DP_REMOVE -- Remove the i-th star from the list of stars in the current
# group.

procedure dp_remove (i, nstar, ids, xcen, ycen, mag, sky)

int	i, nstar
int	ids[ARB]
real	xcen[ARB]
real	ycen[ARB]
real	mag[ARB]
real	sky[ARB]

int	ihold
real	xhold, yhold, shold, mhold

begin
	if (i != nstar) {
	    ihold = ids[i]
	    xhold = xcen[i]
	    yhold = ycen[i]
	    shold = sky[i]
	    mhold = mag[i]
	    ids[i] = ids[nstar]
	    xcen[i] = xcen[nstar]
	    ycen[i] = ycen[nstar]
 	    sky[i] = sky[nstar]
	    mag[i] = mag[nstar]
	    ids[nstar] = ihold
	    xcen[nstar] = xhold
	    ycen[nstar] = yhold
	    sky[nstar] = shold
	    mag[nstar] = mhold
	}

	nstar = nstar - 1
end
