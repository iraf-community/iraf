include <mach.h>
include	<imhdr.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/nstardef.h"


# DP_NSTARFIT -- Fit the stellar group.

int procedure dp_nstarfit (dao, im, nin_group, mean_sky, cdimen, iter,
	converge)

pointer	dao			# pointer to the daophot structure
pointer im			# pointer to the input image
int	nin_group		# original group size
real	mean_sky		# the mean sky for the group
int	cdimen			# dimensions of the coefficient matrix
int	iter			# the current iteration
bool	converge		# did the fit converge ?

bool 	clip, refit
int	i, j, k, xpix, ypix, group_size, nterm, ncols, mindex, ifaint, tifaint
int	ntmin, ixmin, ixmax, iymin, iymax, ifxmin, ifxmax, ifymin, ifymax, flag
pointer	apsel, psffit, nstar, subim, ypixel, pixel
real	fitradsq, cutoff, psfradsq, sepcrit, sepmin, perr, peakerr, sky_value
real	read_noise, mingdata, maxgdata, chigrp, wcrit, xmin, xmax, ymin, ymax
real	datum, sumres, grpwt, xtemp, ytemp, weight, ds, pred_pixval, sigmasq
real	relerr, dswt, faint, tfaint, noise

bool	dp_nstmerge(), dp_ntomit(), dp_ntmin(), dp_ncheckc()
pointer imgs2r()
real	dp_ntskyval(), dp_ntsubtract()

begin
	# Define the daophot pointers.
	psffit = DP_PSFFIT (dao)
	apsel = DP_APSEL(dao)
	nstar = DP_NSTAR (dao)

	# Set up some daophot constants. At some point these will be computed
	# when the NSTAR task is started up instead of at the beginning of
	# each group fit. For the moment it is convenient and not too
	# costly to compute them here. Also initialize the fit.

	if (iter == 1) {

	    # Compute the fitting and psf radii.
	    fitradsq = DP_FITRAD (dao) ** 2
	    psfradsq = DP_PSFRAD(dao) ** 2
	    cutoff = CUT_FACTOR * fitradsq

	    # Compute the merging parameters.
	    if (IS_INDEFR(DP_MERGERAD(dao))) {
	        sepcrit = 2.0 * (Memr[DP_PSFPARS(psffit)] ** 2 +
	            Memr[DP_PSFPARS(psffit)+1] ** 2)
	        sepmin = min (1.0, FRACTION_MINSEP * sepcrit)
	    } else {
		sepcrit = DP_MERGERAD(dao) ** 2
	        sepmin = min (1.0, FRACTION_MINSEP * sepcrit)
	    }

	    # Compute the noise parameters.
	    read_noise = (DP_READNOISE(dao) / DP_PHOTADU(dao)) ** 2
	    perr = 0.01 * DP_FLATERR(dao)
	    peakerr = 0.01 * DP_PROFERR(dao) / (Memr[DP_PSFPARS(psffit)] *
	        Memr[DP_PSFPARS(psffit)+1])

	    # Compute the minimum and maximum good pixel values.
	    if (IS_INDEFR(DP_MINGDATA(dao)))
	        mingdata = -MAX_REAL
	    else
	        mingdata = DP_MINGDATA(dao)
	    if (IS_INDEFR(DP_MAXGDATA(dao)))
	        maxgdata = MAX_REAL
	    else
	        maxgdata = DP_MAXGDATA(dao)

	    # Initialize the fit.
	    chigrp = 1.0
	    clip = false
	    if (DP_RECENTER(dao) == YES) {
	        nterm = 3 * nin_group
		ntmin = 3
	    } else {
	        nterm = nin_group
		ntmin = 1
	    }
	    if (DP_FITSKY(dao) == YES) {
		nterm = nterm + 1
		ntmin = ntmin + 1
	    }
	    call aclrr (Memr[DP_NXOLD(nstar)], nterm) 
	    call amovkr (1.0, Memr[DP_NXCLAMP(nstar)], nterm)
	}

	# Start a new iteration.
	group_size = nin_group
	if (DP_RECENTER(dao) == YES)
	    nterm = 3 * group_size
	else
	    nterm = group_size
	if (DP_FITSKY(dao) == YES)
	    nterm = nterm + 1

	# Begin fitting the current group of stars.
	repeat {

	    # Initialize the convergence criteria.
	    converge = false

	    # Check that there is at least 1 star in the group.
	    if (group_size < 1)
	        return (group_size)

	    # Set up the critical error for star rejection.
	    if (iter >= NITER_MAX)
	        wcrit = WCRIT_MAX
	    else if (iter >= NITER_MED)
	        wcrit = WCRIT_MED
	    else if (iter >= NITER_MIN)
	        wcrit = WCRIT_MIN
	    else
	        wcrit = MAX_REAL

	    # Set the sky fitting derivative.
	    if (DP_FITSKY(dao) == YES)
	        Memr[DP_NX(nstar)+nterm-1] = -1.0

	    # Initialize arrays.
	    call aclrr (Memr[DP_APCHI(apsel)], group_size)
	    call aclrr (Memr[DP_NSUMWT(nstar)], group_size)
	    call aclrr (Memr[DP_NNUMER(nstar)], group_size)
	    call aclrr (Memr[DP_NDENOM(nstar)], group_size)
	    call amovki (NSTERR_OK, Memi[DP_NIER(nstar)], group_size)

	    # Compute the minimum and maximum x and y values.
	    call alimr (Memr[DP_APXCEN(apsel)], group_size, xmin, xmax) 
	    call alimr (Memr[DP_APYCEN(apsel)], group_size, ymin, ymax) 

	    # Check to see whether any two stars are within the critical
	    # difference from each other.

	    if ((group_size > 1) && dp_nstmerge (Memr[DP_APXCEN(apsel)],
		Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		Memr[DP_APERR(apsel)], group_size, sepcrit, sepmin, wcrit,
		i, j, k)) {

		# Compute the new centroid and brightness.
		call dp_nstcen (Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_APMAG(apsel)], i, j, k)

		# Print out verbose comments.
		if (DP_VERBOSE (dao) == YES)  {
		    call printf ("\tStar %-5d has merged with star %-5d\n")
		        call pargi (Memi[DP_APID(apsel)+k-1])
		        call pargi (Memi[DP_APID(apsel)+i-1])
		}

		# Recompute the mean sky.
		if ((DP_FITSKY(dao) == NO) && (DP_GROUPSKY(dao) == YES))
		    mean_sky = (mean_sky * group_size -
		        Memr[DP_APMSKY(apsel)+k-1]) / (group_size - 1)

		# Now remove the k-th star from the group.
		call dp_remove (k, group_size, Memi[DP_APID(apsel)],
		    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)],
		    Memi[DP_NIER(nstar)], NSTERR_MERGE)

		# After deleting a star, resize the matrix, release all of
		# the clamps and back up the iteration counter.

		if (DP_RECENTER(dao) == YES)
		    nterm = 3 * group_size
		else
		    nterm = group_size
		if (DP_FITSKY(dao) == YES)
		    nterm = nterm + 1
		clip = false
		call aclrr (Memr[DP_NXOLD(nstar)], nterm)
		call amovkr (1.0, Memr[DP_NXCLAMP(nstar)], nterm)
		iter = max (1, iter - 1)
		next
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

	    call aclrr (Memr[DP_NV(nstar)], nterm)
	    call aclrr (Memr[DP_NC(nstar)], cdimen * cdimen)
	    call aclri (Memi[DP_NNPIX(nstar)], group_size)

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
		        Memr[DP_APYCEN(apsel)], Memr[DP_NRPIXSQ(nstar)],
			Memi[DP_NSKIP(nstar)], group_size, xtemp, ytemp,
			cutoff))
		        next

		    if ((DP_FITSKY(dao) == NO) && (DP_GROUPSKY(dao) == NO)) {
			sky_value = dp_ntskyval (Memr[DP_APMSKY(apsel)],
			    Memi[DP_NSKIP(nstar)], group_size)
			if (IS_INDEFR(sky_value))
			    sky_value = mean_sky
		    } else
			sky_value = mean_sky

		    # Subtract the mean sky from the pixel.
		    #ds = datum - mean_sky
		    ds = datum - sky_value

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

		    weight = dp_ntsubtract (dao, im, Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memr[DP_NRPIXSQ(nstar)],
		        Memr[DP_APMAG(apsel)], Memi[DP_NSKIP(nstar)],
		        Memr[DP_NX(nstar)], group_size, xtemp, ytemp, ds,
			psfradsq, fitradsq, DP_RECENTER(dao))

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
		    if ((pred_pixval > maxgdata) && (iter >= 4))
			next

		    #sigmasq = pred_pixval / DP_PHOTADU (dao) + read_noise + 
		        #(perr * pred_pixval) ** 2 + (peakerr *
		        #(pred_pixval - mean_sky)) ** 2
		    sigmasq = pred_pixval / DP_PHOTADU (dao) + read_noise + 
		        (perr * pred_pixval) ** 2 + (peakerr *
		        (pred_pixval - sky_value)) ** 2
		    if (sigmasq <= 0.0)
			next
		    relerr = abs (ds) / sqrt (sigmasq)

		    # Add this residual into the weighted sum of the
		    # absolute relative residuals.

		    sumres = sumres + relerr * weight
		    grpwt = grpwt + weight

		    # Add into the accumulating sums of the weighted 
		    # absolute relative residuals and of the image sharpness
		    # parameter for each of the stars. Include in the
		    # sharpness index only those pixels within NCORE_SIGMA
		    # sigma of the centroid of the object. This saves time
		    # and floating underflows by excluding pixels
		    # which contribute less than about one part in a
		    # million to the index.

		    call dp_acsharp (Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memi[DP_NSKIP(nstar)],
			Memi[DP_NNPIX(nstar)], Memr[DP_NNUMER(nstar)],
			Memr[DP_NDENOM(nstar)], Memr[DP_NSUMWT(nstar)],
			Memr[DP_APCHI(apsel)], group_size, xtemp, ytemp,
			Memr[DP_PSFPARS(psffit)], Memr[DP_PSFPARS(psffit)+1],
			ds, sigmasq, relerr, weight)

		    # If clipping is in effect, reduce the weight of a bad
		    # pixel. A pixel having a residual of 2.5 sigma gets
		    # reduced to half weight and one with a rersidual of 5
		    # sigma gets weight of 1 / 257.

		    weight = weight / sigmasq
		    if (clip) 
		        weight = weight / (1.0 + (relerr / (DP_CLIPRANGE(dao) *
			    chigrp)) ** DP_CLIPEXP(dao))
		    dswt = ds * weight

		    # Work this pixel into the normal matrix.
		    call dp_mataccum (Memr[DP_NX(nstar)], Memi[DP_NSKIP(nstar)],
		        group_size, Memr[DP_NC(nstar)], Memr[DP_NV(nstar)],
			cdimen, nterm, weight, dswt, DP_RECENTER(dao),
			DP_FITSKY(dao))

	        }

	        ypixel = ypixel + ncols
	    }

	    # Make sure that every star in the group has at least MIN_FIT_PIXEL
	    # pixels within one fitting radius.

	    refit = dp_ntmin (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APMSKY(apsel)], Memi[DP_NNPIX(nstar)],
		Memi[DP_NIER(nstar)], group_size, nterm, DP_RECENTER(dao),
		DP_FITSKY(dao), DP_GROUPSKY(dao), mean_sky, DP_VERBOSE(dao))
	    if (group_size < 1)
	        return (group_size)
	    if (refit)
	        next

	    # Reflect the normal matrix across the diagonal.
	    call dp_mreflect (Memr[DP_NC(nstar)], cdimen, nterm)

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

	    if (grpwt > ntmin) {
	        chigrp = CHI_NORM * sumres * sqrt (1.0 / (grpwt * (grpwt -
	            ntmin)))
	        chigrp = ((grpwt - ntmin) * chigrp + ntmin) / grpwt
	    }

	    # CHIOLD has been pulled toward its expected value of unity to
	    # keep the statistics of a small number of pixels from completely
	    # dominating the error analysis. Similarly, the photometric 
	    # errors for the individual stars will be pulled toward unity
	    # now. Later on, if the number of stars in the group is 
	    # greated than one, CHI will be nudged toward the group average.
	    # In order to work optimally, of course, this requires that	
	    # the # of photons / ADU, the READNOISE and the other noise
	    # contributors are properly specified.

	    call dp_ntchi (Memr[DP_NSUMWT(nstar)], Memr[DP_APCHI(apsel)],
	        group_size, ntmin, chigrp, grpwt)

	    # Invert the matrix.
	    call invers (Memr[DP_NC(nstar)], cdimen, nterm, flag)

	    # Check for a singular matrix.
	    refit = dp_ncheckc (Memr[DP_NC(nstar)], cdimen, nterm,
		Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
		Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		Memr[DP_APMSKY(apsel)], Memi[DP_NIER(nstar)], group_size,
		DP_RECENTER(dao), DP_FITSKY(dao), DP_GROUPSKY(dao), mean_sky,
		DP_VERBOSE(dao)) 
	    if (group_size < 1)
	        return (group_size)
	    if (refit)
	        next

	    # Solve for position and scale factor increments.
	    call mvmul (Memr[DP_NC(nstar)], cdimen, nterm, Memr[DP_NV(nstar)],
	        Memr[DP_NX(nstar)])

	    if (iter <= 1)
	        refit = true
	    else
	        refit = false

	    # Fit the sky.
	    if (DP_FITSKY(dao) == YES) {
	        noise = sqrt (abs (mean_sky / DP_PHOTADU(dao)) + read_noise)
	        mean_sky = mean_sky - max (-3.0 * noise,
		    min (Memr[DP_NX(nstar)+nterm-1], 3.0 * noise))
		if (abs (Memr[DP_NX(nstar)+nterm-1]) > (1.0e-4 * mean_sky))
		    refit = true
	    }

	    # In the beginning, the brightness of each star will be permitted
	    # to change by no more than 2 magnitudes per iteration, and the x,y
	    # coordinates of each centroid will be permitted to change by
	    # no more than 0.4 pixels per iteration. Any time that the 
	    # parameter correction changes sign from one iteration to the
	    # next, the maximum permissible change will be reduced by a factor
	    # of two. These clamps are released any time a star disappears.

	    call dp_ntclamp (Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
	        Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
	        Memr[DP_NSUMWT(nstar)], group_size, Memr[DP_NC(nstar)], cdimen,
		Memr[DP_NX(nstar)], Memr[DP_NXOLD(nstar)],
		Memr[DP_NXCLAMP(nstar)], DP_RECENTER(dao), clip, refit)

	    # Check whether the estimated centroid of the any star has
	    # moved so far out of the limits of the picture that it has fewer
	    # than 4 or 5 pixels within one fitting radius.

	    call dp_ntcentroid (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APMSKY(apsel)], Memi[DP_NIER(nstar)], group_size,
		ixmin, ixmax, iymin, iymax, fitradsq, DP_FITSKY(dao),
		DP_GROUPSKY(dao), mean_sky, refit, DP_VERBOSE(dao))

	    if (group_size < 1)
	        return (group_size)

	    # Update matrix dimensions.
	    if (DP_RECENTER(dao) == YES)
	        nterm = 3 * group_size
	    else
		nterm = group_size
	    if (DP_FITSKY(dao) == YES)
		nterm = nterm + 1

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

	    # No very faint star was detected.
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

	    # Reject the appropriate star.
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

		if ((DP_FITSKY(dao) == NO) && (DP_GROUPSKY(dao) == YES) &&
		    (group_size > 1))
		    mean_sky = (mean_sky * group_size -
		        Memr[DP_APMSKY(apsel)+max(tifaint,ifaint)-1]) /
			    (group_size - 1)

	        call dp_remove (max (tifaint, ifaint), group_size,
	            Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		    Memr[DP_APMSKY(apsel)], Memi[DP_NIER(nstar)],
		    NSTERR_FAINT)

	        if (group_size < 1)
		    return (group_size)

		if (DP_RECENTER(dao) == YES)
	            nterm = 3 * group_size
		else
		    nterm = group_size
		if (DP_FITSKY(dao) == YES)
		    nterm = nterm + 1
	        call aclrr (Memr[DP_NXOLD(nstar)], nterm)
	        call amovkr (1.0, Memr[DP_NXCLAMP(nstar)], nterm)
	        clip = false
	        iter = max (1, iter - 1)
	        next
	    }

	    # Solution has either converged or gone to the maximum number
	    # of iterations.

	    if ((iter < DP_MAXITER(dao)) && (! clip)) {

	        # The first convergence milestone has been reached. Turn on the
	        # clipper, loosen the clamps and keep on going.

		if (DP_CLIPEXP(dao) > 0)
	            clip = true
	        converge = false
	        call aclrr (Memr[DP_NXOLD(nstar)], nterm)
	        call amaxkr (Memr[DP_NXCLAMP(nstar)], 0.25,
	            Memr[DP_NXCLAMP(nstar)], nterm)
	        return (group_size)
	    }

	    converge = true		    

	} until (converge)

	return (group_size)
end


# DP_NSTMERGE -- Decide whether two stars in a group should merge.

bool procedure dp_nstmerge (xcen, ycen, mag, magerr, group_size, sepcrit,
	sepmin, wcrit, i, j, k)

real	xcen[ARB]		# array of x centers	
real	ycen[ARB]		# array of y centers	
real	mag[ARB]		# array of magnitudes
real	magerr[ARB]		# array of magnitude errors
int	group_size		# group size
real	sepcrit			# the critical separation squared
real	sepmin			# the minimum separation
real	wcrit			# critical error for rejection
int	i, j, k			# output indices

real	separation

begin
	do i = 1, group_size {
  	    do j = 1,  i - 1 {

		# Compute the separation.
	        separation = (xcen[j] - xcen[i]) ** 2 +
		    (ycen[j] - ycen[i]) ** 2
		if (separation > sepcrit)
		    next

		# Find the fainter of the two stars.
		k = j
		if (mag[i] < mag[j]) 
		    k = i
		if ((separation <  sepmin) || ((magerr[k] / mag[k])  > wcrit))
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
	# star is now the fainter of the two, the i-th the
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

bool procedure dp_ntomit (xcen, ycen, rpixsq, skip, group_size, fx, fy, cutoff)

real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	rpixsq[ARB]		# array of distances squared
int	skip[ARB]		# array of skip values
int	group_size		# the group size
real	fx, fy			# pixel position in image
real	cutoff			# radius cuttoff for pixel inclusion

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
	    omit = false
	}	 

	return (omit)
end


# DP_NTSKYVAL -- Compute the average sky value to use for a particular
# pixel by averaging the sky values of all stars for which the
# pixel is within one fitting radius.

real procedure dp_ntskyval (sky, skip, nstar)

real	sky[ARB]		# array of sky values
int	skip[ARB]		# array of skip values
int	nstar			# the number of stars

int	i, npts
real	sum

begin
	sum = 0.0
	npts = 0
	do i = 1, nstar {
	    if (skip[i] == YES)
		next
	    sum = sum + sky[i]
	    npts = npts + 1
	}
	if (npts <= 0)
	    return (INDEFR)
	else
	    return (sum / npts)
end


# DP_NTSUBTRACT -- Procedure to subtract the contribution of a particular
# pixel from a particular star.

real procedure dp_ntsubtract (dao, im, xcen, ycen, rpixsq, mag, skip, x,
	group_size, fx, fy, ds, psfradsq, fitradsq, recenter)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	rpixsq[ARB]		# array of distances squared
real	mag[ARB]		# array of magnitudes
int	skip[ARB]		# array of skip values
real	x[ARB]			# x accumulation vector array
int	group_size		# size of the group
real	fx, fy			# center of pixel in image
real	ds			# pixel value
real	psfradsq		# psf radius squared
real	fitradsq		# fit radius squared
int	recenter		# recenter the coordinates

int	i, i3, k
pointer	psffit
real	weight, dx, dy, deltax, deltay, val, dvdx, dvdy, rsq
real	dp_usepsf()

begin
	psffit = DP_PSFFIT(dao)

	weight = 0.0
	do i = 1, group_size {
	    if (rpixsq[i] >= psfradsq)
		next
	    dx = fx - xcen[i]
	    dy = fy - ycen[i]
	    call dp_wpsf (dao, im, xcen[i], ycen[i], deltax, deltay, 1)
	    deltax = (deltax - 1.0) / DP_PSFX(psffit) - 1.0
	    deltay = (deltay - 1.0) / DP_PSFY(psffit) - 1.0
	    val = dp_usepsf (DP_PSFUNCTION(psffit), dx, dy,
	        DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit), deltax, deltay,
		dvdx, dvdy)
	    ds = ds - mag[i] * val
	    if (skip[i] == YES)
		next
	    rsq = rpixsq[i] / fitradsq
	    weight = max (weight, 5.0 / (5.0 + rsq / (1.0 - rsq)))
	    if (recenter == YES) {
	        i3 = 3 * i
	        k = i3 - 2
	        x[k] = -val
	        k = i3 - 1
	        x[k] = -mag[i] * dvdx
	        x[i3] = -mag[i] * dvdy
	    } else
		x[i] = -val
	}

	return (weight)
end


# DP_ACSHARP -- Procedure to accumulate sums of the weighted absolute
# relative residuals and the image sharpness parameter for each of the
# stars.

procedure dp_acsharp (xcen, ycen, skip, npix, numer, denom, sumwt, chi,
	group_size, fx, fy, fwhmx, fwhmy, ds, sigmasq, relerr, weight)

real	xcen[ARB]		# array of object x centers
real	ycen[ARB]		# array of object y centers
int	skip[ARB]		# array of skip values
int	npix[ARB]		# array of numbers of pixels
real	numer[ARB]		# numerator array
real	denom[ARB]		# denominator array
real	sumwt[ARB]		# array of summed weights
real	chi[ARB]		# array of chis
int	group_size		# group size paramter.
real	fx, fy			# position of data in image
real	fwhmx, fwhmy		# gaussian core widths in x and y
real	ds			# the data residual
real	sigmasq			# the sigma squared
real	relerr			# the relative error
real	weight			# the weight

int	i
real	rhosq, dfdsig

begin
	do i = 1, group_size {

	    if (skip[i] == YES)
		next

	    # Accumulate the number of pixels, chi and sum of the weights.
	    npix[i] = npix[i] + 1
	    chi[i] = chi[i] + relerr * weight
	    sumwt[i] = sumwt[i] + weight

	    # Include in the sharpness index only those pixels
	    # within NCORE_SIGMASQ of the centroid of the
	    # object. (This saves time and floating underflows
	    # by excluding pixels which contribute very little
	    # to the index.

	    rhosq = ((xcen[i] - fx) / fwhmx) ** 2 + ((ycen[i] - fy) /
	        fwhmy) ** 2
	    if (rhosq > NCORE_SIGMASQ)
		next
	    rhosq = 0.6931472 * rhosq
	    dfdsig = exp (-rhosq) * (rhosq - 1.0)
	    numer[i] = numer[i] + dfdsig * ds / sigmasq
	    denom[i] = denom[i] + (dfdsig ** 2) / sigmasq
	}			
end


# DP_MATACCUM -- Procedure to accumulate the data into the matrices.

procedure dp_mataccum (x, skip, group_size, c, v, cdimen, nterm, weight, dswt,
	recenter, fitsky)

real	x[ARB]			# x array
int	skip[ARB]		# skip vector
int	group_size		# size of the group
real	c[cdimen,ARB]		# coefficient matrix
real	v[ARB]			# vector array
int	cdimen			# dimensions of the coefficient matrix
int	nterm			# the number of terms
real	weight			# weight
real	dswt			# data weight
int	recenter		# recenter the coordinates
int	fitsky			# fit the sky value

int	i, i3, i3m2, k, j, l

begin
	if (fitsky == YES) {
	    c[nterm,nterm] = c[nterm,nterm] + weight
	    v[nterm] = v[nterm] - dswt
	}

	do i = 1, group_size {
	    if (skip[i] == YES)
		next
	    if (recenter == YES) {
	        i3 = i * 3
	        i3m2 = i3 - 2
	        do k = i3m2, i3 {
		    if (fitsky == YES)
			c[nterm,k] = c[nterm,k] - x[k] * weight
		    v[k] = v[k] + x[k] * dswt
		}
	        do j = 1, i {
		    if (skip[j] == YES)
		        next
		    do k = i3m2, i3 {
		        do l = 3 * j - 2, min (k, 3 * j)
			    c[k,l] = c[k,l] + x[k] * x[l] * weight
		    }
	        }
	    } else {
		v[i] = v[i] + x[i] * dswt
		if (fitsky == YES)
			c[nterm,i] = c[nterm,i] - x[i] * weight
		do j = 1, i {
		    if (skip[j] == YES)
		        next
		    c[i,j] = c[i,j] + x[i] * x[j] * weight
		}
	    }
	}
end


# DP_NTMIN -- Make sure that every star in the group has at least 
# MIN_NPIX pixels within one fitting radius.

bool procedure dp_ntmin (ids, xcen, ycen, mag, sky, npix, nier, group_size,
	nterm, recenter, fitsky, groupsky, mean_sky, verbose)

int	ids[ARB]		# array of ids
real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of magnitudes
real	sky[ARB]		# array of sky values
int	npix[ARB]		# array of pixel numbers
int	nier[ARB]		# array of error codes
int	group_size		# size of the group
int	nterm			# number of terms
int	recenter		# recenter the objects
int	fitsky			# fit the sky value
int	groupsky		# use group sky value
real	mean_sky		# the current mean sky value
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
	    if ((fitsky == NO) && (groupsky == YES) && (group_size > 1))
		mean_sky = (mean_sky * group_size - sky[i]) / (group_size - 1)
	    call dp_remove (i, group_size, ids, xcen, ycen, mag, sky,
		nier, NSTERR_NOPIX)
	    if (group_size <= 0)
		return (redo)
	    if (recenter == YES)
	        nterm = 3 * group_size
	    else
		nterm = group_size
	    if (fitsky == YES)
		nterm = nterm + 1
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

procedure dp_ntchi (sumwt, chi, group_size, ntmin, chigrp, grpwt)

real	sumwt[ARB]		# sum of the weights
real	chi[ARB]		# the chis:wq
int	group_size		# size of the group
int	ntmin			# minimum number of points
real	chigrp			# the group chi
real	grpwt			# the group weight

int	i

begin
	do i = 1, group_size {
	    if (sumwt[i] > ntmin) {
		chi[i] = CHI_NORM * chi[i] / sqrt ((sumwt[i] - ntmin) *
		    sumwt[i])
		sumwt[i] = ((sumwt[i] - ntmin) * chi[i] + MIN_SUMWT) /
		    sumwt[i]
	    } else { 
		chi[i] = chigrp
		sumwt[i] = grpwt
	    }
	}
end


# DP_NCHECKC -- Check the inverted matrix for singularity.

bool procedure dp_ncheckc (c, cdimen, nterm, ids, xcen, ycen, mag, sky,
	nier, group_size, recenter, fitsky, groupsky, mean_sky, verbose)

real	c[cdimen,ARB]		# coefficient matrix
int	cdimen			# dimension of the c matrix
int	nterm			# number of terms
int	ids[ARB]		# array of ids
real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of magnitudes
real	sky[ARB]		# array of sky values
int	nier[ARB]		# array of error codes
int	group_size		# size of the group
int	recenter		# recenter the objects
int	fitsky			# fit the sky value
int	groupsky		# use group sky value
real	mean_sky		# the current mean sky value
int	verbose			# verbose flag

bool	redo
int	j, starno, i

begin
	redo = false
	do j = 1, nterm {
	    if (c[j,j] > 0.0)
		next
	    redo = true
	    if ((j == nterm) && (fitsky == YES))
		starno = 0
	    else if (recenter == YES)
		starno = (j + 2) / 3
	    else
		starno = j
	    if (starno == 0) {
		if (verbose == YES) {
		    do i = 1, group_size {
		        call printf (
			    "\tStar %-5d has been deleted: singular matrix\n")
			    call pargi (ids[i])
		    }
		}
		call amovki (NSTERR_SINGULAR, nier, group_size)
		group_size = 0
	    } else {
		if (verbose == YES) {
		    call printf (
			"\tStar %-5d has been deleted: singular matrix\n")
			call pargi (ids[starno])
		}
		if ((fitsky == NO) && (groupsky == YES) && (group_size > 1))
		    mean_sky = (mean_sky * group_size - sky[starno]) /
			(group_size - 1)
		call dp_remove (starno, group_size, ids, xcen, ycen, mag,
		    sky, nier, NSTERR_SINGULAR)
	    }
	    if (group_size <= 0)
		return (redo)
	    if (recenter == YES)
		nterm = 3 * group_size
	    else
		nterm = group_size
	    if (fitsky == YES)
		nterm = nterm + 1
	}

	return (redo)
end


# DP_NTCLAMP -- Restrict the amount the solution can vary on each iteration.

procedure dp_ntclamp (xcen, ycen, mag, magerr, sumwt, group_size, c, cdimen,
	x, xold, clamp, recenter, clip, redo)

real	xcen[ARB]		# x centers array
real	ycen[ARB]		# y centers array
real	mag[ARB]		# magnitude array
real	magerr[ARB]		# magnitude errors array
real	sumwt[ARB]		# array of weight sums
int	group_size		# size of the group
real	c[cdimen, ARB]		# coefficient matrix
int	cdimen			# dimensions of c
real	x[ARB]			# x vector
real	xold[ARB]		# old x vector
real	clamp[ARB]		# clamp on solution matrix
int	recenter		# recenter the objects
bool	clip			# clip the matrix
bool	redo			# redo the solution

int	i, l, j, k
real	df

begin
	do i = 1, group_size {


	    # If any correction has changed sign since the last
	    # iteration, reduce the maximum permissible change by
	    # a factor of two.

	    # Note that the sign of the correction is such that it
	    # must be SUBTRACTED from the current value of the 
	    # parameter to get the improved parameter value. This being
	    # the case, if the correction to the brightness is
	    # negative (the least-squares thinks that the star should
	    # be brighter) a change of 1 magnitude is a change of a
	    # factor of 2.5; if the brightness correction is positive
	    # (the star should be fainter) a change of 1 magnitude
	    # is a change of 60%.

	    if (recenter == YES) {

	        l = 3 * i
	        k = l - 1
	        j = l - 2

	        if ((xold[j] * x[j]) < 0.0) 
		    clamp[j] = 0.5 * clamp[j]
	        mag[i] = mag[i] - x[j] / (1.0 + max (x[j] /
		    (MAX_DELTA_FAINTER * mag[i]), -x[j] / (MAX_DELTA_BRIGHTER *
		    mag[i])) / clamp[j])
	        xold[j] = x[j]

	        if ((xold[k] * x[k]) < 0.0)
		    clamp[k] = 0.5 * clamp[k]
	        if ((xold[l] * x[l]) < 0.0)
		    clamp[l] = 0.5 * clamp[l]
	        xcen[i] = xcen[i] - x[k] / (1.0 + abs(x[k]) / (clamp[k] *
	            MAX_DELTA_PIX))
	        ycen[i] = ycen[i] - x[l] / (1.0 + abs(x[l]) / (clamp[l] *
	            MAX_DELTA_PIX))
	        xold[k] = x[k]
	        xold[l] = x[l]
	        magerr[i] =sumwt[i] *  sqrt (c[j,j])

	    } else {

	        if ((xold[i] * x[i]) < 0.0) 
		    clamp[i] = 0.5 * clamp[i]
	        mag[i] = mag[i] - x[i] / (1.0 + max (x[i] /
		    (MAX_DELTA_FAINTER * mag[i]), -x[i] / (MAX_DELTA_BRIGHTER *
		    mag[i])) / clamp[i])
	        xold[i] = x[i]
	        magerr[i] =sumwt[i] *  sqrt (c[i,i])
	    }


	    # There are two milestones in the convergence process: the fits
	    # proceed normally until each star's magnitude changes by less
	    # than its standard error or MAX_NEW_ERRMAG magnitudes, whichever
	    # is greater, and its x and y centroids change by less than 0.02 
	    # pixel. At this point the least squares begins to apply
	    # down-weighting of pixels with large residuals as described
	    # above.  The fits then continue until each star's
	    # magnitude changes by less than MAX (MAX_NEW_ERRMAG * std. error,
	    # MAX_NEW_RELBRIGHT2 magnitude), ad its centroids change by
	    # less than 0.002 pixel.

	    if (redo)
		next

	    if (clip) {
		if (abs (x[j]) > max (MAX_NEW_ERRMAG * magerr[i],
		        MAX_NEW_RELBRIGHT2 * mag[i])) {
		    redo = true
		} else if (recenter == YES) {
		    df = (MAX_NEW_ERRMAG * sumwt[i]) ** 2
		    if (x[k] ** 2 > max (df * c[k,k], MAX_PIXERR2))
			redo = true
		    else if (x[l] ** 2 > max (df * c[l,l], MAX_PIXERR2))
			redo = true
		}
	    } else {
		if (abs (x[j]) > max (magerr[i], MAX_NEW_RELBRIGHT1 *
		        mag[i])) {
		    redo = true
		} else if (recenter == YES) {
		    df = sumwt[i] ** 2
		    if (x[k] ** 2 > max (df * c[k,k], MAX_PIXERR1))
			redo = true
		    else if (x[l] ** 2 > max (df * c[l,l], MAX_PIXERR1))
			redo = true
		}
	    }
	}
end


# DP_NTCENTROID -- Check the new centroids to see if they have moved too
# far off the edge of the image.

procedure dp_ntcentroid (ids, xcen, ycen, mag, sky, nier, group_size, ixmin,
	ixmax, iymin, iymax, fitradsq, fitsky, groupsky, mean_sky, redo,
	verbose)

int	ids[ARB]			# array of ids
real	xcen[ARB]			# array of x centers
real	ycen[ARB]			# array of y centers
real	mag[ARB]			# array of magnitudes
real	sky[ARB]			# array of sky values
int	nier[ARB]			# array of error codes
int	group_size			# size of the group
int	ixmin,ixmax			# subraster x limits
int	iymin,iymax			# subraster y limits
real	fitradsq			# fit radius squared
int	fitsky				# fit the sky value
int	groupsky			# use the group sky value
real	mean_sky			# the mean sky value
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

	    dx = max (ixmin - xcen[i], xcen[i] - ixmax, 0.0)
	    dy = max (iymin - ycen[i], ycen[i] - iymax, 0.0)
	    if ((dx <= MAX_PIX_INCREMENT) && (dy <= MAX_PIX_INCREMENT))
		    next
	    if (((dx + 1.0) ** 2 + (dy + 1.0) ** 2) < fitradsq)
		next

	    # Print a warning message about the star.
	    if (verbose == YES) {
		call printf (
		"\tStar %-5d has been deleted: new center too far off image\n")
		    call pargi (i)
	    }

	    # Adjust the sky.
	    if ((fitsky == NO) && (groupsky == YES) && (group_size > 1))
		mean_sky = (mean_sky * group_size - sky[i]) / (group_size - 1)

	    # Delete it.
	    call dp_remove (i, group_size, ids, xcen, ycen, mag, sky, nier,
	        NSTERR_OFFIMAGE)
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
	    weight = magerr[i] / mag[i]
	    if (weight < faint)
		next
	    faint = weight
	    ifaint = i
	}
end


# DP_REMOVE -- Remove the i-th star from the list of stars in the current
# group by moving it to the end of the group.

procedure dp_remove (i, nstar, ids, xcen, ycen, mag, sky, nier, pier)

int	i			# the star to be removed
int	nstar			# the number of stars in the group
int	ids[ARB]		# the array of star ids
real	xcen[ARB]		# the array of star x positions
real	ycen[ARB]		# the array of star y positions
real	mag[ARB]		# the array of magnitudes
real	sky[ARB]		# the array of sky values.
int	nier[ARB]		# array of error codes
int	pier			# error code for deleted star

int	ihold, phold
real	xhold, yhold, shold, mhold

begin
	nier[i] = pier
	if (i != nstar) {

	    ihold = ids[i]
	    xhold = xcen[i]
	    yhold = ycen[i]
	    shold = sky[i]
	    mhold = mag[i]
	    phold = nier[i]

	    ids[i] = ids[nstar]
	    xcen[i] = xcen[nstar]
	    ycen[i] = ycen[nstar]
 	    sky[i] = sky[nstar]
	    mag[i] = mag[nstar]
	    nier[i] = nier[nstar]

	    ids[nstar] = ihold
	    xcen[nstar] = xhold
	    ycen[nstar] = yhold
	    sky[nstar] = shold
	    mag[nstar] = mhold
	    nier[nstar] = phold
	}
	nstar = nstar - 1
end
