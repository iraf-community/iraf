include <mach.h>
include	<imhdr.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include	"../lib/allstardef.h"

# DP_ALPHOT -- Perform one iteration on each group of stars.

int procedure dp_alphot (dao, im, ntot, istar, niter, sepcrit, sepmin, wcrit,
	clip, clampmax, version)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
int	ntot			# the total number of stars left unfit
int	istar			# first star in the current group
int	niter			# the current iteration
real	sepcrit			# critical separation for merging
real	sepmin			# minimum separation for merging
real	wcrit			# the critical error for merging
bool	clip			# clip the data
real	clampmax		# maximum clamping factor
int	version			# version number

bool	regroup
int	cdimen, lstar, nstar, fstar, nterm, ixmin, ixmax, iymin, iymax
int	fixmin, fixmax, fiymin, fiymax, nxpix, nypix, flag, i, j, k, ifaint
pointer	apsel, psffit, allstar, data, subt, weights
real	radius, totradius, mean_sky, pererr, peakerr, faint
real	xmin, xmax, ymin, ymax, sumres, grpwt, chigrp

bool	dp_alredo(), dp_checkc(), dp_almerge(), dp_alclamp(), dp_alfaint()
int	dp_laststar(), dp_delfaintest()
pointer	dp_gwt(), dp_gst(), dp_gdc()
real	dp_almsky(), asumr()

begin
	# Get some daophot pointers.
	psffit = DP_PSFFIT (dao)
	apsel = DP_APSEL(dao)
	allstar = DP_ALLSTAR (dao)

	# Define some constants. At some point these should be stored
	# in the allstar structure at task initialization. When the final
	# rewrite gets done this will occur.

	radius = DP_FITRAD(dao)
	totradius = DP_FITRAD(dao) + DP_PSFRAD(dao) + 1
	if (DP_RECENTER(dao) == YES)
	    cdimen = 3 * DP_MAXGROUP(dao) + 1
	else
	    cdimen = DP_MAXGROUP(dao) + 1
	pererr = 0.01 * DP_FLATERR(dao)
	peakerr = 0.01 * DP_PROFERR(dao) / (Memr[DP_PSFPARS(psffit)] *
	    Memr[DP_PSFPARS(psffit)+1])
	regroup = false

	repeat {

	    # Given the last star arrary as computed by dp_regroup, the first
	    # star in the current group, and the total number of stars,
	    # find the last star in the current group and compute the
	    # total number of stars in the group. 

	    lstar = dp_laststar (Memi[DP_ALAST(allstar)], istar, ntot)
	    nstar = lstar - istar + 1

	    # Don't compute the subraster limits if no regroup has been
	    # performed.
	    if (! regroup) {
	        call alimr (Memr[DP_APXCEN(apsel)+istar-1], nstar, xmin, xmax)	
	        call alimr (Memr[DP_APYCEN(apsel)+istar-1], nstar, ymin, ymax)	
	    }

	    # Determine whether the group is too large to be fit.

	    if (nstar > DP_MAXGROUP (dao)) {

	        if (DP_VERBOSE(dao) == YES) {
	            call printf (
		    "Group too large: %5d  Fitting Radius: %5.2f  Regrouping\n")
		        call pargi (nstar)
		        call pargr (radius)
	        }

	        # If size of group too small go to next group.
	        radius = RADIUS_FRACTION * radius
	        if ((DP_RECENTER(dao) == YES) && (niter >= 2)) {
	            if (radius < DENSE_RADIUS1) {
			fstar = dp_delfaintest (Memr[DP_APMAG(apsel)], istar,
			    lstar)
		        Memr[DP_APMAG(apsel)+fstar-1] = INDEFR
			Memi[DP_ASKIP(allstar)+fstar-1] = YES
			Memi[DP_AIER(allstar)+fstar-1] = ALLERR_BIGGROUP
		        if (DP_VERBOSE(dao) == YES) {
		            call printf (
		 "REJECTING: Star ID: %d Group too dense to reduce\n")
				call pargi (Memi[DP_APID(apsel)+fstar-1])
			}
		        return (lstar)
	            }
	        } else {
	            if (radius < DENSE_RADIUS2) {
			fstar = dp_delfaintest (Memr[DP_APMAG(apsel)], istar,
			    lstar)
		        Memr[DP_APMAG(apsel)+fstar-1] = INDEFR
			Memi[DP_ASKIP(allstar)+fstar-1] = YES
			Memi[DP_AIER(allstar)+fstar-1] = ALLERR_BIGGROUP
		        if (DP_VERBOSE(dao) == YES) {
		            call printf (
			"REJECTING: Star ID: %d Group too dense to reduce\n")
				call pargi (Memi[DP_APID(apsel)+fstar-1])
			}
			return (lstar)
	            }
	        }

	        # Regroup the stars.
	        call dp_regroup (Memi[DP_APID(apsel)+istar-1],
	            Memr[DP_APXCEN(apsel)+istar-1], Memr[DP_APYCEN(apsel)+
		    istar-1], Memr[DP_APMAG(apsel)+istar-1],
		    Memr[DP_APMSKY(apsel)+istar-1],
		    Memr[DP_ASUMWT(allstar)+istar-1],
		    Memr[DP_AXOLD(allstar)+ istar-1],
		    Memr[DP_AYOLD(allstar)+istar-1],
		    Memr[DP_AXCLAMP(allstar)+istar-1],
		    Memr[DP_AYCLAMP(allstar)+istar-1], nstar, radius,
		    Memi[DP_ALAST(allstar)+istar-1])
		regroup = true
	        next
	    }

	    # Compute the mean sky for the group. If the sky is undefined
	    # reject the group.
	    mean_sky = dp_almsky (Memr[DP_APMSKY(apsel)+istar-1], nstar)
	    if (IS_INDEFR(mean_sky)) {
		call amovkr (INDEFR, Memr[DP_APMAG(apsel)+istar-1], nstar)
		call amovki (YES, Memi[DP_ASKIP(allstar)+istar-1], nstar)
		call amovki (ALLERR_INDEFSKY, Memi[DP_AIER(allstar)+istar-1],
		    nstar)
		if (DP_VERBOSE(dao) == YES) {
		    do i = istar, lstar {
		        call printf (
			"REJECTING: Star ID: %d Group sky value is undefined\n")
			    call pargi (Memi[DP_APID(apsel)+i-1])
		    }
		}
		 return (lstar)
	    }

	    # Re-compute the number of terms in the fitting equation.
	    if ((DP_RECENTER(dao) == YES) && (niter >= 2))
	        nterm = 3 * nstar
	    else
	        nterm = nstar

	    # Zero the fitting arrays.
	    chigrp = asumr (Memr[DP_ASUMWT(allstar)+istar-1], nstar) / nstar
	    call aclrr (Memr[DP_APCHI(apsel)+istar-1], nstar)
	    call aclrr (Memr[DP_ASUMWT(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_ANUMER(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_ADENOM(allstar)+istar-1], nstar)
	    call aclri (Memi[DP_ANPIX(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_AV(allstar)], nterm)
	    call aclrr (Memr[DP_AC(allstar)], cdimen * cdimen)

	    # Compute the subraster limits.

	    ixmin = min (IM_LEN (im,1), max (1, int (xmin - totradius) + 1))
	    ixmax = min (IM_LEN (im,1), max (1,int (xmax + totradius)))
	    iymin = min (IM_LEN (im,2), max (1, int (ymin - totradius) + 1))
	    iymax = min (IM_LEN (im,2), max (1, int (ymax + totradius)))

	    # Get pointer to the required weight, scratch image and
	    # subtracted image pixels. Need to modify this so writing
	    # is only enabled if iter >= MIN_ITER.

	    subt = dp_gst (dao, im, iymin, iymax, READ_ONLY, NO)
	    weights = dp_gwt (dao, im, iymin, iymax, READ_WRITE, NO)
	    data = dp_gdc (dao, im, iymin, iymax, READ_WRITE, NO)

	    # Compute the fitting limits in the subraster.

	    fixmin = min (ixmax, max (ixmin, int (xmin - DP_FITRAD(dao)) + 1))
	    fixmax = min (ixmax, max (ixmin, int (xmax + DP_FITRAD(dao))))
	    fiymin = min (iymax, max (iymin, int (ymin - DP_FITRAD(dao)) + 1))
	    fiymax = min (iymax, max (iymin, int (ymax + DP_FITRAD(dao))))
	    nypix = fiymax - fiymin + 1
	    nxpix = fixmax - fixmin + 1

	    # Now we deal with the pixels one by one.
	    sumres = 0.0
	    grpwt = 0.0

	    # Accumulate the data into the matrix.
	    call dp_alaccum (dao, im, Memr[data], DP_DNX(allstar),
	        DP_DNY(allstar), DP_DXOFF(allstar), DP_DYOFF(allstar),
		Memr[subt], DP_SNX(allstar), DP_SNY(allstar), DP_SXOFF(allstar),
		DP_SYOFF(allstar), Memr[weights], DP_WNX(allstar),
		DP_WNY(allstar), DP_WXOFF(allstar), DP_WYOFF(allstar), nxpix,
		nypix, fixmin, fiymin, mean_sky, istar, lstar, niter, clip,
		pererr, peakerr, sumres, grpwt, chigrp, cdimen, nterm)

	    # Reflect the normal matrix across the diagonal. 

	    call dp_mreflect (Memr[DP_AC(allstar)], cdimen, nterm)

	    # Compute the estimate of the standard deviation of the residuals
	    # for the group as a whole, and for each star. This estimate starts
	    # out as SQRT (PI / 2) * [sum [weight * ABS (residual / sigma)] /
	    # sum [weight] # and then gets corrected for bias by SQRT (# of
	    # pixels / [# of pixel - # degrees of freedom]).
	    #
	    # But now we drive the value toward unity, depending upon exactly
	    # how many pixels were involved. If CHI is based on exactly a total
	    # weight of 3, then it is extremely poorly determined, and we just 
	    # want to keep CHI = 1. The larger the GRPWT is, the better 
	    # determined CHI is, and the less we want to force it toward unity.
	    # So, just take the weighted average of CHI and unity, with weights
	    # GRPWT = 3, and 1, respectively.

	    if (grpwt > nterm) {
	        chigrp = CHI_NORM * sumres / sqrt (grpwt * (grpwt - nterm))
	        chigrp = ((grpwt - 3.0) * chigrp + 3.0) / grpwt
	    } else {
	        chigrp = 1.0
	    }

	    # CHIGRP has been pulled towards its expected value of unity to
	    # keep the statistics of a small number of pixels from completely
	    # dominating the error analysis. Similarly, the photometric errors
	    # for the individual stars will be pulled toward unity now. Later
	    # on, if the number of stars in the group is greater than one,
	    # CHI(I) will nudged toward the group average. In order to work
	    # optimally this requires that the gain and read noise and any
	    # other parameters which represent the noise sources have been
	    # properly specified.
	    #
	    # At the same time, make sure that every star in the group contains
	    # at least MIN_FIT_PIXEL valid pixels if re-centroiding is being 
	    # performed, 1 valid pixel if not. If any star in the group fails
	    # to meet this criterion, mark that star for deletion and skip
	    # ahead to the next group.

	    if (dp_alredo (Memi[DP_APID(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APCHI(apsel)], Memr[DP_ASUMWT(allstar)],
	        Memi[DP_ANPIX(allstar)], Memi[DP_ASKIP(allstar)],
		Memi[DP_AIER(allstar)], istar, lstar, niter, chigrp,
		DP_RECENTER(dao), DP_VERBOSE(dao)))
		return (lstar)

	    # Invert the matrix.
	    if (version == 1)
		call invers (Memr[DP_AC(allstar)], cdimen, nterm, flag)
	    else
		call invers2 (Memr[DP_AC(allstar)], cdimen, nterm, flag)

	    if (dp_checkc (Memr[DP_AC(allstar)], cdimen, nterm,
	        Memi[DP_APID(apsel)], Memr[DP_APMAG(apsel)],
		Memi[DP_ASKIP(allstar)], Memi[DP_AIER(allstar)], istar, niter,
		DP_RECENTER(dao), DP_VERBOSE(dao)))
	        return (lstar)

	    # Compute the coefficients.
	    call mvmul (Memr[DP_AC(allstar)], cdimen, nterm,
	        Memr[DP_AV(allstar)], Memr[DP_AX(allstar)])

	    # In the beginning, the brightness of each star will be permitted
	    # to change by no more than 2  magnitudes per iteration, and the
	    # x,y coordinates of each centroid will be permitted to change by
	    # no more than 0.4 pixel per iteration. Any time that the
	    # parameter correction changes sign from one iteration to the next
	    # the maximum permissable change will be reduced by a factor of
	    # two. These clamps are released any time a star in the group
	    # disappears.

	    if (dp_alclamp (dao, im, Memi[DP_APID(apsel)],
	        Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		Memr[DP_ASUMWT(allstar)], Memi[DP_ASKIP(allstar)],
		Memr[DP_AXOLD(allstar)], Memr[DP_AXCLAMP(allstar)],
		Memr[DP_AYOLD(allstar)], Memr[DP_AYCLAMP(allstar)], istar,
		lstar, Memr[DP_AC(allstar)], Memr[DP_AX(allstar)], cdimen,
		niter, clampmax, pererr, peakerr)) {

	        # Flush the new data to the output buffers. Actually
		# only data which fits this criterion need be written.
		# However this makes sequential i/o management tricky.
		# Leave this alone for the moment. Note only the cache-
		# state is affected.
	    }
	
	    # If there is more than one star, check to see whether any two
	    # stars have merged.

	    if (nstar > 1) {

	        if (dp_almerge (Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		    Memi[DP_ASKIP(allstar)], istar, lstar, sepcrit, sepmin,
		    wcrit, j, k)) {

		    call dp_alcentroid (Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)], j, k)

		    # Remove the k-th star from this group.
		    if (DP_VERBOSE(dao) == YES) {
		        call printf (
			"REJECTING: Star ID: %d has merged with star ID: %d\n")
			    call pargi (Memi[DP_APID(apsel)+j-1])
			    call pargi (Memi[DP_APID(apsel)+k-1])
		    }
		    Memr[DP_APMAG(apsel)+j-1] = INDEFR
		    Memi[DP_ASKIP(allstar)+j-1] = YES
		    Memi[DP_AIER(allstar)+j-1] = ALLERR_MERGE

		    # Loosen the clamps of every star in the group
		    call aclrr (Memr[DP_AXOLD(allstar)+istar-1], nstar)
		    call aclrr (Memr[DP_AYOLD(allstar)+istar-1], nstar)
		    call amaxkr (Memr[DP_AXCLAMP(allstar)+istar-1], 0.5 *
		        clampmax, Memr[DP_AXCLAMP(allstar)+istar-1], nstar)
		    call amaxkr (Memr[DP_AYCLAMP(allstar)+istar-1], 0.5 *
			clampmax, Memr[DP_AYCLAMP(allstar)+istar-1], nstar)

	        }
	    }

	    # If the number of iterations completed is less than or equal
	    # to 3, perform another iteration no questions asked.

	    if (niter <= (MIN_ITER - 1))
		return (lstar)

	    # If not star has been removed from the group in the previous
	    # iteration, check to see if any of the stars is too faint (more
	    # than 12.5 magnitudes fainter thanthe PSF star). If several stars
	    # are too faint, delete the faintest one, and set the 
	    # brightnesses of the other faint ones exactly to 12.5
	    # magnitudes below the PSF star. That way on the next iteration
	    # we will see whether these  stars want to grow or disappear.

	    if (dp_alfaint (Memr[DP_APMAG(apsel)], Memi[DP_ASKIP(allstar)],
	        istar, lstar, faint, ifaint))
		return (lstar)

	    # If at least one star is more than 12.5 magnitudes fainter
	    # than the PSF, then IFAINT is the index of the faintest of them
	    # and faint is the relative brightness of the faintest of them.

	    if (ifaint > 0) {

	        if (DP_VERBOSE(dao) == YES) {
		    call printf ("REJECTING: Star ID: %d is too faint\n")
		        call pargi (Memi[DP_APID(apsel)+ifaint-1])
	        }
	        Memr[DP_APMAG(apsel)+ifaint-1] = INDEFR
	        Memi[DP_ASKIP(allstar)+ifaint-1] = YES
	        Memi[DP_AIER(allstar)+ifaint-1] = ALLERR_FAINT

	        call aclrr (Memr[DP_AXOLD(allstar)+istar-1], nstar)
	        call aclrr (Memr[DP_AYOLD(allstar)+istar-1], nstar)
	        call amaxkr (Memr[DP_AXCLAMP(allstar)+istar-1], 0.5 *
	            clampmax, Memr[DP_AXCLAMP(allstar)+istar-1], nstar)
	        call amaxkr (Memr[DP_AYCLAMP(allstar)+istar-1], 0.5 *
		    clampmax, Memr[DP_AYCLAMP(allstar)+istar-1], nstar)
		
	    # If no star is more than 12.5 magnitudes fainter than the PSF
	    # then after the 50th iteration delete the least certain star i
	    # if it is less than a one sigma detection; after the 100th
	    # iteration delete the least certain star if it is less than a 
	    # two sigma detection; after the 150th delete the least certain
	    # star if it is less than a 3 sigma detection.

	    } else if (niter >= 5) {

	        call dp_almfaint (Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		    istar, lstar, faint, ifaint)

	        if (faint < wcrit) {

	            if (DP_VERBOSE(dao) == YES) {
		        call printf ("REJECTING: Star ID: %d is too faint\n")
		            call pargi (Memi[DP_APID(apsel)+ifaint-1])
	            }
	            Memr[DP_APMAG(apsel)+ifaint-1] = INDEFR
	            Memi[DP_ASKIP(allstar)+ifaint-1] = YES
	            Memi[DP_AIER(allstar)+ifaint-1] = ALLERR_FAINT

	            # Loosen the clamps of every star in the group.
	            call aclrr (Memr[DP_AXOLD(allstar)+istar-1], nstar)
	            call aclrr (Memr[DP_AYOLD(allstar)+istar-1], nstar)
	            call amaxkr (Memr[DP_AXCLAMP(allstar)+istar-1], 0.5 *
	                clampmax, Memr[DP_AXCLAMP(allstar)+istar-1], nstar)
	            call amaxkr (Memr[DP_AYCLAMP(allstar)+istar-1], 0.5 *
		        clampmax, Memr[DP_AYCLAMP(allstar)+istar-1], nstar)
	        }
	    }

	    break
	}

	return (lstar)
end


# DP_LASTSTAR -- Find the last star in the current group.

int procedure dp_laststar (last, istar, ntot)

int	last[ARB]		# the grouping information array
int	istar			# index of the first star in the group
int	ntot			# total number of stars

int	lstar

begin
	do lstar = istar, ntot, 1 {
	    if (last[lstar] == YES)
		break
	}

	return (lstar)
end


# DP_DELFAINTEST -- Delete the faintest star in the group.

int procedure dp_delfaintest (mag, istar, lstar)

real	mag[ARB]		# the array of magnitudes
int	istar			# the first star in the group
int	lstar			# the last star in the group

int	i, starno
real	faint

begin
	starno = 0
	faint = MAX_REAL
	do i = istar, lstar {
	    if (mag[i] >= faint)
		next
	    faint = mag[i]
	    starno = i
	}

	return (starno)
end


# DP_ALMSKY -- Determine the mean sky value for the current group of stars.

real procedure dp_almsky (sky, nstar)

real	sky[ARB]		# array of sky values
int	nstar			# number of stars in the group

int	i, nsky
real 	sky_sum

begin
	sky_sum = 0.0
	nsky = 0

	do i = 1, nstar {
	    if (IS_INDEFR(sky[i]))
		next
	    sky_sum = sky_sum + sky[i]
	    nsky = nsky + 1
	}

	if (nsky <= 0)
	    return (INDEFR)
	else
	    return (sky_sum / nsky)
end


# DP_ALACCUM -- Accumulate the data into the matrix.

procedure dp_alaccum (dao, im, data, dnx, dny, dxoff, dyoff, subt, snx, sny,
	sxoff, syoff, weights, wnx, wny, wxoff, wyoff, nxpix, nypix, ixmin,
	iymin, mean_sky, istar, lstar, niter, clip, pererr, peakerr, sumres,
	grpwt, chigrp, cdimen, nterm)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
real	data[dnx,dny]		# the subtracted data array
int	dnx, dny		# dimenions of the data array
int	dxoff, dyoff		# lower left corner of the data array
real	subt[snx,sny]		# the scratch array
int	snx, sny		# dimensions of the scratch array
int	sxoff, syoff		# lower left corner of the scratch array
real	weights[wnx,wny]	# the weight array
int	wnx, wny		# dimensions of the weight array
int	wxoff, wyoff		# lower left corner of the weight array
int	nxpix, nypix		# the dimensions of the area of interest
int	ixmin,iymin		# lower left corner of area of interest
real	mean_sky		# the group sky value
int	istar			# the index of the first star in current group
int	lstar			# the index of the last star in current group
int	niter			# the current interation
bool	clip			# clip the errors ?
real	pererr			# flat fielding error factor
real	peakerr			# profile error factor
real	sumres			# sum of the residuals
real	grpwt			# the group weight
real	chigrp			# the group chi value
int	cdimen			# maximum number of maxtrix dimensions
int	nterm			# number of terms in the matrix to fit

real	fitradsq, maxgdata, fix, fiy, d, delta, sigmasq, relerr, wt, dwt
pointer	psffit, apsel, allstar
int	dix, diy, sxdiff, sydiff, wxdiff, wydiff
real	sky_value, dp_alskyval()
bool	dp_alomit()

begin
	# Set up some pointers.
	apsel = DP_APSEL(dao)
	psffit = DP_PSFFIT(dao)
	allstar = DP_ALLSTAR(dao)

	# These constants need to be stored more permanently in the
	# allstar structure at some point. They should all be defined
	# once and for all at task startup. Leave for next phase
	# of code cleanup.

	fitradsq = DP_FITRAD(dao) ** 2
	if (IS_INDEFR(DP_MAXGDATA(dao)))
	    maxgdata = MAX_REAL
	else
	    maxgdata = DP_MAXGDATA(dao)

	# Compute the array offsets.
	sxdiff = dxoff - sxoff
	sydiff = dyoff - syoff 
	wxdiff = dxoff - wxoff
	wydiff = dyoff - wyoff

	do diy = iymin - dyoff + 1, iymin - dyoff + nypix, 1 {
	    fiy = real (diy + dyoff - 1)
	    do dix = ixmin - dxoff + 1, ixmin - dxoff + nxpix, 1 {

		# Skip data with negative weights.
		if (weights[dix+wxdiff,diy+wydiff] < 0.0) 
		    next
		fix = real (dix + dxoff - 1)

		# If this current pixel is within one fitting radius of
		# at least one star in the current group, include it in the
		# calculation. Otherwise skip it. While figuring this out,
		# compute the squared distance of this pixel from the
		# centroid of each star in the group.

		if (dp_alomit (Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_ARPIXSQ(allstar)], istar, lstar, fix, fiy,
		    fitradsq))
		    next

		call dp_alsetskip (Memr[DP_ARPIXSQ(allstar)],
		    Memi[DP_ASKIP(allstar)], istar, lstar, fitradsq)

		if (DP_GROUPSKY(dao) == NO) {
		    sky_value = dp_alskyval (Memr[DP_APMSKY(apsel)],
		        Memi[DP_ASKIP(allstar)], istar, lstar)
		    if (IS_INDEFR(sky_value))
			sky_value = mean_sky
		} else
		    sky_value = mean_sky

		# The expected random error in the pixel is the quadratic
		# sum of the Poisson statistics, plus the readout noise, 
		# plus an estimated error of 0.75% of the total brightness
		# for the difficulty of flat-fielding and bias-correcting 
		# the chip, plus an estimated error of some fraction of the
		# of the fourth derivative at the peak of the profile, to
		# account for the difficulty of accurately interpolating
		# within the PSF. The fourth derivative of the PSF is
		# proportional to H/hwhm ** 4 (hwhm is the width of the
		# stellar core); using the geometric mean of hwhmx and
		# hwhmy, this becomes H/(hwhmx * hwhmy) ** 2. The ratio of
		# the fitting error to this quantity is estimated from a
		# good-seeing CTIO frame to be approimxately 0.027.

		#d = subt[dix+sxdiff,diy+sydiff] - mean_sky
		d = subt[dix+sxdiff,diy+sydiff] - sky_value
		delta = max (0.0, data[dix,diy] - d)
		if ((delta > maxgdata) && (niter >= MIN_ITER))
		    next

		# Dpos = raw data - residual
		#      = model-predicted brightness in this pixel consisting
		#        of sky plus all stellar profiles, which is
		#        presumably non-negative
		#
		# The four noise sources in the model are
		#        readout noise
		#        poisson noise
		#        flat-field errors
		#        profile errors 
		#
		# Numerically the squares of these quantities are
		#        ronoise = sigma[i,j]
		#        poisson noise = delta / gain
		#        flat-field error = constant * delta ** 2
		#        profile error = constant * sum of profile ** 2

		#sigmasq = weights[dix+wxdiff,diy+wydiff] + delta / 
		    #DP_PHOTADU(dao) + (pererr * delta) ** 2 +
		    #(peakerr * (delta - mean_sky)) ** 2
		sigmasq = weights[dix+wxdiff,diy+wydiff] + delta / 
		    DP_PHOTADU(dao) + (pererr * delta) ** 2 +
		    (peakerr * (delta - sky_value)) ** 2
		if (sigmasq <= 0.0)
		    next
		relerr = abs (d) / sqrt (sigmasq)

		if (clip && (relerr > MAX_RELERR))
		    next

		# Now include this pixel in the fitting equation for the
		# group.

		wt = 0.0
		call dp_alxaccum (dao, im,  Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		    Memr[DP_ARPIXSQ(allstar)], Memi[DP_ASKIP(allstar)],
		    Memr[DP_AX(allstar)], Memr[DP_ANUMER(allstar)],
		    Memr[DP_ADENOM(allstar)], istar, lstar, fix, fiy, d,
		    wt, sigmasq, nterm, fitradsq)

		# At this point the vector X contains  the first
		# derivative of the condition equation for pixel (I,J)
		# with respect to each of the fitting parameters for
		# all of the stars. Now these derivatives will be added 
		# into the normal matrix and the vector of residuals.
		# Add this residual into the weighted sum of the absolute
		# relative residuals

		dwt = wt * relerr
		sumres = sumres + dwt
		grpwt = grpwt + wt

		# SUMRES is the weighted sum for all the pixels in the group.
		# Now also add the weigghted value of the residuals into the
		# accumulating sum for each of the stars.

		call dp_alchi (Memi[DP_ASKIP(allstar)], Memr[DP_APCHI(apsel)],
		    Memr[DP_ASUMWT(allstar)], Memi[DP_ANPIX(allstar)], istar,
		    lstar, wt, dwt)

		# Up until now, WT represents only the radial weighting 
		# profile. Now figure in the anticipated standard error
		# of the pixel.

		wt = wt / sigmasq

		# After the third iteration, reduce the weight of a bad pixel
		# Note that for the first iteration, only the stellar magnitude
		# is being solved for, which is a problem in LINEAR least 
		# squares, and so should be solved exactly the first time.
		# After that, the star is given two iterations to adjust it's
		# centroid before the clipping is turned on.  After that a
		# pixel having a residual of DP_CLIPRANGE times sigma gets
		# reduced to half weight;  a pixel having a residual of n
		# sigma gets a weight of 1 / [1 + (n/DP_CLIPRANGE) **
		# DP_CLIPEXP].

		if (clip)
		    wt = wt / (1.0 + (relerr / (chigrp * DP_CLIPRANGE (dao))) **
			 DP_CLIPEXP (dao))

		# Now work this pixel into the normal matrix
		dwt = d * wt
		call dp_alcaccum (Memr[DP_AX(allstar)], Memr[DP_AC(allstar)],
		    Memr[DP_AV(allstar)], Memi[DP_ASKIP(allstar)], cdimen,
		    nterm, istar, lstar, wt, dwt)
	    }
	}
end


# DP_ALOMIT - Omit pixels which are too far from the center of any star
# in the group.

bool procedure dp_alomit (xcen, ycen, rpixsq, istar, lstar, fix, fiy,
	fitradsq)

real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	rpixsq[ARB]		# array of pixel differences
int	istar, lstar		# first and last star
real	fix, fiy		# current star position
real	fitradsq		# fit radius squared

bool	omit
int	i

begin
	omit = true
	do i = istar, lstar {
	    rpixsq[i] = (fix - xcen[i]) ** 2 + (fiy - ycen[i]) ** 2
	    if (rpixsq[i] < fitradsq)
		omit = false
	}

	return (omit)
end


# DP_ALSETSKIP -- Initialize the skip array.

procedure dp_alsetskip (rpixsq, skip, istar, lstar, fitradsq)

real	rpixsq[ARB]		# pixel distance squared
int	skip[ARB]		# skip array
int	istar			# the index of the first star
int	lstar			# the index of the last star
real	fitradsq		# the fitting radius squared

int	i

begin
	do i = istar, lstar {
	    if (rpixsq[i] < fitradsq)
		skip[i] = NO
	    else
		skip[i] = YES
	}
end


# DP_ALSKYVAL -- Compute the sky value to be subtracted from a given pixel
# by averaging the sky values of all the the stars for which the pixel in
# question is within one fitting radius.

real procedure dp_alskyval (sky, skip, istar, lstar)

real	sky[ARB]		# the array of sky values
int	skip[ARB]		# the array of skip values
int	istar			# the index of the first star
int	lstar			# the index of the last star

int	i, npts
real	sum

begin
	sum = 0.0
	npts = 0
	do i = istar, lstar {
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


# DP_ALXACCUM - Accumulate the x vector.

procedure dp_alxaccum (dao, im, xcen, ycen, mag, rpixsq, skip, x, numer1,
	denom1, istar, lstar, fix, fiy, resid, wt, sigmasq, nterm, fitradsq)

pointer	dao			# pointer to the daopot structure
pointer	im			# pointer to the input image
real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of relative brightnesses
real	rpixsq[ARB]		# array of pixel distances squared
int	skip[ARB]		# the include / exclude array
real	x[ARB]			# the x vector to be accumulated
real	numer1[ARB]		# first numerator array
real	denom1[ARB]		# first denominator array
int	istar			# index of the first star in group
int	lstar			# index of the last star in group
real	fix			# the x position of the current pixel
real	fiy			# the y position of the current pixel
real	resid			# the input data residual
real	wt			# the output weight value
real	sigmasq			# the sigma squared value
int	nterm			# number of terms in the matrix
real	fitradsq		# fitting radius squared

real	psfsigsqx, psfsigsqy, dx, dy, deltax, deltay, value, radsq, rhosq
real	dvdx, dvdy, dfdsig
pointer	psffit
int	nstar, i, i3, k
real	dp_usepsf()

begin
	psffit = DP_PSFFIT(dao)

	nstar = lstar - istar + 1
	psfsigsqx = Memr[DP_PSFPARS(psffit)]
	psfsigsqy = Memr[DP_PSFPARS(psffit)+1]

	do i = istar, lstar {

	    if (skip[i] == YES)
		next
	    radsq = rpixsq[i] / fitradsq
	    if (radsq >= MAX_RSQ) 
		next
	    wt = max (wt, 5.0 / (5.0 + radsq / (1.0 - radsq)))
		    
	    # The condition equation for pixel[I,J] is the following.
	    # 
	    # data[I,J] - sum [scale * psf[I,J]] - mean_sky = residual
	    #
	    # Then we will adjust the scale factors, x centers and ycenters
	    # such that sum [weight * residual ** 2] is minimized.
	    # WT will be a function (1) of the distance of this pixel from
	    # the center of the nearest star, (2) of the model predicted
	    # brightness of the pixel (taking into consideration the
	    # readout noise, the photons/ADU, and the interpolation error
	    # of the PSF), and (3) of the size of the residual itself. (1) is
	    # necessary to prevent the non-linear least-squares from
	    # fit from oscillating. (2) is simply sensible weighting and (3)
	    # is a crude attempt at making the solution more robust
	    # against bad pixels.

	    dx = fix - xcen[i]
	    dy = fiy - ycen[i]
	    call dp_wpsf (dao, im, xcen[i], ycen[i], deltax, deltay, 1)
	    deltax = (deltax - 1.0) / DP_PSFX(psffit) - 1.0
	    deltay = (deltay - 1.0) / DP_PSFY(psffit) - 1.0
	    value = dp_usepsf (DP_PSFUNCTION(psffit), dx, dy,
	        DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit), deltax, deltay,
	        dvdx, dvdy)

	    if (nterm > nstar) {
		i3 = (i - istar + 1) * 3
		k = i3 - 2
		x[k] = -value
		k = i3 - 1
		x[k] = -mag[i] * dvdx
		x[i3] = -mag[i] * dvdy
	    } else {
		k = i - istar + 1
		x[k] = -value
	    }

	    rhosq =  (dx / psfsigsqx) ** 2 + (dy / psfsigsqy) ** 2
	    if (rhosq > MAX_RHOSQ)
		next
	    rhosq = 0.6931472 * rhosq
	    dfdsig = exp (-rhosq) * (rhosq - 1.)
	    numer1[i] = numer1[i] + dfdsig * resid / sigmasq
	    denom1[i] = denom1[i] + dfdsig ** 2 / sigmasq
	}
end


# DP_ALCHI -- Compute the weights and chis.

procedure dp_alchi (skip, chi, sumwt, npix, istar, lstar, wt, dwt)

int	skip[ARB]		# include / exclude array for the stars
real	chi[ARB]		# array of chi values
real	sumwt[ARB]		# array of weight sums
int	npix[ARB]		# array of pixel counts
int	istar			# index of first star
int	lstar			# index of last star
real	wt			# input weight
real	dwt			# weighted residual

int	i

begin
	do i = istar, lstar {
	    if (skip[i] == YES)
		next
	    chi[i] = chi[i] + dwt
	    sumwt[i] = sumwt[i] + wt
	    npix[i] = npix[i] + 1
	}
end


# DP_ALCACCUM -- Accumulate the main matrix.

procedure dp_alcaccum (x, c, v, skip, cdimen, nterm, istar, lstar, wt, dwt)


real	x[ARB]			# x array
real	c[cdimen,ARB]		# maxtrix array
real	v[ARB]			# v vector
int	skip[ARB]		# include / exclude array
int	cdimen			# maximum size of c matrix
int	nterm			# number of terms to be fit
int	istar			# index of the first star
int	lstar			# index of the last star 
real	wt			# input weight
real	dwt			# input residual weight

int	nstar, i, j, k, l, i3, i3m2, j3
real	xkwt

begin
	nstar = lstar - istar + 1
	do i = istar, lstar {
	    if (skip[i] == YES)
		next
	    if (nterm > nstar) {
		i3 = (i - istar + 1) * 3
		i3m2 = i3 - 2
		do k = i3m2, i3 
		    v[k] = v[k] + x[k] * dwt
		do j = istar, i {
		    if (skip[j] == YES)
			next
		    j3 = (j - istar + 1) * 3
		    do k = i3m2, i3 {
			xkwt = x[k] * wt
			do l = j3 - 2, min (k, j3)
			    c[k,l] = c[k,l] + x[l] * xkwt
		    }
		}
	    } else {
		k = i - istar + 1
		v[k] = v[k] + x[k] * dwt
		xkwt = x[k] * wt
		do j = istar, i {
		    if (skip[j] == YES)
			next
		    l = j - istar + 1
		    c[k,l] = c[k,l] + x[l] * xkwt
	        }
	    }
	}
end


# DP_ALREDO -- Check to see that there are enough good pixels to fit the
# star and compute the individual chi values.

bool procedure dp_alredo (id, mag, chi, sumwt, npix, skip, aier, istar, lstar,
	niter, chigrp, center, verbose)

int	id[ARB]			# the array of star ids
real	mag[ARB]		# the array of relative brightnesses
real	chi[ARB]		# the array of chi values
real	sumwt[ARB]		# the array of weight values
int	npix[ARB]		# the array of pixel counts
int	skip[ARB]		# the include / exclude array
int	aier[ARB]		# the array of error codes
int	istar			# index of the first star
int	lstar			# index of the last star
int	niter			# the current iteration
real	chigrp			# chi value of the group
int	center			# recenter the values ?
int	verbose			# verbose mode ?

bool	redo
int	i

begin
	redo = false
	do i = istar, lstar {
	    if (center == YES && (niter >= 2)) {
		if (npix[i] < MIN_NPIX) {
		    redo = true
		    skip[i] = YES
		    if (verbose == YES) {
			call printf (
			    "REJECTING: Star ID: %d has too few good pixels\n")
			call pargi (id[i])
		    }
		    aier[i] = ALLERR_NOPIX
		    mag[i] = INDEFR
		} else {
		    skip[i] = NO
		    if (sumwt[i] > MIN_SUMWT) {
			chi[i] = CHI_NORM * chi[i] / sqrt (sumwt[i] *
			    (sumwt[i] - MIN_SUMWT))
			sumwt[i] = ((sumwt[i] - MIN_SUMWT) * chi[i] +
			    MIN_SUMWT) / sumwt[i]
		    } else
			chi[i] = chigrp
		}
	    } else {
		if (npix[i] < MIN_NPIX) {
		    redo = true
		    skip[i] = YES
		    if (verbose == YES) {
			call printf (
			    "REJECTING: Star ID: %d has too few good pixels\n")
			call pargi (id[i])
		    }
		    aier[i] = ALLERR_NOPIX
		    mag[i] = INDEFR
		} else {
		    skip[i] = NO
		    if (sumwt[i] > 1.0) {
			chi[i] = CHI_NORM * chi[i] / sqrt (sumwt[i] *
			    (sumwt[i] - 1.0))
			sumwt[i] = ((sumwt[i] - MIN_SUMWT) * chi[i] +
			    MIN_SUMWT) / sumwt[i]
		    } else
			chi[i] = chigrp
		}
	    }
	}

	return (redo)
end


# DP_CHECKC - Check the c matrix for valid diagonal elements.

bool procedure  dp_checkc (c, cdimen, nterm, id, mag, skip, aier, istar, niter,
	recenter, verbose)

real	c[cdimen,ARB]		# the c matrix
int	cdimen			# maximum size of the c matrix
int	nterm			# number of terms to be fit
int	id[ARB]			# the array of ids
real	mag[ARB]		# the array of relative brightnesses
int	skip[ARB]		# the include / exclude array
int	aier[ARB]		# the array of error codes
int	istar			# index of the first star
int	niter			# the iteration number
int	recenter		# recenter ?
int	verbose			# verbose mode ?

bool	redo
int	j, starno

begin
	redo = false
	do j = 1, nterm {
	    if (c[j,j] > 0.0)
		next
	    if ((recenter == YES) && (niter >= 2))
		starno = (j + 2) / 3
	    else
		starno = j
	    starno = starno + istar - 1
	    skip[starno] = YES
	    if (verbose == YES) {
		call printf (
		    "REJECTING: Star ID: %d generated a fitting error\n")
		    call pargi (id[starno])
	    }
	    aier[starno] = ALLERR_SINGULAR
	    mag[starno] = INDEFR
	    redo = true
	    break
	}

	return (redo)
end


# DP_ALCLAMP -- Get the answers.

bool procedure dp_alclamp (dao, im, id, xcen, ycen, mag, magerr, sumwt,
	skip, xold, xclamp, yold, yclamp, istar, lstar, c, x,
	cdimen, niter, clampmax, pererr, peakerr)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
int	id[ARB]			# array of star ids
real	xcen[ARB]		# array of star x centers
real	ycen[ARB]		# array of star y centers
real	mag[ARB]		# array of relative brightnesses
real	magerr[ARB]		# array of relative brightness errors
real	sumwt[ARB]		# array of pixel distance squared values
int 	skip[ARB]		# include / exclude array
real	xold[ARB]		# xold array
real	xclamp[ARB]		# xclamp array
real	yold[ARB]		# yold array
real	yclamp[ARB]		# yclamp array
int	istar			# the index of the first star
int	lstar			# the index of the last star
real	c[cdimen,ARB]		# c matrix
real	x[ARB]			# x vector
int	cdimen			# the maximum size of c matrix 
int	niter			# the current iteration
real	clampmax		# the maximum clamp value
real	pererr			# flat field error
real	peakerr			# the profile error

bool	redo, bufwrite
int	i, l, k, j, lx, mx, ly, my, nx, ny
pointer	psffit, allstar
real	dwt, psfrad, psfradsq, df

begin
	# Get some daophot pointers.
	allstar = DP_ALLSTAR(dao)
	psffit = DP_PSFFIT(dao)

	# Get some constants.
	if (DP_PSFSIZE(psffit) == 0)
	    psfrad = DP_PSFRAD(dao)
	else
	    psfrad = (real (DP_PSFSIZE(psffit) - 1) / 2.0 - 1.0) / 2.0
	psfradsq = psfrad * psfrad

	# Initialize.
	bufwrite = false

	do i = istar, lstar {

	    if ((DP_RECENTER(dao) == YES)  && (niter >= 2)) {

		l = 3 * (i - istar + 1)
		k = l - 1
		j = l - 2

		# Note that the sign of the correction is such that it must be
		# SUBTRACTED from the current value of the parameter to get the
		# improved parameter value. This being the case, if the 
		# correction to the brightness is negative a change of one
		# magnitude is a change of factor of 2.5; if the brightness
		# correction is positive a change of one magnitude is a change
		# of 60%.

		dwt = xold[i] * x[k]
		if (dwt < 0.0)
		    xclamp[i] = max (MIN_XYCLAMP, MIN_XYCLAMP_FRACTION *
		        xclamp[i])
		else
		    xclamp[i] = min (clampmax, MAX_XYCLAMP_FRACTION * xclamp[i])
		xcen[i] = xcen[i] - x[k] / (1.0 + abs (x[k] / xclamp[i]))
		xold[i] = x[k]
		
		dwt = yold[i] * x[l]
		if (dwt < 0.0)
		    yclamp[i] = max (MIN_XYCLAMP, MIN_XYCLAMP_FRACTION *
		        yclamp[i])
		else
		    yclamp[i] = min (clampmax, MAX_XYCLAMP_FRACTION * yclamp[i])
		ycen[i] = ycen[i] - x[l] / (1.0 + abs (x[l] / yclamp[i]))
		yold[i] = x[l]

		mag[i] = mag[i] - x[j] / (1.0 + max (x[j] / (MAX_DELTA_FAINTER *
		    mag[i]), -x[j] / (MAX_DELTA_BRIGHTER * mag[i])))
		magerr[i] = sumwt[i] * sqrt (c[j,j])

		if (niter >= MIN_ITER) {
		    redo = false
		    if (abs(x[j]) > max (MAX_NEW_ERRMAG * magerr[i],
		        MAX_NEW_RELBRIGHT2 * mag[i]))
			redo = true
		    else {
			df = (MAX_NEW_ERRMAG * sumwt[i]) ** 2
			if ((x[k] ** 2) > max (df * c[k,k], MAX_PIXERR2))
			    redo = true
			else if ((x[l] ** 2) > max (df * c[l,l], MAX_PIXERR2))
			    redo = true
		    }
		} else
		    redo = true

	    } else {
		j = i - istar + 1
		mag[i] = mag[i] - x[j] / (1.0 + 1.2 * abs (x[j] / mag[i]))
		magerr[i] = sumwt[i] * sqrt (c[j,j])
		if (niter >= 2) {
		    redo = false
		    if (abs(x[j]) > max (MAX_NEW_ERRMAG * magerr[i],
			MAX_NEW_RELBRIGHT2 * mag[i]))
			redo = true
		} else
		    redo = true
	    }

	    if (mag[i] < 2.0 * magerr[i])
		redo = true
	    if (niter >= DP_MAXITER(dao))
		redo = false
	    if (redo && (niter < DP_MAXITER(dao)))
		next

	    # If this star converged, write out the results for it,
	    # flag it for deletion from the star list and subtract it
	    # from the reference copy of the image.

	    call dp_glim (xcen[i], ycen[i], psfrad, DP_DLX(allstar),
	        DP_DMX(allstar), DP_DLY(allstar), DP_DMY(allstar),
	        lx, mx, ly, my)
	    nx = mx - lx + 1
	    ny = my - ly + 1

	    call dp_swstar (dao, im, Memr[DP_DBUF(allstar)], DP_DNX(allstar),
		DP_DNY(allstar), DP_DXOFF(allstar), DP_DYOFF(allstar),
		Memr[DP_WBUF(allstar)], DP_WNX(allstar), DP_WNY(allstar),
		DP_WXOFF(allstar), DP_WYOFF(allstar), xcen[i], ycen[i],
		mag[i], lx, ly, nx, ny, psfradsq, DP_PHOTADU(dao), pererr,
		peakerr)

	    skip[i] = YES

	    if (DP_VERBOSE(dao) == YES) {
		call dp_wout (dao, im, xcen[i], ycen[i], xcen[i], ycen[i], 1)
		call printf (
		"FITTING:   ID: %5d  XCEN: %8.2f  YCEN: %8.2f  MAG: %8.2f\n")
		    call pargi (id[i])
		    call pargr (xcen[i])
		    call pargr (ycen[i])
		if (mag[i] <= 0.0)
		    call pargr (INDEFR)
		else
		    call pargr (DP_PSFMAG(psffit) - 2.5 * log10 (mag[i]))

	    }
	    bufwrite = true
	}

	return (bufwrite)
end


# DP_SWSTAR -- Subtract the fitted star for the data and adjust the
# weights.

procedure dp_swstar (dao, im, data, dnx, dny, dxoff, dyoff, weights, wnx, wny,
	wxoff, wyoff, xcen, ycen, mag, lx, ly, nx, ny, psfradsq, gain,
	pererr, peakerr)

pointer	dao				# pointer to the daophot structure
pointer	im				# pointer to the input image
real	data[dnx,dny]			# the data array
int	dnx, dny			# dimensions of the data array
int	dxoff, dyoff			# lower left corner of data array
real	weights[wnx,wny]		# the weights array
int	wnx, wny			# dimensions of the weights array
int	wxoff, wyoff			# lower left corner of weights array
real	xcen, ycen			# the position of the star
real	mag				# relative brightness of the star
int	lx, ly				# lower left corner region of interest
int	nx, ny				# size of region of interest
real	psfradsq			# the psf radius squared
real	gain				# the gain in photons per adu
real	pererr				# the flat field error factor
real	peakerr				# the peak error factor

real	deltax, deltay, dx, dy, dysq, diff, dvdx, dvdy
pointer	psffit
int	di, dj, wxdiff, wydiff
real	dp_usepsf()

begin
	psffit = DP_PSFFIT(dao)

	wxdiff = dxoff - wxoff
	wydiff = dyoff - wyoff
	call dp_wpsf (dao, im, xcen, ycen, deltax, deltay, 1)
	deltax = (deltax - 1.0) / DP_PSFX(psffit) - 1.0
	deltay = (deltay - 1.0) / DP_PSFY(psffit) - 1.0

	do dj = ly - dyoff + 1, ly - dyoff + ny {
	    dy = (dj + dyoff - 1) - ycen
	    dysq = dy * dy
	    do di = lx - dxoff + 1, lx - dxoff + nx {
		if (weights[di+wxdiff,dj+wydiff] <= -MAX_REAL)
		    next
	        dx = (di + dxoff - 1) - xcen
		if ((dx * dx + dysq) >= psfradsq)
		    next
		diff = mag * dp_usepsf (DP_PSFUNCTION(psffit), dx, dy,
		    DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		    DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit), deltax, deltay,
		    dvdx, dvdy)
		data[di,dj] = data[di,dj] - diff
		if (diff > 0.0) {
		    diff = diff / gain + pererr *
		        2.0 * max (0.0, data[di,dj]) * diff +
			(peakerr * diff) ** 2
		    if (weights[di+wxdiff,dj+wydiff] >= 0.0)
			weights[di+wxdiff,dj+wydiff] = weights[di+wxdiff,
			    dj+wydiff] + diff
		    else
			weights[di+wxdiff,dj+wydiff] = - (abs (weights[di+
			    wxdiff,dj+wydiff]) + diff)
		}
	    }
	}
end


# DP_ALMERGE -- Determine whether or not two stars should merge.

bool procedure dp_almerge (xcen, ycen, mag, magerr, skip, istar, lstar, 
	sepcrit, sepmin, wcrit, k, l)

real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of relative brightnesses
real	magerr[ARB]		# array of relative brightness errors
int	skip[ARB]		# the include / exclude array
int	istar			# index to the first star
int	lstar			# index to the last star
real	sepcrit			# the critical separation
real	sepmin			# the minimum separation
real	wcrit			# the critical error
int	k, l			# indices of stars to be merged

bool	merge
int	i, j
real	rsq, sep

begin
	# Initialize.
	rsq = sepcrit
	merge = false
	k = 0
	l = 0

	# Find the closest two stars within the critical separation.
	do i = istar + 1, lstar {

	    if (skip[i] == YES)
		next

	    do j = istar, i - 1 {
		if (skip[j] == YES)
		    next
		sep = (xcen[i] - xcen[j]) ** 2 + (ycen[i] - ycen[j]) ** 2
		if (sep >= rsq)
		    next
		    
		# Two stars are overlapping. Identify the fainter of the two.
		rsq = sep
		if (mag[i] < mag[j]) {
		    k = i
		    l = j
		} else {
		    k = j
		    l = i
		}

		merge = true
	    }
	}

	# No stars overlap.
	if (! merge)
	    return (merge)

	# The stars are not close enough.
	if ((rsq > sepmin) && (mag[k] > wcrit * magerr[k])) 
	    merge = false

	return (merge)
end


# DP_ALCENTROID -- Merge two stars by adding their relative brightnesses
# and replacing their individual positions with the relative brightness
# weighted mean position.

procedure dp_alcentroid (xcen, ycen, mag, k, l)

real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of magnitudes
int	k, l			# input indices, k is fainter

begin
	xcen[l] = xcen[l] * mag[l] + xcen[k] * mag[k]
	ycen[l] = ycen[l] * mag[l] + ycen[k] * mag[k]
	mag[l] = mag[l] + mag[k]
	xcen[l] = xcen[l] / mag[l]
	ycen[l] = ycen[l] / mag[l]
end


# DP_ALFAINT -- Find all the stars with realtive brightnesss less than
# a given number and set their magnitudes to a given minimum relative
# brightness.

bool procedure dp_alfaint (mag, skip, istar, lstar, faint, ifaint)

real	mag[ARB]		# array of magnitudes
int	skip[ARB]		# skip array
int	istar, lstar		# first and last stars
real	faint			# faintest star
int	ifaint			# index of faintest star

int	i

begin
	# Initialize
	faint = MIN_REL_BRIGHT
	ifaint = 0

	do i = istar, lstar {
	    if (skip[i] == YES)
		return (true)
	    if (mag[i] < faint) {
		faint = mag[i] 
		ifaint = i
	    }
	    if (mag[i] < MIN_REL_BRIGHT)
	        mag[i] = MIN_REL_BRIGHT
	}

	return (false)
end


# DP_ALMFAINT -- Find the star with the greatest error in its relative
# brightness.

procedure dp_almfaint (mag, magerr, istar, lstar, faint, ifaint)

real	mag[ARB]		# array of relative brightnesses
real	magerr[ARB]		# array of relative brightness errors
int	istar			# index of first star
int	lstar			# index of last star
real	faint			# computed faintness limit
int	ifaint			# index of faintest star

int	i
real	wt

begin
	faint = MAX_REAL
	ifaint = 0
	do i = istar, lstar {
	    wt = mag[i] / magerr[i]
	    if (wt < faint) {
		faint = wt
		ifaint = i
	    }
	}
end
