include	<imhdr.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"
include	"../lib/allstardef.h"
include "../lib/apsel.h"

define	CLAMP_FRACTION	     0.25      # max change in fitradius per iter	
define	MINSEP		     2.773     # min sep in gsigma for merging
define	FRACTION_MINSEP      0.14      # min sep in MINSEP * gsigma for merging
define	PEAK_ERR_NUMB	     0.027     # amplitude of flat/bias error
define	RADIUS_FRACTION	     0.95      # % decrease in radius for regrouping
define	DENSE_RADIUS1	     1.2       # 1st limiting radius for regrouping
define	DENSE_RADIUS2	     0.8       # 2nd limiting radius for regrouping
define	EDGE_CONST	     9999.0    # limit on relative error for clipping
define	MIN_ITER	     4	       # minimum number of iterations
define	CHI_NORM	     1.2533141 # sqrt (PI / 2.0)
define	MIN_SUMWT	     3.0       # min value for radial weight sum
define	MIN_NPIX	     3         # min pixels per star for a fit
define	MIN_NEW_ERRMAG	     0.02      # min magnitude error
define	MIN_NEW_RELBRIGHT    0.0003    # min change in relative brightness
define	MAX_PIX_FRACTION     0.05      # max error in x-y
define	MAX_PIXERR	     0.01      # max change in x-y
define	MIN_XYCLAMP	     0.001     # min value of x-y clamp
define	MIN_XYCLAMP_FRACTION 0.5       # min change in value of x-y clamp
define	MAX_XYCLAMP_FRACTION 1.2       # max change in value of x-y clamp
define	MAX_DELTA_FAINTER    0.84      # max permitted brightness decrease
define	MAX_DELTA_BRIGHTER   5.25      # max permitted brightness increase
define	MAX_NEW_ERRMAG	     0.05      # 1st convergence check on magnitude
define	MAX_NEW_RELBRIGHT    0.001     # 2nd convergence check on magnitude
define	INIT_FAINT_FACTOR    100.0     # initial limit on relative brightness
define	MIN_REL_BRIGHT	     1.0e-5    # min relative brightness


# DP_ALPHOT -- Perform one iteration on each group of stars.

int procedure dp_alphot (dao, im, ntot, istar, nconv, ndisap, niter, wcrit,
	ecrit, chigrp)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
int	ntot			# the total number of stars left unfit
int	istar			# first star in the current group
int	nconv			# number of fitted stars
int	ndisap			# number of rejected stars
int	niter			# the current iteration
real	wcrit			# the critical error for merging
real	ecrit			# the critical error for fitting
real	chigrp			# the computed chi value of the group

int	cdimen, nredo, lstar, nstar, nterm
int	ixmin, ixmax, iymin, iymax, fixmin, fixmax, fiymin, fiymax
int	nxpix, nypix, flag, i, j, k, ifaint
pointer	psffit, apsel, allstar, data, subt, weights
real	fitradius, radius, clampmax, sepcrit, sepmin, mean_sky
real	ro32k, peakerr, faint, xmin, xmax, ymin, ymax, totradius, sumres, grpwt

bool	dp_alredo(), dp_checkc(), dp_almerge(), dp_alclamp()
int	dp_laststar()
pointer	dp_gwt(), dp_gst(), dp_gdc()
real	dp_almsky()

begin
	# Get some daophot pointers.
	psffit = DP_PSFFIT (dao)
	apsel = DP_APSEL(dao)
	allstar = DP_ALLSTAR (dao)

	# Define some constants. At some point these should be stored
	# in the allstar structure at task initialization. When the final
	# rewrite gets done this will occur.

	cdimen = 3 * DP_MAXGROUP(dao) + 1
	fitradius = DP_FITRAD (dao)
	totradius = fitradius + DP_PSFRAD(dao) + 1
	radius = fitradius
	clampmax = CLAMP_FRACTION * fitradius
	sepcrit = MINSEP * (DP_PSFSIGX(psffit) ** 2 + DP_PSFSIGY(psffit) ** 2)
	sepmin = min (1.0, FRACTION_MINSEP * sepcrit)
	peakerr = PEAK_ERR_NUMB / (DP_PSFSIGX(psffit) * DP_PSFSIGY(psffit)) ** 2
	ro32k = RO32K

	nredo = ntot

	repeat {

	    # Now get ready to do the next iteration group by group.

	    lstar = dp_laststar (Memi[DP_ALAST(allstar)], istar, ntot)
	    nstar = lstar - istar + 1

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
		        call amovkr (INDEFR, Memr[DP_APMAG(apsel)+istar-1],
			    nstar)
			call amovki (YES, Memi[DP_ASKIP(allstar)+istar-1],
			    nstar)
		        if (DP_VERBOSE(dao) == YES) {
			    do i = istar, lstar {
		                call printf (
			 "REJECTING: Star ID: %d Group too dense to reduce\n")
				call pargi (Memi[DP_APID(apsel)+i-1])
			    }
			}
		        return (lstar)
	            }
	        } else {
	            if (radius < DENSE_RADIUS2) {
		        call amovkr (INDEFR, Memr[DP_APMAG(apsel)+istar-1],
			    nstar)
			call amovki (YES, Memi[DP_ASKIP(allstar)+istar-1],
			    nstar)
		        if (DP_VERBOSE(dao) == YES) {
			    do i = istar, lstar {
		                call printf (
			"REJECTING: Star ID: %d Group too dense to reduce\n")
				call pargi (Memi[DP_APID(apsel)+i-1])
			    }
			}
			return (lstar)
	            }
	        }

	        # Regroup the stars.

	        call dp_regroup (Memi[DP_APID(apsel)+istar-1],
	            Memr[DP_APXCEN(apsel)+istar-1], Memr[DP_APYCEN(apsel)+
		    istar-1], Memr[DP_APMAG(apsel)+istar-1],
		    Memr[DP_APMSKY(apsel)+istar-1], Memr[DP_AXOLD(allstar)+
		    istar-1], Memr[DP_AYOLD(allstar)+istar-1],
		    Memr[DP_AXCLAMP(allstar)+istar-1],
		    Memr[DP_AYCLAMP(allstar)+istar-1], nstar, radius,
		    Memi[DP_ALAST(allstar)+istar-1])
	        next
	    }

	    radius = DP_FITRAD (dao)

	    # Compute the mean sky for the group. If the sky is undefined
	    # reject the group.

	    mean_sky = dp_almsky (Memr[DP_APMSKY(apsel)+istar-1], nstar)
	    if (IS_INDEFR(mean_sky)) {
		call amovkr (INDEFR, Memr[DP_APMAG(apsel)+istar-1], nstar)
		call amovki (YES, Memi[DP_ASKIP(allstar)+istar-1], nstar)
		if (DP_VERBOSE(dao) == YES) {
		    do i = istar, lstar {
		        call printf (
			"REJECTING: Star ID: %d Group sky value is undefined\n")
			    call pargi (Memi[DP_APID(apsel)+i-1])
		    }
		}
		 return (lstar)
	    }

	    # RE-compute the number of terms in the fitting equation.

	    if ((DP_RECENTER(dao) == YES) && (niter >= 2))
	        nterm = 3 * nstar
	    else
	        nterm = nstar

	    # Zero the fitting arrays.

	    call aclrr (Memr[DP_APCHI(apsel)+istar-1], nstar)
	    call aclrr (Memr[DP_ASUMWT(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_ANUMER1(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_ANUMER2(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_ADENOM1(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_ADENOM2(allstar)+istar-1], nstar)
	    call aclri (Memi[DP_ANPIX(allstar)+istar-1], nstar)
	    call aclrr (Memr[DP_AV(allstar)], nterm)
	    call aclrr (Memr[DP_AC(allstar)], cdimen * cdimen)

	    # Compute the subraster limits.

	    call alimr (Memr[DP_APXCEN(apsel)+istar-1], nstar, xmin, xmax)	
	    call alimr (Memr[DP_APYCEN(apsel)+istar-1], nstar, ymin, ymax)	
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

	    fixmin = min (ixmax, max (ixmin, int (xmin - fitradius) + 1))
	    fixmax = min (ixmax, max (ixmin, int (xmax + fitradius)))
	    fiymin = min (iymax, max (iymin, int (ymin - fitradius) + 1))
	    fiymax = min (iymax, max (iymin, int (ymax + fitradius)))
	    nypix = fiymax - fiymin + 1
	    nxpix = fixmax - fixmin + 1

	    # Now we deal with the pixels one by one.
	    sumres = 0.0
	    grpwt = 0.0

	    # Accumulate the data into the matrix.

	    call dp_alaccum (dao, Memr[data], DP_DNX(allstar), DP_DNY(allstar),
	        DP_DXOFF(allstar), DP_DYOFF(allstar), Memr[subt],
		DP_SNX(allstar), DP_SNY(allstar), DP_SXOFF(allstar),
		DP_SYOFF(allstar), Memr[weights], DP_WNX(allstar),
		DP_WNY(allstar), DP_WXOFF(allstar), DP_WYOFF(allstar), nxpix,
		nypix, fixmin, fiymin, mean_sky, istar, lstar, niter, sumres,
		grpwt, cdimen, nterm)

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
	        chigrp = ((grpwt - MIN_SUMWT) * chigrp + MIN_SUMWT) / grpwt
	    } else
	        chigrp = 1

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

	    if (dp_alredo (Memr[DP_AC(allstar)], Memi[DP_APID(apsel)],
	        Memr[DP_APMAG(apsel)], Memr[DP_APCHI(apsel)],
	        Memr[DP_ASUMWT(allstar)], Memr[DP_ARPIXSQ(allstar)],
	        Memi[DP_ANPIX(allstar)], Memi[DP_ASKIP(allstar)], cdimen, istar,
	        lstar, niter, ndisap, nredo, chigrp, DP_RECENTER(dao),
	        DP_VERBOSE(dao)))
		return (lstar)

	    # Invert the matrix.
  	    call invers (Memr[DP_AC(allstar)], cdimen, nterm, flag)

	    if (dp_checkc (Memr[DP_AC(allstar)], Memi[DP_APID(apsel)],
	        Memr[DP_APMAG(apsel)], Memi[DP_ASKIP(allstar)],  cdimen, nterm,
	        istar, lstar, ndisap, nredo, DP_VERBOSE(dao)))
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

	    if (dp_alclamp (dao, Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APERR(apsel)], Memr[DP_ARPIXSQ(allstar)],
	        Memi[DP_ASKIP(allstar)], Memr[DP_AX(allstar)],
	        Memr[DP_AC(allstar)], Memr[DP_AV(allstar)],
	        Memr[DP_AXOLD(allstar)], Memr[DP_AXCLAMP(allstar)],
	        Memr[DP_AYOLD(allstar)], Memr[DP_AYCLAMP(allstar)],
	        istar, lstar, niter, cdimen, clampmax, ro32k,
		peakerr, nconv)) {

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
		    wcrit, i, j, k)) {

		    call dp_alcentroid (Memr[DP_APXCEN(apsel)],
		        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)], i, j, k)

		    # Remove the k-th star from this group.
		    if (DP_VERBOSE(dao) == YES) {
		        call printf (
			"REJECTING: Star ID: %d has merged with star ID: %d\n")
			    call pargi (Memi[DP_APID(apsel)+k-1])
			    call pargi (Memi[DP_APID(apsel)+i-1])
		    }
		    Memr[DP_APMAG(apsel)+k-1] = INDEFR
		    Memi[DP_ASKIP(allstar)+k-1] = YES
		    ndisap = ndisap + 1
		    nredo = nredo - 1

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

	    # Now check to see if any of the stars is too faint (morer than
	    # 12.5 magnitudes fainter thanthe PSF star). If several stars
	    # are too faint, delete the faintest one, and set the 
	    # brightnesses of the other faint ones exactly to 12.5
	    # magnitudes below the PSF star. That way on the next iteration
	    # we will see whether these  stars want to grow or disappear.

	    call dp_alfaint (Memr[DP_APMAG(apsel)], Memi[DP_ASKIP(allstar)],
	        istar, lstar, faint, ifaint)

	    # If at least one star is more than 12.5 magnitudes fainter
	    # than the PSF, then IFAINT is the index of the faintest of them
	    # and faint is the relative brightness of the faintest of them.

	    if (faint < MIN_REL_BRIGHT) {

	        if (DP_VERBOSE(dao) == YES) {
		    call printf ("REJECTING: Star ID: %d is too faint\n")
		        call pargi (Memi[DP_APID(apsel)+ifaint-1])
	        }
	        Memr[DP_APMAG(apsel)+ifaint-1] = INDEFR
	        Memi[DP_ASKIP(allstar)+ifaint-1] = YES
	        ndisap = ndisap + 1
	        nredo = nredo - 1

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

	    } else if (niter >= 50) {

	        call dp_almfaint (Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		    Memi[DP_ASKIP(allstar)], istar, lstar, faint, ifaint)

	        if (faint > ecrit) {

	            if (DP_VERBOSE(dao) == YES) {
		        call printf ("REJECTING: Star ID: %d is too faint\n")
		            call pargi (Memi[DP_APID(apsel)+ifaint-1])
	            }
	            Memr[DP_APMAG(apsel)+ifaint-1] = INDEFR
	            Memi[DP_ASKIP(allstar)+ifaint-1] = YES
	            ndisap = ndisap + 1
	            nredo = nredo - 1

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


# DP_ALACCUM -- Accumulate the data into the matrix.

procedure dp_alaccum (dao, data, dnx, dny, dxoff, dyoff, subt, snx, sny,
	sxoff, syoff, weights, wnx, wny, wxoff, wyoff, nxpix, nypix, ixmin,
	iymin, mean_sky, istar, lstar, niter, sumres, grpwt, cdimen, nterm)

pointer	dao			# pointer to the daophot structure
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
real	sumres			# sum of the residuals
real	grpwt			# the group weight
int	cdimen			# maximum number of maxtrix dimensions
int	nterm			# number of terms in the matrix to fit

int	dix, diy, sxdiff, sydiff, wxdiff, wydiff
pointer	psffit, apsel, allstar
real	fitradsq, psfradsq, peakerr, ro32k, edge
real	fix, fiy, delta, sigmasq, resid, relerr, wt, dwt, rwt
bool	dp_alomit()

begin
	apsel = DP_APSEL(dao)
	psffit = DP_PSFFIT(dao)
	allstar = DP_ALLSTAR(dao)

	# These constants need to be stored more permanently in the
	# allstar structure at some point. They should all be defined
	# once and for all at task startup. Leave for next phase
	# of code cleanup.

	fitradsq = DP_FITRAD(dao) ** 2
	psfradsq = DP_PSFRAD(dao) ** 2
	peakerr = PEAK_ERR_NUMB / (DP_PSFSIGX(psffit) * DP_PSFSIGY(psffit)) ** 2
	ro32k = RO32K
	edge = DP_CLIPRANGE(dao) *
	    (EDGE_CONST) ** (1. / real (DP_CLIPEXP(dao))) 

	# Compute the array offsets.
	sxdiff = dxoff - sxoff
	sydiff = dyoff - syoff 
	wxdiff = dxoff - wxoff
	wydiff = dyoff - wyoff

	do diy = iymin - dyoff + 1, iymin - dyoff + nypix, 1 {

	    fiy = real (diy + dyoff - 1)

	    do dix = ixmin - dxoff + 1, ixmin - dxoff + nxpix, 1 {

		# Skip data with undefined or negative weights.
		if (IS_INDEFR(weights[dix+wxdiff,diy+wydiff]))
		    next
		if (weights[dix+wxdiff,diy+wydiff] <= 0.0) 
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
		    Memi[DP_ASKIP(allstar)], istar, lstar, psfradsq)

		# The expected random error in the pixel is the quadratic
		# sum of the Poisson statistics, plus the readout noise, 
		# plus an estimated error of 0.75% of the total brightness
		# for the difficulty of flat-fielding and bias-correcting 
		# the chip, plus an estimated error of some fraction of the
		# of the fourth derivative at the peak of the profile, to
		# account for the difficulty of accurately interpolating
		# within the PSF.

		delta = max (0.0, data[dix,diy] - subt[dix+sxdiff,diy+sydiff])

		# Delta is the [raw data - [raw data - sum of stellar profiles]]
		# which is equal to the sum of stellar profiles at this
		# point. This number is presumably non-negative.

		sigmasq = ro32k / weights[dix+wxdiff,diy+wydiff] + (peakerr *
		    delta) ** 2
		resid = subt[dix+sxdiff,diy+sydiff] - mean_sky
		relerr = abs (resid) / sqrt (sigmasq)

		# If this pixel has an error greater than EDGE times
		# sigma then reject it out of hand if the number of iterations
		# is greater than or equal to 4.

		if ((DP_CLIPEXP (dao) > 0) && (niter >= MIN_ITER) &&
		    (relerr > edge))
		    next

		wt = 0.0

		# Now include this pixel in the fitting equation for the
		# group.

		call dp_alxaccum (Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		    Memr[DP_ARPIXSQ(allstar)], Memi[DP_ASKIP(allstar)],
		    Memr[DP_AX(allstar)], Memr[DP_ANUMER1(allstar)],
		    Memr[DP_ANUMER2(allstar)], Memr[DP_ADENOM1(allstar)],
		    Memr[DP_ADENOM2(allstar)], istar, lstar, fix, fiy, resid,
		    wt, nterm, fitradsq, psffit, DP_VARPSF(dao))

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

		if ((niter >= MIN_ITER) && (DP_CLIPEXP (dao) > 0)) 
		    wt = wt / (1.0 + (relerr / DP_CLIPRANGE (dao)) **
			 DP_CLIPEXP (dao))
		rwt = resid * wt

		# Now work this pixel into the normal matrix
		call dp_alcaccum (Memr[DP_AX(allstar)], Memr[DP_AC(allstar)],
		    Memr[DP_AV(allstar)], Memi[DP_ASKIP(allstar)], cdimen,
		    nterm, istar, lstar, wt, rwt)

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
	    if (rpixsq[i] <= fitradsq)
		omit = false
	}

	return (omit)
end


# DP_ALSETSKIP -- Initialize the skip array.

procedure dp_alsetskip (rpixsq, skip, istar, lstar, psfradsq)

real	rpixsq[ARB]		# pixel distance squared
int	skip[ARB]		# skip array
int	istar			# first star
int	lstar			# last star
real	psfradsq		# psf radius squared

int	i

begin
	do i = istar, lstar {
	    if (rpixsq[i] < psfradsq)
		skip[i] = NO
	    else
		skip[i] = YES
	}
end


# DP_ALXACCUM - Accumulate the x vector.

procedure dp_alxaccum (xcen, ycen, mag, rpixsq, skip, x, numer1, numer2,
	denom1, denom2, istar, lstar, fix, fiy, resid, wt, nterm, fitradsq,
	psffit, varpsf)

real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of relative brightnesses
real	rpixsq[ARB]		# array of pixel distances squared
int	skip[ARB]		# the include / exclude array
real	x[ARB]			# the x vector to be accumulated
real	numer1[ARB]		# first numerator array
real	numer2[ARB]		# second numerator array
real	denom1[ARB]		# first denominator array
real	denom2[ARB]		# second denominator array
int	istar			# index of the first star in group
int	lstar			# index of the last star in group
real	fix			# the x position of the current pixel
real	fiy			# the y position of the current pixel
real	resid			# the input data residual
real	wt			# the output weight value
int	nterm			# number of terms in the matrix
real	fitradsq		# fitting radius squared
pointer	psffit			# pointer to  psf structure
int	varpsf			# variable psf ?

int	nstar, i, i3, k
real	deltax, deltay, psfsigsqx, psfsigsqy
real	dx, dy, dvdx, dvdy, value, radsq, rhosq
real	dp_evalpsf()

begin
	nstar = lstar - istar + 1
	psfsigsqx = DP_PSFSIGX(psffit)
	psfsigsqy = DP_PSFSIGY(psffit)

	do i = istar, lstar {

	    if (skip[i] == YES)
		next
		    
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
	    deltax = xcen[i] - DP_XPSF(psffit)
	    deltay = ycen[i] - DP_YPSF(psffit)
	    value = dp_evalpsf (dx, dy, psffit, deltax, deltay, varpsf,
	        dvdx, dvdy)

	    radsq = rpixsq[i] / fitradsq
	    if (radsq < 1.0) 
	        wt = max (wt, 5.0 / (5.0 + radsq / (1.0 - radsq)))
		    
	    if (nterm > nstar) {
		i3 = (i - istar + 1) * 3
		k = i3 - 2
		x[k] = -value
		k = i3 - 1
		x[k] = mag[i] * dvdx
		x[i3] = mag[i] * dvdy
	    } else {
		k = i - istar + 1
		x[k] = -value
	    }

	    rhosq =  (dx / psfsigsqx) ** 2 + (dy / psfsigsqy) ** 2
	    denom1[i] = denom1[i] + value ** 2
	    denom2[i] = denom2[i] + (rhosq * value) ** 2
	    numer1[i] = numer1[i] + rhosq * value ** 2
	    numer2[i] = numer2[i] + rhosq * value * resid
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

procedure dp_alcaccum (x, c, v, skip, cdimen, nterm, istar, lstar, wt, rwt)


real	x[ARB]			# x array
real	c[cdimen,ARB]		# maxtrix array
real	v[ARB]			# v vector
int	skip[ARB]		# include / exclude array
int	cdimen			# maximum size of c matrix
int	nterm			# number of terms to be fit
int	istar			# index of the first star
int	lstar			# index of the last star 
real	wt			# input weight
real	rwt			# input residual weight

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
		    v[k] = v[k] + x[k] * rwt
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
		v[k] = v[k] + x[k] * rwt
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

bool procedure dp_alredo (c, id, mag, chi, sumwt, rpixsq, npix, skip, cdimen,
	istar, lstar, niter, ndisap, nredo, chigrp, center, verbose)

real	c[cdimen,ARB]		# the matrix
int	id[ARB]			# the array of star ids
real	mag[ARB]		# the array of relative brightnesses
real	chi[ARB]		# the array of chi values
real	sumwt[ARB]		# the array of weight values
real	rpixsq[ARB]		# the array of pixel distance squared values
int	npix[ARB]		# the array of pixel counts
int	skip[ARB]		# the include / exclude array
int	cdimen			# maximum size of the c matrix
int	istar			# index of the first star
int	lstar			# index of the last star
int	niter			# the current iteration
int	ndisap			# number of stars which have disappeared
int	nredo			# number of stars to redo
real	chigrp			# chi value of the group
int	center			# recenter the values ?
int	verbose			# verbose mode ?

bool	redo
int	i, i3

begin
	redo = false

	do i = istar, lstar {
	    if (center == YES && (niter >= 2)) {
		if (npix[i] < MIN_NPIX) {
		    redo = true
		    skip[i] = YES
		    ndisap = ndisap + 1
		    nredo = nredo - 1
		    if (verbose == YES) {
			call printf (
			    "REJECTING: Star ID: %d has too few good pixels\n")
			call pargi (id[i])
		    }
		    mag[i] = INDEFR
		} else {
		    skip[i] = NO
		    i3 = (i - istar + 1) * 3 - 2
		    rpixsq[i] = c[i3,i3]
		    if (sumwt[i] > MIN_SUMWT) {
			chi[i] = CHI_NORM * chi[i] / sqrt (sumwt[i] *
			    (sumwt[i] - MIN_SUMWT))
			chi[i] = ((npix[i] - MIN_SUMWT) * chi[i] + MIN_SUMWT *
			    chigrp) / real (npix[i])
		    } else
			chi[i] = chigrp
		}
	    } else {
		if (npix[i] < 1) {
		    redo = true
		    skip[i] = YES
		    ndisap = ndisap + 1
		    nredo = nredo - 1
		    if (verbose == YES) {
			call printf (
			    "REJECTING: Star ID: %d has too few good pixels\n")
			call pargi (id[i])
		    }
		    mag[i] = INDEFR
		} else {
		    skip[i] = NO
		    i3 = i - istar + 1
		    rpixsq[i] = c[i3,i3]
		    if (sumwt[i] > 1.0) {
			chi[i] = CHI_NORM * chi[i] / sqrt (sumwt[i] *
			    (sumwt[i] - 1.0))
			chi[i] = ((npix[i] - 1.0) * chi[i] + chigrp) /
			    real (npix[i])
		    } else
			chi[i] = chigrp
		}
	    }
	}

	return (redo)
end


# DP_CHECKC - Check the c matrix for valid diagonal elements.

bool procedure  dp_checkc (c, id, mag, skip, cdimen, nterm, istar, lstar,
	ndisap, nredo, verbose)

real	c[cdimen,ARB]		# the c matrix
int	id[ARB]			# the array of ids
real	mag[ARB]		# the array of relative brightnesses
int	skip[ARB]		# the include / exclude array
int	cdimen			# maximum size of the c matrix
int	nterm			# number of terms to be fit
int	istar			# index of the first star
int	lstar			# index of the last star
int	ndisap			# number of lost stars
int	nredo			# number to redo
int	verbose			# verbose mode

bool	redo
int	nstar, j, i

begin
	redo = false
	nstar = lstar - istar + 1

	do j = 1, nterm {
	    if (c[j,j] > 0.0)
		next
	    do i = istar, lstar  {
		skip[i] = YES
		if (verbose == YES) {
		    call printf (
		    "REJECTING: Star ID: %d generated a fittting error\n")
			call pargi (id[i])
		}
		mag[i] = INDEFR
	    }
	    ndisap = ndisap + nstar
	    nredo = nredo - nstar
	    redo = true
	    break
	}

	return (redo)
end


# DP_ALCLAMP -- Get the answers.

bool procedure dp_alclamp (dao, id, xcen, ycen, mag, magerr, rpixsq,
	skip, x, c, v, xold, xclamp, yold, yclamp, istar, lstar, niter,
	cdimen, clampmax, ro32k, peakerr, nconv)

pointer	dao			# pointer to the daophot structure
int	id[ARB]			# array of star ids
real	xcen[ARB]		# array of star x centers
real	ycen[ARB]		# array of star y centers
real	mag[ARB]		# array of relative brightnesses
real	magerr[ARB]		# array of relative brightness errors
real	rpixsq[ARB]		# array of pixel distance squared values
int 	skip[ARB]		# include / exclude array
real	x[ARB]			# x vector
real	c[cdimen,ARB]		# c matrix
real	v[ARB]			# v vector
real	xold[ARB]		# xold array
real	xclamp[ARB]		# xclamp array
real	yold[ARB]		# yold array
real	yclamp[ARB]		# yclamp array
int	istar			# the index of the first star
int	lstar			# the index of the last star
int	niter			# the current iteration
int	cdimen			# the maximum size of c matrix 
real	clampmax		# the maximum clamp value
real	ro32k			# the weight factor
real	peakerr			# the peak error
int	nconv			# the number of fitted stars

bool	redo, bufwrite
int	i, l, k, j, lx, mx, ly, my, nx, ny
pointer	psffit, allstar
real	dwt, psfrad, psfradsq

begin
	allstar = DP_ALLSTAR(dao)
	psffit = DP_PSFFIT(dao)
	psfrad = DP_PSFRAD(dao)
	psfradsq = psfrad * psfrad
	bufwrite = false

	do i = istar, lstar {

	    if (niter >= MIN_ITER)
	        redo = false
	    else
		redo = true

	    if ((DP_RECENTER(dao) == YES)  && (niter >= 2)) {

		l = 3 * (i - istar + 1)
		k = l - 1
		j = l - 2
		dwt = max (MIN_NEW_ERRMAG * sqrt (c[j,j]), MIN_NEW_RELBRIGHT *
		    mag[i])
		if (abs (v[j] / rpixsq[i]) > dwt)
		    redo = true
		
		# Once we know that the solution hasn't converged, don't
		# bother to keep checking.

		if (! redo) {
		    if (abs (x[j]) > dwt)
			redo = true
		    else if (abs (x[k]) > max (MAX_PIX_FRACTION *
		        sqrt (c[k,k]), MAX_PIXERR))
			redo = true
		    else if (abs (x[l]) > max (MAX_PIX_FRACTION *
		        sqrt (c[l,l]), MAX_PIXERR))
			redo = true
		}

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
		else if (dwt > 0.0)
		    xclamp[i] = min (clampmax, MAX_XYCLAMP_FRACTION * xclamp[i])
		xcen[i] = xcen[i] - x[k] / (1.0 + abs (x[k] / xclamp[i]))
		xold[i] = x[k]
		
		dwt = yold[i] * x[l]
		if (dwt < 0.0)
		    yclamp[i] = max (MIN_XYCLAMP, MIN_XYCLAMP_FRACTION *
		        yclamp[i])
		else if (dwt > 0.0)
		    yclamp[i] = min (clampmax, MAX_XYCLAMP_FRACTION * yclamp[i])
		ycen[i] = ycen[i] - x[l] / (1.0 + abs (x[l] / yclamp[i]))
		yold[i] = x[l]

		mag[i] = mag[i] - x[j] / (1.0 + max (x[j] / (MAX_DELTA_FAINTER *
		    mag[i]), -x[j] / (MAX_DELTA_BRIGHTER * mag[i])))

	    } else {
		j = i - istar + 1
		dwt = max (MAX_NEW_ERRMAG * sqrt (c[j,j]), MAX_NEW_RELBRIGHT *
		    mag[i])
		if (abs (v[j] / rpixsq[i]) > dwt) 
		    redo = true
		else if (abs (x[j]) > dwt)
		    redo = true
		mag[i] = mag[i] - x[j] / (1. + MAX_XYCLAMP_FRACTION *
		    abs (x[j] / mag[i]))
	    }

	    magerr[i] = c[j,j]

	    # If this star converged, write out the results for it,
	    # flag it for deletion from the star list and subtract it
	    # from the reference copy of the image.

	    if (niter >= DP_MAXITER(dao))
		redo = false

	    if (! redo || (niter >= DP_MAXITER (dao))) {

		call dp_glim (xcen[i], ycen[i], psfrad, DP_DLX(allstar),
		    DP_DMX(allstar), DP_DLY(allstar), DP_DMY(allstar),
		    lx, mx, ly, my)
		nx = mx - lx + 1
		ny = my - ly + 1

		call dp_swstar (Memr[DP_DBUF(allstar)], DP_DNX(allstar),
		    DP_DNY(allstar), DP_DXOFF(allstar), DP_DYOFF(allstar),
		    Memr[DP_WBUF(allstar)], DP_WNX(allstar), DP_WNY(allstar),
		    DP_WXOFF(allstar), DP_WYOFF(allstar), xcen[i], ycen[i],
		    mag[i], DP_VARPSF(dao), lx, ly, nx, ny, psfradsq, ro32k,
		    peakerr, psffit)

		skip[i] = YES
		nconv = nconv + 1

	        if (DP_VERBOSE(dao) == YES) {
		    call printf (
		 "FITTING:   ID: %5d  XCEN: %8.2f  YCEN: %8.2f  MAG: %8.2f\n")
		        call pargi (id[i])
		        call pargr (xcen[i])
		        call pargr (ycen[i])
			if (mag[i] <= 0.0)
		            call pargr (INDEFR)
			else
			    call pargr (DP_PSFMAG(psffit) - 2.5 *
			        log10 (mag[i]))

	        }
		bufwrite = true
	    }
	}

	return (bufwrite)
end


# DP_SWSTAR -- Subtract the fitted star for the data and adjust the
# weights.

procedure dp_swstar (data, dnx, dny, dxoff, dyoff, weights, wnx, wny,
	wxoff, wyoff, xcen, ycen, mag, varpsf, lx, ly, nx, ny, psfradsq,
	ro32k, peakerr, psffit)

real	data[dnx,dny]			# the data array
int	dnx, dny			# dimensions of the data array
int	dxoff, dyoff			# lower left corner of data array
real	weights[wnx,wny]		# the weights array
int	wnx, wny			# dimensions of the weights array
int	wxoff, wyoff			# lower left corner of weights array
real	xcen, ycen			# the position of the star
real	mag				# relative brightness of the star
int	varpsf				# variable psf ?
int	lx, ly				# lower left corner region of interest
int	nx, ny				# size of region of interest
real	psfradsq			# the psf radius squared
real	ro32k				# the weighting constant
real	peakerr				# the peak error factor
pointer	psffit				# pointer to the psf structure

int	di, dj, wxdiff, wydiff
real	deltax, deltay, dx, dy, dysq, diff, dvdx, dvdy, wt
real	dp_evalpsf()

begin
	wxdiff = dxoff - wxoff
	wydiff = dyoff - wyoff
	deltax = xcen - DP_XPSF(psffit)
	deltay = ycen - DP_YPSF(psffit)

	do dj = ly - dyoff + 1, ly - dyoff + ny {
	    dy = (dj + dyoff - 1) - ycen
	    dysq = dy * dy
	    do di = lx - dxoff + 1, lx - dxoff + nx {
		if (IS_INDEFR(weights[di+wxdiff,dj+wydiff]))
		    next
	        dx = (di + dxoff - 1) - xcen
		if ((dx * dx + dysq) > psfradsq)
		    next
		diff = mag * dp_evalpsf (dx, dy, psffit, deltax, deltay,
			varpsf, dvdx, dvdy)
		data[di,dj] = data[di,dj] - diff
		if ((diff > 0.0) && (weights[di+wxdiff,dj+wydiff] != 0.0)) {
		    wt = ro32k / abs (weights[di+wxdiff,dj+wydiff]) +
			(peakerr * diff) ** 2
		    if (weights[di+wxdiff,dj+wydiff] > 0.0)
			weights[di+wxdiff,dj+wydiff] = ro32k / wt
		    else
			weights[di+wxdiff,dj+wydiff] = - ro32k / wt
		}
	    }
	}
end


# DP_ALMERGE -- Determine whether or not two stars should merge.

bool procedure dp_almerge (xcen, ycen, mag, magerr, skip, istar, lstar, 
	sepcrit, sepmin, wcrit, i, j, k)

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
int	i, j, k			# indices

real	sep

begin
	do i = istar + 1, lstar {

	    if (skip[i] == YES)
		next

	    do j = istar, i - 1 {
		if (skip[j] == YES)
		    next
		sep = (xcen[i] - xcen[j]) ** 2 + (ycen[i] - ycen[j]) ** 2
		if (sep > sepcrit)
		    next
		    
		# Two stars are overlapping. Identify the fainter of the two.
		k= j 
		if (mag[i] < mag[j])
		    k = i
		if ((sep < sepmin) || (magerr[k] / mag[k] ** 2 > wcrit)) 
		    return (true)
	    }
	}

	return (false)
end


# DP_ALCENTROID -- Merge two stars by adding their relative brightnesses
# and replacing their individual positions with the relative brightness
# weighted mean position.

procedure dp_alcentroid (xcen, ycen, mag, i, j, k)

real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of magnitudes
int	i, j, k			# input indices

begin
	if (mag[i] < mag[j]) 
	    i = j
	xcen[i] = xcen[i] * mag[i] + xcen[k] * mag[k]
	ycen[i] = ycen[i] * mag[i] + ycen[k] * mag[k]
	mag[i] = mag[i] + mag[k]
	xcen[i] = xcen[i] / mag[i]
	ycen[i] = ycen[i] / mag[i]
end


# DP_ALFAINT -- Find all the stars with realtive brightnesss less than
# a given number and set their magnitudes to a given minimum relative
# brightness.

procedure dp_alfaint (mag, skip, istar, lstar, faint, ifaint)

real	mag[ARB]		# array of magnitudes
int	skip[ARB]		# skip array
int	istar, lstar		# first and last stars
real	faint			# faintest star
int	ifaint			# index of faintest star

int	i

begin
	faint = INIT_FAINT_FACTOR
	ifaint = 0
	do i = istar, lstar {
	    if ((skip[i] == YES) || (mag[i] >= MIN_REL_BRIGHT)) 
		next
	    if (mag[i] <= faint) {
		faint = mag[i] 
		ifaint = i
	    }
	    mag[i] = MIN_REL_BRIGHT
	}
end


# DP_ALMFAINT -- Find the star with the greatest error in its relative
# brightness.

procedure dp_almfaint (mag, magerr, skip, istar, lstar, faint, ifaint)

real	mag[ARB]		# array of relative brightnesses
real	magerr[ARB]		# array of relative brightness errors
int 	skip[ARB]		# array of include / exclude values
int	istar			# index of first star
int	lstar			# index of last star
real	faint			# computed faintness limit
int	ifaint			# index of faintest star

int	i
real	wt

begin
	faint = 0.0
	ifaint = 0
	do i = istar, lstar {
	    if (skip[i] == YES)
		next
	    wt = magerr[i] / mag[i] ** 2
	    if (wt > faint) {
		faint = wt
		ifaint = i
	    }
	}
end
