include <mach.h>
include <imhdr.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include	"../lib/allstardef.h"


# DP_ASTAR -- Begin doing the photometry.

procedure dp_astar (dao, im, subim, allfd, rejfd, cache, savesub, version)

pointer	dao		# pointer to the daophot structure
pointer	im		# pointer to the input image
pointer	subim		# pointer to the subtracted image
int	allfd		# file descriptor for the output photometry file
int	rejfd		# file descriptor for rejections files
int	cache 		# cache the data ?
int	savesub		# save the subtracted image ?
int	version		# version number

bool	clip
int	nstar, row1_number, row2_number, niter, itsky, x1, x2, y1, y2, istar
int	lstar, ldummy, newsky
pointer	apsel, psffit, allstar, sp, indices, colpoints
real	fradius, sepcrt, sepmin, clmpmx, wcrit, radius, xmin, xmax, ymin, ymax
real	csharp, rdummy

int	dp_alphot(), dp_alnstar()
pointer	dp_gst(), dp_gwt(), dp_gdc()
real	dp_usepsf()

begin
	# Define some daophot structure pointers.
	apsel = DP_APSEL(dao)
	psffit = DP_PSFFIT (dao)
	allstar = DP_ALLSTAR (dao)

	# Store the original fitting radius
	fradius = DP_FITRAD(dao)
	DP_FITRAD(dao) = min (DP_FITRAD(dao), DP_PSFRAD(dao))
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)

	# Get some working memory.
	call smark (sp)
	call salloc (indices, NAPPAR, TY_INT)
	call salloc (colpoints, ALL_NOUTCOLUMN, TY_INT)

	# Define some daophot constants.
	nstar = DP_APNUM(apsel)
	csharp = 1.4427 * Memr[DP_PSFPARS(psffit)] *
	    Memr[DP_PSFPARS(psffit)+1] / dp_usepsf (DP_PSFUNCTION(psffit), 0.0,
	    0.0, DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
	    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit), DP_NVLTABLE(psffit),
	    DP_NFEXTABLE(psffit), 0.0, 0.0, rdummy, rdummy)
	if (IS_INDEFR(DP_MERGERAD(dao)))
	    sepcrt = 2.0 * (Memr[DP_PSFPARS(psffit)] ** 2 +
	        Memr[DP_PSFPARS(psffit)+1] ** 2)
	else
	    sepcrt = DP_MERGERAD(dao) ** 2
	sepmin = min (1.0, FRACTION_MINSEP * sepcrt) 
	itsky = 3

	# Initialize the output photometry file for writing.
	row1_number = 0
	row2_number = 0
	if (DP_TEXT(dao) == YES) {
	    call dp_xnewal (dao, allfd)
	    if (rejfd != NULL)
	        call dp_xnewal (dao, rejfd)
	} else {
	    call dp_tnewal (dao, allfd, Memi[colpoints])
	    if (rejfd != NULL)
	        call dp_tnewal (dao, rejfd, Memi[colpoints])
	}

	# Allocate memory for the input, output and fitting arrays.
	call dp_gnindices (Memi[indices])
	call dp_rmemapsel (dao, Memi[indices], NAPPAR, nstar)
	call dp_almemstar (dao, nstar, DP_MAXGROUP(dao))
	call dp_cache (dao, im, subim, cache)

	# Read in the input data and assign the initial weights.
	call dp_setwt (dao, im)

	# Convert the magnitudes to relative brightnesses.
	call dp_alzero (dao, Memr[DP_APMAG(apsel)], nstar)

	# Initialize all the fit/nofit indices and the error codes.
	call amovki (NO, Memi[DP_ASKIP(allstar)], nstar)
	call amovki (ALLERR_OK, Memi[DP_AIER(allstar)], nstar)

	# Remove stars that have INDEF centers, are off the image altogether,
	# or are too close to another star before beginning to fit.
	call dp_strip (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	    Memr[DP_APMSKY(apsel)], Memi[DP_ASKIP(allstar)],
	    Memi[DP_AIER(allstar)], nstar, sepmin, int(IM_LEN(im,1)),
	    int(IM_LEN(im,2)), DP_FITRAD(dao), DP_VERBOSE(dao))

	# Write out results for the rejected stars.
	call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+nstar],
	    Memr[DP_APYCEN(apsel)+nstar], Memr[DP_APXCEN(apsel)+nstar],
	    Memr[DP_APYCEN(apsel)+nstar], DP_APNUM(apsel) - nstar)
	if (DP_TEXT(dao) == YES) {
	    call dp_xalwrite (allfd, rejfd, Memi[DP_APID(apsel)],
		Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		Memr[DP_APMSKY(apsel)], Memr[DP_APCHI(apsel)],
		Memr[DP_ANUMER(allstar)], Memr[DP_ADENOM(allstar)],
		Memi[DP_ASKIP(allstar)], Memi[DP_AIER(allstar)], niter,
		nstar + 1, DP_APNUM(apsel), DP_PSFMAG(psffit), csharp)
	} else {
	    call dp_talwrite (allfd, rejfd, Memi[colpoints],
	        Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APERR(apsel)], Memr[DP_APMSKY(apsel)],
	        Memr[DP_APCHI(apsel)], Memr[DP_ANUMER(allstar)],
	        Memr[DP_ADENOM(allstar)], Memi[DP_ASKIP(allstar)],
		Memi[DP_AIER(allstar)], niter, nstar + 1, DP_APNUM(apsel),
		row1_number, row2_number, DP_PSFMAG(psffit), csharp)
	}

	# Do some initialization for the fit.
	clmpmx = CLAMP_FRACTION * DP_FITRAD(dao)
	call aclrr (Memr[DP_AXOLD(allstar)], nstar)
	call aclrr (Memr[DP_AYOLD(allstar)], nstar)
	call amovkr (clmpmx, Memr[DP_AXCLAMP(allstar)], nstar)
	call amovkr (clmpmx, Memr[DP_AYCLAMP(allstar)], nstar)
	call amovkr (1.0, Memr[DP_ASUMWT(allstar)], nstar)

	# Begin iterating.
	for (niter = 1; (nstar > 0) && (niter <= DP_MAXITER (dao));
	    niter = niter + 1) {

	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("NITER = %d\n")
		    call pargi (niter)
	    }

	    if ((DP_CLIPEXP(dao) != 0) && (niter >= 4))
		clip = true
	    else
		clip = false

	    # Define the critical errors.
	    if (niter >= 15)
	        wcrit = WCRIT15
	    else if (niter >= 10)
	        wcrit = WCRIT10
	    else if (niter >= 5)
	        wcrit = WCRIT5
	    else
	        wcrit = WCRIT0 

	    # Sort the data on y.
	    call dp_alsort (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)], 
		Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)], 
		Memr[DP_APMSKY(apsel)], Memr[DP_ASUMWT(allstar)],
		Memr[DP_AXOLD(allstar)], Memr[DP_AYOLD(allstar)],
		Memr[DP_AXCLAMP(allstar)], Memr[DP_AYCLAMP(allstar)], nstar)

	    # Compute the working radius. This is either the fitting radius
	    # or the outer sky radius if this is an iteration during which
	    # sky is to be determined whichever is larger.
	    if ((DP_FITSKY(dao) == YES) && (mod (niter, itsky) == 0)) {
		newsky = YES
	        if ((DP_ANNULUS(dao) + DP_DANNULUS(dao)) > DP_FITRAD(dao))
		    radius = DP_ANNULUS(dao) + DP_DANNULUS(dao)
		else
		    radius = DP_FITRAD(dao)
	    } else {
		newsky = NO
		radius = DP_FITRAD(dao)
	    }

	    # Define region of the image used to fit the remaining stars.
	    call alimr (Memr[DP_APXCEN(apsel)], nstar, xmin, xmax)
	    call alimr (Memr[DP_APYCEN(apsel)], nstar, ymin, ymax)
	    x1 = max (1, min (IM_LEN(im,1), int (xmin-radius)+1))
	    x2 = max (1, min (IM_LEN(im,1), int (xmax+radius)))
	    y1 = max (1, min (IM_LEN(im,2), int (ymin-radius)+1))
	    y2 = max (1, min (IM_LEN (im,2), int (ymax+radius)))

	    # Reinitialize the weight and scratch arrays / images .
	    call dp_wstinit (dao, im, Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		nstar, radius, x1, x2, y1, y2)

	    # Recompute the initial sky estimates if that switch is enabled.
	    if (newsky == YES)
	        call dp_alsky (dao, im, Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMSKY(apsel)], nstar,
		    x1, x2, y1, y2, DP_ANNULUS(dao), DP_ANNULUS(dao) +
		    DP_DANNULUS(dao), 100, -MAX_REAL)

	    # Group the remaining stars.
	    call dp_regroup (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)], 
		Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)], 
		Memr[DP_APMSKY(apsel)], Memr[DP_ASUMWT(allstar)],
		Memr[DP_AXOLD(allstar)], Memr[DP_AYOLD(allstar)],
		Memr[DP_AXCLAMP(allstar)], Memr[DP_AYCLAMP(allstar)], nstar,
		DP_FITRAD(dao), Memi[DP_ALAST(allstar)])

	    # Reset the error codes.
	    call amovki (ALLERR_OK, Memi[DP_AIER(allstar)], nstar)

	    # Do the serious fitting one group at a time.
	    for (istar = 1; istar <= nstar; istar = lstar + 1) {

		# Do the fitting a group at a time.
	        lstar = dp_alphot (dao, im, nstar, istar, niter, sepcrt,
		    sepmin, wcrit, clip, clmpmx, version) 

		# Write the results.
	        if (DP_TEXT(dao) == YES) {
		    call dp_xalwrite (allfd, rejfd, Memi[DP_APID(apsel)],
		        Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		        Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		        Memr[DP_APMSKY(apsel)], Memr[DP_APCHI(apsel)],
		        Memr[DP_ANUMER(allstar)], Memr[DP_ADENOM(allstar)],
		        Memi[DP_ASKIP(allstar)], Memi[DP_AIER(allstar)],
			niter, istar, lstar, DP_PSFMAG(psffit), csharp)
	        } else {
		    call dp_talwrite (allfd, rejfd, Memi[colpoints],
		    Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		    Memr[DP_APERR(apsel)], Memr[DP_APMSKY(apsel)],
		    Memr[DP_APCHI(apsel)], Memr[DP_ANUMER(allstar)],
		    Memr[DP_ADENOM(allstar)], Memi[DP_ASKIP(allstar)],
		    Memi[DP_AIER(allstar)], niter, istar, lstar, row1_number,
		    row2_number, DP_PSFMAG(psffit), csharp)
		}

	    }

	    # Find the last star in the list that still needs more work.
	    nstar = dp_alnstar (Memi[DP_ASKIP(allstar)], nstar)

	    # Remove the fitted stars from the list.
	    for (istar = 1;  (istar < nstar) && nstar > 0; istar = istar + 1) {
		call dp_alswitch (Memi[DP_APID(apsel)],
		    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		    Memr[DP_APMSKY(apsel)], Memr[DP_ASUMWT(allstar)],
		    Memr[DP_AXOLD(allstar)], Memr[DP_AYOLD(allstar)],
		    Memr[DP_AXCLAMP(allstar)], Memr[DP_AYCLAMP(allstar)],
		    Memi[DP_ASKIP(allstar)], istar, nstar)
	    }

	    # Flush the output buffers.
	    if (dp_gwt (dao, im, ldummy, ldummy, READ_WRITE, YES) == NULL)
		;
	    if (dp_gst (dao, im, ldummy, ldummy, READ_ONLY, YES) == NULL)
		;
	    if (dp_gdc (dao, im, ldummy, ldummy, READ_WRITE, YES) == NULL)
		;
	}

	# Release cached memory.
	call dp_uncache (dao, subim, savesub)

	call sfree (sp)

	# Restore the original fitting radius
	DP_FITRAD(dao) = fradius
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)
end


# DP_ALNSTAR -- Compute the number of stars left

int procedure dp_alnstar (skip, nstar)

int	skip[ARB]		# skip array
int	nstar			# number of stars

begin
	while (nstar > 0) {
	    if (skip[nstar] == NO)
		break
	    nstar = nstar - 1
	}

	return (nstar)
end


# DP_ALSWITCH -- Switch array elements.

procedure dp_alswitch (id, xcen, ycen, mag, magerr, sky, chi, xold, yold,
	xclamp, yclamp, skip, istar, nstar)

int	id[ARB]			# array of ids
real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of magnitudes
real	magerr[ARB]		# array of magnitude errors
real	sky[ARB]		# array of sky values
real	chi[ARB]		# array of chi values
real	xold[ARB]		# old x array
real	yold[ARB]		# old y array
real	xclamp[ARB]		# array of x clamps
real	yclamp[ARB]		# array of y clamps
int	skip[ARB]		# skip array
int	istar			# next star
int	nstar			# total number of stars

int	dp_alnstar()

begin
	if (skip[istar] == YES) {
	    id[istar] = id[nstar]
	    xcen[istar] = xcen[nstar]
	    ycen[istar] = ycen[nstar]
	    mag[istar] = mag[nstar]
	    magerr[istar] = magerr[nstar]
	    sky[istar] = sky[nstar]
	    chi[istar] = chi[nstar]
	    xold[istar] = xold[nstar]
	    yold[istar] = yold[nstar]
	    xclamp[istar] = xclamp[nstar]
	    yclamp[istar] = yclamp[nstar]
	    skip[istar] = NO
	    nstar = nstar - 1
	    nstar = dp_alnstar (skip, nstar)
	}
end
