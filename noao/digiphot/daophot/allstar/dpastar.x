include <imhdr.h>
include "../lib/daophotdef.h"
include "../lib/apsel.h"
include	"../lib/allstardef.h"

# Limit on N/S ** 2 for two stars to merge.

define	WCRIT0		16.0
define	WCRIT4		1.0
define	WCRIT8		0.4
define	WCRIT12		0.16

# Limit on N/S ** 2 for a star to be fit at high iterations

define	ECRIT50		1.0
define	ECRIT100	0.25
define	ECRIT150	0.1111111

define	NCOLUMN		9

# DP_ASTAR -- Begin doing the photometry.

procedure dp_astar (dao, im, subim, allfd, cache, savesub)

pointer	dao		# pointer to the daophot structure
pointer	im		# pointer to the input image
pointer	subim		# pointer to the subtracted image
int	allfd		# file descriptor for the output photometry file
int	cache 		# cache the data ?
int	savesub		# save the subtracted image ?

int	nstar, niter, nconv, ndisap, istar, lstar, row_number
int	x1, x2, y1, y2, ldummy
pointer	sp, indices, colpoints, psffit, apsel, allstar
real	xmin, xmax, ymin, ymax, wcrit, ecrit, chigrp, fitradius, sepmin
int	dp_alphot(), dp_alnstar()
pointer	dp_gst(), dp_gwt(), dp_gdc()

begin
	# Get some working memory.
	call smark (sp)
	call salloc (indices, NAPPAR, TY_INT)
	call salloc (colpoints, NCOLUMN, TY_INT)

	# Define some daophot pointers.
	psffit = DP_PSFFIT (dao)
	apsel = DP_APSEL(dao)
	allstar = DP_ALLSTAR (dao)

	# Define some daophot constants.
	fitradius = DP_FITRAD(dao)
	nstar = DP_APNUM(apsel)
	sepmin = sqrt (min (1.0, 0.14 * 2.773 * (DP_PSFSIGX(psffit) ** 2 +
	    DP_PSFSIGY(psffit) ** 2)))

	# Initialize the output photometry file for writing.
	if (DP_TEXT(dao) == YES)
	    call dp_xnewal (dao, allfd)
	else
	    call dp_tnewal (dao, allfd, Memi[colpoints])
	row_number = 0

	# Allocate memory for the input, output and fitting arrays.
	call dp_gnindices (Memi[indices])
	call dp_rmemapsel (dao, Memi[indices], NAPPAR, nstar)
	call dp_almemstar (dao, nstar, DP_MAXGROUP(dao))
	call dp_cache (dao, im, subim, cache)

	# Read in the input data and assign the initial weights.
	call dp_setwt (dao, im)

	# Convert the magnitudes to relative brightnesses.
	call dp_alzero (dao, Memr[DP_APMAG(apsel)], nstar)

	# Remove stars that have INDEF centers, are off the image altogether,
	# or are too close to another star before beginning to fit.
	call dp_strip (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	    Memr[DP_APMSKY(apsel)], Memi[DP_ASKIP(allstar)], nstar, sepmin,
	    int(IM_LEN(im,1)), int(IM_LEN(im,2)), DP_FITRAD(dao),
	    DP_VERBOSE(dao))

	# Write out results for the rejected stars.
	if (DP_TEXT(dao) == YES) {
	    call dp_xalwrite (allfd, Memi[DP_APID(apsel)],
		Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		Memr[DP_APMSKY(apsel)], Memr[DP_APCHI(apsel)],
		Memr[DP_ANUMER1(allstar)], Memr[DP_ANUMER2(allstar)],
		Memr[DP_ADENOM1(allstar)], Memr[DP_ADENOM2(allstar)],
		Memi[DP_ASKIP(allstar)], niter, nstar + 1, DP_APNUM(apsel),
		DP_PSFMAG(psffit), chigrp)
	} else {
	    call dp_talwrite (allfd, Memi[colpoints],
	        Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	        Memr[DP_APERR(apsel)], Memr[DP_APMSKY(apsel)],
	        Memr[DP_APCHI(apsel)], Memr[DP_ANUMER1(allstar)],
	        Memr[DP_ANUMER2(allstar)], Memr[DP_ADENOM1(allstar)],
	        Memr[DP_ADENOM2(allstar)], Memi[DP_ASKIP(allstar)], niter,
	        nstar + 1, DP_APNUM(apsel), row_number, DP_PSFMAG(psffit),
		chigrp)
	}

	# Do some initialization for the fit.
	nconv = 0
	ndisap = DP_APNUM(dao) - nstar
	call aclrr (Memr[DP_AXOLD(allstar)], nstar)
	call aclrr (Memr[DP_AYOLD(allstar)], nstar)
	call amovkr (0.25 * fitradius, Memr[DP_AXCLAMP(allstar)], nstar)
	call amovkr (0.25 * fitradius, Memr[DP_AYCLAMP(allstar)], nstar)

	# Begin iterating.
	for (niter = 1; (nstar > 0) && (niter <= DP_MAXITER (dao));
	    niter = niter + 1) {

	    # Group the remaining stars.
	    call dp_regroup (Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)], 
		Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)], 
		Memr[DP_APMSKY(apsel)], Memr[DP_AXOLD(allstar)],
		Memr[DP_AYOLD(allstar)], Memr[DP_AXCLAMP(allstar)],
		Memr[DP_AYCLAMP(allstar)], nstar, fitradius,
		Memi[DP_ALAST(allstar)])

	    # Define region of the image used to fit the remaining stars.
	    call alimr (Memr[DP_APXCEN(apsel)], nstar, xmin, xmax)
	    call alimr (Memr[DP_APYCEN(apsel)], nstar, ymin, ymax)
	    x1 = max (1, min (IM_LEN(im,1), int (xmin-fitradius)+1))
	    x2 = max (1, min (IM_LEN(im,1), int (xmax+fitradius)))
	    y1 = max (1, min (IM_LEN(im,2), int (ymin-fitradius)+1))
	    y2 = max (1, min (IM_LEN (im,2), int (ymax+fitradius)))

	    # Reinitialize the weight array for all the remaining stars.
	    call dp_wtinit (dao, im, Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memi[DP_ALAST(allstar)], nstar, x1,
		x2, y1, y2)

	    # Initialize the scratch array.
	    call dp_stinit (dao, im, Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		Memi[DP_ALAST(allstar)], nstar, x1, x2, y1, y2)

	    # Define the critical errors.
	    if (niter >= 12)
	        wcrit = WCRIT12
	    else if (niter >= 8)
	        wcrit = WCRIT8
	    else if (niter >= 4)
	        wcrit = WCRIT4
	    else
	        wcrit = WCRIT0 / niter ** 2

	    if (niter >= 15)
	        ecrit = ECRIT150
	    else if (niter >= 10)
	        ecrit = ECRIT100
	    else if (niter >= 5)
	        ecrit = ECRIT50

	    # Do the serious fitting one group at a time.
	    for (istar = 1; istar <= nstar; istar = lstar + 1) {

		# Do the fitting a group at a time.
	        lstar = dp_alphot (dao, im, nstar, istar, nconv, ndisap, niter,
		    wcrit, ecrit, chigrp) 

		# Write the results.
	        if (DP_TEXT(dao) == YES) {
		    call dp_xalwrite (allfd, Memi[DP_APID(apsel)],
		        Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		        Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		        Memr[DP_APMSKY(apsel)], Memr[DP_APCHI(apsel)],
		        Memr[DP_ANUMER1(allstar)], Memr[DP_ANUMER2(allstar)],
		        Memr[DP_ADENOM1(allstar)], Memr[DP_ADENOM2(allstar)],
		        Memi[DP_ASKIP(allstar)], niter, istar, lstar,
		        DP_PSFMAG(psffit), chigrp)
	        } else {
		    call dp_talwrite (allfd, Memi[colpoints],
		    Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
		    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
		    Memr[DP_APERR(apsel)], Memr[DP_APMSKY(apsel)],
		    Memr[DP_APCHI(apsel)], Memr[DP_ANUMER1(allstar)],
		    Memr[DP_ANUMER2(allstar)], Memr[DP_ADENOM1(allstar)],
		    Memr[DP_ADENOM2(allstar)], Memi[DP_ASKIP(allstar)], niter,
		    istar, lstar, row_number, DP_PSFMAG(psffit), chigrp)
		}

	    }

	    # Find the last star in the list that still needs more work.
	    nstar = dp_alnstar (Memi[DP_ASKIP(allstar)], nstar)

	    # Remove the fitted stars from the list.
	    for (istar = 1;  (istar < nstar) && nstar > 0; istar = istar + 1) {
		call dp_alswitch (Memi[DP_APID(apsel)],
		    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_APMAG(apsel)], Memr[DP_APERR(apsel)],
		    Memr[DP_APMSKY(apsel)], Memr[DP_AXOLD(allstar)],
		    Memr[DP_AYOLD(allstar)], Memr[DP_AXCLAMP(allstar)],
		    Memr[DP_AYCLAMP(allstar)], Memi[DP_ASKIP(allstar)],
		    istar, nstar)
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

procedure dp_alswitch (id, xcen, ycen, mag, magerr, sky, xold, yold, xclamp,
	yclamp, skip, istar, nstar)

int	id[ARB]			# array of ids
real	xcen[ARB]		# array of x centers
real	ycen[ARB]		# array of y centers
real	mag[ARB]		# array of magnitudes
real	magerr[ARB]		# array of magnitude errors
real	sky[ARB]		# array of sky values
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
	    xold[istar] = xold[nstar]
	    yold[istar] = yold[nstar]
	    xclamp[istar] = xclamp[nstar]
	    yclamp[istar] = yclamp[nstar]
	    skip[istar] = NO
	    nstar = nstar - 1
	    nstar = dp_alnstar (skip, nstar)
	}
end
