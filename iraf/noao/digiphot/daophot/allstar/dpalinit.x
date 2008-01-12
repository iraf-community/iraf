include <mach.h>
include	<imhdr.h>
include <math.h>
include "../lib/daophotdef.h"
include	"../lib/allstardef.h"

# DP_SETWT -- Initialize the subtracted image and compute the initial weights
# image from the input image.

procedure dp_setwt (dao, im) 

pointer	dao				# pointer to daophot structure
pointer	im				# input image decriptor

int	i, ncol, nline
pointer	sp, v1, v2, v3, v4, line1, line2, line3, line4
pointer	allstar, wtim, dataim, subtim
real	rdnoise, mingdata, maxgdata
int	imgnlr(), impnlr()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call salloc (v4, IM_MAXDIM, TY_LONG)

	# Define the allstar pointer.
	allstar = DP_ALLSTAR (dao)

	# Define some useful constants.
	rdnoise = (DP_READNOISE(dao) / DP_PHOTADU(dao)) ** 2
	if (IS_INDEFR(DP_MINGDATA(dao)))
	    mingdata = -MAX_REAL
	else
	    mingdata = DP_MINGDATA(dao)
	if (IS_INDEFR(DP_MAXGDATA(dao)))
	    maxgdata = MAX_REAL
	else
	    maxgdata = DP_MAXGDATA(dao)

	wtim = DP_WEIGHTS (allstar)
	dataim = DP_DATA(allstar)
	subtim = DP_SUBT(allstar)
	ncol = IM_LEN (im,1)
	nline = IM_LEN(im,2)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)
	call amovkl (long(1), Meml[v4], IM_MAXDIM)
	do i = 1, nline {

	    # Get the next line
	    if (imgnlr (im, line1, Meml[v1]) == EOF) 
		break

	    # Initialize the subtracted image.
	    if (DP_CACHE(allstar, A_DCOPY) == YES) {
		call amovr (Memr[line1], Memr[dataim], ncol)
		dataim = dataim + ncol
	    } else if (impnlr (dataim, line2, Meml[v2]) != EOF) {
		call amovr (Memr[line1], Memr[line2], ncol)
	    }

	    # Initialize the weights image.
	    if (DP_CACHE(allstar, A_WEIGHT) == YES) {
		call dp_wtvector (Memr[line1], Memr[wtim], ncol, mingdata,
		    maxgdata, rdnoise)
		wtim = wtim + ncol
	    } else if (impnlr (wtim, line3, Meml[v3]) != EOF) {
		call dp_wtvector (Memr[line1], Memr[line3], ncol, mingdata,
		    maxgdata, rdnoise)
	    }

	    # Initilize the subtracted image.
	    if (DP_CACHE(allstar, A_WEIGHT) == YES) {
		;
	    } else if (impnlr (subtim, line4, Meml[v4]) != EOF) {
		call amovkr (0.0, Memr[line4], ncol)
	    }

	}

	# Make sure all the changes are written to disk.
	if (DP_CACHE(allstar, A_WEIGHT) == NO)
	    call imflush (DP_WEIGHTS(allstar))
	if (DP_CACHE(allstar, A_DCOPY) == NO)
	    call imflush (DP_DATA(allstar))
	if (DP_CACHE(allstar, A_SUBT) == NO)
	    call imflush (DP_SUBT(allstar))

	call sfree (sp)
end


# DP_WTVECTOR -- Compute the initial weights for a vector of input data.

procedure dp_wtvector (a, b, ncols, mingdata, maxgdata, rnoisesq)

real	a[ARB]			# input array
real	b[ARB]			# output array
int	ncols			# number of points
real	mingdata		# minimum good data value
real	maxgdata		# maximum good data value
real	rnoisesq		# read noise squared in adu

int	i

begin
	do i = 1, ncols {
	    if (a[i] < mingdata || a[i] > maxgdata)
		b[i] = -MAX_REAL
	    else
		b[i] = rnoisesq
	}
end


define	DELTA_MAG		12.5
define	INIT_REL_BRIGHT		0.003

# DP_ALZERO --  Convert from magnitudes to relative brightnesses before
# fitting the stars. If the star's magnitude is undefined or more than
# 12.5 magnitudes fainter than the PSF magnitude, a default relative
# brightness is defined.

procedure dp_alzero (dao, mag, nstars)

pointer	dao			# pointer to the daophot strucuture
real	mag[ARB]		# the magnitude array
int	nstars			# number of stars

int	i
pointer	psffit
real	faint

begin
	psffit = DP_PSFFIT (dao)
	faint = DP_PSFMAG(psffit) + DELTA_MAG

	do i = 1, nstars {
	    if (IS_INDEFR(mag[i])) {
		mag[i] = INIT_REL_BRIGHT
	    } else if (mag[i] >= faint) {
		mag[i] = INIT_REL_BRIGHT
	    } else {
	        mag[i] = DAO_RELBRIGHT (psffit, mag[i])
	    }
	}
end


# DP_STRIP -- Remove stars with undefined centers, stars with centers that
# are off the input image, and duplicate stars from the input photometry
# list, where duplicate stars are defined as those within a distance of
# radius pixels of each other. The duplicate stars are moved to the end of
# the star list and the number of stars is recomputed.

procedure dp_strip (id, x, y, mag, sky, skip, aier, nstar, sepradsq, ncol,
	nline, fitradius, verbose)

int	id[ARB]			# array of star ids
real	x[ARB]			# array of star x values
real	y[ARB]			# array of star y values
real	mag[ARB]		# array of star magnitudes
real	sky[ARB]		# array of star sky values
int	skip[ARB]		# array of fit/nofit indicators
int	aier[ARB]		# array of error codes
int	nstar			# number of stars (may change)
real	sepradsq		# separation radius squared
int	ncol			# number of columns in the image
int	nline			# number of columns in the image
real	fitradius		# the fitting radius
int	verbose			# print error messages ?

int	i, j, ier, ahold
pointer	sp, index, idhold
real	seprad, dx, dy, xhold, yhold, maghold, skyhold

begin
	# Duplicate stars are impossible.
	if (nstar <= 1)
	    return

	# Allocate some working space.
	call smark (sp)
	call salloc (index, nstar, TY_INT)

	# Sort the data in y.
	call quick (y, nstar, Memi[index], ier)

	# Rectify the remaining arrays.
	call dp_irectify (id, Memi[index], nstar)
	call dp_rectify (x, Memi[index], nstar)
	call dp_rectify (mag, Memi[index], nstar)
	call dp_rectify (sky, Memi[index], nstar)

	# Determine whether any stars are close to star i.
	seprad = sqrt (sepradsq)
	do i = 1, nstar - 1 {

	    # This star was rejected on a previous loop.
	    if (skip[i] == YES)
		next

	    # Reject if star has an INDEF valued position or is off image.
	    if (IS_INDEFR(x[i]) || IS_INDEFR(y[i])) {
		mag[i] = INDEFR
		skip[i] = YES
		aier[i] = ALLERR_OFFIMAGE
		if (verbose == YES)
		    call printf (
		        "REJECTING: Star %d has an undefined x or y value\n")
			call pargi (id[i])
	    } else if ((int (x[i] - fitradius) + 1) > ncol) {
		mag[i] = INDEFR
		skip[i] = YES
		aier[i] = ALLERR_OFFIMAGE
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    } else if (int (x[i] + fitradius) < 1) {
		mag[i] = INDEFR
		skip[i] = YES
		aier[i] = ALLERR_OFFIMAGE
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    } else if ((int (y[i] - fitradius) + 1) > nline) {
		mag[i] = INDEFR
		skip[i] = YES
		aier[i] = ALLERR_OFFIMAGE
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    } else if (int (y[i] + fitradius) < 1) {
		mag[i] = INDEFR
		skip[i] = YES
		aier[i] = ALLERR_OFFIMAGE
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    }

	    # This star was rejected on this loop. 
	    if (skip[i] == YES)
		next

	    # Loop over the remaining stars.
	    do j = i + 1, nstar {

	        # Star has already been rejected on previous loop.
	        if (skip[j] == YES)
		    next

		# Test for INDEF.
	        if (IS_INDEFR(x[j]) || IS_INDEFR(y[j])) {
		    mag[j] = INDEFR
		    skip[j] = YES
		    aier[j] = ALLERR_OFFIMAGE
		    if (verbose == YES)
		        call printf (
		    	"REJECTING: Star %d has an undefined x or y value\n")
			    call pargi (id[j])
	        } else if ((int (x[j] - fitradius) + 1) > ncol) {
		    mag[j] = INDEFR
		    skip[j] = YES
		    aier[j] = ALLERR_OFFIMAGE
		    if (verbose == YES)
		        call printf (
			    "REJECTING: Star %d is outside the image\n")
			    call pargi (id[j])
	        } else if (int (x[j] + fitradius) < 1) {
		    mag[j] = INDEFR
		    skip[j] = YES
		    aier[j] = ALLERR_OFFIMAGE
		    if (verbose == YES)
		        call printf (
			    "REJECTING: Star %d is outside the image\n")
			    call pargi (id[j])
	        } else if ((int (y[j] - fitradius) + 1) > nline) {
		    mag[j] = INDEFR
		    skip[j] = YES
		    aier[j] = ALLERR_OFFIMAGE
		    if (verbose == YES)
		        call printf (
			    "REJECTING: Star %d is outside the image\n")
			    call pargi (id[j])
	        } else if (int (y[j] + fitradius) < 1) {
		    mag[j] = INDEFR
		    skip[j] = YES
		    aier[j] = ALLERR_OFFIMAGE
		    if (verbose == YES)
		        call printf (
			    "REJECTING: Star %d is outside the image\n")
			    call pargi (id[j])
	        }

		# Star was rejected.
		if (skip[j] == YES)
		    next

		# Test for proximity.
		dy = y[j] - y[i]
		if (dy > seprad)
		    break
		dx = x[j] - x[i]
		if (abs (dx) > seprad)
		    next
		if ((dx * dx + dy * dy) > sepradsq)
		    next

		# Set the magnitude of the star to INDEF and skip.
		if (mag[j] <= mag[i]) {
		    mag[j] = INDEFR
		    skip[j] = YES
		    aier[j] = ALLERR_MERGE
		    if (verbose == YES) {
		        call printf (
			    "REJECTING: Star %d has merged with star %d\n")
			    call pargi (id[j])
		            call pargi (id[i])
		    }
		} else {
		    mag[i] = INDEFR
		    skip[i] = YES
		    aier[i] = ALLERR_MERGE
		    if (verbose == YES) {
		        call printf (
			    "REJECTING: Star %d has merged with star %d\n")
			    call pargi (id[i])
			    call pargi (id[j])
		    }
		    break
		}
	    }
	}

	# Remove the duplicate stars.
	for (i = 1; i <= nstar; i = i + 1) {

	    # Redefine the number of stars by removing skipped stars from
	    # the end of the star list.
	    while (nstar >= 1) {
		if (skip[nstar] == NO)
		    break
		nstar = nstar - 1
	    }
	    if (i > nstar)
		break
	    if (skip[i] == NO)
		next

	    # Switch the rejected star with the one at the end of the list.
	    idhold = id[i]
	    xhold = x[i]
	    yhold = y[i]
	    maghold = mag[i]
	    skyhold = sky[i]
	    ahold = aier[i]

	    id[i] = id[nstar]
	    x[i] = x[nstar]
	    y[i] = y[nstar]
	    mag[i] = mag[nstar]
	    sky[i] = sky[nstar]
	    skip[i] = NO
	    aier[i] = aier[nstar]

	    id[nstar] = idhold
	    x[nstar] = xhold
	    y[nstar] = yhold
	    mag[nstar] = maghold
	    sky[nstar] = skyhold
	    skip[nstar] = YES
	    aier[nstar] = ahold

	    nstar = nstar - 1
	}

	call sfree (sp)
end


# DP_WSTINIT -- Initialize the weight and scratch arrays / images.

procedure dp_wstinit (dao, im, xcen, ycen, mag, nstar, radius, x1, x2, y1, y2)

pointer	dao				# pointer to the daophot structure
pointer	im				# pointer to the input image
real	xcen[ARB]			# the x centers array
real	ycen[ARB]			# the y centers array
real	mag[ARB]			# the magnitude array
int	nstar				# the number of stars
real	radius				# radius for subraction
int	x1, x2				# column limits
int	y1, y2				# line limits

int	j, l, ncol, npix, nl, ns, lx, mx
pointer psffit, allstar, databuf, wtbuf, owtbuf, subtbuf
real	rsq, psfradius, psfradsq, x, y, dy, dysq, deltax, deltay
pointer	imgs2r(), imps2r()

begin
	# Set up some pointers.
	psffit = DP_PSFFIT(dao)
	allstar = DP_ALLSTAR(dao)

	# Set up some constants.
	ncol = IM_LEN(im,1)
	npix = x2 - x1 + 1
	rsq = radius ** 2
	if (DP_PSFSIZE(psffit) == 0)
	    psfradius = DP_PSFRAD(dao)
	else
	    psfradius = (real (DP_PSFSIZE(psffit) - 1) / 2.0 - 1.0) / 2.0
	psfradsq = psfradius * psfradius

	# Begin the search of the groups at the first group.
	ns = 0
	nl = 0

	ns = 0
	do j = y1, y2 {

	    # Get the data.
	    if (DP_CACHE(allstar,A_DCOPY) == YES)
		databuf = DP_DATA(allstar) + (j - 1) * ncol + x1 - 1
	    else
		databuf = imgs2r (DP_DATA(allstar), x1, x2, j, j)
	    if (DP_CACHE(allstar,A_WEIGHT) == YES) {
		wtbuf = DP_WEIGHTS(allstar) + (j - 1) * ncol + x1 - 1
		owtbuf = wtbuf
	    } else {
		owtbuf = imps2r (DP_WEIGHTS(allstar), x1, x2, j, j)
		wtbuf = imgs2r (DP_WEIGHTS(allstar), x1, x2, j, j)
	    }
	    if (DP_CACHE(allstar,A_SUBT) == YES)
		subtbuf = DP_SUBT(allstar) + (j - 1) * ncol + x1 - 1
	    else
		subtbuf = imps2r (DP_SUBT(allstar), x1, x2, j, j)

	    # Set all the weights in the working array negative.
	    call aabsr (Memr[wtbuf], Memr[owtbuf], npix)
	    call anegr (Memr[owtbuf], Memr[owtbuf], npix)
	    call amovkr (0.0, Memr[subtbuf], npix)

	    # Set the y coordinate.
	    y = real (j)

	    # Initialize the weight and scratch arrays.

	    # Find all the points within one working radius of each star.
	    # Set all the sigmas positive again and copy them from DATA
	    # to SUBT.
	    do l = ns + 1, nstar {
		dy = y - ycen[l] 
		if (dy > radius) {
		    ns = l
		    next
		} else if (dy < -radius)
		    break 
		dysq = dy ** 2
		lx = max (x1, min (x2, int (xcen[l] - radius) + 1))
		mx = max (x1, min (x2, int (xcen[l] + radius)))
		x = xcen[l] - lx + 1.0
		lx = lx - x1 + 1
		mx = mx - x1 + 1
		call dp_wstvector (x, Memr[databuf+lx-1], Memr[owtbuf+lx-1],
		    Memr[subtbuf+lx-1], mx - lx + 1, dysq, rsq, -MAX_REAL)
	    }

	    do l = nl + 1, nstar {
		dy = y - ycen[l] 
		if (dy > psfradius) {
		    nl = l
		    next
		} else if (dy < -psfradius)
		    break 
		dysq = dy ** 2
		lx = max (x1, min (x2, int (xcen[l] - psfradius) + 1))
		mx = max (x1, min (x2, int (xcen[l] + psfradius)))
		x = xcen[l] - lx + 1.0
		lx = lx - x1 + 1
		mx = mx - x1 + 1
		call dp_wpsf (dao, im, xcen[l], ycen[l], deltax, deltay, 1)
		deltax = (deltax - 1.0) / DP_PSFX(psffit) - 1.0
		deltay = (deltay - 1.0) / DP_PSFY(psffit) - 1.0
		call dp_alsubstar (psffit, x, dy, deltax, deltay, mag[l],
		    Memr[subtbuf+lx-1], Memr[owtbuf+lx-1], mx - lx + 1, dysq,
		    psfradsq)
	    }
	}

	if (DP_CACHE(allstar,A_WEIGHT) == NO)
	    call imflush (DP_WEIGHTS(allstar))
	if (DP_CACHE(allstar,A_SUBT) == NO)
	    call imflush (DP_SUBT(allstar))
end


# DP_WSTVECTOR -- Set the weight and scratch vector

procedure dp_wstvector (xcen, data, weight, subt, npix, dysq, rsq, badwt)

real	xcen			# x coordinate of center
real	data[ARB]		# the data array
real	weight[ARB]		# the weight array
real	subt[ARB]		# the subtracted array array
int	npix			# the number of pixels
real	dysq			# the y distance squared
real	rsq			# the inclusion radius squared
real	badwt			# badwt value

int	i
real	dx

begin
	do i = 1, npix {
	    dx = (real (i) - xcen)
	    if ((dx ** 2 + dysq) <= rsq) {
		if (weight[i] <= badwt)
		    next
		weight[i] = abs (weight[i])
		subt[i] = data[i]
	    } else if (dx >= 0.0)
		break
	}
end


# DP_ALSUBSTAR -- The subtraction vector.

procedure dp_alsubstar (psffit, xcen, dy, deltax, deltay, mag, subt, weight,
	npix, dysq, psfradsq)

pointer	psffit			# pointer to the psf fitting structure
real	xcen			# x coordinate of center
real	dy			# y offset from psf center
real	deltax			# x distance from psf position
real	deltay			# y distance from psf position
real	mag			# the magnitude of the star
real	subt[ARB]		# the subtracted array array
real	weight[ARB]		# the weight array
int	npix			# the number of pixels
real	dysq			# the y distance squared
real	psfradsq		# the inclusion radius squared

int	i
real	dx, dvdxc, dvdyc
real	dp_usepsf()

begin
	do i = 1, npix {
	    if (weight[i] < 0.0)
		next
	    dx = real (i) - xcen
	    if ((dx ** 2 + dysq) < psfradsq) {
		subt[i] = subt[i] - mag * dp_usepsf (DP_PSFUNCTION(psffit),
		    dx, dy, DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		    DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit), deltax, deltay,
		    dvdxc, dvdyc)
	    } else if (dx > 0.0)
		break
	}
end


# DP_ALSKY -- Recompute the sky values.

procedure dp_alsky (dao, im, xcen, ycen, sky, nstar, x1, x2, y1, y2, rinsky,
	routsky, minnsky, badwt)

pointer	dao			# pointer to the daophot structure
int	im			# the input image pointer
real	xcen[ARB]		# x coordinates
real	ycen[ARB]		# y coordinates
real	sky[ARB]		# sky values
int	nstar			# number of stars
int	x1, x2			# coordinate limits
int	y1, y2			# coordinate limits
real	rinsky			# inner radius of the sky annulus
real	routsky			# outer radius of the sky annulus
int	minnsky			# minimum number of sky pixels
real	badwt			# the bad weight value

int	istar, i, j, ncols, lenskybuf, lx, mx, ly, my, line1, line2, nsky, ier
pointer	allstar, sub, psub, wgt, pwgt, skyvals, index
real	risq, rosq, dannulus, dysq, dx, rsq
pointer	dp_gst(), dp_gwt()

begin
	# Get the allstar pointer.
	allstar = DP_ALLSTAR(dao)

	# Define some constants
	ncols = IM_LEN(im,1)
	risq = rinsky ** 2 
	rosq = routsky ** 2
	dannulus = routsky - rinsky

	# Allcoate some working memory.
	lenskybuf = PI * (2.0 * rinsky + dannulus + 1.0) * (dannulus + 0.5)
	call malloc (skyvals, lenskybuf, TY_REAL)
	call malloc (index, lenskybuf, TY_INT)

	# Accumulate the sky buffer.
	do istar = 1, nstar {

	    # Get the data.
	    lx = max (x1, min (x2, int (xcen[istar] - routsky) + 1))
	    mx = max (x1, min (x2, int (xcen[istar] + routsky)))
	    ly = max (y1, min (y2, int (ycen[istar] - routsky) + 1))
	    my = max (y1, min (y2, int (ycen[istar] + routsky)))
	    line1 = ly
	    line2 = my
	    sub = dp_gst (dao, im, line1, line2, READ_ONLY, NO)
	    wgt = dp_gwt (dao, im, line1, line2, READ_ONLY, NO)

	    nsky = 0
	    psub = sub + (ly - DP_SYOFF(allstar)) * ncols 
	    pwgt = wgt + (ly - DP_WYOFF(allstar)) * ncols
	    do j = ly, my {
		dysq = (real (j) - ycen[istar]) ** 2
		do i = lx, mx {
		    dx = (real (i) - xcen[istar])
		    rsq = dx ** 2 + dysq
		    if (rsq > rosq) {
			if (dx > 0.0)
			    break
			else 
			    next
		    }
		    if (rsq < risq)
			next
		    if (Memr[pwgt+i-1] <= badwt)
			next
		    Memr[skyvals+nsky] = Memr[psub+i-1]
		    nsky = nsky + 1
		}
		psub = psub + ncols
		pwgt = pwgt + ncols
	    }

	    # Compute the new sky value.
	    if (nsky > minnsky) {
	        call quick (Memr[skyvals], nsky, Memi[index], ier)
	        j = nint (0.2 * nsky)
	        dx = 0.0
	        do i = (nsky + 1) / 2 - j, (nsky / 2) + j + 1
		    dx = dx + Memr[skyvals+i-1]
	        sky[istar] = dx / real ((nsky / 2) + 2 * j + 2 -
		    (nsky + 1) / 2) 
	    }
	}


	call mfree (skyvals, TY_REAL)
	call mfree (index, TY_INT)
	sub = dp_gst (dao, im, line1, line2, READ_ONLY, YES)
	wgt = dp_gwt (dao, im, line1, line2, READ_ONLY, YES)
end
