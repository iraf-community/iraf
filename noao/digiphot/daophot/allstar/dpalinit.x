include <mach.h>
include	<imhdr.h>
include "../lib/daophotdef.h"
include	"../lib/allstardef.h"

# DP_SETWT -- Initialize the subtracted image and compute the initial weights
# image from the input image.

procedure dp_setwt (dao, im) 

pointer	dao				# pointer to daophot structure
pointer	im				# input image decriptor

int	ncol
pointer	sp, v1, v2, v3, allstar, wtim, dataim, line1, line2, line3
real	rdnoise, ro32k, mingdata, maxgdata
int	imgnlr(), impnlr()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)

	# Define the allstar pointer.
	allstar = DP_ALLSTAR (dao)

	# Define some useful constants.
	rdnoise = (DP_READ_NOISE(dao) / DP_PHOT_ADC(dao)) ** 2
	ro32k = RO32K 
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
	ncol = IM_LEN (im, 1)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)
	while (imgnlr (im, line1, Meml[v1]) != EOF) {

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
		    maxgdata, rdnoise, ro32k, DP_PHOT_ADC(dao))
		wtim = wtim + ncol
	    } else if (impnlr (wtim, line3, Meml[v3]) != EOF) {
		call dp_wtvector (Memr[line1], Memr[line3], ncol, mingdata,
		    maxgdata, rdnoise, ro32k, DP_PHOT_ADC(dao))
	    }

	}

	# Make sure all the changes are written to disk.
	if (DP_CACHE(allstar, A_WEIGHT) == NO)
	    call imflush (DP_WEIGHTS(allstar))
	if (DP_CACHE(allstar, A_DCOPY) == NO)
	    call imflush (DP_DATA(allstar))

	call sfree (sp)
end


define 	MAGIC 	0.0075		# amplitude of the interpolation error

# DP_WTVECTOR -- Compute the weights for a vector of input data. The scaling
# constant is a hangover from the days when the weights were stored in a
# short integer array to save space and should someday be removed after
# carefull checking through the code for unforeseen dynamic range side
# affects. The compute weight includes the effects of readout noise,
# Poisson statistics and interpolation error.

procedure dp_wtvector (a, b, ncols, mingdata, maxgdata, rnoisesq, ro32k, gain)

real	a[ARB]			# input array
real	b[ARB]			# output array
int	ncols			# number of points
real	mingdata		# minimum good data value
real	maxgdata		# maximum good data value
real	rnoisesq		# read noise squared in adu
real	ro32k			# scaling constant
real	gain			# gain in electrons per adu

int	i
real	dwt

begin
	do i = 1, ncols {
	    if (a[i] < mingdata || a[i] > maxgdata) {
		b[i] = INDEFR
	    } else {
		dwt = max (0.0, a[i])
		dwt = rnoisesq + dwt / gain + (MAGIC * dwt) ** 2
		if (dwt <= 0.0)
		    b[i] = INDEFR
		else
		    b[i] = ro32k / dwt
	    }
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
real	mag[ARB]		# pointer to the magnitude array
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

procedure dp_strip (id, x, y, mag, sky, skip, nstar, sepradius, ncol, nline,
	fitradius, verbose)

int	id[ARB]			# array of star ids
real	x[ARB]			# array of star x values
real	y[ARB]			# array of star y values
real	mag[ARB]		# array of star magnitudes
real	sky[ARB]		# array of star sky values
int	skip[ARB]		# array of fit/nofit indicators
int	nstar			# number of stars (may change)
real	sepradius		# separation radius
int	ncol			# number of columns in the image
int	nline			# number of columns in the image
real	fitradius		# the fitting radius
int	verbose			# print error messages ?

int	i, j
pointer	sp, index, idhold
real	dx, dy, sepradsq, xhold, yhold, maghold, skyhold

begin
	# Duplicate stars are impossible.
	if (nstar <= 1)
	    return

	# Allocate some working space.
	call smark (sp)
	call salloc (index, nstar, TY_INT)

	# Initialize the fit/nofit flags.
	sepradsq = sepradius * sepradius
	do i = 1, nstar
	    skip[i] = NO

	# Sort the data in y.
	call quick (y, nstar, Memi[index])

	# Rectify the remaining arrays.
	call dp_irectify (id, Memi[index], nstar)
	call dp_rectify (x, Memi[index], nstar)
	call dp_rectify (mag, Memi[index], nstar)
	call dp_rectify (sky, Memi[index], nstar)

	# Determine whether any stars are close to star i.
	do i = 1, nstar - 1 {

	    # Star has already been rejected.
	    if (skip[i] == YES)
		next

	    # Reject if star has an INDEF valued position or is off image.
	    if (IS_INDEFR(x[i]) || IS_INDEFR(y[i])) {
		skip[i] = YES
		mag[i] = INDEFR
		if (verbose == YES)
		    call printf (
		        "REJECTING: Star %d has an undefined x or y value\n")
			call pargi (id[i])
	    } else if ((int (x[i] - fitradius) + 1) > ncol) {
		skip[i] = YES
		mag[i] = INDEFR
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    } else if (int (x[i] + fitradius) < 1) {
		skip[i] = YES
		mag[i] = INDEFR
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    } else if ((int (y[i] - fitradius) + 1) > nline) {
		skip[i] = YES
		mag[i] = INDEFR
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    } else if (int (y[i] + fitradius) < 1) {
		skip[i] = YES
		mag[i] = INDEFR
		if (verbose == YES)
		    call printf ("REJECTING: Star %d is outside the image\n")
			call pargi (id[i])
	    }

	    # This star was rejected.
	    if (skip[i] == YES)
		next

	    # Loop over the remaining stars.
	    do j = i + 1, nstar {

	        # Star has already been rejected on previous loop.
	        if (skip[j] == YES)
		    next

		# Test for INDEF.
	        if (IS_INDEFR(x[j]) || IS_INDEFR(y[j])) {
		    skip[j] = YES
		    mag[j] = INDEF
		    if (verbose == YES)
		        call printf (
		    	"REJECTING: Star %d has an undefined x or y value\n")
			    call pargi (id[j])
	        } else if ((int (x[j] - fitradius) + 1) > ncol) {
		    skip[j] = YES
		    mag[j] = INDEFR
		    if (verbose == YES)
		        call printf (
			    "REJECTING: Star %d is outside the image\n")
			    call pargi (id[j])
	        } else if (int (x[j] + fitradius) < 1) {
		    skip[j] = YES
		    mag[j] = INDEFR
		    if (verbose == YES)
		        call printf (
			    "REJECTING: Star %d is outside the image\n")
			    call pargi (id[j])
	        } else if ((int (y[j] - fitradius) + 1) > nline) {
		    skip[j] = YES
		    mag[j] = INDEFR
		    if (verbose == YES)
		        call printf (
			    "REJECTING: Star %d is outside the image\n")
			    call pargi (id[j])
	        } else if (int (y[j] + fitradius) < 1) {
		    skip[j] = YES
		    mag[j] = INDEFR
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
		if (dy > sepradius)
		    break
		dx = x[j] - x[i]
		if (abs (dx) > sepradius)
		    next
		if ((dx * dx + dy * dy) > sepradsq)
		    next

		# Set the magnitude of the star to INDEF and skip.
		if (mag[j] <= mag[i]) {
		    skip[j] = YES
		    mag[j] = INDEFR
		    if (verbose == YES) {
		        call printf (
			    "REJECTING: Star %d has merged with star %d\n")
			    call pargi (id[j])
		            call pargi (id[i])
		    }
		} else {
		    skip[i] = YES
		    mag[i] = INDEFR
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

	    # Remove skipped stars from the end of the list.
	    while ((skip[nstar] == YES) && (nstar >= 1))
		nstar = nstar - 1

	    # Quit the loop if no good stars are left.
	    if (i > nstar)
		break

	    # Place the rejected star at the end of the list.

	    if (skip[i] == NO)
		next

	    idhold = id[i]
	    xhold = x[i]
	    yhold = y[i]
	    maghold = mag[i]
	    skyhold = sky[i]

	    id[i] = id[nstar]
	    x[i] = x[nstar]
	    y[i] = y[nstar]
	    mag[i] = mag[nstar]
	    sky[i] = sky[nstar]
	    skip[i] = NO

	    id[nstar] = idhold
	    x[nstar] = xhold
	    y[nstar] = yhold
	    mag[nstar] = maghold
	    sky[nstar] = skyhold
	    skip[nstar] = YES

	    nstar = nstar - 1
	}

	call sfree (sp)
end


# DP_WTINIT -- Initialize the weight array for all the unfitted stars.

procedure dp_wtinit (dao, im, xcen, ycen, last, nstar, x1, x2, y1, y2)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
real	xcen[ARB]		# the array of star x centers
real	ycen[ARB]		# the array of star y centers
int	last[ARB]		# pointer to the grouping array
int	nstar			# the number of stars
int	x1, x2			# the star list column limits in x and y
int	y1, y2			# the star list line limits in x and y

int	i, j, l, ncol, npix, lx1, lx2, ly1, ly2, gstar, lstar, star1, star2
pointer allstar, wtim, wtbuf, ibuf, obuf
real	radius
int	dp_laststar()
pointer	imps2r(), imgs2r()

begin
	# Define the allstar pointer.
	allstar = DP_ALLSTAR(dao)

	# Set up some constants.
	ncol = IM_LEN(im,1)
	npix = x2 - x1 + 1
	radius = DP_FITRAD(dao)

	# Get the data.
	wtim = DP_WEIGHTS(allstar)

	if (DP_CACHE(allstar,A_WEIGHT) == YES) {

	    # Set the weights in the region of interest to be negative.
	    wtbuf  = wtim + (y1 - 1) * ncol
	    do j = y1, y2 {
		do i = x1, x2 {
		    if (! IS_INDEFR(Memr[wtbuf+i-1]))
		        Memr[wtbuf+i-1] = - abs (Memr[wtbuf+i-1])
		}
		wtbuf = wtbuf + ncol
	    }

	    # Set the weights of the unfitted stars to be negative.
	    do l = 1, nstar {
	        call dp_glim (xcen[l], ycen[l], radius, x1, x2, y1, y2,
		    lx1, lx2, ly1, ly2)
	        wtbuf = wtim + (ly1 - 1) * ncol
		do j = ly1, ly2 {
		    do i = lx1, lx2 {
		        if (! IS_INDEF(Memr[wtbuf+i-1]))
			    Memr[wtbuf+i-1] = abs (Memr[wtbuf+i-1])
		    }
		    wtbuf = wtbuf + ncol
		}
	    }

	} else {

	    # Begin searching on the first group.
	    gstar = 1

	    # Loop over the lines in the region of interest.
	    do j = y1, y2 {

		# Set the weights in the line of interest to be negative.
		obuf = imps2r (wtim, x1, x2, j, j)
		ibuf = imgs2r (wtim, x1, x2, j, j)
		do i = 1, npix {
		    if (! IS_INDEFR(Memr[ibuf+i-1]))
		        Memr[obuf+i-1] = - abs (Memr[ibuf+i-1])
		}

		# Initialize the star pointers.
		star1 = nstar + 1
		star2 = 0

		# Loop over the groups,  deciding whether any overlap the
		# line of interest.
		for (l = gstar; l <= nstar; l = lstar + 1) {
		    lstar = dp_laststar (last, l, nstar)
		    ly1 = max (y1, min (y2, int (ycen[l]- radius) + 1))
		    ly2 = max (y1, min (y2, int (ycen[lstar]+radius))) 
		    if (ly2 < j) {
			gstar = lstar + 1
		    } else if (ly1 > j) {
			break
		    } else {
			star1 = min (star1, l)
			star2 = max (star2, lstar)
		    }
		}

		# Test the star pointers.
		if (star2 < star1)
		    next

		# Set the appropriate regions around the stars to be positive.
		do l = star1, star2 {
		    call dp_glim (xcen[l], ycen[l], radius, x1, x2, y1, y2,
		        lx1, lx2, ly1, ly2)
		    if (j < ly1 || j > ly2)
			next
		    do i = lx1 - x1 + 1, lx2 - x1 + 1 {
			if (! IS_INDEFR(Memr[obuf+i-1]))
			    Memr[obuf+i-1] = abs (Memr[obuf+i-1])
		    }
		}

	    }

	    call imflush (wtim)
	}
end


# DP_STINIT -- Initialize the scratch image.

procedure dp_stinit (dao, im, xcen, ycen, mag, last, nstar, x1, x2, y1, y2)

pointer	dao				# pointer to the daophot structure
pointer	im				# pointer to the input image
real	xcen[ARB]			# the x centers array
real	ycen[ARB]			# the y centers array
real	mag[ARB]			# the magnitude array
int	last[ARB]			# the group definition array
int	nstar				# the number of stars
int	x1, x2				# column limits
int	y1, y2				# line limits

int	i, j, l, ncol, npix, gstar, star1, star2, lstar, lx1, lx2, ly1, ly2
pointer psffit, allstar, databuf, subtbuf, wtbuf
real	psfrad, psfradsq
int	dp_laststar()
pointer	imgs2r(), imps2r()

begin
	# Set up some constants
	psffit = DP_PSFFIT(dao)
	allstar = DP_ALLSTAR(dao)
	ncol = IM_LEN(im,1)
	npix = x2 - x1 + 1
	psfrad = DP_PSFRAD(dao)
	psfradsq = psfrad * psfrad

	# Begin the search of the groups at the first group.
	gstar = 1

	do j = y1, y2 {

	    # Get the data.
	    if (DP_CACHE(allstar,A_SUBT) == YES)
		subtbuf = DP_SUBT(allstar) + (j - 1) * ncol + x1 - 1
	    else
		subtbuf = imps2r (DP_SUBT(allstar), x1, x2, j, j)
	    if (DP_CACHE(allstar,A_WEIGHT) == YES)
		wtbuf = DP_WEIGHTS(allstar) + (j - 1) * ncol + x1 - 1
	    else
		wtbuf = imgs2r (DP_WEIGHTS(allstar), x1, x2, j, j)
	    if (DP_CACHE(allstar,A_DCOPY) == YES)
		databuf = DP_DATA(allstar) + (j - 1) * ncol + x1 - 1
	    else
		databuf = imgs2r (DP_DATA(allstar), x1, x2, j, j)

	    # Initialize the scratch image.
	    do i = 1, npix {
		if (IS_INDEFR(Memr[wtbuf+i-1]) || Memr[wtbuf+i-1] <= 0.0)
		    Memr[subtbuf+i-1] = 0.0
		else
		    Memr[subtbuf+i-1] = Memr[databuf+i-1]
	    }

	    # Initialize the star pointers.
	    star1 = nstar + 1
	    star2 = 0

	    # Decide whether any of the groups overlap the line of interest.
	    for (l = gstar; l <= nstar; l = lstar + 1) {
		lstar = dp_laststar (last, l, nstar)
		ly1 = max (y1, min (y2, int (ycen[l] - psfrad) + 1))
		ly2 = max (y1, min (y2, int (ycen[lstar] + psfrad)))
		if (ly2 < j) {
		    gstar = lstar + 1
		} else if (ly1 > j) {
		    break
		} else {
		    star1 = min (star1, l)
		    star2 = max (star2, lstar)
		}
	    }

	    # Test the star pointers.
	    if (star2 < star1)
		next

	    do l = star1, star2 {
		call dp_glim (xcen[l], ycen[l], psfrad, x1, x2, y1, y2,
		    lx1, lx2, ly1, ly2)
		if (j < ly1 || j > ly2)
		    next
		call dp_alsubstar (Memr[wtbuf], npix, 1, x1, j, Memr[subtbuf],
		    npix, 1, x1, j, lx1, j, lx2 - lx1 + 1, 1, xcen[l],
		    ycen[l], mag[l], DP_VARPSF(dao), psfradsq, psffit)
	    }

	}

	if (DP_CACHE(allstar,A_SUBT) == NO)
	    call imflush (DP_SUBT(allstar))
end
