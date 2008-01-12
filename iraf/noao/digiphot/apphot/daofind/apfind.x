include <gset.h>
include <mach.h>
include <imhdr.h>
include "../lib/apphot.h"

# AP_FIND -- Detect images in the convolved image and then compute image
# characteristics using the original image.

int procedure ap_find (ap, im, cnv, out, id, ker2d, skip, nxk, nyk, skymode,
	threshold, relerr, emission, xsigsq, ysigsq, datamin, datamax,
	sharplo, sharphi, roundlo, roundhi, interactive, stid, mkdetections)

pointer ap			# the apphot descriptor
pointer	im			# pointer to the input image
pointer	cnv			# pointer to the output image
int	out			# the output file descriptor
pointer	id			# pointer to the display stream
real	ker2d[nxk,ARB]		# 2D Gaussian kernel
int	skip[nxk,ARB]		# 2D skip kernel
int	nxk, nyk		# dimensions of the kernel
real	skymode			# estimate of the sky
real	threshold		# threshold for image detection
real	relerr			# the relative error of the convolution kernel
int	emission		# emission features
real	xsigsq, ysigsq		# sigma of gaussian in x and y
real	datamin, datamax	# minimum and maximum good data values
real	sharplo, sharphi	# sharpness limits
real	roundlo,roundhi		# roundness parameter limits
int	interactive		# interactive mode
int	stid			# sequence number
int	mkdetections		# mark detections

int	inline, i, j, ncols, col1, col2, line1, line2, index, pos
int	xmiddle, ymiddle, nonzero, nobjs, nstars, ntotal
pointer	sp, bufptrs, imlbuf, cnvlbuf, imbuf, cnvbuf, cols
pointer	satur, sharp, round1, round2, x, y

int	ap_detect(), ap_test(), apstati()
pointer	imgs2r()
errchk	imgs2r()

begin
	# Set up useful line and column limits.
	ncols = IM_LEN(im,1) + nxk - 1
	col1 = 1 - nxk / 2
	col2 = IM_LEN(im,1) + nxk / 2
	line1 = 1 + nyk / 2
	line2 = IM_LEN(im,2) + nyk / 2
	xmiddle = 1 + nxk / 2
	ymiddle = 1 + nyk / 2

	# Compute find the number of defined elements in the kernel.
	nonzero = 0
	#skip[xmiddle,ymiddle] = NO
	do j = 1, nyk {
	    do i = 1, nxk {
		if (skip[i,j] == NO)
		    nonzero = nonzero + 1
	    }
	}
	skip[xmiddle,ymiddle] = YES
	nonzero = nonzero - 1

	# Set up a cylindrical buffers and some working space for
	# the detected images.
	call smark (sp)
	call salloc (bufptrs, nyk, TY_INT)
	call salloc (imbuf, nyk * ncols, TY_REAL)
	call salloc (cnvbuf, nyk * ncols, TY_REAL)
	call salloc (cols, ncols, TY_INT)
	call salloc (satur, ncols, TY_INT)
	call salloc (sharp, ncols, TY_REAL)
	call salloc (round1, ncols, TY_REAL)
	call salloc (round2, ncols, TY_REAL)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)

	# Read in the first nyk - 1 lines.
	pos = nyk
	do inline = 1 - nyk / 2, nyk / 2 {
	    imlbuf = imgs2r (im, col1, col2, inline, inline)
	    cnvlbuf = imgs2r (cnv, col1, col2, inline, inline)
	    if (emission == YES) {
	        call amovr (Memr[imlbuf], Memr[imbuf+(inline+ymiddle-2)*ncols],
	            ncols)
	        call amovr (Memr[cnvlbuf], Memr[cnvbuf+(inline+ymiddle-2)*
		    ncols], ncols)
	    } else {
	        call amulkr (Memr[imlbuf], -1.0, Memr[imbuf+(inline+ymiddle-2)*
		    ncols], ncols)
	        call amulkr (Memr[cnvlbuf], -1.0, Memr[cnvbuf+(inline+ymiddle-
		    2)* ncols], ncols)
	    }
	    Memi[bufptrs+pos-1] = pos - 1
	    pos = pos - 1
	}

	# Generate the starlist line by line.
	ntotal = 0
	pos = nyk
	do inline = line1, line2 {

	    # Setup the buffer pointer array.
	    do j = 2, nyk
		Memi[bufptrs+j-2] = Memi[bufptrs+j-1]
	    Memi[bufptrs+nyk-1] = pos
	    index = (pos - 1) * ncols

	    # Read in new image line.
	    imlbuf = imgs2r (im, col1, col2, inline, inline)
	    cnvlbuf = imgs2r (cnv, col1, col2, inline, inline)

	    # Copy new lines into cylindrical buffer.
	    if (emission == YES) {
	        call amovr (Memr[imlbuf], Memr[imbuf+index], ncols)
	        call amovr (Memr[cnvlbuf], Memr[cnvbuf+index], ncols)
	    } else {
	        call amulkr (Memr[imlbuf], -1.0, Memr[imbuf+index], ncols)
	        call amulkr (Memr[cnvlbuf], -1.0, Memr[cnvbuf+index], ncols)
	    }

	    # Detect stars in each image line. In order for a given pixel
	    # to be detected as an image the pixel must be above threshold
	    # and be greater than any other pixel within nsigma sigma.

	    # Increment the cylindrical buffer.
	    if (mod (pos, nyk) == 0)
		pos = 1
	    else
		pos = pos + 1

	    nobjs = ap_detect (Memr[cnvbuf], Memi[bufptrs], ncols, skip, nxk,
	        nyk, relerr * threshold, Memi[cols])
	    if (nobjs <= 0)
		next

	    # Compute the sharpness parameter.
	    call ap_sharp_round (Memr[imbuf], Memr[cnvbuf], Memi[bufptrs],
	        ncols, skip, nxk, nyk, Memi[cols], Memi[satur], Memr[round1],
		Memr[sharp], nobjs, nonzero, skymode, datamin, datamax) 

	    # Compute the roundness parameters.
	    call ap_xy_round (Memr[imbuf], Memi[bufptrs], ncols, ker2d, nxk,
	         nyk, Memi[cols], inline, Memr[round2], Memr[x],
		 Memr[y], nobjs, skymode, datamin, datamax, xsigsq, ysigsq)

	    # Test the image characeteristics of detected objects.
	    nstars = ap_test (Memi[cols], Memr[x], Memr[y], Memi[satur],
		Memr[round1], Memr[round2], Memr[sharp], nobjs, IM_LEN(im,1),
		IM_LEN(im,2), sharplo, sharphi, roundlo, roundhi)

	    # Mark the stars on the display.
	    if ((nstars > 0) && (interactive == YES) && (id != NULL) &&
	        (mkdetections == YES)) {
		call greactivate (id, 0)
		do j = 1, nstars {
		    #call ap_ltov (im, Memr[x+j-1], Memr[y+j-1], xc, yc, 1)
		    #call gmark (id, xc, yc, GM_PLUS, 1.0, 1.0)
		    call gmark (id, Memr[x+j-1], Memr[y+j-1], GM_PLUS, 1.0, 1.0)
		}
		call gdeactivate (id, 0)
	    }

	    switch (apstati (ap, WCSOUT)) {
	    case WCS_PHYSICAL:
		call ap_ltoo (ap, Memr[x], Memr[y], Memr[x], Memr[y], nstars)
	    case WCS_TV:
		call ap_ltov (im, Memr[x], Memr[y], Memr[x], Memr[y], nstars)
	    default:
		;
	    }

	    # Print results on the standard output.
	    if (interactive == YES)
	        call apstdout (Memr[cnvbuf], Memi[bufptrs], ncols, nyk,
	            Memi[cols], Memr[x], Memr[y], Memr[sharp], Memr[round1],
		    Memr[round2], nstars, ntotal, relerr * threshold)

	    # Save the results in the file.
	    call apdtfout (out, Memr[cnvbuf], Memi[bufptrs], ncols, nyk,
	        Memi[cols], Memr[x], Memr[y], Memr[sharp], Memr[round1],
		Memr[round2], nstars, ntotal, relerr * threshold, stid)


	    ntotal = ntotal + nstars

	}

	# Free space
	call sfree (sp)

	return (ntotal)
end


# AP_DETECT -- Detec stellar objects in an image line. In order to be
# detected as a star the candidate object must be above threshold and have
# a maximum pixel value greater than any pixels within nsigma * sigma.

int procedure ap_detect (density, ptrs, ncols, skip, nxk, nyk, threshold, cols)

real	density[ncols, ARB]	# density array
int	ptrs[ARB]		# pointer array
int	ncols			# x dimesnsion of intensity buffer
int	skip[nxk,ARB]		# skip array
int	nxk, nyk		# size of convolution kernel
real	threshold		# density threshold
int	cols[ARB]		# column numbers of detected stars

int	i, j, k, kk, middle, nhalf, nobjs
define	nextpix_	11

begin
	middle = 1 + nyk / 2
	nhalf = nxk / 2

	# Loop over all the columns in an image line.
	nobjs = 0
	for (i = 1 + nhalf; i <= ncols - nhalf; ) {

	    # Test whether the density enhancement is above threshold.
	    if (density[i,ptrs[middle]] < threshold)
		goto nextpix_

	    # Test whether a given density enhancement is a local maximum.
	    do j = 1, nyk {
		kk = 1
	        do k = i - nhalf, i + nhalf {
		    if (skip[kk,j] == NO) {
		        if (density[i,ptrs[middle]] < density[k,ptrs[j]])
		           goto nextpix_
		    }
		    kk = kk + 1
	        }
	    }

	    # Add the detected object to the list.
	    nobjs = nobjs + 1
	    cols[nobjs] = i

	    # If a local maximum is detected there can be no need to
	    # check pixels in this row between i and i + nhalf.
	    i = i + nhalf
nextpix_
	    # Work on the next pixel.
	    i = i + 1
	}

	return (nobjs)
end


# AP_SHARP_ROUND -- Compute an estimate of the roundness and sharpness of the
# detected objects. The roundness parameter is computed by comparing a measure
# of the bilateral symmetry with a measure of the four-fold symmetry. The
# sharpness parameter is defined as the ratio of the difference between the
# height of the central pixel and the mean of the surrounding pixels to the
# density enhancement of the central pixel.

procedure ap_sharp_round (data, density, ptrs, ncols, skip, nxk, nyk, cols,
        satur, round, sharps, nobjs, nonzero, skymode, datamin, datamax) 

real	data[ncols,ARB]		# image data
real	density[ncols,ARB]	# density enhancements
int	ptrs[ARB]		# buffer pointers
int	ncols			# length of data array
int	skip[nxk,ARB]		# 2D kernel
int	nxk, nyk		# size of convolution kernel
int	cols[ARB]		# array of columns
int	satur[ARB]		# array of saturated state parameters
real	round[ARB]		# array of roundness parameters
real	sharps[ARB]		# array of sharpness parameters
int	nobjs			# number of objects
int	nonzero			# number of nonzero kernel elements
real	skymode			# estimate of the sky mode
real	datamin, datamax	# minimum and maximum good data values

int	i, j, k, xmiddle, ymiddle, npixels, nhalf
real	pixval, midpix, temp, sharp, sum2, sum4

begin
	# Loop over the detected objects.
	nhalf = min (nxk / 2, nyk / 2)
	xmiddle = 1 + nxk / 2 
	ymiddle = 1 + nyk / 2
	do i = 1, nobjs {

	    # Compute the first estimate of roundness.
	    sum2 = 0.0
	    sum4 = 0.0
	    do k = 0, nhalf {
		do j = 1, nhalf {
		    sum2 = sum2 +
		        density[cols[i]-k,ptrs[ymiddle-j]] +
		        density[cols[i]+k,ptrs[ymiddle+j]] -
		        density[cols[i]-j,ptrs[ymiddle+k]] -
		        density[cols[i]+j,ptrs[ymiddle-k]]
		    sum4 = sum4 +
		        abs (density[cols[i]-k,ptrs[ymiddle-j]]) +
		        abs (density[cols[i]+k,ptrs[ymiddle+j]]) +
		        abs (density[cols[i]-j,ptrs[ymiddle+k]]) +
		        abs (density[cols[i]+j,ptrs[ymiddle-k]])
		}
	    }
	    if (sum2 == 0.0)
		round[i] = 0.0
	    else if (sum4 <= 0.0)
		round[i] = INDEFR
	    else
	        round[i] = 2.0 * sum2 / sum4

	    satur[i] = NO

	    # Eliminate the sharpness test if the central pixel is bad.
	    midpix = data[cols[i],ptrs[ymiddle]]
	    if (midpix > datamax) {
		satur[i] = YES
		sharps[i] = INDEFR
		next
	    }
	    if (midpix < datamin) {
		sharps[i] = INDEFR
		next
	    }

	    # Accumulate the sharpness statistic.
	    sharp = 0.0
	    npixels = nonzero
	    do j = 1, nyk {
		temp = 0.0
	        do k = 1, nxk {
		    if (skip[k,j] == YES)
			next
		    pixval = data[cols[i]-xmiddle+k,ptrs[j]]
		    if (pixval > datamax) {
			satur[i] = YES
			npixels = npixels - 1
		    } else if (pixval < datamin) {
			npixels = npixels - 1
		    } else {
		        temp = temp + (pixval - skymode)
		    }
		}
		sharp = sharp + temp
	    }

	    # Compute the sharpness statistic.
	    if (density[cols[i],ptrs[ymiddle]] <= 0.0 || npixels <= 0)
		sharps[i] = INDEFR
	    else
	        sharps[i] = (midpix - skymode - sharp / real (npixels)) /
		    density[cols[i],ptrs[ymiddle]]

	}
end


# AP_XY_ROUND -- Estimate the x-y centers and the roundness of the detected
# objects. The height of the equivalent Gaussian function in x and y is fit by
# least squares to the marginal distribution of the image data. If either
# of these of these heights is negative set the roundess characteristic to
# -MAX_REAL, otherwise compute a roundness characteristic. At the same
# time setup the necessary sums for computing the first order corection
# to the centroid of the gaussian profile.

procedure ap_xy_round (data, ptrs, ncols, ker2d, nxk, nyk, cols, inline,
	rounds, x, y, nobjs, skymode, datamin, datamax, xsigsq, ysigsq)

real	data[ncols,ARB]		# density enhancements
int	ptrs[ARB]		# buffer pointers
int	ncols			# number of columns in cylindrical buffer
real	ker2d[nxk,ARB]		# the gaussian convolution kernel
int	nxk			# size of kernel in x
int	nyk			# size of kernel in y
int	cols[ARB]		# the input positions	
int	inline			# the input image line
real	rounds[ARB]		# array of sharpness parameters
real	x[ARB]			# output x coords
real	y[ARB]			# output y coords
int	nobjs			# number of objects
real	skymode			# estimate of the sky mode
real	datamin, datamax	# minium and maximum data values
real	xsigsq, ysigsq		# x-y gaussian sigma squared

int	i, j, k, xmiddle, ymiddle, n
real	sumgd, sumgsq, sumg, sumd, sumdx, dgdx, sdgdx, sdgdxsq, sddgdx, sgdgdx
real	pixval, p, sg, sd, wt, hx, hy, dx, dy, skylvl, xhalf, yhalf

begin
	xhalf = real (nxk / 2) + 0.5
	yhalf = real (nyk / 2) + 0.5
	xmiddle = 1 + nxk / 2
	ymiddle = 1 + nyk / 2

	# Loop over the detected objects.
	do i = 1, nobjs {

	    # Initialize the x fit.
	    sumgd = 0.0
	    sumgsq = 0.0
	    sumg = 0.0
	    sumd = 0.0
	    sumdx = 0.0
	    sdgdx = 0.0
	    sdgdxsq = 0.0
	    sddgdx = 0.0
	    sgdgdx = 0.0
	    p = 0.0
	    n = 0

	    # Compute the sums required for the x fit.
	    do k = 1, nxk {

		sg = 0.0
		sd = 0.0
		do j = 1, nyk {
		    wt = real (ymiddle - abs (j - ymiddle))
		    pixval = data[cols[i]-xmiddle+k,ptrs[j]]
		    if (pixval < datamin || pixval > datamax)
			next
		    sd = sd + (pixval - skymode) * wt
		    sg = sg + ker2d[k,j] * wt
		}

	        if (sg <= 0.0)
		    next
	        wt = real (xmiddle - abs (k - xmiddle))
	        sumgd = sumgd + wt * sg * sd
	        sumgsq = sumgsq + wt * sg ** 2
	        sumg = sumg + wt * sg
	        sumd = sumd + wt * sd
		sumdx = sumdx + wt * sd * (xmiddle - k)
	        p = p + wt
	        n = n + 1
	        dgdx = sg * (xmiddle - k)
	        sdgdxsq = sdgdxsq + wt * dgdx ** 2
	        sdgdx = sdgdx + wt * dgdx
	        sddgdx = sddgdx + wt * sd * dgdx
	        sgdgdx = sgdgdx + wt * sg * dgdx
	    }

	    # Need at least three points to estimate the x height, position
	    # and local sky brightness of the star.

	    if (n <= 2 || p <= 0.0) {
		x[i] = INDEFR
		y[i] = INDEFR
		rounds[i] = INDEFR
		next
	    }

	    # Solve for the height of the best-fitting gaussian to the
	    # xmarginal. Reject the star if the height is non-positive.

	    hx = sumgsq - (sumg ** 2) / p
	    if (hx <= 0.0) {
		x[i] = INDEFR
		y[i] = INDEFR
		rounds[i] = INDEFR
		next
	    }
	    hx = (sumgd - sumg * sumd / p) / hx
	    if (hx <= 0.0) {
		x[i] = INDEFR
		y[i] = INDEFR
		rounds[i] = INDEFR
		next
	    }

	    # Solve for the new x centroid.
	    skylvl = (sumd - hx * sumg) / p
	    dx = (sgdgdx - (sddgdx - sdgdx * (hx * sumg + skylvl * p))) /
		(hx * sdgdxsq / xsigsq)
	    if (abs (dx) > xhalf) {
		if (sumd == 0.0)
		    dx = 0.0
		else
		    dx = sumdx / sumd 
		if (abs (dx) > xhalf)
		    dx = 0.0
	    }
	    x[i] = (cols[i] - xmiddle + 1) + dx 

	    # Initialize y fit.
	    sumgd = 0.0
	    sumgsq = 0.0
	    sumg = 0.0
	    sumd = 0.0
	    sumdx = 0.0
	    sdgdx = 0.0
	    sdgdxsq = 0.0
	    sddgdx = 0.0
	    sgdgdx = 0.0
	    p = 0.0
	    n = 0

	    do j = 1, nyk {
		sg = 0.0
		sd = 0.0
		do k = 1, nxk {
		    wt = real (xmiddle - abs (k - xmiddle))
		    pixval = data[cols[i]-xmiddle+k,ptrs[j]]
		    if (pixval < datamin || pixval > datamax)
			next
		    sd = sd + (pixval - skymode) * wt
		    sg = sg + ker2d[k,j] * wt
		}
	        if (sg <= 0.0)
		    next
	        wt = real (ymiddle - abs (j - ymiddle))
	        sumgd = sumgd + wt * sg * sd
	        sumgsq = sumgsq + wt * sg ** 2
	        sumg = sumg + wt * sg
	        sumd = sumd + wt * sd
		sumdx = sumdx + wt * sd * (j - ymiddle)
	        p = p + wt
	        n = n + 1
	        dgdx = sg * (ymiddle - j)
	        sdgdx = sdgdx + wt * dgdx
	        sdgdxsq = sdgdxsq + wt * dgdx ** 2
	        sddgdx = sddgdx + wt * sd * dgdx
	        sgdgdx = sgdgdx + wt * sg * dgdx
	    }

	    # Need at least three points to estimate the y height, position
	    # and local sky brightness of the star.

	    if (n <= 2 || p <= 0.0) {
		x[i] = INDEFR
		y[i] = INDEFR
		rounds[i] = INDEFR
		next
	    }

	    # Solve for the height of the best-fitting gaussian to the
	    # y marginal. Reject the star if the height is non-positive.

	    hy = sumgsq - (sumg ** 2) / p 
	    if (hy <= 0.0) {
		x[i] = INDEFR
		y[i] = INDEFR
		rounds[i] = INDEFR
		next
	    }
	    hy = (sumgd - sumg * sumd / p) / (sumgsq - (sumg ** 2) / p)
	    if (hy <= 0.0) {
		x[i] = INDEFR
		y[i] = INDEFR
		rounds[i] = INDEFR
		next
	    }

	    # Solve for the new x centroid.
	    skylvl = (sumd - hy * sumg) / p
	    dy = (sgdgdx - (sddgdx - sdgdx * (hy * sumg + skylvl * p))) /
		(hy * sdgdxsq / ysigsq)
	    if (abs (dy) > yhalf) {
		if (sumd == 0.0)
		    dy = 0.0
		else
		    dy = sumdx / sumd 
		if (abs (dy) > yhalf)
		    dy = 0.0
	    }
	    y[i] = (inline - ymiddle + 1) + dy

	    # Compute the roundness.
	    rounds[i] = 2.0 * (hx - hy) / (hx + hy)
	}
end


# AP_TEST -- Test the characteristic of the detected images for roundness
# and sharpness.

int procedure ap_test (cols, x, y, satur, round1, round2, sharps, nobjs,
	ncols, nlines, sharplo, sharphi, roundlo, roundhi)

int	cols[ARB]			# col IDS of detected images
real	x[ARB]				# x positions
real	y[ARB]				# y positions
int	satur[ARB]			# saturation condition
real	round1[ARB]			# first roundness parameters
real	round2[ARB]			# second roundness parameters
real	sharps[ARB]			# sharpness parameters
int	nobjs				# number of objects
int	ncols, nlines			# size of the input image
real	sharplo, sharphi		# sharpness parameters
real	roundlo, roundhi		# roundness parameters

int	i, nstars

begin
	# Loop over the detected objects.
	nstars = 0
	do i = 1, nobjs {

	    # Compute the sharpness statistic
	    if (! IS_INDEFR(sharps[i]) && (sharps[i] < sharplo ||
	        sharps[i] > sharphi))
		next
	    if (IS_INDEFR(round1[i]) || round1[i] < roundlo ||
	        round1[i] > roundhi)
		next
	    if (satur[i] == NO) {
	        if (IS_INDEFR(round2[i]) || round2[i] < roundlo ||
	            round2[i] > roundhi)
		next
	    }
	    if (IS_INDEFR(x[i]) || x[i] < 0.5 || x[i] > (ncols+0.5))
		next
	    if (IS_INDEFR(y[i]) || y[i] < 0.5 || y[i] > (nlines+0.5))
		next

	    # Add object to the list.
	    nstars = nstars + 1
	    cols[nstars] = cols[i]
	    x[nstars] = x[i]
	    y[nstars] = y[i]
	    sharps[nstars] = sharps[i]
	    round1[nstars] = round1[i]
	    round2[nstars] = round2[i]
	}

	return (nstars)
end
