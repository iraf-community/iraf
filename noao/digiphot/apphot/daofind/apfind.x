include <mach.h>
include <imhdr.h>
include <gset.h>

# AP_FIND -- Detect images in the convolved image and then compute image
# characteristics using the original image.

int procedure ap_find (im, cnv, out, id, ker1x, ker1y, skip, nxk, nyk,
	threshold, emission, sharplo, sharphi, roundlo, roundhi, interactive,
	stid, mkdetections)

pointer	im			# pointer to the input image
pointer	cnv			# pointer to the output image
pointer	out			# pointer to the output text file
pointer	id			# pointer to the display stream
real	ker1x[ARB]		# 1D X Gaussian kernel
real	ker1y[ARB]		# 1D Y Gaussian kernel
int	skip[nxk,ARB]		# 2D Gaussian kernel
int	nxk, nyk		# dimensions of the kernel
real	threshold		# threshold for image detection
int	emission		# emission features
real	sharplo, sharphi	# sharpness limits
real	roundlo,roundhi		# roundness parameter limits
int	interactive		# interactive mode
int	stid			# sequence number
int	mkdetections		# mark detections

int	inline, i, j, ncols, col1, col2, line1, line2, index, pos
int	xmiddle, ymiddle, nonzero, nobjs, nstars, ntotal
pointer	sp, bufptrs, imlbuf, cnvlbuf, imbuf, cnvbuf, cols, sharp, round, x, y
int	ap_detect(), ap_test()
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
	skip[xmiddle,ymiddle] = NO
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
	call salloc (sharp, ncols, TY_REAL)
	call salloc (round, ncols, TY_REAL)
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
	        nyk, threshold, Memi[cols])
	    if (nobjs <= 0)
		next

	    # Compute the sharpness parameter.
	    call ap_sharp (Memr[imbuf], Memr[cnvbuf], Memi[bufptrs], ncols,
	        skip, nxk, nyk, Memi[cols], Memr[sharp], nobjs, nonzero) 

	    # Compute the roundness parameters.
	    call ap_round (Memr[imbuf], Memi[bufptrs], ncols, ker1x, nxk,
	        ker1y, nyk, Memi[cols], Memr[round], nobjs)

	    # Compute the x and y centroid positions.
	    call ap_xypos (Memr[imbuf], Memi[bufptrs], ncols, nxk, nyk,
	        Memi[cols], Memr[x], Memr[y], nobjs, inline)

	    # Test the image characeteristics of detected objects.
	    nstars = ap_test (Memi[cols], Memr[x], Memr[y], Memr[round],
	        Memr[sharp], nobjs, sharplo, sharphi, roundlo, roundhi)

	    # Print results on the standard output.
	    if (interactive == YES)
	        call apstdout (Memr[cnvbuf], Memi[bufptrs], ncols, nyk,
	            Memi[cols], Memr[x], Memr[y], Memr[sharp], Memr[round],
		    nstars, ntotal, threshold)

	    # Save the results in the file.
	    call apdtfout (out, Memr[cnvbuf], Memi[bufptrs], ncols, nyk,
	        Memi[cols], Memr[x], Memr[y], Memr[sharp], Memr[round],
		nstars, ntotal, threshold, stid)

	    # Mark the stars on the display.
	    if ((nstars > 0) && (interactive == YES) && (id != NULL) &&
	        (mkdetections == YES)) {
		call greactivate (id, 0)
		do j = 1, nstars
		    call gmark (id, Memr[x+j-1], Memr[y+j-1], GM_PLUS, 1.0, 1.0)
		call gdeactivate (id, 0)
	    }

	    ntotal = ntotal + nstars

	}
	 
	# free space
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
	    # i = i + nhalf
nextpix_
	    # Work on the next pixel.
	    i = i + 1
	}

	return (nobjs)
end


# AP_SHARP -- Compute an estimate of the sharpness of the detected
# objects. The sharpness parameter is defined as the ratio of the difference
# between the height of the central pixel and the mean of the surrounding
# pixels to the density enhancement of the central pixel.

procedure ap_sharp (data, density, ptrs, ncols, skip, nxk, nyk, cols, sharps,
        nobjs, nonzero) 

real	data[ncols,ARB]		# image data
real	density[ncols,ARB]	# density enhancements
int	ptrs[ARB]		# buffer pointers
int	ncols			# length of data array
int	skip[nxk,ARB]		# 2D kernel
int	nxk, nyk		# size of convolution kernel
int	cols[ARB]		# columns
real	sharps[ARB]		# array of sharpness parameters
int	nobjs			# number of objects
int	nonzero			# number of nonzero kernel elements

int	i, j, k, xmiddle, ymiddle
real	sharp

begin
	# Loop over the detected objects.
	xmiddle = 1 + nxk / 2 
	ymiddle = 1 + nyk / 2
	do i = 1, nobjs {

	    # Accumulate the sharpness statistic.
	    sharp = 0.0
	    do j = 1, nyk {
	        do k = 1, nxk {
		    if (skip[k,j] == NO)
		        sharp = sharp + data[cols[i]-xmiddle+k,ptrs[j]]
		}
	    }

	    # Compute the sharpness statistic.
	    if (density[cols[i],ptrs[ymiddle]] <= 0.0)
		sharps[i] = MAX_REAL
	    else
	        sharps[i] = (data[cols[i],ptrs[ymiddle]] - sharp / (nonzero)) /
		    density[cols[i],ptrs[ymiddle]]

	}
end


# AP_ROUND -- Estimate the roundness of the detected objects.
# The height of the equivalent Gaussian function in x and y is fit by
# least squares to the marginals. If either of these of these heights
# is negative set the roundess characteristic to -MAX_REAL. Otherwise
# compute a roundness characteristic

procedure ap_round (data, ptrs, ncols, ker1x, nxk, ker1y, nyk, cols, rounds,
        nobjs)

real	data[ncols,ARB]		# density enhancements
int	ptrs[ARB]		# buffer pointers
int	ncols			# number of columns in cylindrical buffer
real	ker1x[ARB]		# 1D X kernel
int	nxk			# size of X kernel
real	ker1y[ARB]		# 1D Y kernel
int	nyk			# size of Y kernel
int	cols[ARB]		# columns
real	rounds[ARB]		# array of sharpness parameters
int	nobjs			# number of objects

int	i, j, k, middle
real	dx, dy

begin
	# Loop over the objects.
	middle = 1 + nxk / 2
	do i = 1, nobjs {

	    # Accumulate the roundness statistic.
	    dx = 0.0
	    dy = 0.0
	    do j = 1, nyk {
		do k = 1, nxk {
		    dx = dx + data[cols[i]-middle+k,ptrs[j]] * ker1x[k]
		    dy = dy + data[cols[i]-middle+k,ptrs[j]] * ker1y[j]
		}
	    }

	    # Compute the roundness statistic.
	    if (dx <= 0.0 || dy <= 0.0)
		rounds[i] = -MAX_REAL
	    else
	        rounds[i] = 2.0 * (dx - dy) / (dx + dy)
	}
end


# AP_XYPOSS -- Compute the x and y centroids of the star. The computation
# is done using an nx * (ny - 2) box to determine the x center and
# an (nx - 2) * ny box to determine the y center. The (nx(y) - 2) pixels
# in the perpendicular direction are added together and adjacent pixels
# in the parallel direction are subtracted to yield (nx(y) - 1) numerical
# first derivatives at positions delta x (or delta y) = - (nx(y) - 2) / 2, ...,
# -1.5,-0.5,0.5,1.5,...,(nx(y) - 2) / 2 pixels from the center of the
# array. A straight line is fitted to the derivatives by least squares
# with with weight one-half at the end, unity in the middle, and linear
# inbetween. The zero crossing of the line is taken to be the centroid.
# If the slope of the line is non-negative or the coordinate is outside
# the box set the dx and dy values to zero.

procedure ap_xypos (data, ptrs, ncols, nxk, nyk, cols, x, y, nobjs, inline)

real	data[ncols,ARB]		# density enhancements
int	ptrs[ARB]		# buffer pointers
int	ncols			# number of columns in cylindrical buffer
int	nxk, nyk		# size of convolution kernel
int	cols[ARB]		# columns
real	x[ARB]			# output x coords
real	y[ARB]			# output y coords
int	nobjs			# number of objects
int	inline			# input line number

int	i, j, k, xmiddle, ymiddle, xnhalf, ynhalf, xnhalfm1, ynhalfm1
real	sumd, sumxd, sumxsq, sumc, deriv, w, dx, dy

begin
	xmiddle = 1 + nxk / 2
	ymiddle = 1 + nyk / 2
	xnhalf = nxk / 2
	ynhalf = nyk / 2
	xnhalfm1 = xnhalf - 1
	ynhalfm1 = ynhalf - 1

	# Loop over all the columns in an image line.
	do i = 1, nobjs {

	    # Zero the x accumulators.
	    sumd = 0.0
	    sumxd = 0.0
	    sumxsq = 0.0
	    sumc = 0.0

	    # Accumulate the x derivatives.
	    do j = 1, nxk - 1 {
		deriv = 0.0
		dx = j + 0.5 - xmiddle
		w = 1.0 - 0.5 * (abs (dx) - 0.5) / (xmiddle - 1.5)
		do k = ymiddle - ynhalfm1, ymiddle + ynhalfm1
		    deriv = deriv + (data[cols[i]-xmiddle+j+1,ptrs[k]] -
		        data[cols[i]-xmiddle+j,ptrs[k]])
		sumd = sumd + w * deriv
		sumxd = sumxd + w * dx * deriv
		sumxsq = sumxsq + w * dx ** 2
		sumc = sumc + w
	    }

	    # Reject if x derivative not decreasing.
	    if (sumxd >= 0.0) {
		x[i] = 0.0
		next
	    }

	    # Test whether x centroid is inside the box.
	    dx = sumxsq * sumd / (sumc * sumxd)
	    if (abs (dx) > real (xnhalf)) {
		x[i] = 0.0
		next
	    }
	    x[i] = cols[i] - xnhalf - dx

	    # Zero the y accumulators.
	    sumd = 0.0
	    sumxd = 0.0
	    sumxsq = 0.0
	    sumc = 0.0

	    # Accumlate the y derivatives.
	    do j = 1, nyk - 1 {
		deriv = 0.0
		dy = j + 0.5 - ymiddle
		w = 1.0 - 0.5 * (abs (dy) - 0.5) / (ymiddle - 1.5)
		do k = cols[i] - xnhalfm1, cols[i] + xnhalfm1
		    deriv = deriv + (data[k,ptrs[j+1]] - data[k,ptrs[j]])
		sumd = sumd + w * deriv
		sumxd = sumxd + w * dy * deriv
		sumxsq = sumxsq + w * dy ** 2
		sumc = sumc + w
	    }

	    # Reject if y derivative not decreasing.
	    if (sumxd >= 0.0) {
		y[i] = 0.0
		next
	    }

	    # Test whether y centroid is inside the box.
	    dy = sumxsq * sumd / (sumc * sumxd)
	    if (abs (dy) > real (ynhalf)) {
		y[i] = 0.0
		next
	    }
	    y[i] = inline - ynhalf - dy
	}
end


# AP_TEST -- Test the characteristic of the detected images for roundness
# and sharpness.

int procedure ap_test (cols, x, y, rounds, sharps, nobjs, sharplo, sharphi,
        roundlo, roundhi)

int	cols[ARB]			# col IDS of detected images
real	x[ARB]				# x positions
real	y[ARB]				# y positions
real	rounds[ARB]			# roundness parameters
real	sharps[ARB]			# sharpness parameters
int	nobjs				# number of objects
real	sharplo, sharphi		# sharpness parameters
real	roundlo, roundhi		# roundness parameters

int	i, nstars

begin
	# Loop over the detected objects.
	nstars = 0
	do i = 1, nobjs {

	    # Compute the sharpness statistic
	    if ((sharps[i] < sharplo) || (sharps[i] > sharphi))
		next
	    if (rounds[i] < roundlo || rounds[i] > roundhi)
		next
	    if (x[i] <= 0.0 || y[i] <= 0.0)
		next

	    # Add object to the list.
	    nstars = nstars + 1
	    cols[nstars] = cols[i]
	    x[nstars] = x[i]
	    y[nstars] = y[i]
	    sharps[nstars] = sharps[i]
	    rounds[nstars] = rounds[i]
	}

	return (nstars)
end
