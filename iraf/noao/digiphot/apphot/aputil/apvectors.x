
# AP_ABOXR -- Vector boxcar smooth. The input vector is convolved with a
# uniform kernel of length knpix. Boundary extension is assumed to have
# been performed on the input array before entering the routine.

procedure ap_aboxr (in, out, npix, knpix)

real	in[npix+knpix-1]	# input array with boundary exntension
real	out[npix]		# output array
size_t	npix			# number of pixels
size_t	knpix			# length of the kernel

long	i
real	sum

begin
	sum = 0.0
	do i = 1, knpix - 1
	    sum = sum + in[i]

	do i = 1, npix {
	    sum = sum + in[i+knpix-1] 
	    out[i] = sum / knpix
	    sum = sum - in[i]
	}

end


# AP_SBOXR -- Boxcar smooth for a vector that has not been boundary
# extended.

procedure ap_sboxr (in, out, npix, nsmooth)

real	in[npix]		# input array 
real	out[npix]		# output array
size_t	npix			# number of pixels
size_t	nsmooth			# half width of smoothing box

long	i, j, ib, ie, ns
real	sum

begin
	ns = 2 * nsmooth + 1
	do i = 1, npix {
	    ib = max (i - nsmooth, 1)
	    ie = min (i + nsmooth, npix)
	    sum = 0.0
	    do j = ib, ie
	        sum = sum + in[j]
	    out[i] = sum / ns
	}
end


# AP_ALIMR -- Compute the maximum and minimum data values and indices of a
# 1D array.

procedure ap_alimr (data, npts, mindat, maxdat, imin, imax)

real	data[npts]	# data array
size_t	npts		# number of points
real	mindat, maxdat	# min and max data value
long	imin, imax	# indices of min and max data values

long	i

begin
	imin = 1
	imax = 1
	mindat = data[1]
	maxdat = data[1]

	do i = 2, npts {
	    if (data[i] > maxdat) {
		imax = i
		maxdat = data[i]
	    }
	    if (data[i] < mindat) {
		imin = i
		mindat = data[i]
	    }
	}
end


# AP_IALIMR -- Compute the maximum and minimum data values of a 1D indexed
# array

procedure ap_ialimr (data, index, npts, mindat, maxdat)

real	data[npts]	# data array
long	index[npts]	# index array
size_t	npts		# number of points
real	mindat, maxdat	# min and max data value

long	i

begin
	mindat = data[index[1]]
	maxdat = data[index[1]]
	do i = 2, npts {
	    if (data[index[i]] > maxdat)
		maxdat = data[index[i]]
	    if (data[index[i]] < mindat)
		mindat = data[index[i]]
	}
end


# AP_ASUMR - Compute the sum of an index sorted array

real procedure ap_asumr (data, index, npts)

real	data[npts]	# data array
long	index[npts]	# index array
size_t	npts		# number of points

double	sum
long	i

begin
	sum = 0.0d0
	do i = 1, npts
	    sum = sum + data[index[i]]
	return (real (sum))
end


# AP_AMAXEL -- Find the maximum value and its index of a 1D array.

procedure ap_amaxel (a, npts, maxdat, imax)

real	a[ARB]		# the data array
size_t	npts		# number of points
real	maxdat		# maximum value
long	imax		# imdex of max value

long	i

begin
	maxdat = a[1]
	imax = 1
	do i = 2, npts {
	    if (a[i] > maxdat) {
		maxdat = a[i]
		imax = i
	    }
	}
end


# AHGMR -- Accumulate the histogram of the input vector.  The output vector
# hgm (the histogram) should be cleared prior to the first call. The procedure
# returns the number of data values it could not include in the histogram.

long procedure aphgmr (data, wgt, npix, hgm, nbins, z1, z2)

real 	data[ARB]		# data vector
real	wgt[ARB]		# weights vector
size_t	npix			# number of pixels
real	hgm[ARB]		# output histogram
size_t	nbins			# number of bins in histogram
real	z1, z2			# greyscale values of first and last bins

real	dz
long	bin, i, nreject
long	lint()

begin
	if (nbins < 2)
	    return (0)

	nreject = 0
	dz = real (nbins - 1) / real (z2 - z1)
	do i = 1, npix {
	    if (data[i] < z1 || data[i] > z2) {
		nreject = nreject + 1
		wgt[i] = 0.0
		next
	    }
	    bin = lint ((data[i] - z1) * dz) + 1
	    hgm[bin] = hgm[bin] + 1.0
	}

	return (nreject)
end


# APHIGMR -- Accumulate the histogram of the input vector.  The output vector
# hgm (the histogram) should be cleared prior to the first call. The procedure
# returns the number of data values it could not include in the histogram.

long procedure aphigmr (data, wgt, index, npix, hgm, nbins, z1, z2)

real 	data[ARB]		# data vector
real	wgt[ARB]		# weights vector
long	index[ARB]		# index vector
size_t	npix			# number of pixels
real	hgm[ARB]		# output histogram
size_t	nbins			# number of bins in histogram
real	z1, z2			# greyscale values of first and last bins

real	dz
long	bin, i, nreject
long	lint()

begin
	if (nbins < 2)
	    return (0)

	nreject = 0
	dz = real (nbins - 1) / real (z2 - z1)
	do i = 1, npix {
	    if (data[index[i]] < z1 || data[index[i]] > z2) {
		nreject = nreject + 1
		wgt[index[i]] = 0.0
		next
	    }
	    bin = lint ((data[index[i]] - z1) * dz) + 1
	    hgm[bin] = hgm[bin] + 1.0
	}

	return (nreject)
end


# AP_IJTOR -- Compute radius values given the center coordinates, the line
# number in a the subraster and the length of the subraster x axis.

procedure ap_ijtor (r, nr, line, xc, yc)

real	r[nr]		# array of output r values
size_t	nr		# length of r array
long	line		# line number
real	xc, yc		# subraster center

long	i
real	temp

begin
	temp = (line - yc ) ** 2
	do i = 1, nr
	    r[i] = sqrt ((i - xc) ** 2 + temp)
end


# AP_IJTOR2 -- Compute radius values given the center coordinates and the size
# of the subraster.

procedure ap_ijtor2 (r, nx, ny, xc, yc)

real	r[nx,ARB]	# array of output r values
size_t	nx		# x dimension of output array
size_t	ny		# Y dimension of output array
real	xc, yc		# subraster center

long	i, j
real	temp

begin
	do j = 1, ny {
	    temp = (j - yc) ** 2
	    do i = 1, nx
	        r[i,j] = sqrt ((i - xc) ** 2 + temp)
	}
end


# AP_2DALIMR -- Compute min and max values of a 2D array along with
# the indices of the minimum and maximum pixel.

procedure ap_2dalimr (data, nx, ny, mindat, maxdat, imin, jmin, imax, jmax)

real	data[nx, ny]	# data
size_t	nx, ny		# array dimensions
real	mindat, maxdat	# min, max data values
long	imin, jmin	# indices of min element
long	imax, jmax	# indices of max element

long	i, j

begin
	imin = 1
	jmin = 1
	imax = 1
	jmax = 1
	mindat = data[1,1]
	maxdat = data[1,1]

	do j = 2, ny {
	    do i = 1, nx {
		if (data[i,j] < mindat) {
		    imin = i
		    jmin = j
		    mindat = data[i,j]
		} else if (data[i,j] > maxdat) {
		    imax = i
		    jmax = j
		    maxdat = data[i,j]
		}
	    }
	}
end


define	LOGPTR		20			# log2(maxpts) (1e6)

# APQSORT -- Vector Quicksort. In this version the index array is
# sorted.

procedure apqsort (data, a, b, npix)

real	data[ARB]		# data array
long	a[ARB], b[ARB]		# index array
size_t	npix			# number of pixels

long	i, j, lv[LOGPTR], uv[LOGPTR], temp
int	p
real	pivot

begin
	# Initialize the indices for an inplace sort.
	do i = 1, npix
	    a[i] = i
	call amovl (a, b, npix)

	p = 1
	lv[1] = 1
	uv[1] = npix
	while (p > 0) {

	    # If only one elem in subset pop stack otherwise pivot line.
	    if (lv[p] >= uv[p])
		p = p - 1
	    else {
		i = lv[p] - 1
		j = uv[p]
		pivot = data[b[j]]

		while (i < j) {
		    for (i=i+1;  data[b[i]] < pivot;  i=i+1)
			;
		    for (j=j-1;  j > i;  j=j-1)
			if (data[b[j]] <= pivot)
			    break
		    if (i < j) {		# out of order pair
			temp = b[j]		# interchange elements
			b[j] = b[i]
			b[i] = temp
		    }
		}

		j = uv[p]			# move pivot to position i
		temp = b[j]			# interchange elements
		b[j] = b[i]
		b[i] = temp

		if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
		    lv[p+1] = lv[p]
		    uv[p+1] = i - 1
		    lv[p] = i + 1
		} else {
		    lv[p+1] = i + 1
		    uv[p+1] = uv[p]
		    uv[p] = i - 1
		}

		p = p + 1			# push onto stack
	    }
	}
end



# APRECIPROCAL -- Compute the reciprocal of the absolute value of a vector.

procedure apreciprocal (a, b, npts, value)

real	a[ARB]		# the input vector
real	b[ARB]		# the output vector
size_t	npts		# the number of data points
real	value		# the value to be assigned to b[i] if a[i] = 0

long	i

begin
	do i = 1, npts {
	    if (a[i] > 0.0)
		b[i] = 1.0 / a[i]
	    else if (a[i] < 0.0)
		b[i] = - 1.0 / a[i]
	    else
		b[i] = value
	}
end


# AP_WLIMR -- Set the weights of all data points outside a given minimum and
# maximum values to zero. Compute the minimum and maximum values and their
# indices of the remaining data.

procedure ap_wlimr (pix, w, npts, datamin, datamax, dmin, dmax, imin, imax)

real	pix[ARB]		# input pixel array
real	w[ARB]			# weight array
size_t	npts			# number of points
real	datamin			# minimum good data point
real	datamax			# maximum good data point
real	dmin			# output data minimum
real	dmax			# output data maximum
long	imin			# index of the data minimum
long	imax			# index of the data maximum

long	i
real	value

begin
	dmin = datamax
	dmax = datamin
	imin = 1
	imax = 1

	do i = 1, npts {
	    value = pix[i]
	    if ((value < datamin) || (value > datamax)) {
		w[i] = 0.0
		next
	    }
	    if (value  < dmin) {
		dmin = value
		imin = i
	    } else if (value  > dmax) {
		dmax = value
		imax = i
	    }
	}
end


# APWSSQR -- Compute the weighted sum of the squares of a vector.

real procedure apwssqr (a, wgt, npts)

real	a[ARB]		# data array
real	wgt[ARB]	# array of weights
size_t	npts		# number of points

long	i
real	sum

begin
	sum = 0.0
	do i = 1, npts
	    sum = sum + wgt[i] * a[i] * a[i]
	return (sum)
end


# AP_XYTOR -- Change the single integer coord of the sky pixels to a radial
# distance value. The integer coordinate is equal to coord = (i - xc + 1) +
# blklen * (j - yc).

procedure ap_xytor (coords, index, r, nskypix, xc, yc, blklen)

long	coords[ARB]		# coordinate array
long	index[ARB]		# the index array
real	r[ARB]			# radial coordinates
size_t	nskypix			# number of sky pixels
real	xc, yc			# center of sky subraster
size_t	blklen			# x dimension of sky subraster

long	i, l_blklen
real	x, y
long	lmod()

begin
	l_blklen = blklen
	do i = 1, nskypix {
	    x = real (lmod (coords[index[i]], l_blklen))
	    if (x == 0)
		x = blklen
	    y = (coords[index[i]] - x) / blklen + 1 
	    r[i] = sqrt ((x - xc) ** 2 + (y - yc) ** 2)
	}
end


# AP_W1SUR -- Linearly combine two vectors where the weight for the first
# vectors is 1.0 and is k2 for the second vector

procedure ap_w1sur (a, b, c, npts, k2)

real	a[ARB]		# the first input vector
real	b[ARB]		# the second input vector
real	c[ARB]		# the output vector
size_t	npts		# number of points
real	k2		# the weigting factor for the second vector

long	i

begin
	do i = 1, npts
	    c[i] = a[i] + k2 * b[i]
end


# AP_INDEX -- Define an index array.

procedure ap_index (index, npix)

long	index[ARB]		# the index array
size_t	npix			# the number of pixels

long	i

begin
	do i = 1, npix
	    index[i] = i
end
