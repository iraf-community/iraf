# AP_GROW_HIST -- Procedure to reject pixels with region growing.

int procedure ap_grow_hist (skypix, coords, wgt, nskypix, index, snx, sny, hgm,
    nbins, z1, z2, rgrow)

real	skypix[ARB]		# sky pixels
int	coords[ARB]		# sky pixel coordinates
real	wgt[ARB]		# array of weights
int	nskypix			# number of sky pixels
int	index			# index of pixel to be rejected
int	snx, sny		# size of sky subraster
real	hgm[ARB]		# histogram
int	nbins			# number of bins
real	z1, z2			# value of first and last histogram bin
real	rgrow			# region growing radius

int	j, k, ixc, iyc, ymin, ymax, xmin, xmax, nreject, cstart, c, bin
real	dh, r2, rgrow2, d

begin
	# Find the x and y coordinates of the pixel to be rejected.
	ixc = mod (coords[index], snx)
	if (ixc == 0)
	    ixc = snx
	iyc = (coords[index] - ixc) / snx + 1

	# Find the coordinate space to be used for regions growing.
	ymin = max (1, int (iyc - rgrow))
	ymax = min (sny, int (iyc + rgrow))
	xmin = max (1, int (ixc - rgrow))
	xmax = min (snx, int (ixc + rgrow))
	if (ymin <= iyc)
	    cstart = min (nskypix, max (1, index - int (rgrow) + snx *
	        (ymin - iyc)))
	else
	    cstart = index

	# Perform the region growing.
	dh = real (nbins - 1) / (z2 - z1)
	rgrow2 = rgrow ** 2
	nreject = 0
	do j = ymin, ymax {
	    d = rgrow2 - (j - iyc) ** 2
	    if (d <= 0.0)
		d = 0.0
	    else
		d = sqrt (d)
	    do k = max (xmin, int (ixc - d)), min (xmax, int (ixc + d)) {
		c = k + (j - 1) * snx
		while (coords[cstart] < c && cstart < nskypix)
		    cstart = cstart + 1
		r2 = (k - ixc) ** 2 + (j - iyc) ** 2
		if (r2 <= rgrow2 && c == coords[cstart] && wgt[cstart] > 0.0) {
		    nreject = nreject + 1
		    wgt[cstart] = 0.0
		    if (skypix[cstart] >= z1 && skypix[cstart] <= z2) {
		        bin = int ((skypix[cstart] - z1) * dh) + 1
		        hgm[bin] = hgm[bin] - 1.0
		    }
		}
	    }
	}

	return (nreject)
end


# AP_GROW_HIST2 -- Procedure to reject pixels with region growing.

int procedure ap_grow_hist2 (skypix, coords, wgt, nskypix, sky_zero, index,
    snx, sny, hgm, nbins, z1, z2, rgrow, sumpx, sumsqpx, sumcbpx)

real	skypix[ARB]		# sky pixels
int	coords[ARB]		# coordinates
real	wgt[ARB]		# array of weights
int	nskypix			# number of sky pixels
real	sky_zero		# the sky zero point for moment analysis
int	index			# index of pixel to be rejected
int	snx, sny		# size of sky subraster
real	hgm[ARB]		# histogram
int	nbins			# number of bins
real	z1, z2			# value of first and last histogram bin
real	rgrow			# region growing radius
double	sumpx			# sum of sky values
double	sumsqpx			# sum of sky values squared
double	sumcbpx			# sum of sky values cubed

double	dsky
int	j, k, ixc, iyc, ymin, ymax, xmin, xmax, nreject, cstart, c, bin
real	dh, r2, rgrow2, d

begin
	# Find the coordinates of the region growing center.
	ixc = mod (coords[index], snx)
	if (ixc == 0)
	    ixc = snx
	iyc = (coords[index] - ixc) / snx + 1

	ymin = max (1, int (iyc - rgrow))
	ymax = min (sny, int (iyc + rgrow))
	xmin = max (1, int (ixc - rgrow))
	xmax = min (snx, int (ixc + rgrow))
	dh = real (nbins - 1) / (z2 - z1)
	if (ymin <= iyc)
	    cstart = min (nskypix, max (1, index - int (rgrow) + snx *
	        (ymin - iyc)))
	else
	    cstart = index

	# Perform the region growing.
	nreject = 0
	rgrow2 = rgrow ** 2
	do j = ymin, ymax {
	    d = sqrt (rgrow2 - (j - iyc) ** 2)
	    do k = max (xmin, int (ixc - d)), min (xmax, int (ixc + d)) {
		c = k + (j - 1) * snx
		while (coords[cstart] < c && cstart < nskypix)
		    cstart = cstart + 1
		r2 = (k - ixc) ** 2 + (j - iyc) ** 2
		if (r2 <= rgrow2 && c == coords[cstart] && wgt[cstart] > 0.0) {
		    nreject = nreject + 1
		    wgt[cstart] = 0.0
		    dsky = skypix[cstart] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		    if (skypix[cstart] >= z1 && skypix[cstart] <= z2) {
		        bin = int ((skypix[cstart] - z1) * dh) + 1
		        hgm[bin] = hgm[bin] - 1.0
		    }
		}
	    }
	}

	return (nreject)
end
