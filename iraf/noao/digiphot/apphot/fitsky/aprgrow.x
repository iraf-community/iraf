# AP_GROW_REGIONS -- Perform region growing around a rejected pixel.

long procedure ap_grow_regions (skypix, coords, wgt, nskypix, sky_zero,
	index, snx, sny, rgrow, sumpx, sumsqpx, sumcbpx)

real	skypix[ARB]		# sky pixels
long	coords[ARB]		# sky cordinates
real	wgt[ARB]		# weights
size_t	nskypix			# total number of sky pixels
real	sky_zero		# sky zero point for moment analysis
long	index			# index of pixel to be rejected
size_t	snx, sny		# size of sky subraster
real	rgrow			# region growing radius
double	sumpx, sumsqpx, sumcbpx	# sum and sum of squares of sky pixels

double	dsky
long	j, k, ixc, iyc, xmin, xmax, ymin, ymax, cstart, c, nreject, l_val
real	rgrow2, r2, d
long	lint(), lmod()

begin
	# Find the center of the region to be rejected.
	l_val = snx
	ixc = lmod (coords[index], l_val)
	if (ixc == 0)
	    ixc = snx
	iyc = (coords[index] - ixc) / snx + 1

	# Define the region to be searched.
	rgrow2 = rgrow ** 2
	ymin = max (1, lint (iyc - rgrow)) 
	ymax = min (sny, lint (iyc + rgrow))
	xmin = max (1, lint (ixc - rgrow))
	xmax = min (snx, lint (ixc + rgrow)) 
	if (ymin <= iyc)
	    cstart = min (nskypix, max (1, index - lint (rgrow) + snx *
	    (ymin - iyc)))
	else
	    cstart = index

	# Reject the pixels.
	nreject = 0
	do j = ymin, ymax {
	    d = rgrow2 - (j - iyc) ** 2
	    if (d <= 0.0)
		d = 0.0
	    else
		d = sqrt (d)
	    do k = max (xmin, lint (ixc - d)), min (xmax, lint (ixc + d)) {
		c = k + (j - 1) * snx
	        while (coords[cstart] < c && cstart < nskypix)
	    	    cstart = cstart + 1
		r2 = (k - ixc) ** 2 + (j - iyc) ** 2
		if (r2 <= rgrow2 && c == coords[cstart] && wgt[cstart] > 0.0) {
		    dsky = skypix[cstart] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
		    nreject = nreject + 1
		    wgt[cstart] = 0.0
		}
	    }
	}

	return (nreject)
end
