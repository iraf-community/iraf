# APMOMENTS -- Procedure to compute the first three moments of a
# distribution given the appropriate sums.

procedure apmoments (sumpx, sumsqpx, sumcbpx, npix, sky_zero, mean, sigma, skew)

double	sumpx		# sum of the pixels
double	sumsqpx		# sum of pixels squared
double	sumcbpx		# sum of cubes of pixels
int	npix		# number of pixels
real	sky_zero	# sky zero point for moment analysis
real	mean		# mean of pixels
real	sigma		# sigma of pixels
real	skew		# skew of pixels

double	dmean, dsigma, dskew

begin
	# Recompute the moments.
	dmean = sumpx / npix
	dsigma = sumsqpx / npix - dmean ** 2
	if (dsigma <= 0.0d0) {
	    sigma = 0.0
	    skew = 0.0
	} else {
	    dskew = sumcbpx / npix - 3.0d0 * dmean * dsigma - dmean ** 3
	    sigma = sqrt (dsigma)
	    if (dskew < 0.0d0)
	        skew = - (abs (dskew) ** (1.0d0 / 3.0d0))
	    else if (dskew > 0.0d0)
	        skew = dskew ** (1.0d0 / 3.0d0)
	    else
		skew = 0.0
	}
	mean = sky_zero + dmean
end


# APFMOMENTS -- Procedure to compute the first three moments of a distribution
# given the data. The sums are returned as well.

procedure apfmoments (pix, npix, sky_zero, sumpx, sumsqpx, sumcbpx, mean,
	sigma, skew)

real	pix[npix]	# array of pixels
int	npix		# number of pixels
real	sky_zero	# sky zero point for moment analysis
double	sumpx		# sum of the pixels
double	sumsqpx		# sum of pixels squared
double	sumcbpx		# sum of cubes of pixels
real	mean		# mean of pixels
real	sigma		# sigma of pixels
real	skew		# skew of pixels

double	dpix, dmean, dsigma, dskew
int	i

begin
	# Zero and accumulate the sums.
	sumpx = 0.0d0
	sumsqpx = 0.0d0
	sumcbpx = 0.0d0
	do i = 1, npix {
	    dpix = pix[i] - sky_zero
	    sumpx = sumpx + dpix
	    sumsqpx = sumsqpx + dpix * dpix
	    sumcbpx = sumcbpx + dpix * dpix * dpix
	}

	# Compute the moments.
	dmean = sumpx / npix
	dsigma = sumsqpx / npix - dmean ** 2
	if (dsigma <= 0.0) {
	    sigma = 0.0
	    skew = 0.0
	} else {
	    dskew = sumcbpx / npix - 3.0d0 * dmean * dsigma - dmean ** 3
	    sigma = sqrt (dsigma)
	    if (dskew < 0.0d0)
	        skew = - (abs (dskew) ** (1.0d0 / 3.0d0))
	    else if (dskew > 0.0d0)
	        skew = dskew ** (1.0d0 / 3.0d0)
	    else
		skew = 0.0
	}
	mean = sky_zero + dmean
end


# APFIMOMENTS -- Procedure to compute the first three moments of a distribution
# given the data. The sums are returned as well.

procedure apfimoments (pix, index, npix, sky_zero, sumpx, sumsqpx, sumcbpx,
	mean, sigma, skew)

real	pix[ARB]	# array of pixels
int	index[ARB]	# the index array
int	npix		# number of pixels
real	sky_zero	# sky zero point for moment analysis
double	sumpx		# sum of the pixels
double	sumsqpx		# sum of pixels squared
double	sumcbpx		# sum of cubes of pixels
real	mean		# mean of pixels
real	sigma		# sigma of pixels
real	skew		# skew of pixels

double	dpix, dmean, dsigma, dskew
int	i

begin
	# Zero and accumulate the sums.
	sumpx = 0.0d0
	sumsqpx = 0.0d0
	sumcbpx = 0.0d0
	do i = 1, npix {
	    dpix = pix[index[i]] - sky_zero
	    sumpx = sumpx + dpix
	    sumsqpx = sumsqpx + dpix * dpix
	    sumcbpx = sumcbpx + dpix * dpix * dpix
	}

	# Compute the moments.
	dmean = sumpx / npix
	dsigma = sumsqpx / npix - dmean ** 2
	if (dsigma <= 0.0d0) {
	    sigma = 0.0
	    skew = 0.0
	} else {
	    dskew = sumcbpx / npix - 3.0d0 * dmean * dsigma - dmean ** 3
	    sigma = sqrt (dsigma)
	    if (dskew < 0.0d0)
	        skew = - (abs (dskew) ** (1.0d0 / 3.0d0))
	    else if (dskew > 0.0d0)
	        skew = dskew ** (1.0d0 / 3.0d0)
	    else
		skew = 0.0
	}
	mean = dmean + sky_zero
end
