# APMOMENTS -- Procedure to compute the first three moments of a
# distribution given the appropriate sums.

procedure apmoments (sumpx, sumsqpx, sumcbpx, npix, mean, sigma, skew)

double	sumpx		# sum of the pixels
double	sumsqpx		# sum of pixels squared
double	sumcbpx		# sum of cubes of pixels
int	npix		# number of pixels
real	mean		# mean of pixels
real	sigma		# sigma of pixels
real	skew		# skew of pixels

bool	fp_equalr()

begin
	# Recompute the moments.
	mean = sumpx / npix
	sigma = sumsqpx / npix - mean ** 2
	if (sigma <= 0.0) {
	    sigma = 0.0
	    skew = 0.0
	} else {
	    skew = sumcbpx / npix - 3.0 * mean * sigma - mean ** 3
	    sigma = sqrt (sigma)
	    if (fp_equalr (skew, 0.0))
		skew = 0.0
	    else if (skew < 0.0)
	        skew = - (abs (skew) ** (1.0 / 3.0))
	    else
	        skew = skew ** (1.0 / 3.0)
	}
end


# APFMOMENTS -- Procedure to compute the first three moments of a distribution
# given the data. The sums are returned as well.

procedure apfmoments (pix, npix, sumpx, sumsqpx, sumcbpx, mean, sigma, skew)

real	pix[npix]	# array of pixels
int	npix		# number of pixels
double	sumpx		# sum of the pixels
double	sumsqpx		# sum of pixels squared
double	sumcbpx		# sum of cubes of pixels
real	mean		# mean of pixels
real	sigma		# sigma of pixels
real	skew		# skew of pixels

bool	fp_equalr()
int	i

begin
	# Zero and accumulate the sums.
	sumpx = 0.0d0
	sumsqpx = 0.0d0
	sumcbpx = 0.0d0
	do i = 1, npix {
	    sumpx = sumpx + pix[i]
	    sumsqpx = sumsqpx + pix[i] ** 2
	    sumcbpx = sumcbpx + pix[i] ** 3
	}

	# Compute the moments.
	mean = sumpx / npix
	sigma = sumsqpx / npix - mean ** 2
	if (sigma <= 0.0) {
	    sigma = 0.0
	    skew = 0.0
	} else {
	    skew = sumcbpx / npix - 3.0 * mean * sigma - mean ** 3
	    sigma = sqrt (sigma)
	    if (fp_equalr (skew, 0.0))
		skew = 0.0
	    else if (skew < 0.0)
	        skew = - (abs (skew) ** (1.0 / 3.0))
	    else
	        skew = skew ** (1.0 / 3.0)
	}
end
