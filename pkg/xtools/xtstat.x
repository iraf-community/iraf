# XT_STAT -- Compute statistics from a sample.
#
# The sample array will be sorted.

define	NMIN	10	# Minimum number of pixels for mode calculation
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.


procedure xt_stats (sample, nsample, frac, mean, sigma, median, mode)

short	sample[nsample]			#I Sample
int	nsample				#I Number of sample pixels
real	frac				#I Fraction of data to use
real	mean, sigma, median, mode	#O Statistics

int	i, j, k, nmax
real	z1, z2, zstep, zbin
bool	fp_equalr()

begin
	# Sort the sample.
	call asrts (sample, sample, nsample)

	# Set fraction to use.
	i = max (1, 1 + nsample * (1. - frac) / 2.)
	j = min (nsample, 1 + nsample * (1. + frac) / 2.)
	z1 = sample[i]
	z2 = sample[j]

	# Compute the mean and sigma.
	call aavgs (sample[i], j-i+1, mean, sigma)

	# Compute the median.
	median =  sample[nsample/2]

	z1 = median - 2 * sigma
	if (z1 < sample[1])
	    i = 1
	else {
	    k = i
	    do i = k, 2, -1 {
		if (sample[i] <= z1)
		    break
	    }
	}
	z1 = sample[i]

	z2 = median + 2 * sigma
	if (z2 > sample[nsample])
	    i = nsample
	else {
	    k = j
	    do j = k, nsample-1 {
		if (sample[j] >= z1)
		    break
	    }
	}
	z2 = sample[j]

	# Compute the mode.

	if (nsample < NMIN)
	    mode = median

	else if (fp_equalr (z1, z2))
	    mode = z1

	else {
	    zstep = ZSTEP * sigma
	    zbin = ZBIN * sigma
	    zstep = max (1., zstep)
	    zbin = max (1., zbin)

	    z1 = z1 - zstep
	    k = i
	    nmax = 0
	    repeat {
		z1 = z1 + zstep
		z2 = z1 + zbin
		for (; i < j && sample[i] < z1; i=i+1)
		    ;
		for (; k < j && sample[k] < z2; k=k+1)
		    ;
		if (k - i > nmax) {
		    nmax = k - i
		    mode = sample[(i+k)/2]
		}
	    } until (k >= j)
	}
end

procedure xt_stati (sample, nsample, frac, mean, sigma, median, mode)

int	sample[nsample]			#I Sample
int	nsample				#I Number of sample pixels
real	frac				#I Fraction of data to use
real	mean, sigma, median, mode	#O Statistics

int	i, j, k, nmax
real	z1, z2, zstep, zbin
bool	fp_equalr()

begin
	# Sort the sample.
	call asrti (sample, sample, nsample)

	# Set fraction to use.
	i = max (1, 1 + nsample * (1. - frac) / 2.)
	j = min (nsample, 1 + nsample * (1. + frac) / 2.)
	z1 = sample[i]
	z2 = sample[j]

	# Compute the mean and sigma.
	call aavgi (sample[i], j-i+1, mean, sigma)

	# Compute the median.
	median =  sample[nsample/2]

	z1 = median - 2 * sigma
	if (z1 < sample[1])
	    i = 1
	else {
	    k = i
	    do i = k, 2, -1 {
		if (sample[i] <= z1)
		    break
	    }
	}
	z1 = sample[i]

	z2 = median + 2 * sigma
	if (z2 > sample[nsample])
	    i = nsample
	else {
	    k = j
	    do j = k, nsample-1 {
		if (sample[j] >= z1)
		    break
	    }
	}
	z2 = sample[j]

	# Compute the mode.

	if (nsample < NMIN)
	    mode = median

	else if (fp_equalr (z1, z2))
	    mode = z1

	else {
	    zstep = ZSTEP * sigma
	    zbin = ZBIN * sigma
	    zstep = max (1., zstep)
	    zbin = max (1., zbin)

	    z1 = z1 - zstep
	    k = i
	    nmax = 0
	    repeat {
		z1 = z1 + zstep
		z2 = z1 + zbin
		for (; i < j && sample[i] < z1; i=i+1)
		    ;
		for (; k < j && sample[k] < z2; k=k+1)
		    ;
		if (k - i > nmax) {
		    nmax = k - i
		    mode = sample[(i+k)/2]
		}
	    } until (k >= j)
	}
end

procedure xt_statr (sample, nsample, frac, mean, sigma, median, mode)

real	sample[nsample]			#I Sample
int	nsample				#I Number of sample pixels
real	frac				#I Fraction of data to use
real	mean, sigma, median, mode	#O Statistics

int	i, j, k, nmax
real	z1, z2, zstep, zbin
bool	fp_equalr()

begin
	# Sort the sample.
	call asrtr (sample, sample, nsample)

	# Set fraction to use.
	i = max (1, 1 + nsample * (1. - frac) / 2.)
	j = min (nsample, 1 + nsample * (1. + frac) / 2.)
	z1 = sample[i]
	z2 = sample[j]

	# Compute the mean and sigma.
	call aavgr (sample[i], j-i+1, mean, sigma)

	# Compute the median.
	median =  sample[nsample/2]

	z1 = median - 2 * sigma
	if (z1 < sample[1])
	    i = 1
	else {
	    k = i
	    do i = k, 2, -1 {
		if (sample[i] <= z1)
		    break
	    }
	}
	z1 = sample[i]

	z2 = median + 2 * sigma
	if (z2 > sample[nsample])
	    i = nsample
	else {
	    k = j
	    do j = k, nsample-1 {
		if (sample[j] >= z1)
		    break
	    }
	}
	z2 = sample[j]

	# Compute the mode.

	if (nsample < NMIN)
	    mode = median

	else if (fp_equalr (z1, z2))
	    mode = z1

	else {
	    zstep = ZSTEP * sigma
	    zbin = ZBIN * sigma

	    z1 = z1 - zstep
	    k = i
	    nmax = 0
	    repeat {
		z1 = z1 + zstep
		z2 = z1 + zbin
		for (; i < j && sample[i] < z1; i=i+1)
		    ;
		for (; k < j && sample[k] < z2; k=k+1)
		    ;
		if (k - i > nmax) {
		    nmax = k - i
		    mode = sample[(i+k)/2]
		}
	    } until (k >= j)
	}
end

procedure xt_statd (sample, nsample, frac, mean, sigma, median, mode)

double	sample[nsample]			#I Sample
int	nsample				#I Number of sample pixels
real	frac				#I Fraction of data to use
double	mean, sigma, median, mode	#O Statistics

int	i, j, k, nmax
double	z1, z2, zstep, zbin
bool	fp_equald()

begin
	# Sort the sample.
	call asrtd (sample, sample, nsample)

	# Set fraction to use.
	i = max (1, 1 + nsample * (1. - frac) / 2.)
	j = min (nsample, 1 + nsample * (1. + frac) / 2.)
	z1 = sample[i]
	z2 = sample[j]

	# Compute the mean and sigma.
	call aavgd (sample[i], j-i+1, mean, sigma)

	# Compute the median.
	median =  sample[nsample/2]

	z1 = median - 2 * sigma
	if (z1 < sample[1])
	    i = 1
	else {
	    k = i
	    do i = k, 2, -1 {
		if (sample[i] <= z1)
		    break
	    }
	}
	z1 = sample[i]

	z2 = median + 2 * sigma
	if (z2 > sample[nsample])
	    i = nsample
	else {
	    k = j
	    do j = k, nsample-1 {
		if (sample[j] >= z1)
		    break
	    }
	}
	z2 = sample[j]

	# Compute the mode.

	if (nsample < NMIN)
	    mode = median

	else if (fp_equald (z1, z2))
	    mode = z1

	else {
	    zstep = ZSTEP * sigma
	    zbin = ZBIN * sigma

	    z1 = z1 - zstep
	    k = i
	    nmax = 0
	    repeat {
		z1 = z1 + zstep
		z2 = z1 + zbin
		for (; i < j && sample[i] < z1; i=i+1)
		    ;
		for (; k < j && sample[k] < z2; k=k+1)
		    ;
		if (k - i > nmax) {
		    nmax = k - i
		    mode = sample[(i+k)/2]
		}
	    } until (k >= j)
	}
end

