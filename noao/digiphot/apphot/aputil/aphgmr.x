# AHGMR -- Accumulate the histogram of the input vector.  The output vector
# hmg (the histogram) should be cleared prior to the first call. The procedure
# returns the number of data values it could not include in the header.

int procedure aphgmr (data, wgt, npix, hgm, nbins, z1, z2)

real 	data[ARB]		# data vector
real	wgt[ARB]		# weights
int	npix			# number of pixels
real	hgm[ARB]		# output histogram
int	nbins			# number of bins in histogram
real	z1, z2			# greyscale values of first and last bins

real	dz
int	bin, i, nreject

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
	    bin = int ((data[i] - z1) * dz) + 1
	    hgm[bin] = hgm[bin] + 1.0
	}

	return (nreject)
end
