# APSMED -- Compute the averaged median given a sorted array and an averaging
# half width.

real procedure apsmed (pix, index, npts, medcut)

real	pix[ARB]	# array of sky pixels
int	index[ARB]	# sorted index array
int	npts		# number of pixels
int	medcut		# averaging half width

int	med, j, nmed
real	sumed

begin
	sumed = 0.0
	med = (npts + 1) / 2

	nmed = 0
	do j = max (1, med - medcut), min (npts, med + medcut) {
	    sumed = sumed + pix[index[j]]
	    nmed = nmed + 1
	}

	return (sumed / nmed)
end


# APIMED -- Compute the index of new median value. Weight is an arbitrary
# weight array which is assumed to be zero if the pixels has been rejected
# and is positive otherwise.

int procedure apimed (weight, index, lo, hi, nmed)

real	weight[ARB]		# array of weights
int	index[ARB]		# array of sorted indices
int	lo, hi			# ranges of weights
int	nmed			# number of good sky pixels

int	npts, med

begin
	npts = 0
	for (med = lo; med <= hi && npts < nmed; med = med + 1) {
	    if (weight[index[med]] > 0.0)
		npts = npts + 1
	}

	if (npts == 0)
	    return (0)
	else
	    return (med)
end


# APWSMED -- Compute the new averaged median given a sorted input array,
# an averaging half-width, and assuming that there has been pixel rejection.

real procedure apwsmed (pix, index, weight, npix, med, medcut)

real	pix[ARB]		# pixel values array
int	index[ARB]		# sorted indices array
real	weight[ARB]		# the weights array
int	npix			# number of pixels
int	med			# index of median value
int	medcut			# of median cut

int	j, nmed
real	sumed

begin
	sumed = pix[index[med]]

	nmed = 1
	for (j = med - 1; j >= 1; j = j - 1) {
	    if (nmed >=  medcut + 1)
		break
	    if (weight[index[j]] > 0.0) {
		sumed = sumed + pix[index[j]]
		nmed = nmed + 1
	    }
	}
	for (j = med + 1; j <= npix; j = j + 1) {
	    if (nmed >= 2 * medcut + 1)
		break
	    if (weight[index[j]] > 0.0) {
		sumed = sumed + pix[index[j]]
		nmed = nmed + 1
	    }
	}

	return (sumed / nmed)
end
