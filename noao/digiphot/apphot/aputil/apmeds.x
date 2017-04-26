# APSMED -- Compute the averaged median given a sorted array and an averaging
# half width.

real procedure apsmed (pix, index, npts, medcut)

real	pix[ARB]	# array of sky pixels
int	index[ARB]	# sorted index array
int	npts		# number of pixels
int	medcut		# averaging half width

int	med, j, nmed, medlo, medhi
real	sumed

begin
	med = (npts + 1) / 2
	if (mod (med, 2) == 1) {
	    medlo = max (1, med - medcut)
	    medhi = min (npts, med + medcut)
	} else {
	    medlo = max (1, med - medcut)
	    medhi = min (npts, med + medcut + 1)
	}

	sumed = 0.0
	nmed = 0
	do j = medlo, medhi {
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

int	j, nmed, maxmed
real	sumed

begin
	sumed = pix[index[med]]
	if (mod (med, 2) == 1)
	    maxmed = 2 * medcut + 1 
	else
	    maxmed = 2 * medcut + 2 

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
	    if (nmed >= maxmed)
		break
	    if (weight[index[j]] > 0.0) {
		sumed = sumed + pix[index[j]]
		nmed = nmed + 1
	    }
	}

	return (sumed / nmed)
end


# APMEDR -- Vector median selection. The selection is carried out in a temporary
# array, leaving the input vector unmodified.  Especially demanding applications
# may wish to call the asok routine directory to avoid the call to the memory
# allocator.

real procedure apmedr (a, index, npix)

real	a[ARB]				# input array of values
int	index[ARB]			# sorted index array
int	npix				# number of pixels

int	i
pointer	sp, aa
real	median
real	asokr()		# select the Kth smallest element from A

begin
	switch (npix) {
	case 1, 2:
	    return (a[1])

	case 3:
	    if (a[1] < a[2]) {
		if (a[2] < a[3])
		    return (a[2])
		else if (a[1] < a[3])
		    return (a[3])
		else
		    return (a[1])
	    } else {
		if (a[2] > a[3])
		    return (a[2])
		else if (a[1] < a[3])
		    return (a[1])
		else
		    return (a[3])
	    }

	default:
	    call smark (sp)
	    call salloc (aa, npix, TY_REAL)
	    do i = 1, npix
		Memr[aa+i-1] = a[index[i]]
	    median = asokr (Memr[aa], npix, (npix + 1) / 2)
	    call sfree (sp)

	    return (median)
	}
end
